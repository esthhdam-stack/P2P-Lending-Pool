
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1001))
(define-constant ERR-LOAN-NOT-FOUND (err u1002))
(define-constant ERR-LOAN-ACTIVE (err u1003))
(define-constant ERR-POOL-EMPTY (err u1004))
(define-constant ERR-INVALID-AMOUNT (err u1005))
(define-constant ERR-HEALTHY-LOAN (err u1006))

(define-data-var pool-total-assets uint u0)
(define-data-var total-borrowed-assets uint u0)
(define-data-var interest-rate-per-block uint u5) ;; 0.05% basis points representation or similar simple scale
(define-data-var liquidation-threshold uint u80) ;; 80% LTV
(define-data-var collateral-ratio uint u150) ;; 150% collateral required

(define-map lenders principal uint)
(define-map loans principal {
    amount: uint,
    collateral: uint,
    start-height: uint,
    last-interaction: uint
})

(define-private (calculate-interest (amount uint) (blocks-elapsed uint))
    (let
        (
            (rate (var-get interest-rate-per-block))
            (interest-accumulated (/ (* amount (* rate blocks-elapsed)) u10000))
        )
        interest-accumulated
    )
)

(define-read-only (get-pool-balance)
    (ok (var-get pool-total-assets))
)

(define-read-only (get-total-borrowed)
    (ok (var-get total-borrowed-assets))
)

(define-read-only (get-lender-balance (lender principal))
    (default-to u0 (map-get? lenders lender))
)

(define-read-only (get-loan-data (borrower principal))
    (map-get? loans borrower)
)

(define-public (lend (amount uint))
    (let
        (
            (current-balance (default-to u0 (map-get? lenders tx-sender)))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set lenders tx-sender (+ current-balance amount))
        (var-set pool-total-assets (+ (var-get pool-total-assets) amount))
        (ok amount)
    )
)

(define-public (withdraw-funds (amount uint))
    (let
        (
            (current-balance (default-to u0 (map-get? lenders tx-sender)))
            (pool-available (- (var-get pool-total-assets) (var-get total-borrowed-assets)))
        )
        (asserts! (>= current-balance amount) ERR-INSUFFICIENT-FUNDS)
        (asserts! (>= pool-available amount) ERR-POOL-EMPTY)
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set lenders tx-sender (- current-balance amount))
        (var-set pool-total-assets (- (var-get pool-total-assets) amount))
        (ok amount)
    )
)

(define-public (borrow (amount uint) (collateral-amount uint))
    (let
        (
            (pool-available (- (var-get pool-total-assets) (var-get total-borrowed-assets)))
            (existing-loan (map-get? loans tx-sender))
            (required-collateral (/ (* amount (var-get collateral-ratio)) u100))
        )
        (asserts! (is-none existing-loan) ERR-LOAN-ACTIVE)
        (asserts! (>= pool-available amount) ERR-POOL-EMPTY)
        (asserts! (>= collateral-amount required-collateral) ERR-INSUFFICIENT-FUNDS)
        
        ;; Transfer collateral to contract
        (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
        
        ;; Transfer borrowed amount to borrower from pool (conceptually, though they are same asset here so it's a bit circular but shows logic)
        ;; To make this meaningful in a single-asset demo, we assume the 'collateral' is locked separatedly from the pool or just track it.
        ;; For this MVP self-contained logic: we just lock the collateral effectively by keeping it in the contract under 'loans' map.
        ;; And then we send them the borrowed amount.
        ;; WAIT: If I lend 100 STX collateral to borrow 50 STX, I just sent 100 STX to contract. Then contract sends me 50 STX. Net -50 STX.
        ;; This works for a "Lending Pool" logic demo.
        
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        
        (map-set loans tx-sender {
            amount: amount,
            collateral: collateral-amount,
            start-height: block-height,
            last-interaction: block-height
        })
        (var-set total-borrowed-assets (+ (var-get total-borrowed-assets) amount))
        (ok amount)
    )
)

(define-public (repay (repay-amount uint))
    (let
        (
            (loan (unwrap! (map-get? loans tx-sender) ERR-LOAN-NOT-FOUND))
            (principal-amt (get amount loan))
            (collateral-amt (get collateral loan))
            (start-h (get start-height loan))
            (interest (calculate-interest principal-amt (- block-height start-h)))
            (total-due (+ principal-amt interest))
        )
        (asserts! (>= repay-amount total-due) ERR-INSUFFICIENT-FUNDS)
        
        ;; Borrower sends Repayment Amount
        (try! (stx-transfer? total-due tx-sender (as-contract tx-sender)))
        
        ;; Contract returns Collateral
        (try! (as-contract (stx-transfer? collateral-amt tx-sender tx-sender)))
        
        (var-set total-borrowed-assets (- (var-get total-borrowed-assets) principal-amt))
        (var-set pool-total-assets (+ (var-get pool-total-assets) interest)) 
        
        (map-delete loans tx-sender)
        (ok total-due)
    )
)

(define-public (liquidate (borrower principal))
    (let
        (
            (loan (unwrap! (map-get? loans borrower) ERR-LOAN-NOT-FOUND))
            (principal-amt (get amount loan))
            (collateral-amt (get collateral loan))
            (start-h (get start-height loan))
            (interest (calculate-interest principal-amt (- block-height start-h)))
            (total-due (+ principal-amt interest))
            ;; If collateral value < total due * margin, can liquidate
            ;; Simplified: if total-due > collateral * 0.9 (threshold)
            (threshold-factor (var-get liquidation-threshold))
            (health-benchmark (/ (* collateral-amt threshold-factor) u100))
        )
        ;; If debt is too high compared to collateral
        (asserts! (> total-due health-benchmark) ERR-HEALTHY-LOAN)
        
        ;; Liquidator pays the debt (principal + interest)
        (try! (stx-transfer? total-due tx-sender (as-contract tx-sender)))
        
        ;; Liquidator gets the collateral
        (try! (as-contract (stx-transfer? collateral-amt tx-sender tx-sender)))
        
        (var-set total-borrowed-assets (- (var-get total-borrowed-assets) principal-amt))
        (var-set pool-total-assets (+ (var-get pool-total-assets) interest))
        
        (map-delete loans borrower)
        (ok collateral-amt)
    )
)

(define-public (add-liquidity-rewards (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set pool-total-assets (+ (var-get pool-total-assets) amount))
        (ok amount)
    )
)

;; Admin functions for parameters

(define-data-var contract-owner principal tx-sender)

(define-public (set-interest-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set interest-rate-per-block new-rate)
        (ok new-rate)
    )
)

(define-public (set-collateral-ratio (new-ratio uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set collateral-ratio new-ratio)
        (ok new-ratio)
    )
)

(define-public (set-liquidation-threshold (new-threshold uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set liquidation-threshold new-threshold)
        (ok new-threshold)
    )
)

(define-read-only (get-current-interest-accrued (borrower principal))
    (let
        (
            (loan (unwrap! (map-get? loans borrower) (err u0)))
            (amt (get amount loan))
            (start (get start-height loan))
        )
        (ok (calculate-interest amt (- block-height start)))
    )
)

(define-read-only (get-health-factor (borrower principal))
    (let
        (
            (loan (unwrap! (map-get? loans borrower) (err u0)))
            (collateral-val (get collateral loan))
            (amt (get amount loan))
            (start (get start-height loan))
            (interest (calculate-interest amt (- block-height start)))
            (total-debt (+ amt interest))
        )
        (if (is-eq total-debt u0)
            (ok u999999) ;; infinite health
            (ok (/ (* collateral-val u100) total-debt))
        )
    )
)

;; Initialization check
(begin
    (print "P2P Lending Pool Contract Initialized")
)
