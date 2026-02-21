(define-trait flash-loan-receiver (
    (execute-operation
        (uint uint)
        (response bool uint)
    )
))

(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INSUFFICIENT-FUNDS (err u1001))
(define-constant ERR-LOAN-NOT-FOUND (err u1002))
(define-constant ERR-LOAN-ACTIVE (err u1003))
(define-constant ERR-POOL-EMPTY (err u1004))
(define-constant ERR-INVALID-AMOUNT (err u1005))
(define-constant ERR-HEALTHY-LOAN (err u1006))
(define-constant ERR-INVALID-FLASH-LOAN (err u1007))
(define-constant ERR-COLLATERAL-TOO-LOW (err u1008))
(define-constant FLASH-LOAN-FEE-BPS u5)

(define-data-var pool-total-assets uint u0)
(define-data-var total-borrowed-assets uint u0)
(define-data-var interest-rate-per-block uint u5)
(define-data-var penalty-rate-per-block uint u15)
(define-data-var grace-period-blocks uint u1440)
(define-data-var liquidation-threshold uint u80)
(define-data-var collateral-ratio uint u150)
(define-data-var contract-owner principal tx-sender)

(define-map lenders
    principal
    uint
)
(define-map loans
    principal
    {
        amount: uint,
        collateral: uint,
        start-height: uint,
        last-interaction: uint,
        due-block: uint,
        rate-at-borrow: uint,
    }
)

(define-private (calculate-interest
        (principal uint)
        (start-h uint)
        (due-h uint)
        (normal-rate uint)
        (penalty-rate uint)
        (current-h uint)
    )
    (if (<= current-h due-h)
        (/ (* principal (* normal-rate (- current-h start-h))) u10000)
        (let (
                (normal-blocks (- due-h start-h))
                (penalty-blocks (- current-h due-h))
                (normal-interest (/ (* principal (* normal-rate normal-blocks)) u10000))
                (penalty-interest (/ (* principal (* penalty-rate penalty-blocks)) u10000))
            )
            (+ normal-interest penalty-interest)
        )
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

(define-read-only (get-grace-period-blocks)
    (ok (var-get grace-period-blocks))
)

(define-read-only (get-penalty-rate-per-block)
    (ok (var-get penalty-rate-per-block))
)

(define-read-only (get-overdue-status (borrower principal))
    (let (
            (loan (unwrap! (map-get? loans borrower) (err u0)))
            (due-h (get due-block loan))
        )
        (if (> burn-block-height due-h)
            (ok { overdue: true, blocks-past-due: (- burn-block-height due-h) })
            (ok { overdue: false, blocks-past-due: u0 })
        )
    )
)

(define-read-only (get-current-interest-accrued (borrower principal))
    (let (
            (loan (unwrap! (map-get? loans borrower) (err u0)))
            (amt (get amount loan))
            (start (get start-height loan))
            (due-h (get due-block loan))
            (normal-rate (get rate-at-borrow loan))
            (penalty-rate (var-get penalty-rate-per-block))
        )
        (ok (calculate-interest amt start due-h normal-rate penalty-rate burn-block-height))
    )
)

(define-read-only (get-health-factor (borrower principal))
    (let (
            (loan (unwrap! (map-get? loans borrower) (err u0)))
            (collateral-val (get collateral loan))
            (amt (get amount loan))
            (start (get start-height loan))
            (due-h (get due-block loan))
            (normal-rate (get rate-at-borrow loan))
            (penalty-rate (var-get penalty-rate-per-block))
            (interest (calculate-interest amt start due-h normal-rate penalty-rate burn-block-height))
            (total-debt (+ amt interest))
        )
        (if (is-eq total-debt u0)
            (ok u999999)
            (ok (/ (* collateral-val u100) total-debt))
        )
    )
)

(define-public (lend (amount uint))
    (let ((current-balance (default-to u0 (map-get? lenders tx-sender))))
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set lenders tx-sender (+ current-balance amount))
        (var-set pool-total-assets (+ (var-get pool-total-assets) amount))
        (ok amount)
    )
)

(define-public (withdraw-funds (amount uint))
    (let (
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

(define-public (borrow
        (amount uint)
        (collateral-amount uint)
    )
    (let (
            (pool-available (- (var-get pool-total-assets) (var-get total-borrowed-assets)))
            (existing-loan (map-get? loans tx-sender))
            (required-collateral (/ (* amount (var-get collateral-ratio)) u100))
            (current-rate (var-get interest-rate-per-block))
            (due-h (+ burn-block-height (var-get grace-period-blocks)))
        )
        (asserts! (is-none existing-loan) ERR-LOAN-ACTIVE)
        (asserts! (>= pool-available amount) ERR-POOL-EMPTY)
        (asserts! (>= collateral-amount required-collateral) ERR-INSUFFICIENT-FUNDS)
        (try! (stx-transfer? collateral-amount tx-sender (as-contract tx-sender)))
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set loans tx-sender {
            amount: amount,
            collateral: collateral-amount,
            start-height: burn-block-height,
            last-interaction: burn-block-height,
            due-block: due-h,
            rate-at-borrow: current-rate,
        })
        (var-set total-borrowed-assets (+ (var-get total-borrowed-assets) amount))
        (ok amount)
    )
)

(define-public (repay (repay-amount uint))
    (let (
            (loan (unwrap! (map-get? loans tx-sender) ERR-LOAN-NOT-FOUND))
            (principal-amt (get amount loan))
            (collateral-amt (get collateral loan))
            (start-h (get start-height loan))
            (due-h (get due-block loan))
            (normal-rate (get rate-at-borrow loan))
            (penalty-rate (var-get penalty-rate-per-block))
            (interest (calculate-interest principal-amt start-h due-h normal-rate penalty-rate burn-block-height))
            (total-due (+ principal-amt interest))
        )
        (asserts! (>= repay-amount total-due) ERR-INSUFFICIENT-FUNDS)
        (try! (stx-transfer? total-due tx-sender (as-contract tx-sender)))
        (try! (as-contract (stx-transfer? collateral-amt tx-sender tx-sender)))
        (var-set total-borrowed-assets
            (- (var-get total-borrowed-assets) principal-amt)
        )
        (var-set pool-total-assets (+ (var-get pool-total-assets) interest))
        (map-delete loans tx-sender)
        (ok total-due)
    )
)

(define-public (liquidate (borrower principal))
    (let (
            (loan (unwrap! (map-get? loans borrower) ERR-LOAN-NOT-FOUND))
            (principal-amt (get amount loan))
            (collateral-amt (get collateral loan))
            (start-h (get start-height loan))
            (due-h (get due-block loan))
            (normal-rate (get rate-at-borrow loan))
            (penalty-rate (var-get penalty-rate-per-block))
            (interest (calculate-interest principal-amt start-h due-h normal-rate penalty-rate burn-block-height))
            (total-due (+ principal-amt interest))
            (threshold-factor (var-get liquidation-threshold))
            (health-benchmark (/ (* collateral-amt threshold-factor) u100))
        )
        (asserts! (> total-due health-benchmark) ERR-HEALTHY-LOAN)
        (try! (stx-transfer? total-due tx-sender (as-contract tx-sender)))
        (try! (as-contract (stx-transfer? collateral-amt tx-sender tx-sender)))
        (var-set total-borrowed-assets
            (- (var-get total-borrowed-assets) principal-amt)
        )
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

(define-public (flash-loan
        (amount uint)
        (recipient <flash-loan-receiver>)
    )
    (let (
            (pre-bal (stx-get-balance (as-contract tx-sender)))
            (fee (/ (* amount FLASH-LOAN-FEE-BPS) u10000))
            (total-repayment (+ amount fee))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (<= amount pre-bal) ERR-INSUFFICIENT-FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender (contract-of recipient))))
        (try! (contract-call? recipient execute-operation amount fee))
        (asserts! (>= (stx-get-balance (as-contract tx-sender)) (+ pre-bal fee))
            ERR-INVALID-FLASH-LOAN
        )
        (var-set pool-total-assets (+ (var-get pool-total-assets) fee))
        (ok amount)
    )
)

(define-public (deposit-collateral (amount uint))
    (let (
            (loan (unwrap! (map-get? loans tx-sender) ERR-LOAN-NOT-FOUND))
            (current-collateral (get collateral loan))
        )
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set loans tx-sender
            (merge loan { collateral: (+ current-collateral amount) })
        )
        (ok amount)
    )
)

(define-public (withdraw-collateral (amount uint))
    (let (
            (loan (unwrap! (map-get? loans tx-sender) ERR-LOAN-NOT-FOUND))
            (current-collateral (get collateral loan))
            (principal-amt (get amount loan))
            (start-h (get start-height loan))
            (due-h (get due-block loan))
            (normal-rate (get rate-at-borrow loan))
            (penalty-rate (var-get penalty-rate-per-block))
            (interest (calculate-interest principal-amt start-h due-h normal-rate penalty-rate burn-block-height))
            (total-debt (+ principal-amt interest))
            (required-collateral (/ (* total-debt (var-get collateral-ratio)) u100))
            (new-collateral (- current-collateral amount))
        )
        (asserts! (>= current-collateral amount) ERR-INSUFFICIENT-FUNDS)
        (asserts! (>= new-collateral required-collateral) ERR-COLLATERAL-TOO-LOW)
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set loans tx-sender (merge loan { collateral: new-collateral }))
        (ok amount)
    )
)

(define-data-var contract-owner-var principal tx-sender)

(define-public (set-interest-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set interest-rate-per-block new-rate)
        (ok new-rate)
    )
)

(define-public (set-penalty-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (>= new-rate (var-get interest-rate-per-block)) ERR-INVALID-AMOUNT)
        (var-set penalty-rate-per-block new-rate)
        (ok new-rate)
    )
)

(define-public (set-grace-period (new-blocks uint))
    (begin
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (> new-blocks u0) ERR-INVALID-AMOUNT)
        (var-set grace-period-blocks new-blocks)
        (ok new-blocks)
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

(begin
    (print "P2P Lending Pool Contract Initialized")
)