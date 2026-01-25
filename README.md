# 🏦 P2P Lending Pool MVP

Welcome to the **P2P Lending Pool**, a decentralized simplified lending protocol on Stacks! 🚀

This project demonstrates core DeFi concepts using Clarity:
- **Lending**: Users can deposit STX into a shared pool.
- **Borrowing**: Borrowers use STX collateral to borrow from the pool.
- **Interest**: Auto-accruing interest calculated per block.
- **Liquidation**: Unhealthy loans can be liquidated to protect the pool.

## 📦 Features

- 💸 **Lend STX**: Earn (theoretical) interest by supplying liquidity.
- 🤝 **Borrow STX**: Over-collateralized loans (150% ratio).
- 📉 **Liquidations**: Secure the protocol by liquidating under-collateralized positions.
- ⚙️ **Admin Controls**: Adjustable rates and ratios.

## 🛠 Usage

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed.

### Run Tests
```bash
clarinet check
```

### Deployment
1. **Lend**: Call `lend` with amount.
2. **Borrow**: Call `borrow` with loan amount and collateral.
3. **Repay**: Call `repay` to unlock collateral.

## 📄 Contract Interface
- `lend (amount uint)`
- `withdraw-funds (amount uint)`
- `borrow (amount uint) (collateral-amount uint)`
- `repay (repay-amount uint)`
- `liquidate (borrower principal)`

---
**One-line Commit**:
`feat: init p2p lending pool mvp with core borrowing logic`

**PR Title**:
`feat: Add P2P Lending Pool MVP Contracts`

**PR Description**:
`This PR introduces the MVP for the P2P Lending Pool. It includes the main `p2p-lending` contract with deposit, borrow, repay, and liquidation functionalities. It implements a basic interest accrual model based on block height and includes admin controls for protocol parameters.`
