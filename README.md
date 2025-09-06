# 🏦 Bondex - Tokenized Bonds for SMEs

> 💼 Issue and trade small business debt through tokenized bonds on the Stacks blockchain

## 🌟 Overview

Bondex is a decentralized platform that enables Small and Medium Enterprises (SMEs) to issue tokenized bonds for raising capital. Investors can purchase, trade, and redeem these bonds, creating a liquid secondary market for SME debt.

## ✨ Key Features

- 🏭 **Bond Issuance**: SMEs can issue tokenized bonds with custom parameters
- 💰 **Trading**: Seamless transfer and trading of bond tokens
- 📈 **Yield Calculation**: Automatic interest and yield calculations
- 🔄 **Redemption**: Mature bonds can be redeemed for face value plus interest
- 👥 **Issuer Profiles**: Track company information and credit history
- 🛡️ **Security**: Built-in authorization and validation mechanisms

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd bondex
clarinet check
```

## 📋 Usage

### For Bond Issuers (SMEs)

#### 1. Issue a Bond 📊
```clarity
(contract-call? .Bondex issue-bond 
  u1000000    ;; face-value (1 STX)
  u500        ;; coupon-rate (5%)
  u52560      ;; maturity-blocks (~1 year)
  u100        ;; total-supply
  "TechCorp"  ;; company-name
)
```

#### 2. Check Issuer Profile 👤
```clarity
(contract-call? .Bondex get-issuer-profile 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

### For Investors

#### 1. Purchase Bonds 💳
```clarity
(contract-call? .Bondex purchase-bond 
  u1    ;; bond-id
  u10   ;; amount
)
```

#### 2. Transfer Bonds 🔄
```clarity
(contract-call? .Bondex transfer-bond 
  u1                                        ;; bond-id
  u5                                        ;; amount
  'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG  ;; recipient
)
```

#### 3. Redeem Mature Bonds 💎
```clarity
(contract-call? .Bondex redeem-bond u1)
```

### Read-Only Functions 📖

#### Get Bond Information
```clarity
(contract-call? .Bondex get-bond-info u1)
```

#### Check Bond Balance
```clarity
(contract-call? .Bondex get-bond-balance u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

#### Calculate Current Yield
````clarity
(contract-call? .Bon# Bondex)

