
;; title: Bondex
;; version:
;; summary:
;; description:


(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_BOND_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_BOND_NOT_MATURE (err u103))
(define-constant ERR_BOND_ALREADY_REDEEMED (err u104))
(define-constant ERR_INVALID_AMOUNT (err u105))
(define-constant ERR_BOND_EXPIRED (err u106))
(define-constant ERR_INVALID_PARAMETERS (err u107))
(define-constant ERR_TRANSFER_FAILED (err u108))

(define-data-var next-bond-id uint u1)
(define-data-var platform-fee-rate uint u250)

(define-map bonds
  { bond-id: uint }
  {
    issuer: principal,
    face-value: uint,
    coupon-rate: uint,
    maturity-block: uint,
    issue-block: uint,
    total-supply: uint,
    redeemed: bool,
    active: bool
  }
)

(define-map bond-balances
  { bond-id: uint, holder: principal }
  { balance: uint }
)

(define-map bond-allowances
  { bond-id: uint, owner: principal, spender: principal }
  { allowance: uint }
)

(define-map issuer-profiles
  { issuer: principal }
  {
    company-name: (string-ascii 50),
    total-bonds-issued: uint,
    total-amount-raised: uint,
    credit-score: uint
  }
)

(define-public (issue-bond (face-value uint) (coupon-rate uint) (maturity-blocks uint) (total-supply uint) (company-name (string-ascii 50)))
  (let
    (
      (bond-id (var-get next-bond-id))
      (maturity-block (+ stacks-block-height maturity-blocks))
    )
    (asserts! (> face-value u0) ERR_INVALID_PARAMETERS)
    (asserts! (> total-supply u0) ERR_INVALID_PARAMETERS)
    (asserts! (> maturity-blocks u0) ERR_INVALID_PARAMETERS)
    (asserts! (<= coupon-rate u10000) ERR_INVALID_PARAMETERS)
    
    (map-set bonds
      { bond-id: bond-id }
      {
        issuer: tx-sender,
        face-value: face-value,
        coupon-rate: coupon-rate,
        maturity-block: maturity-block,
        issue-block: stacks-block-height,
        total-supply: total-supply,
        redeemed: false,
        active: true
      }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: total-supply }
    )
    
    (map-set issuer-profiles
      { issuer: tx-sender }
      {
        company-name: company-name,
        total-bonds-issued: (+ (get-issuer-bond-count tx-sender) u1),
        total-amount-raised: (+ (get-issuer-total-raised tx-sender) (* face-value total-supply)),
        credit-score: u750
      }
    )
    
    (var-set next-bond-id (+ bond-id u1))
    (ok bond-id)
  )
)

(define-public (transfer-bond (bond-id uint) (amount uint) (recipient principal))
  (let
    (
      (sender-balance (get-bond-balance bond-id tx-sender))
      (recipient-balance (get-bond-balance bond-id recipient))
    )
    (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (is-some (map-get? bonds { bond-id: bond-id })) ERR_BOND_NOT_FOUND)
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: (- sender-balance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: recipient }
      { balance: (+ recipient-balance amount) }
    )
    
    (ok true)
  )
)

(define-public (approve-bond (bond-id uint) (spender principal) (amount uint))
  (begin
    (asserts! (is-some (map-get? bonds { bond-id: bond-id })) ERR_BOND_NOT_FOUND)
    (map-set bond-allowances
      { bond-id: bond-id, owner: tx-sender, spender: spender }
      { allowance: amount }
    )
    (ok true)
  )
)

(define-public (transfer-from-bond (bond-id uint) (owner principal) (recipient principal) (amount uint))
  (let
    (
      (allowance (get-bond-allowance bond-id owner tx-sender))
      (owner-balance (get-bond-balance bond-id owner))
      (recipient-balance (get-bond-balance bond-id recipient))
    )
    (asserts! (>= allowance amount) ERR_UNAUTHORIZED)
    (asserts! (>= owner-balance amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    
    (map-set bond-allowances
      { bond-id: bond-id, owner: owner, spender: tx-sender }
      { allowance: (- allowance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: owner }
      { balance: (- owner-balance amount) }
    )
    
    (map-set bond-balances
      { bond-id: bond-id, holder: recipient }
      { balance: (+ recipient-balance amount) }
    )
    
    (ok true)
  )
)

(define-public (redeem-bond (bond-id uint))
  (let
    (
      (bond-data (unwrap! (map-get? bonds { bond-id: bond-id }) ERR_BOND_NOT_FOUND))
      (holder-balance (get-bond-balance bond-id tx-sender))
      (redemption-amount (calculate-redemption-value bond-id holder-balance))
    )
    (asserts! (>= stacks-block-height (get maturity-block bond-data)) ERR_BOND_NOT_MATURE)
    (asserts! (not (get redeemed bond-data)) ERR_BOND_ALREADY_REDEEMED)
    (asserts! (> holder-balance u0) ERR_INSUFFICIENT_FUNDS)
    
    (try! (stx-transfer? redemption-amount (get issuer bond-data) tx-sender))
    
    (map-set bond-balances
      { bond-id: bond-id, holder: tx-sender }
      { balance: u0 }
    )
    
    (ok redemption-amount)
  )
)

(define-public (purchase-bond (bond-id uint) (amount uint))
  (let
    (
      (bond-data (unwrap! (map-get? bonds { bond-id: bond-id }) ERR_BOND_NOT_FOUND))
      (issuer-balance (get-bond-balance bond-id (get issuer bond-data)))
      (purchase-price (* amount (get face-value bond-data)))
      (platform-fee (/ (* purchase-price (var-get platform-fee-rate)) u10000))
      (issuer-amount (- purchase-price platform-fee))
    )
    (asserts! (>= issuer-balance amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (get active bond-data) ERR_BOND_EXPIRED)
    
    (try! (stx-transfer? purchase-price tx-sender (get issuer bond-data)))
    
    (try! (transfer-bond bond-id amount tx-sender))
    
    (ok true)
  )
)

(define-public (set-platform-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-rate u1000) ERR_INVALID_PARAMETERS)
    (var-set platform-fee-rate new-fee-rate)
    (ok true)
  )
)

(define-read-only (get-bond-info (bond-id uint))
  (map-get? bonds { bond-id: bond-id })
)

(define-read-only (get-bond-balance (bond-id uint) (holder principal))
  (default-to u0 (get balance (map-get? bond-balances { bond-id: bond-id, holder: holder })))
)

(define-read-only (get-bond-allowance (bond-id uint) (owner principal) (spender principal))
  (default-to u0 (get allowance (map-get? bond-allowances { bond-id: bond-id, owner: owner, spender: spender })))
)

(define-read-only (get-issuer-profile (issuer principal))
  (map-get? issuer-profiles { issuer: issuer })
)

(define-read-only (calculate-redemption-value (bond-id uint) (amount uint))
  (let
    (
      (bond-data (unwrap! (map-get? bonds { bond-id: bond-id }) u0))
      (face-value (get face-value bond-data))
      (coupon-rate (get coupon-rate bond-data))
      (blocks-held (- stacks-block-height (get issue-block bond-data)))
      (interest (/ (* (* face-value coupon-rate) blocks-held) u1000000))
    )
    (* amount (+ face-value interest))
  )
)

(define-read-only (get-current-bond-price (bond-id uint))
  (let
    (
      (bond-data (unwrap! (map-get? bonds { bond-id: bond-id }) u0))
      (blocks-to-maturity (- (get maturity-block bond-data) stacks-block-height))
      (discount-rate u500)
    )
    (if (> blocks-to-maturity u0)
      (- (get face-value bond-data) (/ (* (get face-value bond-data) discount-rate blocks-to-maturity) u1000000))
      (get face-value bond-data)
    )
  )
)

(define-read-only (get-issuer-bond-count (issuer principal))
  (default-to u0 (get total-bonds-issued (map-get? issuer-profiles { issuer: issuer })))
)

(define-read-only (get-issuer-total-raised (issuer principal))
  (default-to u0 (get total-amount-raised (map-get? issuer-profiles { issuer: issuer })))
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (get-next-bond-id)
  (var-get next-bond-id)
)

(define-read-only (is-bond-mature (bond-id uint))
  (let
    (
      (bond-data (unwrap! (map-get? bonds { bond-id: bond-id }) false))
    )
    (>= stacks-block-height (get maturity-block bond-data))
  )
)

(define-read-only (get-bond-yield (bond-id uint))
  (let
    (
      (bond-data (unwrap! (map-get? bonds { bond-id: bond-id }) u0))
      (current-price (get-current-bond-price bond-id))
      (face-value (get face-value bond-data))
      (coupon-rate (get coupon-rate bond-data))
    )
    (if (> current-price u0)
      (+ coupon-rate (/ (* (- face-value current-price) u10000) current-price))
      u0
    )
  )
)