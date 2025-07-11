
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

(define-constant ERR_ORDER_NOT_FOUND (err u200))
(define-constant ERR_ORDER_EXPIRED (err u201))
(define-constant ERR_INVALID_ORDER_TYPE (err u202))
(define-constant ERR_AUCTION_NOT_ACTIVE (err u203))
(define-constant ERR_AUCTION_ENDED (err u204))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u205))
(define-constant ERR_ORDER_ALREADY_FILLED (err u206))
(define-constant ERR_INVALID_PRICE (err u207))
(define-constant ERR_AUCTION_NOT_FOUND (err u208))
(define-constant ERR_BID_TOO_LOW (err u209))
(define-constant ERR_AUCTION_ALREADY_SETTLED (err u210))

(define-data-var next-order-id uint u1)
(define-data-var next-auction-id uint u1)
(define-data-var marketplace-fee-rate uint u100)

(define-map marketplace-orders
  { order-id: uint }
  {
    bond-id: uint,
    maker: principal,
    order-type: (string-ascii 10),
    amount: uint,
    price: uint,
    filled-amount: uint,
    expiry-block: uint,
    active: bool,
    collateral-locked: uint
  }
)

(define-map bond-auctions
  { auction-id: uint }
  {
    bond-id: uint,
    seller: principal,
    amount: uint,
    min-price: uint,
    start-block: uint,
    end-block: uint,
    highest-bid: uint,
    highest-bidder: (optional principal),
    settled: bool,
    collateral-locked: uint
  }
)

(define-map auction-bids
  { auction-id: uint, bidder: principal }
  {
    bid-amount: uint,
    bid-price: uint,
    collateral-locked: uint
  }
)

(define-map user-collateral
  { user: principal }
  { amount: uint }
)

(define-map order-book
  { bond-id: uint, price: uint, order-type: (string-ascii 10) }
  { total-amount: uint, order-count: uint }
)

(define-public (deposit-collateral (amount uint))
  (let
    (
      (current-collateral (get-user-collateral tx-sender))
    )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set user-collateral
      { user: tx-sender }
      { amount: (+ current-collateral amount) }
    )
    (ok true)
  )
)

(define-public (withdraw-collateral (amount uint))
  (let
    (
      (current-collateral-amount (get-user-collateral tx-sender))
    )
    (asserts! (>= current-collateral-amount amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set user-collateral
      { user: tx-sender }
      { amount: (- current-collateral-amount amount) }
    )
    (ok true)
  )
)

(define-public (place-limit-order (bond-id uint) (order-type (string-ascii 10)) (amount uint) (price uint) (expiry-blocks uint))
  (let
    (
      (order-id (var-get next-order-id))
      (expiry-block (+ stacks-block-height expiry-blocks))
      (collateral-needed (if (is-eq order-type "buy") (* amount price) u0))
      (bond-collateral-needed (if (is-eq order-type "sell") amount u0))
      (user-collateral-amount (get-user-collateral tx-sender))
      (user-bond-balance (get-bond-balance bond-id tx-sender))
    )
    (asserts! (is-some (map-get? bonds { bond-id: bond-id })) ERR_BOND_NOT_FOUND)
    (asserts! (or (is-eq order-type "buy") (is-eq order-type "sell")) ERR_INVALID_ORDER_TYPE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> price u0) ERR_INVALID_PRICE)
    (asserts! (> expiry-blocks u0) ERR_INVALID_PARAMETERS)
    
    (asserts! (if (is-eq order-type "buy")
      (>= user-collateral-amount collateral-needed)
      (>= user-bond-balance bond-collateral-needed)
    ) (if (is-eq order-type "buy") ERR_INSUFFICIENT_COLLATERAL ERR_INSUFFICIENT_FUNDS))
    
    (map-set marketplace-orders
      { order-id: order-id }
      {
        bond-id: bond-id,
        maker: tx-sender,
        order-type: order-type,
        amount: amount,
        price: price,
        filled-amount: u0,
        expiry-block: expiry-block,
        active: true,
        collateral-locked: collateral-needed
      }
    )
    
    (if (is-eq order-type "buy")
      (map-set user-collateral
        { user: tx-sender }
        { amount: (- user-collateral-amount collateral-needed) }
      )
      true
    )
    
    (let ((ignore-result (update-order-book bond-id price order-type amount true)))
      (var-set next-order-id (+ order-id u1))
      (ok order-id)
    )
  )
)

(define-public (cancel-order (order-id uint))
  (let
    (
      (order-data (unwrap! (map-get? marketplace-orders { order-id: order-id }) ERR_ORDER_NOT_FOUND))
      (user-collateral-amount (get-user-collateral tx-sender))
    )
    (asserts! (is-eq tx-sender (get maker order-data)) ERR_UNAUTHORIZED)
    (asserts! (get active order-data) ERR_ORDER_ALREADY_FILLED)
    
    (map-set marketplace-orders
      { order-id: order-id }
      (merge order-data { active: false })
    )
    
    (if (is-eq (get order-type order-data) "buy")
      (map-set user-collateral
        { user: tx-sender }
        { amount: (+ user-collateral-amount (get collateral-locked order-data)) }
      )
      true
    )
    
    (let ((ignore-result (update-order-book 
      (get bond-id order-data) 
      (get price order-data) 
      (get order-type order-data) 
      (- (get amount order-data) (get filled-amount order-data))
      false
    )))
      (ok true)
    )
  )
)

(define-public (execute-market-order (bond-id uint) (order-type (string-ascii 10)) (amount uint))
  (let
    (
      (best-price (get-best-price bond-id order-type))
      (user-collateral-amount (get-user-collateral tx-sender))
      (user-bond-balance (get-bond-balance bond-id tx-sender))
      (required-collateral (if (is-eq order-type "buy") (* amount best-price) u0))
    )
    (asserts! (is-some (map-get? bonds { bond-id: bond-id })) ERR_BOND_NOT_FOUND)
    (asserts! (or (is-eq order-type "buy") (is-eq order-type "sell")) ERR_INVALID_ORDER_TYPE)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> best-price u0) ERR_ORDER_NOT_FOUND)
    
    (if (is-eq order-type "buy")
      (asserts! (>= user-collateral-amount required-collateral) ERR_INSUFFICIENT_COLLATERAL)
      (asserts! (>= user-bond-balance amount) ERR_INSUFFICIENT_FUNDS)
    )
    
    (try! (match-market-order bond-id order-type amount best-price))
    (ok true)
  )
)

(define-public (create-auction (bond-id uint) (amount uint) (min-price uint) (duration-blocks uint))
  (let
    (
      (auction-id (var-get next-auction-id))
      (start-block stacks-block-height)
      (end-block (+ stacks-block-height duration-blocks))
      (user-bond-balance (get-bond-balance bond-id tx-sender))
    )
    (asserts! (is-some (map-get? bonds { bond-id: bond-id })) ERR_BOND_NOT_FOUND)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (> min-price u0) ERR_INVALID_PRICE)
    (asserts! (> duration-blocks u0) ERR_INVALID_PARAMETERS)
    (asserts! (>= user-bond-balance amount) ERR_INSUFFICIENT_FUNDS)
    
    (map-set bond-auctions
      { auction-id: auction-id }
      {
        bond-id: bond-id,
        seller: tx-sender,
        amount: amount,
        min-price: min-price,
        start-block: start-block,
        end-block: end-block,
        highest-bid: u0,
        highest-bidder: none,
        settled: false,
        collateral-locked: amount
      }
    )
    
    (var-set next-auction-id (+ auction-id u1))
    (ok auction-id)
  )
)

(define-public (place-auction-bid (auction-id uint) (bid-price uint))
  (let
    (
      (auction-data (unwrap! (map-get? bond-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (user-collateral-amount (get-user-collateral tx-sender))
      (bid-amount (get amount auction-data))
      (collateral-needed (* bid-amount bid-price))
      (current-highest-bid (get highest-bid auction-data))
    )
    (asserts! (< stacks-block-height (get end-block auction-data)) ERR_AUCTION_ENDED)
    (asserts! (>= stacks-block-height (get start-block auction-data)) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (not (get settled auction-data)) ERR_AUCTION_ALREADY_SETTLED)
    (asserts! (> bid-price (get min-price auction-data)) ERR_BID_TOO_LOW)
    (asserts! (> bid-price current-highest-bid) ERR_BID_TOO_LOW)
    (asserts! (>= user-collateral-amount collateral-needed) ERR_INSUFFICIENT_COLLATERAL)
    
    (if (is-some (get highest-bidder auction-data))
      (let
        (
          (previous-bidder (unwrap-panic (get highest-bidder auction-data)))
          (previous-collateral-amount (get-user-collateral previous-bidder))
          (previous-collateral-locked (* bid-amount current-highest-bid))
        )
        (map-set user-collateral
          { user: previous-bidder }
          { amount: (+ previous-collateral-amount previous-collateral-locked) }
        )
      )
      true
    )
    
    (map-set auction-bids
      { auction-id: auction-id, bidder: tx-sender }
      {
        bid-amount: bid-amount,
        bid-price: bid-price,
        collateral-locked: collateral-needed
      }
    )
    
    (map-set bond-auctions
      { auction-id: auction-id }
      (merge auction-data {
        highest-bid: bid-price,
        highest-bidder: (some tx-sender)
      })
    )
    
    (map-set user-collateral
      { user: tx-sender }
      { amount: (- user-collateral-amount collateral-needed) }
    )
    
    (ok true)
  )
)

(define-public (settle-auction (auction-id uint))
  (let
    (
      (auction-data (unwrap! (map-get? bond-auctions { auction-id: auction-id }) ERR_AUCTION_NOT_FOUND))
      (seller (get seller auction-data))
      (bond-id (get bond-id auction-data))
      (amount (get amount auction-data))
      (highest-bid (get highest-bid auction-data))
      (highest-bidder (get highest-bidder auction-data))
      (marketplace-fee (/ (* amount highest-bid (var-get marketplace-fee-rate)) u10000))
      (seller-proceeds (- (* amount highest-bid) marketplace-fee))
    )
    (asserts! (>= stacks-block-height (get end-block auction-data)) ERR_AUCTION_NOT_ACTIVE)
    (asserts! (not (get settled auction-data)) ERR_AUCTION_ALREADY_SETTLED)
    
    (if (is-some highest-bidder)
      (let
        (
          (winner (unwrap-panic highest-bidder))
          (seller-collateral-amount (get-user-collateral seller))
        )
        (try! (transfer-bond bond-id amount winner))
        (map-set user-collateral
          { user: seller }
          { amount: (+ seller-collateral-amount seller-proceeds) }
        )
        (map-set bond-auctions
          { auction-id: auction-id }
          (merge auction-data { settled: true })
        )
        (ok { winner: (some winner), final-price: highest-bid })
      )
      (begin
        (map-set bond-auctions
          { auction-id: auction-id }
          (merge auction-data { settled: true })
        )
        (ok { winner: none, final-price: u0 })
      )
    )
  )
)

(define-private (update-order-book (bond-id uint) (price uint) (order-type (string-ascii 10)) (amount uint) (add bool))
  (let
    (
      (current-data (default-to { total-amount: u0, order-count: u0 } 
        (map-get? order-book { bond-id: bond-id, price: price, order-type: order-type })))
    )
    (map-set order-book
      { bond-id: bond-id, price: price, order-type: order-type }
      {
        total-amount: (if add 
          (+ (get total-amount current-data) amount)
          (- (get total-amount current-data) amount)
        ),
        order-count: (if add 
          (+ (get order-count current-data) u1)
          (- (get order-count current-data) u1)
        )
      }
    )
    (ok true)
  )
)

(define-private (match-market-order (bond-id uint) (order-type (string-ascii 10)) (amount uint) (price uint))
  (let
    (
      (marketplace-fee (/ (* amount price (var-get marketplace-fee-rate)) u10000))
      (net-amount (- (* amount price) marketplace-fee))
      (user-collateral-amount (get-user-collateral tx-sender))
    )
    (if (is-eq order-type "buy")
      (begin
        (map-set user-collateral
          { user: tx-sender }
          { amount: (- user-collateral-amount (* amount price)) }
        )
        (try! (transfer-bond bond-id amount tx-sender))
        (ok true)
      )
      (begin
        (map-set user-collateral
          { user: tx-sender }
          { amount: (+ user-collateral-amount net-amount) }
        )
        (try! (transfer-bond bond-id amount tx-sender))
        (ok true)
      )
    )
  )
)

(define-private (get-best-price (bond-id uint) (order-type (string-ascii 10)))
  (let
    (
      (opposite-type (if (is-eq order-type "buy") "sell" "buy"))
    )
    (fold find-best-price (list u100 u200 u300 u400 u500 u600 u700 u800 u900 u1000) u0)
  )
)

(define-private (find-best-price (price uint) (current-best uint))
  (let
    (
      (order-data (map-get? order-book { bond-id: u1, price: price, order-type: "sell" }))
    )
    (if (and (is-some order-data) (> (get total-amount (unwrap-panic order-data)) u0))
      (if (or (is-eq current-best u0) (< price current-best))
        price
        current-best
      )
      current-best
    )
  )
)

(define-read-only (get-order-info (order-id uint))
  (map-get? marketplace-orders { order-id: order-id })
)

(define-read-only (get-auction-info (auction-id uint))
  (map-get? bond-auctions { auction-id: auction-id })
)

(define-read-only (get-user-collateral (user principal))
  (default-to u0 (get amount (map-get? user-collateral { user: user })))
)

(define-read-only (get-order-book-depth (bond-id uint) (price uint) (order-type (string-ascii 10)))
  (map-get? order-book { bond-id: bond-id, price: price, order-type: order-type })
)

(define-read-only (get-auction-bid (auction-id uint) (bidder principal))
  (map-get? auction-bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-marketplace-fee-rate)
  (var-get marketplace-fee-rate)
)

(define-read-only (get-next-order-id)
  (var-get next-order-id)
)

(define-read-only (get-next-auction-id)
  (var-get next-auction-id)
)

(define-public (set-marketplace-fee (new-fee-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= new-fee-rate u500) ERR_INVALID_PARAMETERS)
    (var-set marketplace-fee-rate new-fee-rate)
    (ok true)
  )
)