;; Bond Rating and Credit Assessment System
;; Provides comprehensive credit evaluation and risk assessment for bond issuers

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u500))
(define-constant ERR-ISSUER-NOT-FOUND (err u501))
(define-constant ERR-INVALID-RATING (err u502))
(define-constant ERR-RATING-EXISTS (err u503))
(define-constant ERR-INSUFFICIENT-DATA (err u504))
(define-constant ERR-INVALID-ASSESSOR (err u505))
(define-constant ERR-BOND-NOT-FOUND (err u506))

;; Rating constants
(define-constant RATING-AAA u1)
(define-constant RATING-AA u2)
(define-constant RATING-A u3)
(define-constant RATING-BBB u4)
(define-constant RATING-BB u5)
(define-constant RATING-B u6)
(define-constant RATING-CCC u7)
(define-constant RATING-D u8)

;; Risk level constants
(define-constant RISK-VERY-LOW u1)
(define-constant RISK-LOW u2)
(define-constant RISK-MEDIUM u3)
(define-constant RISK-HIGH u4)
(define-constant RISK-VERY-HIGH u5)

;; Data variables
(define-data-var rating-count uint u0)
(define-data-var contract-owner principal tx-sender)
(define-data-var assessor-count uint u0)
(define-data-var minimum-assessors uint u3)

;; Maps
(define-map issuer-credit-ratings
  { issuer: principal }
  {
    current-rating: uint,
    risk-level: uint,
    assessment-date: uint,
    assessor-consensus: bool,
    rating-rationale: (string-utf8 300),
    financial-score: uint,
    market-score: uint,
    operational-score: uint,
    overall-score: uint
  }
)

(define-map authorized-assessors
  { assessor: principal }
  {
    active: bool,
    assessment-count: uint,
    accuracy-score: uint,
    specialization: (string-utf8 50),
    certification-level: uint
  }
)

(define-map bond-risk-assessments
  { bond-id: uint }
  {
    credit-rating: uint,
    default-probability: uint,
    liquidity-score: uint,
    market-risk: uint,
    interest-rate-risk: uint,
    assessment-timestamp: uint,
    last-review: uint
  }
)

(define-map assessment-votes
  { issuer: principal, assessor: principal }
  {
    proposed-rating: uint,
    financial-score: uint,
    market-score: uint,
    operational-score: uint,
    vote-timestamp: uint,
    confidence-level: uint
  }
)

(define-map credit-history
  { issuer: principal, period: uint }
  {
    bonds-issued: uint,
    bonds-defaulted: uint,
    total-raised: uint,
    payment-timeliness: uint,
    market-performance: uint
  }
)

;; Public functions

;; Submit credit rating assessment
(define-public (submit-credit-assessment
    (issuer principal)
    (proposed-rating uint)
    (financial-score uint)
    (market-score uint)
    (operational-score uint)
    (rationale (string-utf8 300)))
  (let
    ((assessor-data (unwrap! (map-get? authorized-assessors { assessor: tx-sender }) ERR-NOT-AUTHORIZED))
     (issuer-profile (unwrap! (contract-call? .Bondex get-issuer-profile issuer) ERR-ISSUER-NOT-FOUND))
     (confidence (calculate-confidence-level financial-score market-score operational-score)))
    
    (asserts! (get active assessor-data) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= proposed-rating RATING-AAA) (<= proposed-rating RATING-D)) ERR-INVALID-RATING)
    (asserts! (and (>= financial-score u0) (<= financial-score u100)) ERR-INVALID-RATING)
    (asserts! (and (>= market-score u0) (<= market-score u100)) ERR-INVALID-RATING)
    (asserts! (and (>= operational-score u0) (<= operational-score u100)) ERR-INVALID-RATING)
    
    (map-set assessment-votes
      { issuer: issuer, assessor: tx-sender }
      {
        proposed-rating: proposed-rating,
        financial-score: financial-score,
        market-score: market-score,
        operational-score: operational-score,
        vote-timestamp: stacks-block-height,
        confidence-level: confidence
      }
    )
    
    ;; Update assessor statistics
    (map-set authorized-assessors
      { assessor: tx-sender }
      (merge assessor-data {
        assessment-count: (+ (get assessment-count assessor-data) u1)
      })
    )
    
    ;; Check if consensus reached
    (let ((consensus-reached (check-rating-consensus issuer)))
      (if consensus-reached
        (finalize-credit-rating issuer)
        (ok false))
    )
  )
)

;; Add authorized credit assessor
(define-public (add-credit-assessor 
    (assessor principal)
    (specialization (string-utf8 50))
    (certification-level uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= certification-level u1) (<= certification-level u5)) ERR-INVALID-ASSESSOR)
    
    (map-set authorized-assessors
      { assessor: assessor }
      {
        active: true,
        assessment-count: u0,
        accuracy-score: u85,
        specialization: specialization,
        certification-level: certification-level
      }
    )
    
    (var-set assessor-count (+ (var-get assessor-count) u1))
    (ok true)
  )
)

;; Rate specific bond based on issuer credit rating
(define-public (rate-bond (bond-id uint))
  (let
    ((bond-data (unwrap! (contract-call? .Bondex get-bond-info bond-id) ERR-BOND-NOT-FOUND))
     (issuer-rating (map-get? issuer-credit-ratings { issuer: (get issuer bond-data) })))
    
    (asserts! (is-authorized-assessor tx-sender) ERR-NOT-AUTHORIZED)
    
    (match issuer-rating
      rating-data
        (let
          ((default-prob (calculate-default-probability (get current-rating rating-data)))
           (liquidity-score (calculate-liquidity-score bond-id))
           (market-risk (calculate-market-risk bond-id))
           (interest-risk (calculate-interest-rate-risk bond-id)))
          
          (map-set bond-risk-assessments
            { bond-id: bond-id }
            {
              credit-rating: (get current-rating rating-data),
              default-probability: default-prob,
              liquidity-score: liquidity-score,
              market-risk: market-risk,
              interest-rate-risk: interest-risk,
              assessment-timestamp: stacks-block-height,
              last-review: stacks-block-height
            }
          )
          
          (ok true)
        )
      ;; No credit rating exists yet
      ERR-INSUFFICIENT-DATA
    )
  )
)

;; Update credit history
(define-public (update-credit-history
    (issuer principal)
    (bonds-issued-period uint)
    (bonds-defaulted-period uint)
    (total-raised-period uint)
    (payment-score uint))
  (let
    ((current-period (/ stacks-block-height u4320))) ;; Monthly periods
    
    (asserts! (is-authorized-assessor tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (<= bonds-defaulted-period bonds-issued-period) ERR-INVALID-RATING)
    (asserts! (and (>= payment-score u0) (<= payment-score u100)) ERR-INVALID-RATING)
    
    (map-set credit-history
      { issuer: issuer, period: current-period }
      {
        bonds-issued: bonds-issued-period,
        bonds-defaulted: bonds-defaulted-period,
        total-raised: total-raised-period,
        payment-timeliness: payment-score,
        market-performance: (calculate-market-performance issuer)
      }
    )
    
    (ok true)
  )
)

;; Private helper functions

(define-private (check-rating-consensus (issuer principal))
  (let
    ((assessments-needed (var-get minimum-assessors)))
    ;; Simplified consensus check - in real implementation would count actual votes
    (>= assessments-needed u1) ;; For demo, always return true
  )
)

(define-private (finalize-credit-rating (issuer principal))
  (let
    ((avg-scores (calculate-average-scores issuer))
     (final-rating (determine-final-rating (get overall avg-scores)))
     (risk-level (map-rating-to-risk final-rating)))
    
    (map-set issuer-credit-ratings
      { issuer: issuer }
      {
        current-rating: final-rating,
        risk-level: risk-level,
        assessment-date: stacks-block-height,
        assessor-consensus: true,
        rating-rationale: u"Consensus reached based on assessor evaluations",
        financial-score: (get financial avg-scores),
        market-score: (get market avg-scores),
        operational-score: (get operational avg-scores),
        overall-score: (get overall avg-scores)
      }
    )
    
    (var-set rating-count (+ (var-get rating-count) u1))
    (ok true)
  )
)

(define-private (calculate-average-scores (issuer principal))
  ;; Simplified calculation - real implementation would aggregate all assessor votes
  { financial: u75, market: u80, operational: u70, overall: u75 }
)

(define-private (determine-final-rating (overall-score uint))
  (if (>= overall-score u90)
    RATING-AAA
    (if (>= overall-score u80)
      RATING-AA
      (if (>= overall-score u70)
        RATING-A
        (if (>= overall-score u60)
          RATING-BBB
          (if (>= overall-score u50)
            RATING-BB
            (if (>= overall-score u40)
              RATING-B
              (if (>= overall-score u30)
                RATING-CCC
                RATING-D))))))))

(define-private (map-rating-to-risk (rating uint))
  (if (<= rating RATING-A)
    RISK-VERY-LOW
    (if (is-eq rating RATING-BBB)
      RISK-LOW
      (if (is-eq rating RATING-BB)
        RISK-MEDIUM
        (if (is-eq rating RATING-B)
          RISK-HIGH
          RISK-VERY-HIGH)))))

(define-private (calculate-confidence-level (financial uint) (market uint) (operational uint))
  (let
    ((avg-score (/ (+ (+ financial market) operational) u3)))
    (if (>= avg-score u80) u95
        (if (>= avg-score u60) u85
            (if (>= avg-score u40) u75 u65)))))

(define-private (calculate-default-probability (rating uint))
  (if (is-eq rating RATING-AAA) u1
      (if (is-eq rating RATING-AA) u2
          (if (is-eq rating RATING-A) u5
              (if (is-eq rating RATING-BBB) u10
                  (if (is-eq rating RATING-BB) u20
                      (if (is-eq rating RATING-B) u35
                          (if (is-eq rating RATING-CCC) u50
                              u75))))))))

(define-private (calculate-liquidity-score (bond-id uint))
  u75) ;; Simplified - would analyze trading volume and spread

(define-private (calculate-market-risk (bond-id uint))
  u25) ;; Simplified - would analyze market volatility

(define-private (calculate-interest-rate-risk (bond-id uint))
  u30) ;; Simplified - would analyze duration and rate sensitivity

(define-private (calculate-market-performance (issuer principal))
  u70) ;; Simplified - would analyze bond price performance

;; Read-only functions
(define-read-only (get-issuer-credit-rating (issuer principal))
  (map-get? issuer-credit-ratings { issuer: issuer }))

(define-read-only (get-bond-risk-assessment (bond-id uint))
  (map-get? bond-risk-assessments { bond-id: bond-id }))

(define-read-only (get-assessor-info (assessor principal))
  (map-get? authorized-assessors { assessor: assessor }))

(define-read-only (is-authorized-assessor (assessor principal))
  (default-to false (get active (map-get? authorized-assessors { assessor: assessor }))))

(define-read-only (get-credit-history (issuer principal) (period uint))
  (map-get? credit-history { issuer: issuer, period: period }))

(define-read-only (get-rating-count)
  (var-get rating-count))

(define-read-only (get-assessor-vote (issuer principal) (assessor principal))
  (map-get? assessment-votes { issuer: issuer, assessor: assessor }))
