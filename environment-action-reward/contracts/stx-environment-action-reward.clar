;; Environmental Action Rewards Contract
;; A smart contract for rewarding verified green behaviors with eco-tokens

;; Define the fungible token for eco-rewards
(define-fungible-token eco-token)

;; Contract owner
(define-constant contract-owner tx-sender)

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-invalid-action (err u101))
(define-constant err-already-claimed (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-invalid-verifier (err u104))
(define-constant err-action-not-found (err u105))

;; Define green action types and their reward amounts
(define-map green-actions
  { action-id: uint }
  { 
    name: (string-ascii 50),
    description: (string-ascii 200),
    reward-amount: uint,
    verification-required: bool,
    active: bool
  }
)

;; Track user claims to prevent double-spending
(define-map user-claims
  { user: principal, action-id: uint, timestamp: uint }
  { claimed: bool, verified: bool }
)

;; Authorized verifiers (environmental organizations, sensors, etc.)
(define-map authorized-verifiers
  { verifier: principal }
  { active: bool, organization: (string-ascii 100) }
)

;; User eco-scores and statistics
(define-map user-stats
  { user: principal }
  { 
    total-actions: uint,
    total-tokens-earned: uint,
    eco-score: uint,
    last-action-timestamp: uint
  }
)

;; Initialize some default green actions
(map-set green-actions { action-id: u1 }
  { 
    name: "Solar Panel Usage",
    description: "Verified solar energy usage for 30 days",
    reward-amount: u100,
    verification-required: true,
    active: true
  }
)

(map-set green-actions { action-id: u2 }
  { 
    name: "Public Transport",
    description: "Using public transport instead of private vehicle",
    reward-amount: u10,
    verification-required: false,
    active: true
  }
)

(map-set green-actions { action-id: u3 }
  { 
    name: "Recycling Activity",
    description: "Verified recycling of materials",
    reward-amount: u25,
    verification-required: true,
    active: true
  }
)

(map-set green-actions { action-id: u4 }
  { 
    name: "Tree Planting",
    description: "Planting and maintaining trees",
    reward-amount: u50,
    verification-required: true,
    active: true
  }
)

(map-set green-actions { action-id: u5 }
  { 
    name: "Energy Conservation",
    description: "Reducing energy consumption by 20%+",
    reward-amount: u75,
    verification-required: true,
    active: true
  }
)

;; Read-only functions

(define-read-only (get-eco-token-balance (user principal))
  (ft-get-balance eco-token user)
)

(define-read-only (get-green-action (action-id uint))
  (map-get? green-actions { action-id: action-id })
)

(define-read-only (get-user-stats (user principal))
  (default-to 
    { total-actions: u0, total-tokens-earned: u0, eco-score: u0, last-action-timestamp: u0 }
    (map-get? user-stats { user: user })
  )
)

(define-read-only (has-claimed-action (user principal) (action-id uint) (timestamp uint))
  (default-to 
    { claimed: false, verified: false }
    (map-get? user-claims { user: user, action-id: action-id, timestamp: timestamp })
  )
)

(define-read-only (is-authorized-verifier (verifier principal))
  (match (map-get? authorized-verifiers { verifier: verifier })
    verifier-data (get active verifier-data)
    false
  )
)

(define-read-only (get-token-name)
  "ECO-TOKEN"
)

(define-read-only (get-token-symbol)
  "ECO"
)

(define-read-only (get-token-decimals)
  u6
)

;; Public functions

;; Add a new authorized verifier (only contract owner)
(define-public (add-verifier (verifier principal) (organization (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set authorized-verifiers 
      { verifier: verifier }
      { active: true, organization: organization }
    ))
  )
)

;; Add or update a green action (only contract owner)
(define-public (add-green-action 
  (action-id uint) 
  (name (string-ascii 50)) 
  (description (string-ascii 200))
  (reward-amount uint)
  (verification-required bool)
)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set green-actions 
      { action-id: action-id }
      { 
        name: name,
        description: description,
        reward-amount: reward-amount,
        verification-required: verification-required,
        active: true
      }
    ))
  )
)

;; Self-report a green action (for actions that don't require verification)
(define-public (claim-green-action (action-id uint))
  (let (
    (action-data (unwrap! (map-get? green-actions { action-id: action-id }) err-action-not-found))
    (current-timestamp stacks-block-height) ;; Use stacks-block-height as timestamp
    (claim-key { user: tx-sender, action-id: action-id, timestamp: current-timestamp })
    (existing-claim (map-get? user-claims claim-key))
    (current-stats (get-user-stats tx-sender))
    (reward-amount (get reward-amount action-data))
  )
    (begin
      ;; Check if action is active
      (asserts! (get active action-data) err-invalid-action)
      
      ;; Check if verification is required for this action
      (asserts! (not (get verification-required action-data)) err-invalid-action)
      
      ;; Check if already claimed
      (asserts! (is-none existing-claim) err-already-claimed)
      
      ;; Mint eco-tokens as reward
      (try! (ft-mint? eco-token reward-amount tx-sender))
      
      ;; Record the claim
      (map-set user-claims claim-key { claimed: true, verified: false })
      
      ;; Update user statistics
      (map-set user-stats 
        { user: tx-sender }
        { 
          total-actions: (+ (get total-actions current-stats) u1),
          total-tokens-earned: (+ (get total-tokens-earned current-stats) reward-amount),
          eco-score: (+ (get eco-score current-stats) (/ reward-amount u10)),
          last-action-timestamp: current-timestamp
        }
      )
      
      (ok reward-amount)
    )
  )
)

;; Verify and reward a green action (called by authorized verifiers)
(define-public (verify-green-action 
  (user principal) 
  (action-id uint) 
  (timestamp uint)
  (evidence-hash (buff 32))
)
  (let (
    (action-data (unwrap! (map-get? green-actions { action-id: action-id }) err-action-not-found))
    (claim-key { user: user, action-id: action-id, timestamp: timestamp })
    (current-stats (get-user-stats user))
    (reward-amount (get reward-amount action-data))
  )
    (begin
      ;; Check if sender is authorized verifier
      (asserts! (is-authorized-verifier tx-sender) err-invalid-verifier)
      
      ;; Check if action exists and is active
      (asserts! (get active action-data) err-invalid-action)
      
      ;; Check if action requires verification
      (asserts! (get verification-required action-data) err-invalid-action)
      
      ;; Check if already claimed/verified
      (asserts! 
        (match (map-get? user-claims claim-key)
          existing-claim (not (get verified existing-claim))
          true
        ) 
        err-already-claimed
      )
      
      ;; Mint eco-tokens as reward
      (try! (ft-mint? eco-token reward-amount user))
      
      ;; Record the verified claim
      (map-set user-claims claim-key { claimed: true, verified: true })
      
      ;; Update user statistics with bonus for verified actions
      (let ((bonus-score (/ reward-amount u5))) ;; Verified actions get 2x eco-score
        (map-set user-stats 
          { user: user }
          { 
            total-actions: (+ (get total-actions current-stats) u1),
            total-tokens-earned: (+ (get total-tokens-earned current-stats) reward-amount),
            eco-score: (+ (get eco-score current-stats) bonus-score),
            last-action-timestamp: timestamp
          }
        )
      )
      
      (ok reward-amount)
    )
  )
)

;; Transfer eco-tokens between users
(define-public (transfer-eco-tokens (amount uint) (recipient principal))
  (ft-transfer? eco-token amount tx-sender recipient)
)

;; Burn eco-tokens (for carbon offset purchases, etc.)
(define-public (burn-eco-tokens (amount uint))
  (ft-burn? eco-token amount tx-sender)
)

;; Deactivate a green action (only contract owner)
(define-public (deactivate-green-action (action-id uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((action-data (unwrap! (map-get? green-actions { action-id: action-id }) err-action-not-found)))
      (ok (map-set green-actions 
        { action-id: action-id }
        (merge action-data { active: false })
      ))
    )
  )
)

;; Remove verifier authorization (only contract owner)
(define-public (deactivate-verifier (verifier principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((verifier-data (unwrap! (map-get? authorized-verifiers { verifier: verifier }) err-invalid-verifier)))
      (ok (map-set authorized-verifiers 
        { verifier: verifier }
        (merge verifier-data { active: false })
      ))
    )
  )
)

;; Get total supply of eco-tokens
(define-read-only (get-total-supply)
  (ft-get-supply eco-token)
)

;; Emergency pause function (only contract owner)
(define-data-var contract-paused bool false)

(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set contract-paused (not (var-get contract-paused))))
  )
)