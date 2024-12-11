
;; title: automation-smart-contract
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-supplier (err u101))
(define-constant err-invalid-milestone (err u102))
(define-constant err-milestone-already-completed (err u103))
(define-constant err-sla-violation (err u104))

;; Define data variables
(define-data-var sla-threshold uint u259200) ;; 3 days in seconds

;; Define maps
(define-map suppliers
  principal
  {
    name: (string-ascii 50),
    balance: uint,
    milestone-count: uint,
    last-milestone-timestamp: uint
  }
)

(define-map milestones
  {supplier: principal, milestone-id: uint}
  {
    description: (string-ascii 100),
    amount: uint,
    completed: bool,
    completion-timestamp: uint
  }
)

;; Define functions

;; Function to add a new supplier
(define-public (add-supplier (supplier-principal principal) (supplier-name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (map-set suppliers supplier-principal {
      name: supplier-name,
      balance: u0,
      milestone-count: u0,
      last-milestone-timestamp: u0
    }))
  )
)

;; Function to add a new milestone for a supplier
(define-public (add-milestone (supplier-principal principal) (description (string-ascii 100)) (amount uint))
  (let (
    (supplier (unwrap! (map-get? suppliers supplier-principal) err-not-supplier))
    (new-milestone-id (+ (get milestone-count supplier) u1))
  )
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (map-set milestones {supplier: supplier-principal, milestone-id: new-milestone-id}
        {
          description: description,
          amount: amount,
          completed: false,
          completion-timestamp: u0
        }
      )
      (map-set suppliers supplier-principal (merge supplier {milestone-count: new-milestone-id}))
      (ok new-milestone-id)
    )
  )
)

;; Function to complete a milestone and trigger payment
(define-map suppliers (principal) {balance: uint, last-milestone-block: uint})
(define-map milestones ({supplier: principal, milestone-id: uint}) {completed: bool, amount: uint})

(define-constant err-not-supplier (err u100))
(define-constant err-invalid-milestone (err u101))
(define-constant err-owner-only (err u102))
(define-constant err-milestone-already-completed (err u103))
(define-constant err-sla-violation (err u104))

(define-public (apply-delay-penalty (supplier-principal principal))
  (let (
    (supplier (unwrap! (map-get? suppliers supplier-principal) err-not-supplier))
    (current-block (unwrap-panic (get-block-info? block-height u0)))
    (blocks-since-last-milestone (- current-block (get last-milestone-block supplier)))
    (blocks-late (- blocks-since-last-milestone (var-get sla-threshold-blocks)))
    (penalty-amount (* blocks-late u100000)) ;; 0.1 STX per block late
  )
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (> blocks-since-last-milestone (var-get sla-threshold-blocks)) err-sla-violation)
      
      ;; Apply penalty by reducing supplier balance 
      (map-set suppliers supplier-principal 
        (merge supplier 
          {
            balance: (- (get balance supplier) penalty-amount)
          }
        )
      )
      
      ;; Return penalty amount applied
      (ok penalty-amount)
    )
  )
) 
  
   ;; Trigger payment (in a real-world scenario, this would interact with a stablecoin contract)
      (as-contract (stx-transfer? (get amount milestone) tx-sender supplier-principal))
    
  

;; Function to calculate and apply penalty for delays 
(define-public (apply-delay-penalty (supplier-principal principal))
  (let (
    (supplier (unwrap! (map-get? suppliers supplier-principal) err-not-supplier))
    (current-block (as-contract block-height))
    (blocks-since-last-milestone (- current-block (get last-milestone-block supplier)))
    (blocks-late (- blocks-since-last-milestone (var-get sla-threshold-blocks)))
    (penalty-amount (* blocks-late u100000)) ;; 0.1 STX per block late
  )
    (begin
      (asserts! (is-eq tx-sender contract-owner) err-owner-only)
      (asserts! (> blocks-since-last-milestone (var-get sla-threshold-blocks)) err-sla-violation)
      
      ;; Apply penalty by reducing supplier balance
      (map-set suppliers supplier-principal 
        (merge supplier 
          {
            balance: (- (get balance supplier) penalty-amount)
          }
        )
      )
      
      ;; Return penalty amount applied
      (ok penalty-amo
    )
  )
  )
)
;; Function to withdraw balance (for suppliers)
(define-public (withdraw-balance)
  (let (
    (supplier (unwrap! (map-get? suppliers tx-sender) err-not-supplier))
    (balance (get balance supplier))
  )
    (begin
      (asserts! (> balance u0) (err u105))
      (map-set suppliers tx-sender (merge supplier {balance: u0}))
      (as-contract (stx-transfer? balance tx-sender tx-sender))
    )
  )
)

;; Function to update SLA threshold (only contract owner)
(define-public (update-sla-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set sla-threshold new-threshold))
  )
)