(define-fungible-token airdrop-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-eligible (err u201))
(define-constant err-already-claimed (err u202))
(define-constant err-insufficient-balance (err u203))
(define-constant err-invalid-amount (err u204))

;; Data Variables
(define-data-var total-airdrop-supply uint u1000000) ;; 1M tokens for airdrop
(define-data-var tokens-distributed uint u0)

;; Maps
(define-map airdrop-recipients principal uint) ;; recipient -> allocated amount
(define-map claim-status principal bool)      ;; recipient -> claimed status

(define-public (add-recipient (recipient principal) (amount uint))
  (begin
    ;; Verify only owner can add recipients
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    ;; Verify valid amount
    (asserts! (> amount u0) err-invalid-amount)
    ;; Add recipient to airdrop list
    (map-set airdrop-recipients recipient amount)
    ;; Initialize claim status as false
    (map-set claim-status recipient false)
    ;; Mint tokens to contract for distribution
    (try! (ft-mint? airdrop-token amount (as-contract tx-sender)))
    ;; Log the addition
    (print {
      action: "recipient-added",
      recipient: recipient,
      amount: amount,
      block-height: block-height
    })
    (ok true)))

(define-public (claim-airdrop)
  (let (
    (recipient tx-sender)
    (allocated-amount (default-to u0 (map-get? airdrop-recipients recipient)))
    (already-claimed (default-to false (map-get? claim-status recipient)))
  )
    ;; Check if recipient is eligible (has allocation > 0)
    (asserts! (> allocated-amount u0) err-not-eligible)
    ;; Check if tokens haven't been claimed yet
    (asserts! (not already-claimed) err-already-claimed)
    ;; Verify contract has sufficient balance
    (asserts! (>= (ft-get-balance airdrop-token (as-contract tx-sender)) allocated-amount) err-insufficient-balance)
    ;; Transfer tokens from contract to recipient
    (try! (as-contract (ft-transfer? airdrop-token allocated-amount tx-sender recipient)))
    ;; Mark as claimed
    (map-set claim-status recipient true)
    ;; Update distributed amount
    (var-set tokens-distributed (+ (var-get tokens-distributed) allocated-amount))
    ;; Log the claim
    (print {
      action: "airdrop-claimed",
      recipient: recipient,
      amount: allocated-amount,
      total-distributed: (var-get tokens-distributed),
      block-height: block-height
    })
    (ok allocated-amount)))

;; Read-Only Functions for Information Retrieval

;; Get recipient allocation
(define-read-only (get-recipient-allocation (recipient principal))
  (ok (default-to u0 (map-get? airdrop-recipients recipient))))

;; Check if recipient has claimed
(define-read-only (has-claimed (recipient principal))
  (ok (default-to false (map-get? claim-status recipient))))

;; Get total tokens distributed
(define-read-only (get-total-distributed)
  (ok (var-get tokens-distributed)))

;; Get contract token balance
(define-read-only (get-contract-balance)
  (ok (ft-get-balance airdrop-token (as-contract tx-sender))))

;; Get recipient's current token balance
(define-read-only (get-balance (account principal))
  (ok (ft-get-balance airdrop-token account)))