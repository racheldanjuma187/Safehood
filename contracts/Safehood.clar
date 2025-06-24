(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_FUNDS (err u101))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_VOTING_ENDED (err u104))
(define-constant ERR_PROPOSAL_NOT_PASSED (err u105))
(define-constant ERR_ALREADY_MEMBER (err u106))
(define-constant ERR_NOT_MEMBER (err u107))
(define-constant ERR_INVALID_AMOUNT (err u108))

(define-data-var proposal-counter uint u0)
(define-data-var total-treasury uint u0)

(define-map members principal bool)
(define-map member-contributions principal uint)

(define-map proposals uint {
    id: uint,
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    proposal-type: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool,
    passed: bool
})

(define-map votes {proposal-id: uint, voter: principal} bool)

(define-map safety-incidents uint {
    id: uint,
    reporter: principal,
    location: (string-ascii 100),
    incident-type: (string-ascii 50),
    description: (string-ascii 300),
    timestamp: uint,
    verified: bool
})

(define-data-var incident-counter uint u0)

(define-map equipment uint {
    id: uint,
    equipment-type: (string-ascii 30),
    location: (string-ascii 100),
    cost: uint,
    installed: bool,
    maintenance-due: uint
})

(define-data-var equipment-counter uint u0)

(define-public (join-dao)
    (begin
        (asserts! (is-none (map-get? members tx-sender)) ERR_ALREADY_MEMBER)
        (map-set members tx-sender true)
        (ok true)
    )
)

(define-public (contribute (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set total-treasury (+ (var-get total-treasury) amount))
        (map-set member-contributions tx-sender 
            (+ (default-to u0 (map-get? member-contributions tx-sender)) amount))
        (ok true)
    )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) 
                               (amount uint) (proposal-type (string-ascii 20)))
    (let ((proposal-id (+ (var-get proposal-counter) u1)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (<= amount (var-get total-treasury)) ERR_INSUFFICIENT_FUNDS)
        (map-set proposals proposal-id {
            id: proposal-id,
            proposer: tx-sender,
            title: title,
            description: description,
            amount: amount,
            proposal-type: proposal-type,
            votes-for: u0,
            votes-against: u0,
            end-block: (+ stacks-block-height u144),
            executed: false,
            passed: false
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR_VOTING_ENDED)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} vote-for)
        
        (if vote-for
            (map-set proposals proposal-id (merge proposal {votes-for: (+ (get votes-for proposal) u1)}))
            (map-set proposals proposal-id (merge proposal {votes-against: (+ (get votes-against proposal) u1)}))
        )
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (>= stacks-block-height (get end-block proposal)) ERR_VOTING_ENDED)
        (asserts! (not (get executed proposal)) ERR_PROPOSAL_NOT_PASSED)
        (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_PROPOSAL_NOT_PASSED)
        
        (try! (as-contract (stx-transfer? (get amount proposal) tx-sender (get proposer proposal))))
        (var-set total-treasury (- (var-get total-treasury) (get amount proposal)))
        
        (map-set proposals proposal-id (merge proposal {executed: true, passed: true}))
        (ok true)
    )
)

(define-public (report-incident (location (string-ascii 100)) (incident-type (string-ascii 50)) 
                               (description (string-ascii 300)))
    (let ((incident-id (+ (var-get incident-counter) u1)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (map-set safety-incidents incident-id {
            id: incident-id,
            reporter: tx-sender,
            location: location,
            incident-type: incident-type,
            description: description,
            timestamp: stacks-block-height,
            verified: false
        })
        (var-set incident-counter incident-id)
        (ok incident-id)
    )
)

(define-public (verify-incident (incident-id uint))
    (let ((incident (unwrap! (map-get? safety-incidents incident-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set safety-incidents incident-id (merge incident {verified: true}))
        (ok true)
    )
)

(define-public (add-equipment (equipment-type (string-ascii 30)) (location (string-ascii 100)) (cost uint))
    (let ((equipment-id (+ (var-get equipment-counter) u1)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set equipment equipment-id {
            id: equipment-id,
            equipment-type: equipment-type,
            location: location,
            cost: cost,
            installed: false,
            maintenance-due: (+ stacks-block-height u4320)
        })
        (var-set equipment-counter equipment-id)
        (ok equipment-id)
    )
)

(define-public (mark-equipment-installed (equipment-id uint))
    (let ((equip (unwrap! (map-get? equipment equipment-id) ERR_PROPOSAL_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set equipment equipment-id (merge equip {installed: true}))
        (ok true)
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-member-status (member principal))
    (default-to false (map-get? members member))
)

(define-read-only (get-member-contribution (member principal))
    (default-to u0 (map-get? member-contributions member))
)

(define-read-only (get-treasury-balance)
    (var-get total-treasury)
)

(define-read-only (get-incident (incident-id uint))
    (map-get? safety-incidents incident-id)
)

(define-read-only (get-equipment (equipment-id uint))
    (map-get? equipment equipment-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter})
)

(define-read-only (get-proposal-count)
    (var-get proposal-counter)
)

(define-read-only (get-incident-count)
    (var-get incident-counter)
)

(define-read-only (get-equipment-count)
    (var-get equipment-counter)
)