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
(define-constant ERR_INSUFFICIENT_REPUTATION (err u109))
(define-constant ERR_INVALID_LEVEL (err u110))
(define-constant ERR_ALREADY_CLAIMED (err u111))
(define-constant ERR_NO_REWARDS_AVAILABLE (err u112))

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
(define-data-var reputation-reward-pool uint u0)

(define-map member-reputation principal uint)
(define-map reputation-levels uint {
    level: uint,
    min-points: uint,
    vote-multiplier: uint,
    proposal-threshold: uint,
    monthly-reward: uint
})

(define-map member-activities principal {
    incidents-reported: uint,
    incidents-verified: uint,
    proposals-created: uint,
    votes-cast: uint,
    contributions-made: uint,
    last-activity: uint
})

(define-map reputation-claims {member: principal, month: uint} bool)
(define-data-var current-month uint u1)

(define-public (join-dao)
    (begin
        (asserts! (is-none (map-get? members tx-sender)) ERR_ALREADY_MEMBER)
        (map-set members tx-sender true)
        (map-set member-reputation tx-sender u100)
        (map-set member-activities tx-sender {
            incidents-reported: u0,
            incidents-verified: u0,
            proposals-created: u0,
            votes-cast: u0,
            contributions-made: u0,
            last-activity: stacks-block-height
        })
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
        (unwrap-panic (update-reputation-contribution tx-sender amount))
        (ok true)
    )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) 
                               (amount uint) (proposal-type (string-ascii 20)))
    (let ((proposal-id (+ (var-get proposal-counter) u1))
          (member-rep (default-to u0 (map-get? member-reputation tx-sender)))
          (rep-level (get-reputation-level member-rep)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (<= amount (var-get total-treasury)) ERR_INSUFFICIENT_FUNDS)
        (asserts! (>= member-rep (get proposal-threshold rep-level)) ERR_INSUFFICIENT_REPUTATION)
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
        (unwrap-panic (update-reputation-proposal tx-sender))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
          (member-rep (default-to u0 (map-get? member-reputation tx-sender)))
          (rep-level (get-reputation-level member-rep))
          (vote-weight (get vote-multiplier rep-level)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR_VOTING_ENDED)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) ERR_ALREADY_VOTED)
        
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} vote-for)
        
        (if vote-for
            (map-set proposals proposal-id (merge proposal {votes-for: (+ (get votes-for proposal) vote-weight)}))
            (map-set proposals proposal-id (merge proposal {votes-against: (+ (get votes-against proposal) vote-weight)}))
        )
        (unwrap-panic (update-reputation-vote tx-sender))
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
        (unwrap-panic (update-reputation-incident tx-sender))
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

(define-public (initialize-reputation-levels)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set reputation-levels u1 {level: u1, min-points: u0, vote-multiplier: u1, proposal-threshold: u0, monthly-reward: u0})
        (map-set reputation-levels u2 {level: u2, min-points: u500, vote-multiplier: u2, proposal-threshold: u200, monthly-reward: u50})
        (map-set reputation-levels u3 {level: u3, min-points: u1000, vote-multiplier: u3, proposal-threshold: u400, monthly-reward: u100})
        (map-set reputation-levels u4 {level: u4, min-points: u2000, vote-multiplier: u4, proposal-threshold: u600, monthly-reward: u200})
        (map-set reputation-levels u5 {level: u5, min-points: u5000, vote-multiplier: u5, proposal-threshold: u1000, monthly-reward: u500})
        (ok true)
    )
)

(define-private (get-reputation-level (points uint))
    (if (>= points u5000)
        (unwrap-panic (map-get? reputation-levels u5))
        (if (>= points u2000)
            (unwrap-panic (map-get? reputation-levels u4))
            (if (>= points u1000)
                (unwrap-panic (map-get? reputation-levels u3))
                (if (>= points u500)
                    (unwrap-panic (map-get? reputation-levels u2))
                    (unwrap-panic (map-get? reputation-levels u1))
                )
            )
        )
    )
)

(define-private (update-reputation-contribution (member principal) (amount uint))
    (let ((current-rep (default-to u0 (map-get? member-reputation member)))
          (points-to-add (/ amount u1000))
          (current-activity (default-to {incidents-reported: u0, incidents-verified: u0, proposals-created: u0, votes-cast: u0, contributions-made: u0, last-activity: u0} (map-get? member-activities member))))
        (map-set member-reputation member (+ current-rep points-to-add))
        (map-set member-activities member (merge current-activity {
            contributions-made: (+ (get contributions-made current-activity) u1),
            last-activity: stacks-block-height
        }))
        (ok true)
    )
)

(define-private (update-reputation-proposal (member principal))
    (let ((current-rep (default-to u0 (map-get? member-reputation member)))
          (current-activity (default-to {incidents-reported: u0, incidents-verified: u0, proposals-created: u0, votes-cast: u0, contributions-made: u0, last-activity: u0} (map-get? member-activities member))))
        (map-set member-reputation member (+ current-rep u50))
        (map-set member-activities member (merge current-activity {
            proposals-created: (+ (get proposals-created current-activity) u1),
            last-activity: stacks-block-height
        }))
        (ok true)
    )
)

(define-private (update-reputation-vote (member principal))
    (let ((current-rep (default-to u0 (map-get? member-reputation member)))
          (current-activity (default-to {incidents-reported: u0, incidents-verified: u0, proposals-created: u0, votes-cast: u0, contributions-made: u0, last-activity: u0} (map-get? member-activities member))))
        (map-set member-reputation member (+ current-rep u10))
        (map-set member-activities member (merge current-activity {
            votes-cast: (+ (get votes-cast current-activity) u1),
            last-activity: stacks-block-height
        }))
        (ok true)
    )
)

(define-private (update-reputation-incident (member principal))
    (let ((current-rep (default-to u0 (map-get? member-reputation member)))
          (current-activity (default-to {incidents-reported: u0, incidents-verified: u0, proposals-created: u0, votes-cast: u0, contributions-made: u0, last-activity: u0} (map-get? member-activities member))))
        (map-set member-reputation member (+ current-rep u25))
        (map-set member-activities member (merge current-activity {
            incidents-reported: (+ (get incidents-reported current-activity) u1),
            last-activity: stacks-block-height
        }))
        (ok true)
    )
)

(define-public (verify-incident-with-reputation (incident-id uint))
    (let ((incident (unwrap! (map-get? safety-incidents incident-id) ERR_PROPOSAL_NOT_FOUND))
          (reporter (get reporter incident))
          (current-rep (default-to u0 (map-get? member-reputation reporter)))
          (current-activity (default-to {incidents-reported: u0, incidents-verified: u0, proposals-created: u0, votes-cast: u0, contributions-made: u0, last-activity: u0} (map-get? member-activities reporter))))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set safety-incidents incident-id (merge incident {verified: true}))
        (map-set member-reputation reporter (+ current-rep u75))
        (map-set member-activities reporter (merge current-activity {
            incidents-verified: (+ (get incidents-verified current-activity) u1),
            last-activity: stacks-block-height
        }))
        (ok true)
    )
)

(define-public (fund-reputation-rewards (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set reputation-reward-pool (+ (var-get reputation-reward-pool) amount))
        (ok true)
    )
)

(define-public (claim-monthly-reputation-reward)
    (let ((member-rep (default-to u0 (map-get? member-reputation tx-sender)))
          (rep-level (get-reputation-level member-rep))
          (reward-amount (get monthly-reward rep-level))
          (current-month-val (var-get current-month))
          (already-claimed (default-to false (map-get? reputation-claims {member: tx-sender, month: current-month-val}))))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (> reward-amount u0) ERR_NO_REWARDS_AVAILABLE)
        (asserts! (not already-claimed) ERR_ALREADY_CLAIMED)
        (asserts! (>= (var-get reputation-reward-pool) reward-amount) ERR_INSUFFICIENT_FUNDS)
        
        (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
        (var-set reputation-reward-pool (- (var-get reputation-reward-pool) reward-amount))
        (map-set reputation-claims {member: tx-sender, month: current-month-val} true)
        (ok reward-amount)
    )
)

(define-public (advance-month)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set current-month (+ (var-get current-month) u1))
        (ok true)
    )
)

(define-public (decay-reputation (member principal) (decay-amount uint))
    (let ((current-rep (default-to u0 (map-get? member-reputation member)))
          (new-rep (if (> current-rep decay-amount) (- current-rep decay-amount) u0)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set member-reputation member new-rep)
        (ok true)
    )
)

(define-read-only (get-member-reputation (member principal))
    (default-to u0 (map-get? member-reputation member))
)

(define-read-only (get-member-reputation-level (member principal))
    (let ((rep (get-member-reputation member)))
        (get-reputation-level rep)
    )
)

(define-read-only (get-member-activities (member principal))
    (map-get? member-activities member)
)

(define-read-only (get-reputation-level-info (level uint))
    (map-get? reputation-levels level)
)

(define-read-only (get-reputation-reward-pool)
    (var-get reputation-reward-pool)
)

(define-read-only (get-current-month)
    (var-get current-month)
)

(define-read-only (has-claimed-monthly-reward (member principal) (month uint))
    (default-to false (map-get? reputation-claims {member: member, month: month}))
)