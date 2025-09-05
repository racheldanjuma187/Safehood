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
(define-constant ERR_ALERT_NOT_FOUND (err u113))
(define-constant ERR_ALREADY_RESPONDED (err u114))
(define-constant ERR_ALERT_RESOLVED (err u115))
(define-constant ERR_INVALID_SEVERITY (err u116))
(define-constant ERR_INVALID_ZONE (err u117))
(define-constant ERR_SHIFT_CONFLICT (err u118))
(define-constant ERR_SHIFT_NOT_FOUND (err u119))
(define-constant ERR_ALREADY_CHECKED_IN (err u120))
(define-constant ERR_NOT_CHECKED_IN (err u121))
(define-constant ERR_SHIFT_NOT_STARTED (err u122))

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

;; Patrol System - New Feature
(define-data-var shift-counter uint u0)
(define-data-var patrol-observation-counter uint u0)

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

(define-data-var alert-counter uint u0)

(define-map emergency-alerts uint {
    id: uint,
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 400),
    severity: uint,
    zone: (string-ascii 50),
    location: (string-ascii 100),
    timestamp: uint,
    resolved: bool,
    responder-count: uint,
    acknowledgment-count: uint,
    verified: bool
})

(define-map alert-responses {alert-id: uint, responder: principal} {
    response-type: uint,
    timestamp: uint,
    response-note: (string-ascii 200),
    arrived-on-scene: bool
})

(define-map alert-acknowledgments {alert-id: uint, member: principal} uint)

(define-map safety-zones (string-ascii 50) {
    zone-name: (string-ascii 50),
    description: (string-ascii 200),
    priority-level: uint,
    active: bool
})

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

(define-public (initialize-safety-zones)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set safety-zones "RESIDENTIAL" {zone-name: "RESIDENTIAL", description: "Residential neighborhood areas", priority-level: u2, active: true})
        (map-set safety-zones "COMMERCIAL" {zone-name: "COMMERCIAL", description: "Commercial district areas", priority-level: u3, active: true})
        (map-set safety-zones "PARK" {zone-name: "PARK", description: "Public parks and recreation areas", priority-level: u1, active: true})
        (map-set safety-zones "SCHOOL" {zone-name: "SCHOOL", description: "School zones and educational facilities", priority-level: u4, active: true})
        (map-set safety-zones "EMERGENCY" {zone-name: "EMERGENCY", description: "Critical emergency response areas", priority-level: u5, active: true})
        (ok true)
    )
)

(define-public (create-emergency-alert (title (string-ascii 100)) (description (string-ascii 400)) 
                                      (severity uint) (zone (string-ascii 50)) (location (string-ascii 100)))
    (let ((alert-id (+ (var-get alert-counter) u1))
          (zone-info (map-get? safety-zones zone)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (and (>= severity u1) (<= severity u5)) ERR_INVALID_SEVERITY)
        (asserts! (is-some zone-info) ERR_INVALID_ZONE)
        (asserts! (get active (unwrap-panic zone-info)) ERR_INVALID_ZONE)
        
        (map-set emergency-alerts alert-id {
            id: alert-id,
            creator: tx-sender,
            title: title,
            description: description,
            severity: severity,
            zone: zone,
            location: location,
            timestamp: stacks-block-height,
            resolved: false,
            responder-count: u0,
            acknowledgment-count: u0,
            verified: false
        })
        (var-set alert-counter alert-id)
        (ok alert-id)
    )
)

(define-public (acknowledge-alert (alert-id uint))
    (let ((alert (unwrap! (map-get? emergency-alerts alert-id) ERR_ALERT_NOT_FOUND))
          (existing-ack (map-get? alert-acknowledgments {alert-id: alert-id, member: tx-sender})))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (not (get resolved alert)) ERR_ALERT_RESOLVED)
        (asserts! (is-none existing-ack) ERR_ALREADY_RESPONDED)
        
        (map-set alert-acknowledgments {alert-id: alert-id, member: tx-sender} stacks-block-height)
        (map-set emergency-alerts alert-id (merge alert {acknowledgment-count: (+ (get acknowledgment-count alert) u1)}))
        (ok true)
    )
)

(define-public (respond-to-alert (alert-id uint) (response-type uint) (response-note (string-ascii 200)))
    (let ((alert (unwrap! (map-get? emergency-alerts alert-id) ERR_ALERT_NOT_FOUND))
          (existing-response (map-get? alert-responses {alert-id: alert-id, responder: tx-sender})))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (not (get resolved alert)) ERR_ALERT_RESOLVED)
        (asserts! (is-none existing-response) ERR_ALREADY_RESPONDED)
        (asserts! (and (>= response-type u1) (<= response-type u4)) ERR_INVALID_AMOUNT)
        
        (map-set alert-responses {alert-id: alert-id, responder: tx-sender} {
            response-type: response-type,
            timestamp: stacks-block-height,
            response-note: response-note,
            arrived-on-scene: false
        })
        (map-set emergency-alerts alert-id (merge alert {responder-count: (+ (get responder-count alert) u1)}))
        (ok true)
    )
)

(define-public (mark-arrived-on-scene (alert-id uint))
    (let ((alert (unwrap! (map-get? emergency-alerts alert-id) ERR_ALERT_NOT_FOUND))
          (response (unwrap! (map-get? alert-responses {alert-id: alert-id, responder: tx-sender}) ERR_ALERT_NOT_FOUND)))
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (not (get resolved alert)) ERR_ALERT_RESOLVED)
        
        (map-set alert-responses {alert-id: alert-id, responder: tx-sender} (merge response {arrived-on-scene: true}))
        (ok true)
    )
)

(define-public (verify-emergency-alert (alert-id uint))
    (let ((alert (unwrap! (map-get? emergency-alerts alert-id) ERR_ALERT_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (get resolved alert)) ERR_ALERT_RESOLVED)
        
        (map-set emergency-alerts alert-id (merge alert {verified: true}))
        (ok true)
    )
)

(define-public (resolve-emergency-alert (alert-id uint))
    (let ((alert (unwrap! (map-get? emergency-alerts alert-id) ERR_ALERT_NOT_FOUND)))
        (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (is-eq tx-sender (get creator alert))) ERR_UNAUTHORIZED)
        (asserts! (not (get resolved alert)) ERR_ALERT_RESOLVED)
        
        (map-set emergency-alerts alert-id (merge alert {resolved: true}))
        (ok true)
    )
)

(define-public (escalate-alert-severity (alert-id uint) (new-severity uint))
    (let ((alert (unwrap! (map-get? emergency-alerts alert-id) ERR_ALERT_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (get resolved alert)) ERR_ALERT_RESOLVED)
        (asserts! (and (>= new-severity u1) (<= new-severity u5)) ERR_INVALID_SEVERITY)
        (asserts! (> new-severity (get severity alert)) ERR_INVALID_AMOUNT)
        
        (map-set emergency-alerts alert-id (merge alert {severity: new-severity}))
        (ok true)
    )
)

(define-public (add-safety-zone (zone-name (string-ascii 50)) (description (string-ascii 200)) (priority-level uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (and (>= priority-level u1) (<= priority-level u5)) ERR_INVALID_AMOUNT)
        (asserts! (is-none (map-get? safety-zones zone-name)) ERR_ALREADY_MEMBER)
        
        (map-set safety-zones zone-name {
            zone-name: zone-name,
            description: description,
            priority-level: priority-level,
            active: true
        })
        (ok true)
    )
)

(define-public (deactivate-safety-zone (zone-name (string-ascii 50)))
    (let ((zone (unwrap! (map-get? safety-zones zone-name) ERR_INVALID_ZONE)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set safety-zones zone-name (merge zone {active: false}))
        (ok true)
    )
)

(define-public (broadcast-zone-alert (zone (string-ascii 50)) (title (string-ascii 100)) (description (string-ascii 400)) (severity uint))
    (let ((alert-id (+ (var-get alert-counter) u1))
          (zone-info (unwrap! (map-get? safety-zones zone) ERR_INVALID_ZONE)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (and (>= severity u1) (<= severity u5)) ERR_INVALID_SEVERITY)
        (asserts! (get active zone-info) ERR_INVALID_ZONE)
        
        (map-set emergency-alerts alert-id {
            id: alert-id,
            creator: tx-sender,
            title: title,
            description: description,
            severity: severity,
            zone: zone,
            location: "ZONE-WIDE",
            timestamp: stacks-block-height,
            resolved: false,
            responder-count: u0,
            acknowledgment-count: u0,
            verified: true
        })
        (var-set alert-counter alert-id)
        (ok alert-id)
    )
)

(define-read-only (get-emergency-alert (alert-id uint))
    (map-get? emergency-alerts alert-id)
)

(define-read-only (get-alert-response (alert-id uint) (responder principal))
    (map-get? alert-responses {alert-id: alert-id, responder: responder})
)

(define-read-only (get-alert-acknowledgment (alert-id uint) (member principal))
    (map-get? alert-acknowledgments {alert-id: alert-id, member: member})
)

(define-read-only (get-safety-zone (zone-name (string-ascii 50)))
    (map-get? safety-zones zone-name)
)

(define-read-only (get-alert-count)
    (var-get alert-counter)
)

(define-read-only (get-active-alerts-by-severity (min-severity uint))
    (let ((current-counter (var-get alert-counter)))
        (fold check-alert-severity (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) (list))
    )
)

(define-private (check-alert-severity (alert-id uint) (acc (list 10 uint)))
    (let ((alert (map-get? emergency-alerts alert-id)))
        (if (and (is-some alert) 
                 (not (get resolved (unwrap-panic alert)))
                 (>= (get severity (unwrap-panic alert)) u3))
            (unwrap-panic (as-max-len? (append acc alert-id) u10))
            acc
        )
    )
)

(define-read-only (is-zone-emergency (zone (string-ascii 50)))
    (let ((zone-info (map-get? safety-zones zone)))
        (if (is-some zone-info)
            (>= (get priority-level (unwrap-panic zone-info)) u4)
            false
        )
    )
)

;; PATROL SYSTEM - NEW FEATURE

;; Patrol-specific maps
(define-map patrol-shifts uint {
    id: uint,
    patroller: principal,
    zone: (string-ascii 50),
    start-time: uint,
    end-time: uint,
    status: uint,
    check-in-time: (optional uint),
    check-out-time: (optional uint),
    observations-count: uint,
    reputation-awarded: bool
})

(define-map patrol-observations uint {
    id: uint,
    shift-id: uint,
    observer: principal,
    location: (string-ascii 100),
    observation-type: (string-ascii 50),
    description: (string-ascii 300),
    timestamp: uint,
    severity: uint
})

(define-map member-patrol-stats principal {
    total-shifts: uint,
    completed-shifts: uint,
    missed-shifts: uint,
    total-hours: uint,
    observations-made: uint,
    last-patrol: uint
})

;; Schedule a patrol shift
(define-public (schedule-patrol-shift (zone (string-ascii 50)) (start-time uint) (end-time uint))
    (let ((shift-id (+ (var-get shift-counter) u1))
          (current-block stacks-block-height))
        (asserts! (> end-time start-time) ERR_INVALID_AMOUNT)
        (asserts! (> start-time current-block) ERR_INVALID_AMOUNT)
        (asserts! (<= (- end-time start-time) u144) ERR_INVALID_AMOUNT)
        (asserts! (is-some (map-get? members tx-sender)) ERR_NOT_MEMBER)
        (asserts! (not (has-shift-conflict tx-sender start-time end-time)) ERR_SHIFT_CONFLICT)
        
        (map-set patrol-shifts shift-id {
            id: shift-id,
            patroller: tx-sender,
            zone: zone,
            start-time: start-time,
            end-time: end-time,
            status: u1,
            check-in-time: none,
            check-out-time: none,
            observations-count: u0,
            reputation-awarded: false
        })
        
        (var-set shift-counter shift-id)
        (unwrap-panic (update-member-patrol-stats tx-sender u1 u0 u0 u0 u0))
        (ok shift-id)
    )
)

;; Check in to start patrol
(define-public (check-in-patrol (shift-id uint))
    (let ((shift (unwrap! (map-get? patrol-shifts shift-id) ERR_SHIFT_NOT_FOUND))
          (current-block stacks-block-height))
        (asserts! (is-eq (get patroller shift) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status shift) u1) ERR_ALREADY_CHECKED_IN)
        (asserts! (>= current-block (get start-time shift)) ERR_SHIFT_NOT_STARTED)
        (asserts! (<= current-block (get end-time shift)) ERR_INVALID_AMOUNT)
        
        (map-set patrol-shifts shift-id (merge shift {
            status: u2,
            check-in-time: (some current-block)
        }))
        (ok true)
    )
)

;; Check out to end patrol
(define-public (check-out-patrol (shift-id uint))
    (let ((shift (unwrap! (map-get? patrol-shifts shift-id) ERR_SHIFT_NOT_FOUND))
          (current-block stacks-block-height)
          (check-in-block (unwrap! (get check-in-time shift) ERR_NOT_CHECKED_IN))
          (patrol-hours (/ (- current-block check-in-block) u6)))
        
        (asserts! (is-eq (get patroller shift) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status shift) u2) ERR_NOT_CHECKED_IN)
        
        (map-set patrol-shifts shift-id (merge shift {
            status: u3,
            check-out-time: (some current-block)
        }))
        
        (unwrap-panic (update-member-patrol-stats tx-sender u0 u1 u0 patrol-hours u0))
        (unwrap-panic (award-patrol-reputation shift-id patrol-hours))
        (ok true)
    )
)

;; Submit observation during patrol
(define-public (submit-patrol-observation (shift-id uint) (location (string-ascii 100)) 
                                        (observation-type (string-ascii 50)) (description (string-ascii 300)) 
                                        (severity uint))
    (let ((shift (unwrap! (map-get? patrol-shifts shift-id) ERR_SHIFT_NOT_FOUND))
          (obs-id (+ (var-get patrol-observation-counter) u1)))
        
        (asserts! (is-eq (get patroller shift) tx-sender) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status shift) u2) ERR_NOT_CHECKED_IN)
        (asserts! (and (>= severity u1) (<= severity u3)) ERR_INVALID_AMOUNT)
        
        (map-set patrol-observations obs-id {
            id: obs-id,
            shift-id: shift-id,
            observer: tx-sender,
            location: location,
            observation-type: observation-type,
            description: description,
            timestamp: stacks-block-height,
            severity: severity
        })
        
        (map-set patrol-shifts shift-id (merge shift {
            observations-count: (+ (get observations-count shift) u1)
        }))
        
        (var-set patrol-observation-counter obs-id)
        (unwrap-panic (update-member-patrol-stats tx-sender u0 u0 u0 u0 u1))
        (ok obs-id)
    )
)

;; Private functions for patrol system

(define-private (has-shift-conflict (patroller principal) (start-time uint) (end-time uint))
    (get conflict (fold check-single-shift-conflict 
          (list u1 u2 u3 u4 u5)
          {patroller: patroller, start: start-time, end: end-time, conflict: false}))
)

(define-private (check-single-shift-conflict (shift-id uint) 
                                           (params {patroller: principal, start: uint, end: uint, conflict: bool}))
    (let ((shift (map-get? patrol-shifts shift-id)))
        (if (and (is-some shift)
                 (is-eq (get patroller (unwrap-panic shift)) (get patroller params))
                 (not (is-eq (get status (unwrap-panic shift)) u3))
                 (not (is-eq (get status (unwrap-panic shift)) u4)))
            (if (or (and (>= (get start params) (get start-time (unwrap-panic shift)))
                         (< (get start params) (get end-time (unwrap-panic shift))))
                    (and (> (get end params) (get start-time (unwrap-panic shift)))
                         (<= (get end params) (get end-time (unwrap-panic shift)))))
                (merge params {conflict: true})
                params)
            params)
    )
)

(define-private (update-member-patrol-stats (member principal) (total-inc uint) (completed-inc uint) 
                                          (missed-inc uint) (hours-inc uint) (obs-inc uint))
    (let ((current-stats (default-to {total-shifts: u0, completed-shifts: u0, missed-shifts: u0, 
                                      total-hours: u0, observations-made: u0, last-patrol: u0} 
                                     (map-get? member-patrol-stats member))))
        (map-set member-patrol-stats member {
            total-shifts: (+ (get total-shifts current-stats) total-inc),
            completed-shifts: (+ (get completed-shifts current-stats) completed-inc),
            missed-shifts: (+ (get missed-shifts current-stats) missed-inc),
            total-hours: (+ (get total-hours current-stats) hours-inc),
            observations-made: (+ (get observations-made current-stats) obs-inc),
            last-patrol: (if (> completed-inc u0) stacks-block-height (get last-patrol current-stats))
        })
        (ok true)
    )
)

(define-private (award-patrol-reputation (shift-id uint) (patrol-hours uint))
    (let ((shift (unwrap! (map-get? patrol-shifts shift-id) ERR_SHIFT_NOT_FOUND))
          (base-reward u30)
          (hour-bonus (* patrol-hours u5))
          (observation-bonus (* (get observations-count shift) u10))
          (total-reward (+ base-reward (+ hour-bonus observation-bonus))))
        
        (map-set patrol-shifts shift-id (merge shift {reputation-awarded: true}))
        (map-set member-reputation (get patroller shift) (+ (default-to u0 (map-get? member-reputation (get patroller shift))) total-reward))
        (ok total-reward)
    )
)

;; Read-only functions for patrol system
(define-read-only (get-patrol-shift (shift-id uint))
    (map-get? patrol-shifts shift-id)
)

(define-read-only (get-patrol-observation (obs-id uint))
    (map-get? patrol-observations obs-id)
)

(define-read-only (get-member-patrol-stats (member principal))
    (map-get? member-patrol-stats member)
)

(define-read-only (get-shift-count)
    (var-get shift-counter)
)

(define-read-only (get-patrol-observation-count)
    (var-get patrol-observation-counter)
)

(define-read-only (is-patroller-available (patroller principal) (start-time uint) (end-time uint))
    (not (has-shift-conflict patroller start-time end-time))
)


