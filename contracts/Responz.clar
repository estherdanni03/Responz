;; title: Responz
;; version: 1.0.0
;; summary: On-Chain Ambulance Dispatch System
;; description: Smart contract for emergency response coordination and ambulance dispatch

;; traits

;; token definitions

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_EMERGENCY_NOT_FOUND (err u101))
(define-constant ERR_INVALID_STATUS (err u102))
(define-constant ERR_AMBULANCE_NOT_FOUND (err u103))
(define-constant ERR_AMBULANCE_BUSY (err u104))
(define-constant ERR_EMERGENCY_ALREADY_ASSIGNED (err u105))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u106))

(define-constant STATUS_PENDING u0)
(define-constant STATUS_DISPATCHED u1)
(define-constant STATUS_EN_ROUTE u2)
(define-constant STATUS_ON_SCENE u3)
(define-constant STATUS_COMPLETED u4)
(define-constant STATUS_CANCELLED u5)

(define-constant AMBULANCE_AVAILABLE u0)
(define-constant AMBULANCE_BUSY u1)
(define-constant AMBULANCE_MAINTENANCE u2)

(define-constant BASE_FEE u1000000)

(define-constant MIN_RESPONSE_TIME u1)
(define-constant MAX_RESPONSE_TIME u1000)
(define-constant PERFORMANCE_THRESHOLD u800000)
(define-constant MAX_WORKLOAD_POINTS u100)
(define-constant EFFICIENCY_MULTIPLIER u10000)

(define-constant INCIDENT_CREATED u0)
(define-constant INCIDENT_DISPATCHED u1)
(define-constant INCIDENT_EN_ROUTE u2)
(define-constant INCIDENT_ON_SCENE u3)
(define-constant INCIDENT_TRANSPORTING u4)
(define-constant INCIDENT_AT_HOSPITAL u5)
(define-constant INCIDENT_COMPLETED u6)
(define-constant INCIDENT_CANCELLED u7)

(define-constant RESPONDER_AMBULANCE u0)
(define-constant RESPONDER_FIRE u1)
(define-constant RESPONDER_POLICE u2)
(define-constant RESPONDER_HOSPITAL u3)

(define-constant ERR_INCIDENT_NOT_FOUND (err u107))
(define-constant ERR_INVALID_PHASE_TRANSITION (err u108))
(define-constant ERR_RESPONDER_NOT_FOUND (err u109))
(define-constant ERR_INSUFFICIENT_FUNDS (err u110))

(define-constant BASE_SERVICE_COST u500000)
(define-constant TRANSPORT_COST_PER_BLOCK u1000)

;; data vars
(define-data-var emergency-counter uint u0)
(define-data-var ambulance-counter uint u0)
(define-data-var total-emergencies uint u0)
(define-data-var total-completed uint u0)
(define-data-var total-response-time uint u0)
(define-data-var performance-reports-counter uint u0)
(define-data-var incident-counter uint u0)
(define-data-var total-incidents uint u0)
(define-data-var total-incident-cost uint u0)

;; data maps
(define-map emergencies
  { emergency-id: uint }
  {
    caller: principal,
    location-lat: int,
    location-lng: int,
    severity: uint,
    status: uint,
    assigned-ambulance: (optional uint),
    created-at: uint,
    updated-at: uint,
    payment: uint
  }
)

(define-map ambulances
  { ambulance-id: uint }
  {
    operator: principal,
    location-lat: int,
    location-lng: int,
    status: uint,
    current-emergency: (optional uint),
    total-responses: uint,
    registered-at: uint
  }
)

(define-map authorized-dispatchers principal bool)
(define-map emergency-history { emergency-id: uint, timestamp: uint } { status: uint, ambulance-id: (optional uint) })

(define-map ambulance-performance
  { ambulance-id: uint }
  {
    avg-response-time: uint,
    efficiency-score: uint,
    workload-points: uint,
    total-distance: uint,
    success-rate: uint,
    last-updated: uint
  }
)

(define-map performance-reports
  { report-id: uint }
  {
    ambulance-id: uint,
    emergency-id: uint,
    response-time: uint,
    distance-traveled: uint,
    outcome-success: bool,
    report-timestamp: uint,
    reporter: principal
  }
)

(define-map ambulance-workload
  { ambulance-id: uint, date: uint }
  {
    calls-handled: uint,
    total-time-busy: uint,
    avg-response-time: uint
  }
)

(define-map resource-allocation-log
  { allocation-id: uint }
  {
    emergency-id: uint,
    recommended-ambulances: (list 5 uint),
    selected-ambulance: uint,
    allocation-score: uint,
    timestamp: uint
  }
)

(define-map incidents
  { incident-id: uint }
  {
    emergency-id: uint,
    patient-id: (optional principal),
    current-phase: uint,
    priority-level: uint,
    total-cost: uint,
    created-at: uint,
    completed-at: (optional uint),
    created-by: principal
  }
)

(define-map incident-phases
  { incident-id: uint, phase: uint }
  {
    phase-name: uint,
    started-at: uint,
    completed-at: (optional uint),
    responder-id: (optional uint),
    responder-type: uint,
    phase-cost: uint,
    notes: (optional (string-ascii 100))
  }
)

(define-map incident-responders
  { incident-id: uint, responder-id: uint }
  {
    responder-type: uint,
    assigned-at: uint,
    role: (string-ascii 50),
    active: bool,
    performance-score: uint
  }
)

(define-map incident-costs
  { incident-id: uint }
  {
    base-service-cost: uint,
    transport-cost: uint,
    additional-costs: uint,
    insurance-coverage: uint,
    patient-payment: uint,
    total-billed: uint
  }
)

(define-map incident-handoffs
  { incident-id: uint, handoff-id: uint }
  {
    from-responder: uint,
    to-responder: uint,
    handoff-time: uint,
    patient-condition: (string-ascii 200),
    verified: bool
  }
)

;; public functions
(define-public (register-emergency (location-lat int) (location-lng int) (severity uint))
  (let
    (
      (emergency-id (+ (var-get emergency-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (>= (stx-get-balance tx-sender) BASE_FEE) ERR_INSUFFICIENT_PAYMENT)
    (try! (stx-transfer? BASE_FEE tx-sender (as-contract tx-sender)))
    (map-set emergencies
      { emergency-id: emergency-id }
      {
        caller: tx-sender,
        location-lat: location-lat,
        location-lng: location-lng,
        severity: severity,
        status: STATUS_PENDING,
        assigned-ambulance: none,
        created-at: current-block,
        updated-at: current-block,
        payment: BASE_FEE
      }
    )
    (var-set emergency-counter emergency-id)
    (var-set total-emergencies (+ (var-get total-emergencies) u1))
    (ok emergency-id)
  )
)

(define-public (register-ambulance (location-lat int) (location-lng int))
  (let
    (
      (ambulance-id (+ (var-get ambulance-counter) u1))
      (current-block stacks-block-height)
    )
    (map-set ambulances
      { ambulance-id: ambulance-id }
      {
        operator: tx-sender,
        location-lat: location-lat,
        location-lng: location-lng,
        status: AMBULANCE_AVAILABLE,
        current-emergency: none,
        total-responses: u0,
        registered-at: current-block
      }
    )
    (var-set ambulance-counter ambulance-id)
    (ok ambulance-id)
  )
)

(define-public (dispatch-ambulance (emergency-id uint) (ambulance-id uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_EMERGENCY_NOT_FOUND))
      (ambulance (unwrap! (map-get? ambulances { ambulance-id: ambulance-id }) ERR_AMBULANCE_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender CONTRACT_OWNER) (default-to false (map-get? authorized-dispatchers tx-sender))) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status emergency) STATUS_PENDING) ERR_EMERGENCY_ALREADY_ASSIGNED)
    (asserts! (is-eq (get status ambulance) AMBULANCE_AVAILABLE) ERR_AMBULANCE_BUSY)
    
    (map-set emergencies
      { emergency-id: emergency-id }
      (merge emergency {
        status: STATUS_DISPATCHED,
        assigned-ambulance: (some ambulance-id),
        updated-at: current-block
      })
    )
    
    (map-set ambulances
      { ambulance-id: ambulance-id }
      (merge ambulance {
        status: AMBULANCE_BUSY,
        current-emergency: (some emergency-id)
      })
    )
    
    (map-set emergency-history
      { emergency-id: emergency-id, timestamp: current-block }
      { status: STATUS_DISPATCHED, ambulance-id: (some ambulance-id) }
    )
    
    (ok true)
  )
)

(define-public (update-emergency-status (emergency-id uint) (new-status uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_EMERGENCY_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or 
      (is-eq tx-sender (get caller emergency))
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (asserts! (<= new-status STATUS_CANCELLED) ERR_INVALID_STATUS)
    
    (map-set emergencies
      { emergency-id: emergency-id }
      (merge emergency {
        status: new-status,
        updated-at: current-block
      })
    )
    
    (map-set emergency-history
      { emergency-id: emergency-id, timestamp: current-block }
      { status: new-status, ambulance-id: (get assigned-ambulance emergency) }
    )
    
    (if (is-eq new-status STATUS_COMPLETED)
      (begin
        (var-set total-completed (+ (var-get total-completed) u1))
        (match (get assigned-ambulance emergency)
          ambulance-id (update-ambulance-completion ambulance-id)
          true
        )
      )
      true
    )
    
    (ok true)
  )
)

(define-public (update-ambulance-location (ambulance-id uint) (location-lat int) (location-lng int))
  (let
    (
      (ambulance (unwrap! (map-get? ambulances { ambulance-id: ambulance-id }) ERR_AMBULANCE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get operator ambulance)) ERR_UNAUTHORIZED)
    
    (map-set ambulances
      { ambulance-id: ambulance-id }
      (merge ambulance {
        location-lat: location-lat,
        location-lng: location-lng
      })
    )
    
    (ok true)
  )
)

(define-public (set-ambulance-status (ambulance-id uint) (new-status uint))
  (let
    (
      (ambulance (unwrap! (map-get? ambulances { ambulance-id: ambulance-id }) ERR_AMBULANCE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get operator ambulance)) ERR_UNAUTHORIZED)
    (asserts! (<= new-status AMBULANCE_MAINTENANCE) ERR_INVALID_STATUS)
    
    (map-set ambulances
      { ambulance-id: ambulance-id }
      (merge ambulance { status: new-status })
    )
    
    (ok true)
  )
)

(define-public (authorize-dispatcher (dispatcher principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set authorized-dispatchers dispatcher true)
    (ok true)
  )
)

(define-public (revoke-dispatcher (dispatcher principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-delete authorized-dispatchers dispatcher)
    (ok true)
  )
)

(define-public (submit-performance-report (ambulance-id uint) (emergency-id uint) (response-time uint) (distance-traveled uint) (outcome-success bool))
  (let
    (
      (report-id (+ (var-get performance-reports-counter) u1))
      (current-block stacks-block-height)
      (ambulance (unwrap! (map-get? ambulances { ambulance-id: ambulance-id }) ERR_AMBULANCE_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender (get operator ambulance))
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (asserts! (and (>= response-time MIN_RESPONSE_TIME) (<= response-time MAX_RESPONSE_TIME)) ERR_INVALID_STATUS)
    
    (map-set performance-reports
      { report-id: report-id }
      {
        ambulance-id: ambulance-id,
        emergency-id: emergency-id,
        response-time: response-time,
        distance-traveled: distance-traveled,
        outcome-success: outcome-success,
        report-timestamp: current-block,
        reporter: tx-sender
      }
    )
    
    (var-set performance-reports-counter report-id)
    (var-set total-response-time (+ (var-get total-response-time) response-time))
    
    (ok report-id)
  )
)

(define-public (calculate-optimal-dispatch (emergency-id uint))
  (let
    (
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_EMERGENCY_NOT_FOUND))
      (current-block stacks-block-height)
      (ambulance-count (var-get ambulance-counter))
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (let
      (
        (recommended-list (list u1 u2 u3))
        (allocation-id (+ (var-get total-emergencies) u1))
      )
      (map-set resource-allocation-log
        { allocation-id: allocation-id }
        {
          emergency-id: emergency-id,
          recommended-ambulances: recommended-list,
          selected-ambulance: u0,
          allocation-score: (calculate-allocation-score (unwrap-panic (element-at? recommended-list u0))),
          timestamp: current-block
        }
      )
      
      (ok recommended-list)
    )
  )
)

(define-public (update-workload-metrics (ambulance-id uint) (calls-handled uint) (total-time-busy uint) (avg-response-time uint))
  (let
    (
      (ambulance (unwrap! (map-get? ambulances { ambulance-id: ambulance-id }) ERR_AMBULANCE_NOT_FOUND))
      (current-date (/ stacks-block-height u144))
    )
    (asserts! (is-eq tx-sender (get operator ambulance)) ERR_UNAUTHORIZED)
    
    (map-set ambulance-workload
      { ambulance-id: ambulance-id, date: current-date }
      {
        calls-handled: calls-handled,
        total-time-busy: total-time-busy,
        avg-response-time: avg-response-time
      }
    )
    
    (ok true)
  )
)

(define-public (get-performance-recommendations (ambulance-id uint))
  (let
    (
      (performance (default-to 
        { avg-response-time: u0, efficiency-score: u0, workload-points: u0, total-distance: u0, success-rate: u0, last-updated: u0 }
        (map-get? ambulance-performance { ambulance-id: ambulance-id })
      ))
    )
    (ok {
      current-efficiency: (get efficiency-score performance),
      recommended-actions: (get-improvement-actions (get efficiency-score performance) (get workload-points performance)),
      performance-rank: u1,
      workload-status: (if (> (get workload-points performance) MAX_WORKLOAD_POINTS) "overloaded" "normal")
    })
  )
)

(define-public (create-incident (emergency-id uint) (patient-id (optional principal)) (priority-level uint))
  (let
    (
      (incident-id (+ (var-get incident-counter) u1))
      (current-block stacks-block-height)
      (emergency (unwrap! (map-get? emergencies { emergency-id: emergency-id }) ERR_EMERGENCY_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (map-set incidents
      { incident-id: incident-id }
      {
        emergency-id: emergency-id,
        patient-id: patient-id,
        current-phase: INCIDENT_CREATED,
        priority-level: priority-level,
        total-cost: u0,
        created-at: current-block,
        completed-at: none,
        created-by: tx-sender
      }
    )
    
    (map-set incident-phases
      { incident-id: incident-id, phase: INCIDENT_CREATED }
      {
        phase-name: INCIDENT_CREATED,
        started-at: current-block,
        completed-at: none,
        responder-id: none,
        responder-type: RESPONDER_AMBULANCE,
        phase-cost: u0,
        notes: none
      }
    )
    
    (map-set incident-costs
      { incident-id: incident-id }
      {
        base-service-cost: BASE_SERVICE_COST,
        transport-cost: u0,
        additional-costs: u0,
        insurance-coverage: u0,
        patient-payment: u0,
        total-billed: u0
      }
    )
    
    (var-set incident-counter incident-id)
    (var-set total-incidents (+ (var-get total-incidents) u1))
    
    (ok incident-id)
  )
)

(define-public (advance-incident-phase (incident-id uint) (new-phase uint) (responder-id (optional uint)) (responder-type uint))
  (let
    (
      (incident (unwrap! (map-get? incidents { incident-id: incident-id }) ERR_INCIDENT_NOT_FOUND))
      (current-block stacks-block-height)
      (current-phase (get current-phase incident))
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (asserts! (is-valid-phase-transition current-phase new-phase) ERR_INVALID_PHASE_TRANSITION)
    
    (map-set incident-phases
      { incident-id: incident-id, phase: current-phase }
      (merge (unwrap-panic (map-get? incident-phases { incident-id: incident-id, phase: current-phase }))
        { completed-at: (some current-block) }
      )
    )
    
    (map-set incident-phases
      { incident-id: incident-id, phase: new-phase }
      {
        phase-name: new-phase,
        started-at: current-block,
        completed-at: none,
        responder-id: responder-id,
        responder-type: responder-type,
        phase-cost: (calculate-phase-cost new-phase),
        notes: none
      }
    )
    
    (map-set incidents
      { incident-id: incident-id }
      (merge incident { 
        current-phase: new-phase,
        total-cost: (+ (get total-cost incident) (calculate-phase-cost new-phase))
      })
    )
    
    (if (is-eq new-phase INCIDENT_COMPLETED)
      (map-set incidents
        { incident-id: incident-id }
        (merge incident { completed-at: (some current-block) })
      )
      true
    )
    
    (ok true)
  )
)

(define-public (assign-responder (incident-id uint) (responder-id uint) (responder-type uint) (role (string-ascii 50)))
  (let
    (
      (incident (unwrap! (map-get? incidents { incident-id: incident-id }) ERR_INCIDENT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (map-set incident-responders
      { incident-id: incident-id, responder-id: responder-id }
      {
        responder-type: responder-type,
        assigned-at: current-block,
        role: role,
        active: true,
        performance-score: u0
      }
    )
    
    (ok true)
  )
)

(define-public (create-handoff (incident-id uint) (handoff-id uint) (from-responder uint) (to-responder uint) (patient-condition (string-ascii 200)))
  (let
    (
      (incident (unwrap! (map-get? incidents { incident-id: incident-id }) ERR_INCIDENT_NOT_FOUND))
      (current-block stacks-block-height)
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (map-set incident-handoffs
      { incident-id: incident-id, handoff-id: handoff-id }
      {
        from-responder: from-responder,
        to-responder: to-responder,
        handoff-time: current-block,
        patient-condition: patient-condition,
        verified: false
      }
    )
    
    (ok true)
  )
)

(define-public (verify-handoff (incident-id uint) (handoff-id uint))
  (let
    (
      (handoff (unwrap! (map-get? incident-handoffs { incident-id: incident-id, handoff-id: handoff-id }) ERR_INCIDENT_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (map-set incident-handoffs
      { incident-id: incident-id, handoff-id: handoff-id }
      (merge handoff { verified: true })
    )
    
    (ok true)
  )
)

(define-public (finalize-incident-billing (incident-id uint) (insurance-coverage uint) (patient-payment uint))
  (let
    (
      (incident (unwrap! (map-get? incidents { incident-id: incident-id }) ERR_INCIDENT_NOT_FOUND))
      (costs (unwrap! (map-get? incident-costs { incident-id: incident-id }) ERR_INCIDENT_NOT_FOUND))
    )
    (asserts! (or 
      (is-eq tx-sender CONTRACT_OWNER)
      (default-to false (map-get? authorized-dispatchers tx-sender))
    ) ERR_UNAUTHORIZED)
    
    (asserts! (is-eq (get current-phase incident) INCIDENT_COMPLETED) ERR_INVALID_PHASE_TRANSITION)
    
    (let
      (
        (total-billed (+ (get base-service-cost costs) (get transport-cost costs) (get additional-costs costs)))
      )
      (map-set incident-costs
        { incident-id: incident-id }
        (merge costs {
          insurance-coverage: insurance-coverage,
          patient-payment: patient-payment,
          total-billed: total-billed
        })
      )
      
      (var-set total-incident-cost (+ (var-get total-incident-cost) total-billed))
      
      (ok total-billed)
    )
  )
)

;; read only functions
(define-read-only (get-emergency (emergency-id uint))
  (map-get? emergencies { emergency-id: emergency-id })
)

(define-read-only (get-ambulance (ambulance-id uint))
  (map-get? ambulances { ambulance-id: ambulance-id })
)

(define-read-only (get-emergency-count)
  (var-get emergency-counter)
)

(define-read-only (get-ambulance-count)
  (var-get ambulance-counter)
)

(define-read-only (get-stats)
  {
    total-emergencies: (var-get total-emergencies),
    total-completed: (var-get total-completed),
    active-emergencies: (var-get emergency-counter),
    registered-ambulances: (var-get ambulance-counter)
  }
)

(define-read-only (is-authorized-dispatcher (dispatcher principal))
  (default-to false (map-get? authorized-dispatchers dispatcher))
)

(define-read-only (get-emergency-history (emergency-id uint) (timestamp uint))
  (map-get? emergency-history { emergency-id: emergency-id, timestamp: timestamp })
)

(define-read-only (get-ambulance-performance (ambulance-id uint))
  (map-get? ambulance-performance { ambulance-id: ambulance-id })
)

(define-read-only (get-performance-report (report-id uint))
  (map-get? performance-reports { report-id: report-id })
)

(define-read-only (get-workload-metrics (ambulance-id uint) (date uint))
  (map-get? ambulance-workload { ambulance-id: ambulance-id, date: date })
)

(define-read-only (get-allocation-log (allocation-id uint))
  (map-get? resource-allocation-log { allocation-id: allocation-id })
)

(define-read-only (get-system-performance-stats)
  (let
    (
      (total-reports (var-get performance-reports-counter))
      (avg-system-response (if (> total-reports u0) (/ (var-get total-response-time) total-reports) u0))
    )
    {
      total-performance-reports: total-reports,
      avg-system-response-time: avg-system-response,
      total-emergencies: (var-get total-emergencies),
      total-completed: (var-get total-completed),
      system-efficiency: (if (> (var-get total-emergencies) u0) 
        (/ (* (var-get total-completed) EFFICIENCY_MULTIPLIER) (var-get total-emergencies)) 
        u0)
    }
  )
)

(define-read-only (get-ambulance-ranking)
  (list)
)

(define-read-only (get-incident (incident-id uint))
  (map-get? incidents { incident-id: incident-id })
)

(define-read-only (get-incident-phase (incident-id uint) (phase uint))
  (map-get? incident-phases { incident-id: incident-id, phase: phase })
)

(define-read-only (get-incident-responder (incident-id uint) (responder-id uint))
  (map-get? incident-responders { incident-id: incident-id, responder-id: responder-id })
)

(define-read-only (get-incident-costs (incident-id uint))
  (map-get? incident-costs { incident-id: incident-id })
)

(define-read-only (get-incident-handoff (incident-id uint) (handoff-id uint))
  (map-get? incident-handoffs { incident-id: incident-id, handoff-id: handoff-id })
)

(define-read-only (get-incident-statistics)
  {
    total-incidents: (var-get total-incidents),
    active-incidents: (var-get incident-counter),
    total-incident-cost: (var-get total-incident-cost),
    avg-incident-cost: (if (> (var-get total-incidents) u0) 
      (/ (var-get total-incident-cost) (var-get total-incidents)) 
      u0)
  }
)

(define-read-only (get-incident-count)
  (var-get incident-counter)
)

;; private functions
(define-private (update-ambulance-completion (ambulance-id uint))
  (match (map-get? ambulances { ambulance-id: ambulance-id })
    ambulance (map-set ambulances
      { ambulance-id: ambulance-id }
      (merge ambulance {
        status: AMBULANCE_AVAILABLE,
        current-emergency: none,
        total-responses: (+ (get total-responses ambulance) u1)
      })
    )
    false
  )
)

(define-private (update-ambulance-performance (ambulance-id uint) (response-time uint) (distance-traveled uint) (outcome-success bool))
  (let
    (
      (current-performance (default-to 
        { avg-response-time: u0, efficiency-score: u0, workload-points: u0, total-distance: u0, success-rate: u0, last-updated: u0 }
        (map-get? ambulance-performance { ambulance-id: ambulance-id })
      ))
      (current-block stacks-block-height)
      (new-avg-response (if (> (get avg-response-time current-performance) u0)
        (/ (+ (get avg-response-time current-performance) response-time) u2)
        response-time))
      (new-total-distance (+ (get total-distance current-performance) distance-traveled))
      (new-success-rate (if outcome-success 
        (if (> (+ (get success-rate current-performance) u100000) u1000000) u1000000 (+ (get success-rate current-performance) u100000))
        (if (< (get success-rate current-performance) u50000) u0 (- (get success-rate current-performance) u50000))))
      (new-efficiency-score (calculate-efficiency-score new-avg-response new-success-rate (get workload-points current-performance)))
    )
    (map-set ambulance-performance
      { ambulance-id: ambulance-id }
      {
        avg-response-time: new-avg-response,
        efficiency-score: new-efficiency-score,
        workload-points: (get workload-points current-performance),
        total-distance: new-total-distance,
        success-rate: new-success-rate,
        last-updated: current-block
      }
    )
    (ok true)
  )
)

(define-private (update-workload-points (ambulance-id uint) (calls-handled uint) (total-time-busy uint))
  (let
    (
      (current-performance (default-to 
        { avg-response-time: u0, efficiency-score: u0, workload-points: u0, total-distance: u0, success-rate: u0, last-updated: u0 }
        (map-get? ambulance-performance { ambulance-id: ambulance-id })
      ))
      (workload-score (+ calls-handled (/ total-time-busy u100)))
    )
    (map-set ambulance-performance
      { ambulance-id: ambulance-id }
      (merge current-performance { workload-points: workload-score })
    )
    (ok true)
  )
)

(define-private (calculate-efficiency-score (avg-response-time uint) (success-rate uint) (workload-points uint))
  (let
    (
      (response-factor (if (> avg-response-time u0) (/ PERFORMANCE_THRESHOLD avg-response-time) u0))
      (success-factor success-rate)
      (workload-factor (if (< workload-points MAX_WORKLOAD_POINTS) 
        (- MAX_WORKLOAD_POINTS workload-points) 
        u0))
    )
    (/ (+ (* response-factor u3) (* success-factor u2) workload-factor) u6)
  )
)

(define-private (calculate-allocation-score (ambulance-id uint))
  (let
    (
      (performance (default-to 
        { avg-response-time: MAX_RESPONSE_TIME, efficiency-score: u0, workload-points: MAX_WORKLOAD_POINTS, total-distance: u0, success-rate: u0, last-updated: u0 }
        (map-get? ambulance-performance { ambulance-id: ambulance-id })
      ))
      (ambulance (unwrap-panic (map-get? ambulances { ambulance-id: ambulance-id })))
    )
    (if (is-eq (get status ambulance) AMBULANCE_AVAILABLE)
      (+ (get efficiency-score performance) (- MAX_WORKLOAD_POINTS (get workload-points performance)))
      u0)
  )
)



(define-private (get-improvement-actions (efficiency-score uint) (workload-points uint))
  (if (< efficiency-score PERFORMANCE_THRESHOLD)
    (if (> workload-points MAX_WORKLOAD_POINTS)
      "reduce_workload_improve_response"
      "improve_response_time")
    (if (> workload-points MAX_WORKLOAD_POINTS)
      "reduce_workload"
      "maintain_performance")))

(define-private (is-valid-phase-transition (current-phase uint) (new-phase uint))
  (or
    (and (is-eq current-phase INCIDENT_CREATED) 
         (or (is-eq new-phase INCIDENT_DISPATCHED) (is-eq new-phase INCIDENT_CANCELLED)))
    (and (is-eq current-phase INCIDENT_DISPATCHED) 
         (or (is-eq new-phase INCIDENT_EN_ROUTE) (is-eq new-phase INCIDENT_CANCELLED)))
    (and (is-eq current-phase INCIDENT_EN_ROUTE) 
         (or (is-eq new-phase INCIDENT_ON_SCENE) (is-eq new-phase INCIDENT_CANCELLED)))
    (and (is-eq current-phase INCIDENT_ON_SCENE) 
         (or (is-eq new-phase INCIDENT_TRANSPORTING) (is-eq new-phase INCIDENT_COMPLETED) (is-eq new-phase INCIDENT_CANCELLED)))
    (and (is-eq current-phase INCIDENT_TRANSPORTING) 
         (or (is-eq new-phase INCIDENT_AT_HOSPITAL) (is-eq new-phase INCIDENT_CANCELLED)))
    (and (is-eq current-phase INCIDENT_AT_HOSPITAL) 
         (or (is-eq new-phase INCIDENT_COMPLETED) (is-eq new-phase INCIDENT_CANCELLED)))
  )
)

(define-private (calculate-phase-cost (phase uint))
  (if (is-eq phase INCIDENT_TRANSPORTING)
    TRANSPORT_COST_PER_BLOCK
    (if (is-eq phase INCIDENT_AT_HOSPITAL)
      (* BASE_SERVICE_COST u2)
      (if (is-eq phase INCIDENT_ON_SCENE)
        BASE_SERVICE_COST
        u0))))



