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

;; data vars
(define-data-var emergency-counter uint u0)
(define-data-var ambulance-counter uint u0)
(define-data-var total-emergencies uint u0)
(define-data-var total-completed uint u0)

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