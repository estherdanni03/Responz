;; title: Emergency Contact Network
;; version: 1.0.0
;; summary: Emergency contact management and automatic notification system
;; description: Allows users to register emergency contacts and automatically notify them during emergencies

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_CONTACT_NOT_FOUND (err u201))
(define-constant ERR_MAX_CONTACTS_REACHED (err u202))
(define-constant ERR_INVALID_RELATIONSHIP (err u203))
(define-constant ERR_DUPLICATE_CONTACT (err u204))
(define-constant ERR_NOTIFICATION_FAILED (err u205))
(define-constant ERR_INVALID_PRIORITY (err u206))

(define-constant MAX_CONTACTS_PER_USER u5)

;; relationship types
(define-constant RELATIONSHIP_FAMILY u0)
(define-constant RELATIONSHIP_FRIEND u1)
(define-constant RELATIONSHIP_MEDICAL u2)
(define-constant RELATIONSHIP_WORKPLACE u3)

;; priority levels
(define-constant PRIORITY_HIGH u0)
(define-constant PRIORITY_MEDIUM u1)
(define-constant PRIORITY_LOW u2)

;; data vars
(define-data-var contact-counter uint u0)
(define-data-var notification-counter uint u0)
(define-data-var total-notifications uint u0)

;; data maps
(define-map user-contacts
  { user: principal, contact-id: uint }
  {
    contact-address: principal,
    contact-name: (string-ascii 50),
    relationship-type: uint,
    priority-level: uint,
    active: bool,
    created-at: uint
  }
)

(define-map user-contact-count
  { user: principal }
  { total-contacts: uint }
)

(define-map emergency-notifications
  { notification-id: uint }
  {
    emergency-id: uint,
    caller: principal,
    contact-address: principal,
    relationship-type: uint,
    priority-level: uint,
    notification-time: uint,
    message-hash: (buff 32),
    acknowledged: bool
  }
)

(define-map contact-preferences
  { user: principal }
  {
    auto-notify-enabled: bool,
    max-priority-level: uint,
    last-updated: uint
  }
)

(define-map notification-stats
  { user: principal }
  {
    total-sent: uint,
    total-received: uint,
    total-acknowledged: uint
  }
)

;; private functions
(define-private (update-notification-stats (user principal) (notifications-sent uint))
  (let
    (
      (current-stats (default-to { total-sent: u0, total-received: u0, total-acknowledged: u0 } 
                                 (map-get? notification-stats { user: user })))
    )
    (map-set notification-stats
      { user: user }
      (merge current-stats { total-sent: (+ (get total-sent current-stats) notifications-sent) })
    )
    true
  )
)

;; public functions
(define-public (register-contact (contact-address principal) (contact-name (string-ascii 50)) (relationship-type uint) (priority-level uint))
  (let
    (
      (current-count (get total-contacts (default-to { total-contacts: u0 } (map-get? user-contact-count { user: tx-sender }))))
      (contact-id (+ (var-get contact-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (< current-count MAX_CONTACTS_PER_USER) ERR_MAX_CONTACTS_REACHED)
    (asserts! (<= relationship-type RELATIONSHIP_WORKPLACE) ERR_INVALID_RELATIONSHIP)
    (asserts! (<= priority-level PRIORITY_LOW) ERR_INVALID_PRIORITY)
    (asserts! (not (is-eq tx-sender contact-address)) ERR_DUPLICATE_CONTACT)
    
    (map-set user-contacts
      { user: tx-sender, contact-id: contact-id }
      {
        contact-address: contact-address,
        contact-name: contact-name,
        relationship-type: relationship-type,
        priority-level: priority-level,
        active: true,
        created-at: current-block
      }
    )
    
    (map-set user-contact-count
      { user: tx-sender }
      { total-contacts: (+ current-count u1) }
    )
    
    (var-set contact-counter contact-id)
    (ok contact-id)
  )
)

(define-public (update-contact-preferences (auto-notify-enabled bool) (max-priority-level uint))
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (<= max-priority-level PRIORITY_LOW) ERR_INVALID_PRIORITY)
    
    (map-set contact-preferences
      { user: tx-sender }
      {
        auto-notify-enabled: auto-notify-enabled,
        max-priority-level: max-priority-level,
        last-updated: current-block
      }
    )
    (ok true)
  )
)

(define-public (notify-single-contact (emergency-id uint) (caller principal) (contact-id uint) (message-hash (buff 32)))
  (let
    (
      (contact (unwrap! (map-get? user-contacts { user: caller, contact-id: contact-id }) ERR_CONTACT_NOT_FOUND))
      (notification-id (+ (var-get notification-counter) u1))
      (current-block stacks-block-height)
    )
    (asserts! (or (is-eq tx-sender caller) (is-eq tx-sender CONTRACT_OWNER)) ERR_UNAUTHORIZED)
    (asserts! (get active contact) ERR_CONTACT_NOT_FOUND)
    
    (map-set emergency-notifications
      { notification-id: notification-id }
      {
        emergency-id: emergency-id,
        caller: caller,
        contact-address: (get contact-address contact),
        relationship-type: (get relationship-type contact),
        priority-level: (get priority-level contact),
        notification-time: current-block,
        message-hash: message-hash,
        acknowledged: false
      }
    )
    
    (var-set notification-counter notification-id)
    (var-set total-notifications (+ (var-get total-notifications) u1))
    (update-notification-stats caller u1)
    (ok notification-id)
  )
)

(define-public (acknowledge-notification (notification-id uint))
  (let
    (
      (notification (unwrap! (map-get? emergency-notifications { notification-id: notification-id }) ERR_CONTACT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get contact-address notification)) ERR_UNAUTHORIZED)
    (asserts! (not (get acknowledged notification)) ERR_NOTIFICATION_FAILED)
    
    (map-set emergency-notifications
      { notification-id: notification-id }
      (merge notification { acknowledged: true })
    )
    
    (ok true)
  )
)

(define-public (deactivate-contact (contact-id uint))
  (let
    (
      (contact (unwrap! (map-get? user-contacts { user: tx-sender, contact-id: contact-id }) ERR_CONTACT_NOT_FOUND))
    )
    (map-set user-contacts
      { user: tx-sender, contact-id: contact-id }
      (merge contact { active: false })
    )
    (ok true)
  )
)

;; read-only functions
(define-read-only (get-user-contact (user principal) (contact-id uint))
  (map-get? user-contacts { user: user, contact-id: contact-id })
)

(define-read-only (get-contact-count (user principal))
  (get total-contacts (default-to { total-contacts: u0 } (map-get? user-contact-count { user: user })))
)

(define-read-only (get-contact-preferences (user principal))
  (map-get? contact-preferences { user: user })
)

(define-read-only (get-notification (notification-id uint))
  (map-get? emergency-notifications { notification-id: notification-id })
)

(define-read-only (get-notification-stats (user principal))
  (map-get? notification-stats { user: user })
)

(define-read-only (get-system-stats)
  {
    total-contacts: (var-get contact-counter),
    total-notifications: (var-get total-notifications),
    notification-counter: (var-get notification-counter)
  }
)