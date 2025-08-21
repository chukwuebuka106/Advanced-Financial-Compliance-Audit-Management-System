;; Auditrium: Advanced Financial Compliance & Audit Management System
;; Comprehensive solution for regulatory compliance, financial auditing, and transparency reporting

;; Error codes
(define-constant ERR-UNAUTHORIZED-ACCESS (err u200))
(define-constant ERR-INVALID-FINANCIAL-AMOUNT (err u201))
(define-constant ERR-COMPLIANCE-RULE-NOT-FOUND (err u202))
(define-constant ERR-INSUFFICIENT-AUDIT-BALANCE (err u203))
(define-constant ERR-INVALID-COMPLIANCE-RATE (err u204))
(define-constant ERR-UNSUPPORTED-ASSET (err u205))
(define-constant ERR-INVALID-AUDIT-DEDUCTION (err u206))
(define-constant ERR-REIMBURSEMENT-DENIED (err u207))
(define-constant ERR-INVALID-REPORTING-PERIOD (err u208))
(define-constant ERR-TRANSACTION-EXECUTION-FAILED (err u209))

;; SIP-010 Fungible Token Trait
(define-trait sip-010-trait (
    (transfer
        (uint principal principal)
        (response bool uint)
    )
    (get-balance
        (principal)
        (response uint uint)
    )
    (get-total-supply
        ()
        (response uint uint)
    )
    (get-decimals
        ()
        (response uint uint)
    )
    (get-name
        ()
        (response (string-ascii 32) uint)
    )
    (get-symbol
        ()
        (response (string-ascii 32) uint)
    )
    (get-token-uri
        ()
        (response (optional (string-utf8 256)) uint)
    )
))

;; Core system variables
(define-data-var compliance-officer principal tx-sender)
(define-data-var minimum-audit-threshold uint u500) ;; Minimum audit amount in base asset

;; Multi-asset exchange rate registry (scaled by 1e8)
(define-map asset-valuation-registry
    { asset-identifier: (string-ascii 10) }
    {
        valuation-rate: uint,
        rate-timestamp: uint,
        asset-active-status: bool,
    }
)

;; Compliance framework with tiered assessment brackets
(define-map compliance-assessment-tiers
    { compliance-category: (string-ascii 24) }
    {
        assessment-brackets: (list 10
            {
            compliance-threshold: uint,
            assessment-rate: uint,
            tier-description: (string-ascii 64),
        }),
        primary-asset: (string-ascii 10),
        tier-last-updated: uint,
    }
)

;; Audit allowance configuration system
(define-map registered-allowances
    { allowance-identifier: (string-ascii 10) }
    {
        allowance-title: (string-ascii 64),
        maximum-allowance-limit: uint,
        allowance-rate: uint,
        requires-authorization: bool,
    }
)

;; Comprehensive entity audit profiles
(define-map entity-audit-profiles
    principal
    {
        total-compliance-fees: uint,
        total-reimbursements: uint,
        latest-assessment: uint,
        entity-classification: (string-ascii 24),
        approved-allowances: (list 20
            {
            allowance-identifier: (string-ascii 10),
            allowance-value: uint,
            authorization-status: bool,
        }),
        audit-transaction-log: (list 50
            {
            transaction-value: uint,
            transaction-block: uint,
            transaction-asset: (string-ascii 10),
        }),
    }
)

;; Read-only functions for comprehensive reporting
(define-read-only (get-entity-audit-profile (entity principal))
    (map-get? entity-audit-profiles entity)
)

(define-read-only (get-compliance-tier-details (compliance-category (string-ascii 24)))
    (map-get? compliance-assessment-tiers { compliance-category: compliance-category })
)

(define-read-only (get-asset-valuation (asset-identifier (string-ascii 10)))
    (map-get? asset-valuation-registry { asset-identifier: asset-identifier })
)

(define-read-only (get-allowance-configuration (allowance-identifier (string-ascii 10)))
    (map-get? registered-allowances { allowance-identifier: allowance-identifier })
)

;; Cross-asset valuation conversion utility
(define-read-only (convert-asset-valuation
        (amount uint)
        (source-asset (string-ascii 10))
        (target-asset (string-ascii 10))
    )
    (let (
            (source-valuation (unwrap! (get-asset-valuation source-asset) ERR-UNSUPPORTED-ASSET))
            (target-valuation (unwrap! (get-asset-valuation target-asset) ERR-UNSUPPORTED-ASSET))
        )
        (ok (/ (* amount (get valuation-rate target-valuation))
            (get valuation-rate source-valuation)
        ))
    )
)

;; Progressive compliance assessment calculator
(define-read-only (calculate-tiered-compliance-fee
        (assessed-amount uint)
        (compliance-category (string-ascii 24))
    )
    (match (map-get? compliance-assessment-tiers { compliance-category: compliance-category })
        tier-configuration (let ((total-compliance-fee u0))
            (ok (fold calculate-tier-assessment
                (get assessment-brackets tier-configuration) {
                remaining-assessment: assessed-amount,
                accumulated-fees: u0,
            }))
        )
        ERR-COMPLIANCE-RULE-NOT-FOUND
    )
)

;; Private helper for progressive compliance fee calculation
(define-private (calculate-tier-assessment
        (assessment-tier {
            compliance-threshold: uint,
            assessment-rate: uint,
            tier-description: (string-ascii 64),
        })
        (calculation-context {
            remaining-assessment: uint,
            accumulated-fees: uint,
        })
    )
    (let (
            (assessable-tier-amount (if (> (get remaining-assessment calculation-context)
                    (get compliance-threshold assessment-tier)
                )
                (- (get remaining-assessment calculation-context)
                    (get compliance-threshold assessment-tier)
                )
                u0
            ))
            (tier-fee-amount (/ (* assessable-tier-amount (get assessment-rate assessment-tier))
                u100
            ))
        )
        {
            remaining-assessment: (get remaining-assessment calculation-context),
            accumulated-fees: (+ (get accumulated-fees calculation-context) tier-fee-amount),
        }
    )
)

;; Helper function for allowance authorization updates
(define-private (update-allowance-authorization
        (target-index uint)
        (current-index uint)
        (allowance {
            allowance-identifier: (string-ascii 10),
            allowance-value: uint,
            authorization-status: bool,
        })
        (index-to-update uint)
    )
    (if (is-eq current-index index-to-update)
        {
            allowance-identifier: (get allowance-identifier allowance),
            allowance-value: (get allowance-value allowance),
            authorization-status: true,
        }
        allowance
    )
)

;; Administrative functions for system configuration
(define-public (update-asset-valuation-rate
        (asset-identifier (string-ascii 10))
        (new-valuation-rate uint)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get compliance-officer))
            ERR-UNAUTHORIZED-ACCESS
        )
        (ok (map-set asset-valuation-registry { asset-identifier: asset-identifier } {
            valuation-rate: new-valuation-rate,
            rate-timestamp: block-height,
            asset-active-status: true,
        }))
    )
)

(define-public (register-audit-allowance
        (allowance-identifier (string-ascii 10))
        (allowance-title (string-ascii 64))
        (maximum-limit uint)
        (allowance-rate uint)
        (requires-authorization bool)
    )
    (begin
        (asserts! (is-eq tx-sender (var-get compliance-officer))
            ERR-UNAUTHORIZED-ACCESS
        )
        (asserts! (<= allowance-rate u100) ERR-INVALID-COMPLIANCE-RATE)
        (ok (map-set registered-allowances { allowance-identifier: allowance-identifier } {
            allowance-title: allowance-title,
            maximum-allowance-limit: maximum-limit,
            allowance-rate: allowance-rate,
            requires-authorization: requires-authorization,
        }))
    )
)

(define-public (submit-allowance-claim
        (allowance-identifier (string-ascii 10))
        (claimed-amount uint)
    )
    (let (
            (allowance-config (unwrap! (get-allowance-configuration allowance-identifier)
                ERR-INVALID-AUDIT-DEDUCTION
            ))
            (entity-profile (default-to {
                total-compliance-fees: u0,
                total-reimbursements: u0,
                latest-assessment: u0,
                entity-classification: "",
                approved-allowances: (list),
                audit-transaction-log: (list),
            }
                (get-entity-audit-profile tx-sender)
            ))
        )
        (begin
            (asserts!
                (<= claimed-amount (get maximum-allowance-limit allowance-config))
                ERR-INVALID-FINANCIAL-AMOUNT
            )
            (ok (map-set entity-audit-profiles tx-sender {
                total-compliance-fees: (get total-compliance-fees entity-profile),
                total-reimbursements: (get total-reimbursements entity-profile),
                latest-assessment: (get latest-assessment entity-profile),
                entity-classification: (get entity-classification entity-profile),
                approved-allowances: (unwrap-panic (as-max-len?
                    (append (get approved-allowances entity-profile) {
                        allowance-identifier: allowance-identifier,
                        allowance-value: claimed-amount,
                        authorization-status: (not (get requires-authorization allowance-config)),
                    })
                    u20
                )),
                audit-transaction-log: (get audit-transaction-log entity-profile),
            }))
        )
    )
)

;; Enhanced allowance authorization system
(define-public (authorize-allowance-claim
        (entity principal)
        (allowance-index uint)
    )
    (let (
            (entity-profile (unwrap! (get-entity-audit-profile entity)
                ERR-COMPLIANCE-RULE-NOT-FOUND
            ))
            (current-allowances (get approved-allowances entity-profile))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get compliance-officer))
                ERR-UNAUTHORIZED-ACCESS
            )
            (asserts! (< allowance-index (len current-allowances))
                ERR-INVALID-AUDIT-DEDUCTION
            )

            (ok (map-set entity-audit-profiles entity {
                total-compliance-fees: (get total-compliance-fees entity-profile),
                total-reimbursements: (get total-reimbursements entity-profile),
                latest-assessment: (get latest-assessment entity-profile),
                entity-classification: (get entity-classification entity-profile),
                approved-allowances: (unwrap-panic (as-max-len?
                    (map update-allowance-authorization (list allowance-index)
                        (list u0) current-allowances (list allowance-index)
                    )
                    u20
                )),
                audit-transaction-log: (get audit-transaction-log entity-profile),
            }))
        )
    )
)

;; Advanced reimbursement processing with native STX transfers
(define-public (process-compliance-reimbursement
        (entity principal)
        (reimbursement-amount uint)
        (reimbursement-asset (string-ascii 10))
    )
    (let (
            (entity-profile (unwrap! (get-entity-audit-profile entity)
                ERR-COMPLIANCE-RULE-NOT-FOUND
            ))
            (converted-reimbursement (unwrap!
                (convert-asset-valuation reimbursement-amount reimbursement-asset
                    "STX"
                )
                ERR-UNSUPPORTED-ASSET
            ))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get compliance-officer))
                ERR-UNAUTHORIZED-ACCESS
            )
            (asserts!
                (<= converted-reimbursement
                    (get total-compliance-fees entity-profile)
                )
                ERR-REIMBURSEMENT-DENIED
            )
            (try! (stx-transfer? converted-reimbursement (var-get compliance-officer)
                entity
            ))
            (ok (map-set entity-audit-profiles entity {
                total-compliance-fees: (get total-compliance-fees entity-profile),
                total-reimbursements: (+ (get total-reimbursements entity-profile)
                    converted-reimbursement
                ),
                latest-assessment: (get latest-assessment entity-profile),
                entity-classification: (get entity-classification entity-profile),
                approved-allowances: (get approved-allowances entity-profile),
                audit-transaction-log: (unwrap-panic (as-max-len?
                    (append (get audit-transaction-log entity-profile) {
                        transaction-value: (- u0 converted-reimbursement),
                        transaction-block: block-height,
                        transaction-asset: reimbursement-asset,
                    })
                    u50
                )),
            }))
        )
    )
)

;; Comprehensive reporting and analytics functions
(define-read-only (generate-compliance-report
        (entity principal)
        (reporting-period uint)
    )
    (let ((entity-profile (unwrap! (get-entity-audit-profile entity) ERR-COMPLIANCE-RULE-NOT-FOUND)))
        (ok {
            total-fees-assessed: (get total-compliance-fees entity-profile),
            total-reimbursements-issued: (get total-reimbursements entity-profile),
            net-compliance-obligation: (- (get total-compliance-fees entity-profile)
                (get total-reimbursements entity-profile)
            ),
            authorized-allowances: (get approved-allowances entity-profile),
            transaction-audit-trail: (get audit-transaction-log entity-profile),
        })
    )
)

(define-read-only (calculate-net-compliance-position (entity principal))
    (let (
            (entity-profile (unwrap! (get-entity-audit-profile entity)
                ERR-COMPLIANCE-RULE-NOT-FOUND
            ))
            (total-authorized-allowances (fold sum-authorized-allowances
                (get approved-allowances entity-profile) u0
            ))
        )
        (ok (- (get total-compliance-fees entity-profile) total-authorized-allowances))
    )
)

;; Private utility for calculating total authorized allowances
(define-private (sum-authorized-allowances
        (allowance {
            allowance-identifier: (string-ascii 10),
            allowance-value: uint,
            authorization-status: bool,
        })
        (accumulator uint)
    )
    (if (get authorization-status allowance)
        (+ accumulator (get allowance-value allowance))
        accumulator
    )
)
