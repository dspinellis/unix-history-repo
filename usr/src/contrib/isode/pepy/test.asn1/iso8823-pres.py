ISO8823-PRESENTATION DEFINITIONS ::=
BEGIN

CP-type ::= SET
  { [0] IMPLICIT Mode-selector ,
    [1] IMPLICIT SET
      { COMPONENTS OF Reliable-Transfer-APDUs.RTORQapdu } OPTIONAL ,
    [2] IMPLICIT SEQUENCE
      { [0] IMPLICIT Protocol-version DEFAULT { version-1 } ,
        [1] IMPLICIT Calling-presentation-selector OPTIONAL ,
        [2] IMPLICIT Called-presentation-selector OPTIONAL ,
        [4] IMPLICIT Presentation-context-definition-list OPTIONAL ,
        [6] IMPLICIT Default-context-name OPTIONAL ,
        [8] IMPLICIT Presentation-requirements OPTIONAL ,
        [9] IMPLICIT User-session-requirements OPTIONAL ,
        [11] User-data OPTIONAL } OPTIONAL }

CPC-type ::= User-data

CPA-PPDU ::= SET
  { [0] IMPLICIT Mode-selector ,
    [1] IMPLICIT SET
      { COMPONENTS OF Reliable-Transfer-APDUs.RTOACapdu } OPTIONAL ,
    [2] IMPLICIT SEQUENCE
      { [0] IMPLICIT Protocol-version DEFAULT { version-1 } ,
        [3] IMPLICIT Responding-presentation-selector OPTIONAL ,
        [5] IMPLICIT Presentation-context-definition-result-list OPTIONAL ,
        [8] IMPLICIT Presentation-requirements OPTIONAL ,
        [9] IMPLICIT User-session-requirements OPTIONAL ,
        [11] User-data OPTIONAL } OPTIONAL }

CPR-PPDU ::= CHOICE
  { SET
      { COMPONENTS OF Reliable-Transfer-APDUs.RTORJapdu } ,
    SEQUENCE
      { [0] IMPLICIT Protocol-version DEFAULT { version-1 } ,
        [3] IMPLICIT Responding-presentation-selector OPTIONAL ,
        [5] IMPLICIT Presentation-context-definition-result-list OPTIONAL ,
        [7] IMPLICIT Default-context-result OPTIONAL ,
        [10] IMPLICIT Provider-reason OPTIONAL ,
        [11] User-data OPTIONAL } }

Abort-type ::= CHOICE
  { ARU-PPDU ,
    ARP-PPDU }

ARU-PPDU ::= CHOICE
  { SET
      { COMPONENTS OF Reliable-Transfer-APDUs.RTOABapdu } ,
    [0] IMPLICIT SEQUENCE
      { [0] Presentation-context-identifier-list OPTIONAL ,
        [11] User-data OPTIONAL } }

ARP-PPDU ::= SEQUENCE
  { provider-reason [0] IMPLICIT Abort-reason OPTIONAL ,
    [1] IMPLICIT Event-identifier OPTIONAL }

Typed-data-type ::= CHOICE
  { acPPDU [0] IMPLICIT AC-PPDU ,
    acaPPDU [1] IMPLICIT ACA-PPDU ,
    ttdPPDU [11] User-data }

AC-PPDU ::= SEQUENCE
  { [0] IMPLICIT Presentation-context-addition-list OPTIONAL ,
    [1] IMPLICIT Presentation-context-deletion-list OPTIONAL ,
    [11] User-data OPTIONAL }

ACA-PPDU ::= SEQUENCE
  { [0] IMPLICIT Presentation-context-addition-result-list OPTIONAL ,
    [1] IMPLICIT Presentation-context-deletion-result-list OPTIONAL ,
    [11] User-data OPTIONAL }

RS-PPDU ::= SEQUENCE
  { [0] IMPLICIT Presentation-context-identifier-list OPTIONAL ,
    [11] User-data OPTIONAL }

RSA-PPDU ::= SEQUENCE
  { [0] IMPLICIT Presentation-context-identifier-list OPTIONAL ,
    [11] User-data OPTIONAL }

Abort-reason ::= INTEGER { reason-not-specified (0) ,
                           unrecognized-ppdu (1) ,
                           unexpected-ppdu (2) ,
                           unexpected-session-service-primitive (3) ,
                           unrecognized-ppdu-parameter (4) ,
                           unexpected-ppdu-parameter (5) ,
                           invalid-ppdu-parameter-value (6) }

Abstract-syntax-name ::= OBJECT IDENTIFIER

Called-presentation-selector ::= Presentation-selector

Calling-presentation-selector ::= Presentation-selector

Context-list ::= SEQUENCE OF SEQUENCE
  { Presentation-context-identifier ,
    Abstract-syntax-name ,
    SEQUENCE OF Transfer-syntax-name }

Default-context-name ::= SEQUENCE
  { Abstract-syntax-name ,
    Transfer-syntax-name }

Default-context-result ::= Result

Event-identifier ::= INTEGER { cp-PPDU (0) ,
                               cpa-PPDU (1) ,
                               cpr-PPDU (2) ,
                               aru-PPDU (3) ,
                               arp-PPDU (4) ,
                               ac-PPDU (5) ,
                               aca-PPDU (6) ,
                               td-PPDU (7) ,
                               ttd-PPDU (8) ,
                               te-PPDU (9) ,
                               tc-PPDU (10) ,
                               tcc-PPDU (11) ,
                               rs-PPDU (12) ,
                               rsa-PPDU (13) ,
                               s-release-indication (14) ,
                               s-release-confirm (15) ,
                               s-token-give-indication (16) ,
                               s-token-please-indication (17) ,
                               s-control-give-indication (18) ,
                               s-sync-minor-indication (19) ,
                               s-sync-minor-confirm (20) ,
                               s-sync-major-indication (21) ,
                               s-sync-major-confirm (22) ,
                               s-p-exception-report-indication (23) ,
                               s-u-exception-report-indication (24) ,
                               s-activity-start-indication (25) ,
                               s-activity-resume-indication (26) ,
                               s-activity-interrupt-indication (27) ,
                               s-activity-interrupt-confirm (28) ,
                               s-activity-discard-indication (29) ,
                               s-activity-discard-confirm (30) ,
                               s-activity-end-indication (31) ,
                               s-activity-end-confirm (32) }

Mode-selector ::= SET
  { [0] IMPLICIT INTEGER { x410-1984-mode (0) ,
                           normal-mode (1) } }

Presentation-context-addition-list ::= Context-list

Presentation-context-addition-result-list ::= Result-list

Presentation-context-definition-list ::= Context-list

Presentation-context-definition-result-list ::= Result-list

Presentation-context-deletion-list ::= SEQUENCE OF Presentation-context-identifier

Presentation-context-deletion-result-list ::= SEQUENCE OF INTEGER { acceptance (0) ,
                                                                    user-rejection (1) }

Presentation-context-identifier ::= INTEGER

Presentation-context-identifier-list ::= SEQUENCE OF SEQUENCE
  { Presentation-context-identifier ,
    Transfer-syntax-name }

Presentation-requirements ::= BIT STRING { context-management (0) ,
                                           restoration (1) }

Presentation-selector ::= OCTET STRING

Protocol-version ::= BIT STRING { version-1 (0) }

Provider-reason ::= INTEGER { reason-not-specified (0) ,
                              temporary-congestion (1) ,
                              local-limit-exceeded (2) ,
                              called-presentation-address-unknown (3) ,
                              protocol-version-not-supported (4) ,
                              default-context-not-supported (5) ,
                              user-data-not-readable (6) ,
                              no-PSAP-available (7) }

Responding-presentation-selector ::= Presentation-selector

Result ::= INTEGER { acceptance (0) ,
                     provider-rejection (1) ,
                     user-rejection (2) }

Result-list ::= SEQUENCE OF SEQUENCE
  { [0] IMPLICIT Result ,
    [1] IMPLICIT Transfer-syntax-name OPTIONAL ,
    provider-reason [2] IMPLICIT INTEGER { reason-not-specified (0) ,
                                           abstract-syntax-not-supported (1) ,
                                           proposed-transfer-syntaxes-not-supported (2) ,
                                           local-limit-on-DCS-exceeded (3) } OPTIONAL }

Transfer-syntax-name ::= OBJECT IDENTIFIER

User-data ::= CHOICE
  { Simply-encoded-data ,
    Fully-encoded-data }

Simply-encoded-data ::= OCTET STRING

Fully-encoded-data ::= SEQUENCE OF PDV-list

PDV-list ::= SEQUENCE
  { Transfer-syntax-name OPTIONAL ,
    Presentation-context-identifier ,
    presentation-data-values CHOICE
      { single-ASN1-type [0] ANY ,
        octet-aligned [1] IMPLICIT OCTET STRING ,
        arbitrary [2] IMPLICIT BIT STRING } }

User-session-requirements ::= BIT STRING { half-duplex (0) ,
                                           duplex (1) ,
                                           expedited-data (2) ,
                                           minor-synchronize (3) ,
                                           major-synchronize (4) ,
                                           resynchronize (5) ,
                                           activity-management (6) ,
                                           negotiated-release (7) ,
                                           capability-data (8) ,
                                           exceptions (9) ,
                                           typed-data (10) }

END
