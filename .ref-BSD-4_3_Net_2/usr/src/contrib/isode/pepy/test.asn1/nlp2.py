NLP DEFINITIONS ::=
BEGIN

Translation-request ::= SEQUENCE
  { protocol-id [0] VisibleString ,
    action-request [1] CHOICE
      { [0] NRS-name-validation ,
        [1] System-description ,
        [2] Forward-lookup ,
        [3] Reverse-lookup ,
        [4] Inverse-forward-lookup ,
        [5] Inverse-reverse-lookup ,
        [6] NSAP-forward-lookup } }

NRS-name-validation ::= SEQUENCE
  { user-input [0] VisibleString ,
    standard-environment [1] VisibleString ,
    abbreviated-environment [2] VisibleString OPTIONAL ,
    viewpoints [3] SEQUENCE OF SEQUENCE
      { [0] Context ,
        [1] Address-space-id } OPTIONAL }

System-description ::= SEQUENCE
  { user-input [0] VisibleString ,
    standard-environment [1] VisibleString ,
    abbreviated-environment [2] VisibleString OPTIONAL }

Forward-lookup ::= SEQUENCE
  { user-input [0] VisibleString ,
    standard-environment [1] VisibleString ,
    abbreviated-environment [2] VisibleString  OPTIONAL ,
    viewpoints [3] SEQUENCE OF SEQUENCE
      { [0] Context ,
        [1] Address-space-id } }

Reverse-lookup ::= CHOICE
  { ybts-world [0] SEQUENCE
      { address-space [0] Address-space-id ,
        dte-number [1] NumericString ,
        ybts-string [2] VisibleString OPTIONAL ,
        context [3] Context } ,
    osi-world [1] SEQUENCE
      { nsap [0] NumericString ,
        tselector [1] OCTET STRING OPTIONAL ,
        sselector [2] OCTET STRING OPTIONAL ,
        pselector [3] OCTET STRING OPTIONAL ,
        place-holder [4] ANY OPTIONAL ,
        application-title [5] ANY OPTIONAL ,
        per-application-context-info [6] ANY OPTIONAL ,
        context [7] Context OPTIONAL } }

Inverse-forward-lookup ::= Reverse-lookup

Inverse-reverse-lookup ::= SEQUENCE
  { standard-name [0] VisibleString ,
    viewpoints [1] SEQUENCE OF SEQUENCE
      { [0] Context ,
        [1] Address-space-id } }

NSAP-forward-lookup ::= SEQUENCE
  { nsap [0] NumericString ,
    viewpoints [1] SEQUENCE OF Address-space-id }

Context ::= INTEGER { x29 (0) ,
                      ts29 (1) ,
                      niftp (2) ,
                      mail-niftp (3) ,
                      not-used (4) ,
                      mail-telex (5) ,
                      jtmp (6) ,
                      jtmp-files (7) ,
                      jtmp-reg (8) ,
                      ybts-node (9) ,
                      ybts (10) ,
                      ftam (11) ,
                      jtm (12) ,
                      jtm-reg (13) ,
                      vt (14) ,
                      motis (15) }

Address-space-id ::= INTEGER { pss (0) ,
                               janet (1) ,
                               telex (2) ,
                               osi-cons (3) }

END
