NLP DEFINITIONS ::=
BEGIN

Translation-response ::= SEQUENCE
  { protocol-id [0] VisibleString ,
    result [1] INTEGER { success (0) ,
                         truncated (1) ,
                         no-address-info (2) ,
                         lookup-failure (3) ,
                         responder-error (4) ,
                         responder-hardware-error (5) ,
                         invalid-request (6) ,
                         responder-limitation-exceeded (7) } ,
    response [2] CHOICE
      { [0] NRS-name-validation-response ,
        [1] System-description-response ,
        [2] Forward-lookup-response ,
        [3] Reverse-lookup-response ,
        [4] Inverse-forward-lookup-response ,
        [5] Inverse-reverse-lookup-response ,
        [6] NSAP-lookup-response } OPTIONAL }

NRS-name-validation-response ::= SEQUENCE
  { standard-name [0] VisibleString ,
    abbreviated-name [1] VisibleString OPTIONAL ,
    info [2] SEQUENCE OF SEQUENCE
      { [0] Context ,
        [1] Address-space-id ,
        information-present [2] BOOLEAN } OPTIONAL }

System-description-response ::= SEQUENCE
  { standard-name [0] VisibleString ,
    abbreviated-name [1] VisibleString OPTIONAL ,
    description [2] SEQUENCE OF VisibleString }

Forward-lookup-response ::= SEQUENCE
  { standard-name [0] VisibleString ,
    abbreviated-name [1] VisibleString OPTIONAL ,
    info [2] SEQUENCE OF SEQUENCE
      { [0] Context ,
        [1] Address-space-id ,
        routes [2] SEQUENCE OF SEQUENCE
          { route-cost [0] Route-cost ,
            addressing-info [1] Addressing-info } } }

Reverse-lookup-response ::= SEQUENCE
  { standard-form-name [0] VisibleString ,
    abbreviated-form-name [1] VisibleString OPTIONAL ,
    context [2] Context }

Inverse-forward-lookup-response ::= Reverse-lookup-response

Inverse-reverse-lookup-response ::= Forward-lookup-response

NSAP-lookup-response ::= SEQUENCE
  { nsap [0] NumericString ,
    info [1] SEQUENCE OF SEQUENCE
      { viewpoint [0] Address-space-id ,
        routes [1] SEQUENCE OF CHOICE
          { x25-address [0] dte-only<Addressing-info ,
            mac-address [1] BIT STRING } } }

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
                               osi-coms (3) }

Route-cost ::= ANY

Addressing-info ::= CHOICE
  { dte-only [0] NumericString ,
    dte-applic-info [1] SEQUENCE
      { dte-number [0] NumericString ,
        applic-info [1] SEQUENCE OF VisibleString } ,
    dte-cudf [2] SEQUENCE
      { dte-number [0] NumericString ,
        cudf [1] OCTET STRING } ,
    dte-cudf-applic-info [3] SEQUENCE
      { dte-number [0] NumericString ,
        cudf [1] OCTET STRING ,
        applic-info [2] SEQUENCE OF VisibleString } ,
    dte-ybts [4] SEQUENCE
      { dte-number [0] NumericString ,
        ybts-string [1] VisibleString } ,
    dte-ybts-applic-info [5] SEQUENCE
      { dte-number [0] NumericString ,
        ybts-string [1] VisibleString ,
        applic-info [2] SEQUENCE OF VisibleString } ,
    dte-ybts-applic-relays [6] SEQUENCE
      { dte-number [0] NumericString ,
        ybts-string [1] VisibleString ,
        applic-relay [2] SEQUENCE OF VisibleString } ,
    none-needed [7] NULL ,
    osi-addressing [8] SEQUENCE
      { nsap [0] NumericString ,
        tselector [1] OCTET STRING OPTIONAL ,
        sselector [2] OCTET STRING OPTIONAL ,
        pselector [3] OCTET STRING OPTIONAL ,
        place-holder [4] ANY OPTIONAL ,
        application-title [5] ANY OPTIONAL ,
        per-application-context-info [6] ANY OPTIONAL } ,
    osi-nsap-only [9] NumericString ,
    osi-nsap-applic-info [10] SEQUENCE
      { nsap [0] NumericString ,
        applic-info [1] SEQUENCE OF VisibleString } ,
    osi-nsap-applic-relays [11] SEQUENCE
      { nsap [0] NumericString ,
        applic-relay [1] SEQUENCE OF VisibleString } }

END
