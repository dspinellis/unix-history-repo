ISO8650-2-ACSE1 DEFINITIONS ::=
BEGIN

ACSEapdu ::= CHOICE
  { aarq AARQapdu ,
    aare AAREapdu ,
    rlrq RLRQapdu ,
    rlre RLREapdu ,
    abrt ABRTapdu }

AARQapdu ::= [APPLICATION 0] IMPLICIT SEQUENCE
  { protocolVersion [0] IMPLICIT BIT STRING { version1 (0) } ,
    calledAETitle [1] ApplicationTitle OPTIONAL ,
    callingAETitle [2] ApplicationTitle OPTIONAL ,
    applicationContextName [3] ApplicationContextName ,
    userInformation [4] AssociationData OPTIONAL }

AAREapdu ::= [APPLICATION 1] IMPLICIT SEQUENCE
  { protocolVersion [0] IMPLICIT BIT STRING { version1 (0) } ,
    result [1] IMPLICIT AssociateResult ,
    respondingAETitle [2] ApplicationTitle OPTIONAL ,
    applicationContextName [3] ApplicationContextName ,
    userInformation [4] AssociationData OPTIONAL }

RLRQapdu ::= [APPLICATION 2] IMPLICIT SEQUENCE
  { reason [0] IMPLICIT ReleaseRequestReason OPTIONAL ,
    userInformation [1] AssociationData OPTIONAL }

RLREapdu ::= [APPLICATION 3] IMPLICIT SEQUENCE
  { reason [0] IMPLICIT ReleaseResponseReason OPTIONAL ,
    userInformation [1] AssociationData OPTIONAL }

ABRTapdu ::= [APPLICATION 4] IMPLICIT SEQUENCE
  { abortSource [0] IMPLICIT ABRTSource ,
    userInformation [1] AssociationData OPTIONAL }

ApplicationTitle ::= OBJECT IDENTIFIER

ApplicationContextName ::= OBJECT IDENTIFIER

AssociationData ::= EXTERNAL

AssociateResult ::= INTEGER { accepted (0) ,
                              rejectedByResponder-reasonNotSpecified (1) ,
                              rejectedByResponder-transient (2) ,
                              rejectedByResponder-permanent (3) ,
                              rejectedByResponder-AETitleNotRecognized (4) ,
                              rejectedByResponder-AContextNameNotSupported (5) ,
                              rejectedByRespondingACPM (6) }

ReleaseRequestReason ::= INTEGER { normal (0) ,
                                   urgent (1) ,
                                   undefined (2) }

ReleaseResponseReason ::= INTEGER { normal (0) ,
                                    notFinished (1) ,
                                    undefined (2) }

ABRTSource ::= INTEGER { requestingACPM (0) ,
                         requestor (1) }

END
