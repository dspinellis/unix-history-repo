-- P2.py - MHS P2 definitions

-- $Header: /f/osi/pepy/RCS/P2.py,v 7.1 91/02/22 09:34:44 mrose Interim $
--
--
-- $Log:	P2.py,v $
-- Revision 7.1  91/02/22  09:34:44  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:32  mrose
-- Release 6.0
-- 

--
--				  NOTICE
--
--    Acquisition, use, and distribution of this module and related
--    materials are subject to the restrictions of a license agreement.
--    Consult the Preface in the User's Manual for the full terms of
--    this agreement.
--
--


P2 DEFINITIONS	::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/P2.py,v 7.1 91/02/22 09:34:44 mrose Interim $";
#endif
%}

BEGIN

PRINTER	print

-- P2 makes use of types defined in the following modules:
--	P1:  X.411, Section 3.4
--	P3:  X.411, Section 4.3
--	SFD: this Recommendation, Section 5
--	T73: T.73, Section 5

UAPDU ::=
	CHOICE {
	    [0]
		IMPLICIT IM-UAPDU,

	    [1]
		IMPLICIT SR-UAPDU
	}


-- IP-message UAPDU

IM-UAPDU ::=
	SEQUENCE {
	    heading
		Heading,

	    body
		Body
	}


-- heading

Heading ::=
	SET {
	    messageid
		IPMessageId,

	    originator[0]
		IMPLICIT ORDescriptor
		OPTIONAL,

	    authorizingUsers[1]
		IMPLICIT SEQUENCE OF ORDescriptor
		OPTIONAL,
		-- only if not the originator

	    primaryRecipients[2]
		IMPLICIT SEQUENCE OF Recipient
		OPTIONAL,

	    copyRecipients[3]
		IMPLICIT SEQUENCE OF Recipient
		OPTIONAL,

	    blindCopyRecipients[4]
		IMPLICIT SEQUENCE OF Recipient
		OPTIONAL,

	    inReplyTo[5]
		IMPLICIT IPMessageId
		OPTIONAL,
		-- omitted if not in reply to a previous message

	    obsoletes[6]
		IMPLICIT SEQUENCE OF IPMessageId
		OPTIONAL,

	    crossReferences[7]
		IMPLICIT SEQUENCE OF IPMessageId
		OPTIONAL,

	    subject[8]
		CHOICE {
		    T61String
		}
		OPTIONAL,

	    expiryDate[9]
		IMPLICIT P1.Time
		OPTIONAL,
		-- if omitted, expiry date is never

	    replyBy[10]
		IMPLICIT P1.Time
		OPTIONAL,

	    replyToUsers[11]
		IMPLICIT SEQUENCE OF ORDescriptor
		OPTIONAL,
		-- each O/R descriptor must contain an O/R name

	    importance[12]
		IMPLICIT INTEGER {
		    low(0),

		    normal(1),

		    high(2)
		} DEFAULT normal,

	    sensitivity[13]
		IMPLICIT INTEGER {
		    personal(1),

		    private(2),

		    companyConfidential(3)
		} OPTIONAL,

	    autoforwarded[14]
		IMPLICIT BOOLEAN
		DEFAULT FALSE
		-- indicates that the forwarded message body
		-- part(s) were autoforwarded
	}

IPMessageId ::=
	[APPLICATION 11] IMPLICIT SET {
	    ORName OPTIONAL,

	    PrintableString
	}

ORName ::=
	P1.ORName

ORDescriptor ::=
	SET {	-- at least one of the first two members must be present
		ORName OPTIONAL,

	    freeformName[0]
		IMPLICIT T61String
		OPTIONAL,

	    telephoneNumber[1]
		IMPLICIT PrintableString
		OPTIONAL
	}

Recipient ::=
	SET {
	    [0]
		IMPLICIT ORDescriptor,

	    reportRequest[1]
		IMPLICIT BITSTRING {
		    receiptNotification(0),
		    nonreceiptNotification(1),
		    returnIPMessage(2)
		}
		DEFAULT {},
		-- if requested, the O/R descriptor must contain an O/R name

	    replyRequest[2]
		IMPLICIT BOOLEAN
		DEFAULT FALSE
		-- if true, the O/R descriptor must contain an O/R name
	}


-- body

Body ::=
	SEQUENCE OF BodyPart

BodyPart ::=
	CHOICE {
	    [0]
		IMPLICIT IA5Text,

	    [1]
		IMPLICIT TLX,

	    [2]
		IMPLICIT Voice,

	    [3]
		IMPLICIT G3Fax,

	    [4]
		IMPLICIT TIF0,

	    [5]
		IMPLICIT TTX,

	    [6]
		IMPLICIT Videotex,

	    [7]
		NationallyDefined,

	    [8]
		IMPLICIT Encrypted,

	    [9]
		IMPLICIT ForwardedIPMessage,

	    [10]
		IMPLICIT SFD,

	    [11]
		IMPLICIT TIF1
	}


-- body part types

IA5Text ::=
	SEQUENCE {
	    SET {
		repertoire[0]
		    IMPLICIT INTEGER {
			ia5(5),

			ita2(2)
		    } DEFAULT ia5
		    -- additional members of this Set are a
		    -- possible future extension
	    },

	    IA5String
	}

TLX ::=
	ANY -- for further study

Voice ::=
	SEQUENCE {
	    SET, -- members are for further study
	    BITSTRING
	}

G3Fax ::=
	SEQUENCE {
	    SET {
		numberOfPages[0]
		    IMPLICIT INTEGER OPTIONAL,

		[1]
		    IMPLICIT P1.G3NonBasicParams OPTIONAL
	    },

	    SEQUENCE OF BITSTRING
	}

TIF0 ::=
	T73Document

T73Document ::=
	SEQUENCE OF T73.ProtocolElement

TTX ::=
	SEQUENCE {
	    SET {
		numberOfPages[0]
		    IMPLICIT INTEGER OPTIONAL,

		telexCompatible[1]
		    IMPLICIT BOOLEAN DEFAULT FALSE,

		[2]
		    IMPLICIT P1.TeletexNonBasicParams OPTIONAL
	    },

	    SEQUENCE OF T61String
	}

Videotex ::=
	SEQUENCE {
	    SET, -- members are for further study
	    VideotexString
	}

NationallyDefined ::=
	ANY

Encrypted ::=
	SEQUENCE {
	    SET, -- members are for further study
	    BITSTRING
	}

ForwardedIPMessage ::=
	SEQUENCE {
	    SET {
		delivery[0]
		    IMPLICIT P1.Time
		    OPTIONAL,

		[1]
		    IMPLICIT DeliveryInformation
		    OPTIONAL
	    },

	    IM-UAPDU
	}

DeliveryInformation ::=
	P3.DeliverEnvelope

SFD ::=
	SFD.Document

TIF1 ::=
	T73Document


-- IPM-status-report UAPDU

SR-UAPDU ::=
	SET {
	    [0]
		CHOICE {
		    nonReceipt[0]
			IMPLICIT NonReceiptInformation,

		    receipt[1]
			IMPLICIT ReceiptInformation
		},

	    reported
		IPMessageId,

	    actualRecipient[1]
		IMPLICIT ORDescriptor
		OPTIONAL,

	    intendedRecipient[2]
		IMPLICIT ORDescriptor
		OPTIONAL,

	    converted
		P1.EncodedInformationTypes
		OPTIONAL
	}

NonReceiptInformation ::=
	SET {
	    reason[0]
		IMPLICIT INTEGER {
		    uaeInitiatedDiscard(0),

		    autoForwarded(1)
		},

	    nonReceiptQualifer[1]
		IMPLICIT INTEGER {
		    expired(0),

		    obsoleted(1),

		    subscriptionTerminated(2)
		} OPTIONAL,

	    comments[2]
		IMPLICIT PrintableString
		OPTIONAL,
		-- on auto-forward

	    returned[3]
		IMPLICIT IM-UAPDU
		OPTIONAL
	}

ReceiptInformation ::=
	SET {
	    receipt[0]
		IMPLICIT P1.Time,

	    typeOfReceipt[1]
		IMPLICIT INTEGER {
		    explicit(0),

		    automatic(1)
		} DEFAULT explicit,

	[2]
		IMPLICIT P1.SupplementaryInformation OPTIONAL
	}

END
