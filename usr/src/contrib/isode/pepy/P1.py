-- P1.py - MHS P1 definitions

-- $Header: /f/osi/pepy/RCS/P1.py,v 7.1 91/02/22 09:34:43 mrose Interim $
--
--
-- $Log:	P1.py,v $
-- Revision 7.1  91/02/22  09:34:43  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:31  mrose
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


P1 DEFINITIONS	::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/P1.py,v 7.1 91/02/22 09:34:43 mrose Interim $";
#endif

void	parse_p2 ();
%}

BEGIN

PRINTER	print

-- P1 makes use of types defined in the following module:
-- Sa: Recommendation S.a[14]
-- T73: T.73, Section 5

MPDU ::=
	CHOICE {
	    [0]
		IMPLICIT UserMPDU,

		ServiceMPDU
	}

ServiceMPDU ::=
	CHOICE {
	    [1]
		IMPLICIT DeliveryReportMPDU,

	    [2]
		IMPLICIT ProbeMPDU
	}

UserMPDU ::=
	SEQUENCE {
	    envelope
		UMPDUEnvelope,

	    content
		UMPDUContent
	}

UMPDUEnvelope ::=
	SET {
	    mpduID
		MPDUIdentifier,

	    originator
		ORName,

	    original
		EncodedInformationTypes
		OPTIONAL,

	    ContentType,

	    UAContentId OPTIONAL,

	    Priority DEFAULT normal,

	    PerMessageFlag DEFAULT {},

	    deferredDelivery[0]
		IMPLICIT Time
		OPTIONAL,

	    [1]
		IMPLICIT SEQUENCE OF PerDomainBilateralInfo
		OPTIONAL,

	    [2]
		IMPLICIT SEQUENCE OF RecipientInfo,

	    TraceInformation

	    -- this one's for EAN --,
	    [UNIVERSAL 17]
		IMPLICIT ANY
		OPTIONAL
	}

UMPDUContent ::=
	OCTETSTRING
	%{ parse_p2 ($$, $$_len); %}


-- time

Time ::=
	UniversalTime


-- various envelope information

MPDUIdentifier ::=
	[APPLICATION 4] IMPLICIT SEQUENCE {
	    GlobalDomainIdentifier,
	    IA5String
	}

ContentType ::=
	[APPLICATION 6]
	    IMPLICIT INTEGER {
		p2(2)
	    }

UAContentId ::=
	[APPLICATION 10]
	    IMPLICIT PrintableString

Priority ::=
	[APPLICATION 7]
	    IMPLICIT INTEGER {
		normal(0),

		nonUrgent(1),

		urgent(2)
	    }

PerMessageFlag ::=
	[APPLICATION 8]
	    IMPLICIT BITSTRING {
		discloseRecipients(0),
		conversionProhibited(1),
		alternateRecipientAllowed(2),
		contentReturnRequest(3)
	    }

-- per-domain bilateral information

PerDomainBilateralInfo ::=
	SEQUENCE {
	    country
		CountryName,

		AdministrationDomainName,

		BilateralInfo
	}

BilateralInfo ::=
	ANY

-- recipient information

RecipientInfo ::=
	SET {
	    recipient
		ORName,

	    [0]
		IMPLICIT ExtensionIdentifier,

	    [1]
		IMPLICIT PerRecipientFlag,

	    [2]
		IMPLICIT ExplicitConversion DEFAULT {}

	    -- this one's for EAN --,
	    [UNIVERSAL 2]
		IMPLICIT INTEGER
		OPTIONAL
	}

ExtensionIdentifier ::=
	INTEGER

PerRecipientFlag ::=
	BITSTRING -- See Figure 23/X.411

ExplicitConversion ::=
	INTEGER {
	    iA5TextTeletex(0),

	    teletexTelex(1)
	}


-- trace information

TraceInformation ::=
	[APPLICATION 9]
	    IMPLICIT SEQUENCE OF SEQUENCE {
		domainid	    
		    GlobalDomainIdentifier,

		domaininfo
		    DomainSuppliedInfo
	    }

DomainSuppliedInfo ::=
	SET {
	    arrival[0]
		IMPLICIT Time,

	    deferred[1]
		IMPLICIT Time
		OPTIONAL,

	    action[2]
		IMPLICIT INTEGER {
		    relayed(0),

		    rerouted(1)
		},

	    converted
		EncodedInformationTypes
		OPTIONAL,

	    previous
		GlobalDomainIdentifier OPTIONAL
	}


-- global domain identifier

GlobalDomainIdentifier ::=
	[APPLICATION 3]
	    IMPLICIT SEQUENCE {
		CountryName,
		AdministrationDomainName,
		PrivateDomainIdentifier OPTIONAL
	    }			    

CountryName ::=
	[APPLICATION 1]
	    CHOICE {
		NumericString,
		PrintableString
	    }

AdministrationDomainName ::=
	[APPLICATION 2]
	    CHOICE {
		NumericString,
		PrintableString
	    }

PrivateDomainIdentifier ::=
	CHOICE {
	    NumericString,

	    PrintableString
	}


-- O/R name

ORName ::=
	[APPLICATION 0]
	    IMPLICIT SEQUENCE {
		standard
		    StandardAttributeList,

		domaindefined
		    DomainDefinedAttributeList
		    OPTIONAL
	    }

StandardAttributeList ::=
	SEQUENCE {
		CountryName OPTIONAL,

		AdministrationDomainName OPTIONAL,

	    [0]
		IMPLICIT X121Address OPTIONAL,

	    [1]
		IMPLICIT TerminalID OPTIONAL,

	    [2]
		PrivateDomainName OPTIONAL,

	    [3]
		IMPLICIT OrganizationName OPTIONAL,

	    [4]
		IMPLICIT UniqueUAIdentifier OPTIONAL,

	    [5]
		IMPLICIT PersonalName
		OPTIONAL,

	    [6]
		IMPLICIT SEQUENCE OF OrganizationalUnit OPTIONAL
	}

DomainDefinedAttributeList ::=
	SEQUENCE OF DomainDefinedAttribute

DomainDefinedAttribute ::=
	SEQUENCE {
	    type
		PrintableString,

	    value
		PrintableString
	}

X121Address ::=
	NumericString

TerminalID ::=
	PrintableString

OrganizationName ::=
	PrintableString

UniqueUAIdentifier ::=
	NumericString

PersonalName ::=
	SET {
	    surName[0]
		IMPLICIT PrintableString,

	    givenName[1]
		IMPLICIT PrintableString
	        OPTIONAL,

	    initials[2]
		IMPLICIT PrintableString
		OPTIONAL,

	    generalQualifier[3]
		IMPLICIT PrintableString
		OPTIONAL
    }

OrganizationalUnit ::=
	PrintableString

PrivateDomainName ::=
	CHOICE {
	    NumericString,

	    PrintableString
	}


-- encoded information types

EncodedInformationTypes ::=
	[APPLICATION 5] IMPLICIT SET {
	    [0]
		IMPLICIT BITSTRING {
		    undefined(0),
		    tLX(1),
		    iA5Text(2),
		    g3Fax(3),
		    tIF0(4),
		    tTX(5),
		    videotex(6),
		    voice(7),
		    sFD(8),
		    tIF1(9)
		}
		-- this OPTIONAL is for EAN -- OPTIONAL,

	    [1]
		IMPLICIT G3NonBasicParams
		OPTIONAL,

	    [2]
		IMPLICIT TeletexNonBasicParams
		OPTIONAL,

	    [3]
		IMPLICIT PresentationCapabilities
		OPTIONAL

	-- other non-basic parameters are for further study

	    -- but this one's for EAN --,
	    [UNIVERSAL 3]
		IMPLICIT BITSTRING {
		    undefined(0),
		    tLX(1),
		    iA5Text(2),
		    g3Fax(3),
		    tIF0(4),
		    tTX(5),
		    videotex(6),
		    voice(7),
		    sFD(8),
		    tIF1(9)
		}
		OPTIONAL
	}

G3NonBasicParams ::=
	BITSTRING {
 	    twoDimensional(8),
	    fineResolution(9),
	    unlimitedLength(20),
	    b4Length(21),
	    a3Width(22),
	    b4Width(23),
	    uncompressed(30)
	}

TeletexNonBasicParams ::=
	SET {
	    graphicCharacterSets[0]
		IMPLICIT T61String OPTIONAL,

	    controlCharacterSets[1]
		IMPLICIT T61String OPTIONAL,

	    pageFormats[2]
		IMPLICIT OCTETSTRING OPTIONAL,

	    miscTerminalCapabilities[3]
		IMPLICIT T61String OPTIONAL,

	    privateUse[4]
		IMPLICIT OCTETSTRING OPTIONAL
	}

PresentationCapabilities ::=
	T73.PresentationCapabilities


-- Delivery Report MPDU

DeliveryReportMPDU ::=
	SEQUENCE {
	    envelope
		DeliveryReportEnvelope,

	    content
		DeliveryReportContent
	}

DeliveryReportEnvelope ::=
	SET {
	    report
		MPDUIdentifier,

	    originator
		ORName,

		TraceInformation
	}

DeliveryReportContent ::=
	SET {
	    original
		MPDUIdentifier,

	    intermediate
		TraceInformation OPTIONAL,

		UAContentId OPTIONAL,

	    [0]
		IMPLICIT SEQUENCE OF ReportedRecipientInfo,

	    returned[1]
		IMPLICIT UMPDUContent OPTIONAL,

	    billingInformation[2]
		ANY
		OPTIONAL
	}

ReportedRecipientInfo ::=
	SET {
	    recipient[0]
		IMPLICIT ORName,

	    [1]
		IMPLICIT ExtensionIdentifier,

	    [2]
		IMPLICIT PerRecipientFlag,

	    [3]
		IMPLICIT LastTraceInformation,

	    intendedRecipient[4]
		IMPLICIT ORName
		OPTIONAL,

	    [5]
		IMPLICIT SupplementaryInformation OPTIONAL
	}


-- last trace information

LastTraceInformation ::=
	SET {
	    arrival[0]
		IMPLICIT Time,

	    converted
		EncodedInformationTypes
		OPTIONAL,

	    [1]
		Report
	}

Report ::=
	CHOICE {
	    [0]
		IMPLICIT DeliveredInfo,

	    [1]
		IMPLICIT NonDeliveredInfo
	}

DeliveredInfo ::=
	SET {
	    delivery[0]
		IMPLICIT Time,

	    typeOfUA[1]
		IMPLICIT INTEGER {
		    public(0),

		    private(1)
		} DEFAULT public
	}

NonDeliveredInfo::=
	SET {
	    [1]
		IMPLICIT ReasonCode,

	    [2]
		IMPLICIT DiagnosticCode OPTIONAL
	}

ReasonCode ::=
	INTEGER {
	    transferFailure(0),

	    unableToTransfer(1),

	    conversionNotPerformed(2)
	}

DiagnosticCode	::=
	INTEGER {
	    unrecognizedORName(0),

	    ambiguousORName(1),

	    mtaCongestion(2),

	    loopDetected(3),

	    uaUnavailable(4),

	    maximumTimeExpired(5),

	    encodedInformationTypesUnsupported(6),

	    contentTooLong(7),

	    conversionImpractical(8),

	    conversionProhibited(9),

	    implicitConversionNotResgistered(10),

	    invalidParameters(11)
	}


-- supplementary information

SupplementaryInformation ::=
	PrintableString	-- length limited and for further study


-- Probe MPDU

ProbeMPDU ::=
	ProbeEnvelope

ProbeEnvelope ::=
	SET {
	    probe
		MPDUIdentifier,

	    originator
		ORName,

		ContentType,

		UAContentId OPTIONAL,

	    original
		EncodedInformationTypes
		OPTIONAL,

		TraceInformation,

		PerMessageFlag DEFAULT {},

	    contentLength[0]
		IMPLICIT INTEGER
		OPTIONAL,

	    [1]
		IMPLICIT SEQUENCE OF PerDomainBilateralInfo
		OPTIONAL,

	    [2]
		IMPLICIT SEQUENCE OF RecipientInfo
}

END

%{

void	adios ();


void	parse_p2 (octstr, len)
char	*octstr;
int	len;
{
    PS	    ps;
    PE	    pe;

    if ((ps = ps_alloc (str_open)) == NULLPS)
	adios (NULLCP, "ps_alloc");
    if (str_setup (ps, octstr, len, 0) == NOTOK)
	adios (NULLCP, "str_setup (%s)", ps_error (ps -> ps_errno));

    if ((pe = ps2pe (ps)) == NULLPE)
	adios (NULLCP, "ps2pe (%s)", ps_error (ps -> ps_errno));

    (void) print_P2_UAPDU (pe, 1, NULLIP, NULLVP, NullParm);

    pe_free (pe);
    ps_free (ps);
}

%}
