-- EAN Defined Types

-- $Header: /f/osi/pepy/RCS/EAN.py,v 7.1 91/02/22 09:34:40 mrose Interim $
--
--
-- $Log:	EAN.py,v $
-- Revision 7.1  91/02/22  09:34:40  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:29  mrose
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


EAN DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/EAN.py,v 7.1 91/02/22 09:34:40 mrose Interim $";
#endif
%}

BEGIN

PRINTER	print

-- P1: EAN stores the Content of the message after the MPDU, not inside it

MPDU ::=
	CHOICE {
	    [0]
		IMPLICIT UserMPDU,

		ServiceMPDU
	}

ServiceMPDU ::=
	CHOICE {
	    [1]
		IMPLICIT P1.DeliveryReportMPDU,

	    [2]
		IMPLICIT P1.ProbeMPDU
	}

UserMPDU ::=
	SEQUENCE { P1.UMPDUEnvelope }


-- P2: EAN considers the Body OPTIONAL

UAPDU ::=
	CHOICE {
	    [0]
		IMPLICIT IM-UAPDU,

	    [1]
		IMPLICIT P2.SR-UAPDU
	}

IM-UAPDU ::=
	SEQUENCE {
	    heading
		P2.Heading,

	    body
		P2.Body
		OPTIONAL
	}

END
