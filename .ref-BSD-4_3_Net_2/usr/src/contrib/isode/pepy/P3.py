-- P3 Defined Types (stub)

-- $Header: /f/osi/pepy/RCS/P3.py,v 7.1 91/02/22 09:34:46 mrose Interim $
--
--
-- $Log:	P3.py,v $
-- Revision 7.1  91/02/22  09:34:46  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:33  mrose
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


P3 DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/P3.py,v 7.1 91/02/22 09:34:46 mrose Interim $";
#endif
%}

BEGIN

PRINTER	print

DeliverEnvelope ::=
	SET {
	    [0]
		IMPLICIT P1.ContentType,

	    originator
		P1.ORName,

	    original[1]
		IMPLICIT P1.EncodedInformationTypes,

		P1.Priority DEFAULT normal,

	    [2]
		IMPLICIT DeliveryFlags,

	    otherRecipients[3]
		IMPLICIT SEQUENCE OF P1.ORName
		OPTIONAL,

	    thisRecipient[4]
		IMPLICIT P1.ORName,

	    intendedRecipient[5]
		IMPLICIT P1.ORName
		OPTIONAL,

	    converted[6]
		IMPLICIT P1.EncodedInformationTypes,

	    submission[7]
		IMPLICIT P1.Time
	}

DeliveryFlags ::=
	BITSTRING { conversionProhibited(2) }

END
