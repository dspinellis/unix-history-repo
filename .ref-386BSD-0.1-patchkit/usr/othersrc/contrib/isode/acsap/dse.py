-- dse.py - X.500 PresentationAddress syntax

-- $Header: /f/osi/acsap/RCS/dse.py,v 7.4 91/02/22 09:14:37 mrose Interim $
--
--
-- $Log:	dse.py,v $
-- Revision 7.4  91/02/22  09:14:37  mrose
-- Interim 6.8
-- 
-- Revision 7.3  90/08/08  14:02:00  mrose
-- stuff
-- 
-- Revision 7.2  90/07/09  14:31:05  mrose
-- sync
-- 
-- Revision 7.1  89/12/04  08:44:12  mrose
-- touch-up
-- 
-- Revision 7.0  89/11/23  21:22:09  mrose
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


DSE DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/dse.py,v 7.4 91/02/22 09:14:37 mrose Interim $";
#endif
%}

BEGIN

PSAPaddr ::=
	SEQUENCE {
	    pSelector[0]
		OCTET STRING
		OPTIONAL,

	    sSelector[1]
		OCTET STRING
		OPTIONAL,

	    tSelector[2]
		OCTET STRING
		OPTIONAL,

	    nAddress[3]
		SET OF
		    OCTET STRING
	}

END
