-- T73 Defined Types (stub)

-- $Header: /f/osi/pepy/RCS/T73.py,v 7.1 91/02/22 09:34:48 mrose Interim $
--
--
-- $Log:	T73.py,v $
-- Revision 7.1  91/02/22  09:34:48  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:11:35  mrose
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


T73 DEFINITIONS ::=


%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/pepy/RCS/T73.py,v 7.1 91/02/22 09:34:48 mrose Interim $";
#endif
%}

BEGIN

PRINTER	print

ProtocolElement ::=
	ANY

PresentationCapabilities ::=
	SET { --unimportant-- }

END
