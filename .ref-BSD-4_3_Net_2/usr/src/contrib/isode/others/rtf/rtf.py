-- rtf.py - RTF definitions

-- $Header: /f/osi/others/rtf/RCS/rtf.py,v 7.1 91/02/22 09:34:18 mrose Interim $
--
--
-- $Log:	rtf.py,v $
-- Revision 7.1  91/02/22  09:34:18  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  22:10:47  mrose
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

RTF DEFINITIONS ::=

BEGIN

Request ::=
    SEQUENCE {
	user[0]
	    IMPLICIT IA5String,

	password[1]
	    IMPLICIT IA5String,

	file[2]
	    IMPLICIT IA5String
    }

END
