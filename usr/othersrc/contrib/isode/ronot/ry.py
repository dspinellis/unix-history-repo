-- ry.ry - ROSY remote operations definitions

-- $Header: /f/osi/ronot/RCS/ry.py,v 7.1 91/02/22 09:50:36 mrose Interim $
--
--
-- $Log:	ry.py,v $
-- Revision 7.1  91/02/22  09:50:36  mrose
-- Interim 6.8
-- 
-- Revision 7.0  90/07/26  14:28:42  mrose
-- *** empty log message ***
-- 
-- Revision 7.0  89/11/23  21:48:26  mrose
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


Remote-Operations-Notation {joint-iso-ccitt remote-operations(4) notation(0)}

DEFINITIONS ::=

BEGIN

SECTIONS build parse print

BindArgumentValue ::= 
	[16] ANY

BindResultValue ::=
	[17] ANY

BindErrorValue ::= 
	[18] ANY

UnBindArgumentValue ::=
	[19] ANY

UnBindResultValue ::=
	[20] ANY

UnBindErrorValue ::=
	[21] ANY

END
