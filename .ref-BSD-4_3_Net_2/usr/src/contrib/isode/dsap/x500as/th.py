-- th-enc.py - Thorn and RARE defined attribute sytaxes

-- $Header: /f/osi/dsap/x500as/RCS/th.py,v 7.1 91/02/22 09:22:20 mrose Interim $
--
--
-- $Log:	th.py,v $
-- Revision 7.1  91/02/22  09:22:20  mrose
-- Interim 6.8
-- 
-- Revision 7.0  90/12/06  07:35:05  mrose
-- *** empty log message ***
-- 
-- Revision 7.0  89/11/23  21:50:54  mrose
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


Thorn
	{
	ccitt
	data(9)
	pss(2342)
	ucl(19200300)
	thorn(100)
	directoryDefinitions(1)
	}

DEFINITIONS ::=

PREFIXES encode decode print

BEGIN


MailBox [[P struct mailbox *]]
        ::=
        SEQUENCE
        {
	mailboxType
		PrintableString [[ s mtype ]],
	mailbox
		IA5String	[[ s mbox ]]
        }

DocumentStoreSyntax [[P struct documentStore *]]
	::=
	SEQUENCE {
	    method
		INTEGER		[[ i ds_method ]]
		{ ftp(0), ftam(1) },

	    textEncodedHostName
		IA5String	[[ s ds_host ]],

	    directoryName[0]
		IA5String	[[ s ds_dir ]]
		OPTIONAL,

	    fileFsName
		IA5String	[[ s ds_file ]]
	}

END
