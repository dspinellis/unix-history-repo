-- documents.py - FTAM document definitions

-- $Header: /f/osi/ftam2/RCS/docs.py,v 7.3 91/02/22 09:23:36 mrose Interim $
--
--
-- $Log:	docs.py,v $
-- Revision 7.3  91/02/22  09:23:36  mrose
-- Interim 6.8
-- 
-- Revision 7.2  91/01/13  12:26:54  mrose
-- NBS
-- 
-- Revision 7.1  90/11/05  13:29:44  mrose
-- nist
-- 
-- Revision 7.0  89/11/23  21:54:14  mrose
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


DOCS DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/docs.py,v 7.3 91/02/22 09:23:36 mrose Interim $";
#endif
%}

BEGIN

-- FTAM-1: ISO FTAM unstructured text

FTAM-1-Parameters ::=
        SEQUENCE {
            universal-class-number[0]
                IMPLICIT INTEGER
		OPTIONAL,

            maximum-string-length[1]
                IMPLICIT INTEGER
		OPTIONAL,

            string-significance[2]
                IMPLICIT INTEGER {
		    variable(0),
		    fixed(1),
		    not-significant(2)
		}
	        OPTIONAL
        }

FTAM-1-Datatype1 ::=
        CHOICE {
	    printable --* *--
                PrintableString,

	    teletex --* *--
                TeletexString,

	    videotex --* *--
                VideotexString,

	    ia5 --* *--
                IA5String,

	    graphic --* *--
                GraphicString,

	    visible --* *--
                VisibleString,

	    general --* *--
                GeneralString
        }


-- FTAM-3: ISO FTAM unstructured binary

FTAM-3-Parameters ::=
        SEQUENCE {
            maximum-string-length[1]
                IMPLICIT INTEGER
		OPTIONAL,

	    string-significanz --* string-significance *-- [2]
                IMPLICIT INTEGER {
		    variable(0),
		    fixed(1),
		    not-significant(2)
		}
	    	OPTIONAL
        }

FTAM-3-Datatype1 ::=
        OCTET STRING


-- NBS-9: NBS-9 FTAM file directory

NBS-9-Parameters ::=
    [0]
        IMPLICIT --* ISO8571-FTAM *-- FTAM.Attribute-Names

NBS-9-Datatype1 ::=
    [PRIVATE 2]
        --* ISO8571-FTAM *-- FTAM.Read-Attributes

END
