-- fadu.py - FTAM file-access-data-unit definitions
--	lifted directly from ISO8571-2
--
--      Two kinds of changes to the ASN.1
--	    - more commentary-tags for POSY
--	    - Node-Name must be string-valued

-- $Header: /f/osi/ftam/RCS/fadu.py,v 7.1 91/02/22 09:22:26 mrose Interim $
--
--
-- $Log:	fadu.py,v $
-- Revision 7.1  91/02/22  09:22:26  mrose
-- Interim 6.8
-- 
-- Revision 7.0  89/11/23  21:53:15  mrose
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


--* ISO8571-FADU *-- FADU DEFINITIONS ::=

%{
#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/fadu.py,v 7.1 91/02/22 09:22:26 mrose Interim $";
#endif
%}

BEGIN

Subtree ::=
        SEQUENCE {
            node
                Node-Descriptor-Data-Element,

            data[0]
                IMPLICIT DU
                OPTIONAL,
		-- present if and only if a DU is connected to the node

            children[1]
                IMPLICIT Children
                OPTIONAL
		-- a leaf node is characterized by having no children
        }

Children ::=
        SEQUENCE {
            enter-subtree
                Enter-Subtree-Data-Element,

	    subtree --* *--
                SEQUENCE OF
                    Subtree,
		-- subtrees must appear in their proper order according to
		-- their proper ordering as children of their parent node

            exit-subtree
                Exit-Subtree-Data-Element
        }

DU ::=
        SEQUENCE OF
            --* ISO8571-CONTENTS *-- CONT.File-Contents-Data-Element

Node-Descriptor-Data-Element ::=
    [APPLICATION 0]
        IMPLICIT SEQUENCE {
            name
                Node-Name
                OPTIONAL,
		-- present only if the root node of the subtree is a named node

            arc-length[1]
                IMPLICIT INTEGER
                DEFAULT 1,
		-- used to specify the length of the arc to the root node of
		-- the subtree from its parent node

            data-exists[2]
                IMPLICIT BOOLEAN
                DEFAULT TRUE
		-- data-exists = TRUE indicates that a DU is connected to the
		-- root node of the subtree
        }

Node-Name ::=
--*     CHOICE { *--
--*         ftam-coded *-- [0]
                IMPLICIT GraphicString --* , *--
	        -- when ftam-coded is used, the Node-Name belongs to the same
		-- abstract syntax as the structuring information.  Node-Names
		-- are then transferred in the presentatino context established
		-- to support the FTAM FADU abstract syntax.  This form of
		-- Node-Name is only allowed when the content type file
    		-- attribute contains a document type name.  To support this
		-- alternative, at least the G0 character set registered in
		-- character set register entry 2 shall be supported.

--*         user-coded *--
--*             EXTERNAL *--
		-- the actual types allowed are found in the abstract syntax
		-- for the files contents, as specified in the contents type
		-- file attribute for the file
--*     } *--

Enter-Subtree-Data-Element ::=
    [APPLICATION 1]
        IMPLICIT NULL

Exit-Subtree-Data-Element ::=
    [APPLICATION 2]
        IMPLICIT NULL
	-- the enter-subtree and exit-subtree data elements are used to bracket
	-- the list of subtrees, which are children of the preceding node

FADU ::=
        Subtree

Structuring-Data-Element ::=
        CHOICE {
	    node-descriptor --* *--
                Node-Descriptor-Data-Element,

	    enter-subtree --* *--
                Enter-Subtree-Data-Element,

	    exit-subtree --* *--
                Exit-Subtree-Data-Element
        }

-- Data-Element is defined to be a general data type whose values are
--
--	a) a value of the ASN.1 type Structuring-Data-Element in the abstract
--	  syntax "FTAM FADU"; or,
--
--	b) a value of the ASN.1 type ISO8571-CONTENTS.File-Contents-Data-Element
--	  in the abstract syntax derived from the contents type file attribute.

END
