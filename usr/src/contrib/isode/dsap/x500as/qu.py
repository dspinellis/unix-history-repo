-- qu.py

-- $Header: /f/osi/dsap/x500as/RCS/qu.py,v 7.2 91/03/09 11:53:58 mrose Exp $
--
--
-- $Log:	qu.py,v $
-- Revision 7.2  91/03/09  11:53:58  mrose
-- update
-- 
-- Revision 7.1  91/02/22  09:22:18  mrose
-- Interim 6.8
-- 
-- Revision 7.0  90/12/06  07:34:58  mrose
-- *** empty log message ***
-- 
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


Quipu
	{
	ccitt
	data(9)
	pss(2342)
	ucl(19200300)
	quipu(99)
	directoryDefinitions(1)
	}

DEFINITIONS ::=

PREFIXES encode decode print

BEGIN

IMPORTS
	NameError ,
	ServiceError ,
	SecurityError
		FROM DAS
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			directoryAbstractService(2)
			}

	DistinguishedName ,
	RelativeDistinguishedName ,
	Attribute ,
	AttributeType
		FROM IF
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			informationFramework(1)
			}
	AlgorithmIdentifier
		FROM AF {
			joint-iso-ccitt
			ds(5)
			modules(1)
			authenticationFramework(7)
			};

-- ReliableROSData
-- 	::=
-- 	SEQUENCE
-- 	{
-- 	rosOperation
-- 		INTEGER ,
		-- the operation being applied
-- 	data
-- 		ANY ,
		-- the Operation Argument
-- 	oldVersion
-- 		ANY ,
		-- data version to which operation should be applied
-- 	newVersion
-- 		ANY
		-- version number which results from operation
-- 	}

AccessSelector [[P struct acl_info *]]
        ::=
        CHOICE <<acl_selector_type>>
        {
        entry
                [0] NULL ,
        other
                [2] NULL ,
        prefix
                [3] NameList [[p acl_name]] ,
        group
                [4] NameList [[p acl_name]]
        }

AccessCategories [[P struct acl_info *]]
        ::=
        ENUMERATED [[i acl_categories]]
        {
                none (0) ,
                detect (1) ,
                compare (2) ,
                read (3) ,
                add (4) ,
                write (5)
        }

ACLInfo [[P struct acl_info *]]
        ::=
        SET OF [[ T struct acl_info * $ * ]] <<acl_next>>
                SEQUENCE [[T struct acl_info * $ *]]
                {
                        AccessSelector [[p * ]] ,
                        AccessCategories [[p *]]
                }

AttributeACL [[P struct acl_attr *]]
        ::=
        SEQUENCE
        {
                SET OF [[ T struct oid_seq  * $ aa_types ]] <<	oid_next >>
                        OBJECT IDENTIFIER [[O oid_oid]] ,
                ACLInfo [[p aa_acl]]
                    -- DEFAULT {{other , read}, {entry, write}}
                    OPTIONAL <E<test_acl_default(parm->aa_acl) != OK>><D<0>>
        }


ACLSyntax [[P struct acl *]]
        ::=
        SEQUENCE
        {
        childACL
                [0] ACLInfo [[p ac_child]]
                    -- DEFAULT {{other , read}, {entry, write}} ,
                    OPTIONAL <E<test_acl_default(parm->ac_child) != OK>><D<0>> ,
        entryACL
                [1] ACLInfo [[p ac_entry]]
                    -- DEFAULT {{other , read}, {entry, write}} ,
                    OPTIONAL <E<test_acl_default(parm->ac_entry) != OK>><D<0>> ,
        defaultAttributeACL
                [2] ACLInfo [[p ac_default]]
                    -- DEFAULT {{other , read}, {entry, write}} ,
                    OPTIONAL <E<test_acl_default(parm->ac_default) != OK>><D<0>> ,
                [3] SET OF [[ T struct acl_attr * $ ac_attributes ]] <<aa_next>>
                        AttributeACL [[p *]]
        }

NameList [[P struct dn_seq *]]
        ::=
        SET OF [[ T struct dn_seq * $ * ]] << dns_next>>
                DistinguishedName [[p dns_dn]]

EDBInfoSyntax [[P struct edb_info *]]
        ::=
        SEQUENCE
        {
        edb
                DistinguishedName [[p edb_name]] ,
        getFromDSA
                DistinguishedName [[p edb_getfrom]]
		    OPTIONAL,
        sendToDSAs
                NameList [[p edb_sendto]] ,
	getEDBAllowed
		NameList [[p edb_allowed]]
        }

RelativeEntry [[P struct entry *]]
        ::=
        SEQUENCE
        {
                RelativeDistinguishedName [[p parm->e_name]] ,
                SET OF [[ T attrcomp * $ e_attributes ]] << attr_link>>
                        Attribute [[p *]]
        }
	    %D{
		   /* Order them */
		   
		   Attr_Sequence as;
		   Attr_Sequence as_next;
		   Attr_Sequence newas = NULLATTR;
		   
		   for (as=(*parm)->e_attributes; as != NULLATTR; as = as_next) {
		       as_next = as->attr_link;
		       as->attr_link = NULLATTR;
		       newas = as_merge (newas,as);
		   }
		   (*parm)->e_attributes = newas;
	    %}

TreeStructureSyntax [[P struct tree_struct *]]
        ::=
-- Too messy fall back to pepy.
--      SET [[ T objectclass * $ tree_object ]]
--	{
--	mandatoryObjectClasses 
--		[1] SET OF OBJECT IDENTIFIER,
--	optionalObjectClasses
--		[2] SET OF OBJECT IDENTIFIER OPTIONAL,
--	permittedRDNs
--		[3] SET OF SET OF AttributeType
--	}
	ANY
	[[E treestruct_encode]] [[D treestruct_decode]]

EntryDataBlock [[P struct entry *]]
        ::= ANY
-- If we just had the straight link list we could do it this way
-- But this won't work for AVL trees.
--	SEQUENCE OF [[ T struct entry * $ * ]] << e_sibling >> RelativeEntry [[p *]]
--		%D{
--			(*parm)->e_leaf = TRUE;
--			(*parm)->e_complete = TRUE;
--			(*parm)->e_data = E_TYPE_SLAVE;
--		%}

-- Pulled up
EDBVersion 
	::=
	UTCTime 

GetEntryDataBlockArgument [[P struct getedb_arg *]]
	::=
	SET
	{
	entry
		[0] DistinguishedName [[p ga_entry]] ,
	sendIfMoreRecentThan
		[1] EDBVersion [[s ga_version]]
		    OPTIONAL
	}

GetEntryDataBlockResult [[P struct getedb_result *]]
	::=
	SEQUENCE
	{
	versionHeld
		[0] EDBVersion [[s gr_version]] ,
		[1] EntryDataBlock [[p gr_edb]]
			[[E EDB_encode]] [[D EDB_decode]]
			OPTIONAL <E<parm->gr_edb != 0>><D<0>>
	}

ProtectedPassword [[P struct protected_password *]]
	::=
	SEQUENCE 
	{
		algorithm [0] AlgorithmIdentifier [[p alg_id]]
			OPTIONAL,
		salt [1] SET [[T struct protected_password *$ *]]
			{
			time1 [0] UTCTime [[s time1]]
				OPTIONAL,
			time2 [1] UTCTime [[s time2]]
				OPTIONAL,
			random1 [2] BIT STRING 
				OPTIONAL ,
			random2 [3] BIT STRING
				OPTIONAL 
			}
			OPTIONAL <<is_protected $ 1>>,
		password [2] OCTET STRING [[o passwd $ n_octets]]
	}


InheritedList [[P struct attrcomp * ]]
	::= 
	SET OF [[ T struct attrcomp * $ * ]]<<attr_link>>
		CHOICE [[ T struct attrcomp * $ * ]] <D<0>>
		       <E< parm->attr_value == NULLAV ? 1 : 2>>
			{
			AttributeType [[p attr_type]],
			Attribute [[p *]]
			}

InheritedAttribute [[P struct _InheritAttr *]]
	::= 
	SET
	{
	idefault [0]   InheritedList [[p i_default]]
			OPTIONAL,
	always	[1]	InheritedList [[p i_always]]
			OPTIONAL,
	objectclass	OBJECT IDENTIFIER [[O i_oid]]
			OPTIONAL
	}
	%D{

	/* Order the attribute lists */
	   
	Attr_Sequence as;
	Attr_Sequence as_next;
	Attr_Sequence newas = NULLATTR;
	   
	for (as=(*parm)->i_default; as != NULLATTR; as = as_next) {
	       as_next = as->attr_link;
	       as->attr_link = NULLATTR;
	       newas = as_merge (newas,as);
	}
	(*parm)->i_default = newas;

	newas = NULLATTR;

	for (as=(*parm)->i_always; as != NULLATTR; as = as_next) {
	       as_next = as->attr_link;
	       as->attr_link = NULLATTR;
	       newas = as_merge (newas,as);
	}
	(*parm)->i_always = newas;
	
	%}

END
