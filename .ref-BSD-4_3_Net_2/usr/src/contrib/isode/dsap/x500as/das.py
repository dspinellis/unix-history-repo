-- das.py

-- $Header: /f/osi/dsap/x500as/RCS/das.py,v 7.2 91/03/09 11:53:49 mrose Exp $
--
--
-- $Log:	das.py,v $
-- Revision 7.2  91/03/09  11:53:49  mrose
-- update
-- 
-- Revision 7.1  91/02/22  09:21:50  mrose
-- Interim 6.8
-- 
-- Revision 7.0  90/12/06  07:34:42  mrose
-- *** empty log message ***
-- 
-- Revision 7.1	 90/10/17  11:44:06  mrose
-- sync
-- 
-- Revision 7.0	 89/11/23  21:50:14  mrose
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


DAS
	{
	joint-iso-ccitt
	ds(5)
	modules(1)
	directoryAbstractService(2)
	}

DEFINITIONS ::=

PREFIXES encode decode print

BEGIN

-- EXPORTS
--	DirectoryBindArgument ,
--	ReadArgument ,
--	ReadResult ,
--	AbandonArgument ,
--	AbandonResult ,
--	CompareArgument ,
--	CompareResult ,
--	ListArgument ,
--	ListResult ,
--	SearchArgument ,
--	SearchResult ,
--	AddEntryArgument ,
--	AddEntryResult ,
--	RemoveEntryArgument ,
--	RemoveEntryResult ,
--	ModifyEntryArgument ,
--	ModifyEntryResult ,
--	ModifyRDNArgument ,
--	ModifyRDNResult ,
--	AbandonFailedParm ,
--	AttributeErrorParm ,
--	NameErrorParm ,
--	ReferralParm ,
--	SecurityErrorParm ,
--	ServiceErrorParm ,
--	UpdateErrorParm;

IMPORTS
	Attribute ,
	AttributeType ,
	AttributeValue ,
	AttributeValueAssertion ,
	DistinguishedName ,
	Name ,
	RelativeDistinguishedName
		FROM IF
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			informationFramework(1)
			}

	OperationProgress ,
	ContinuationReference ,
	InvokeID
		FROM DO
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			distributedOperations(3)
			}

	Certificate ,
	CertificationPath ,
	AlgorithmIdentifier
		FROM AF
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			authenticationFramework(7)
			};

ENCODER encode

SimpleCredentials [[P struct ds_bind_arg *]]
	::=
	SEQUENCE
	{
	name
		[0] DistinguishedName [[p dba_dn]],
	validity
		[1] SET [[ T struct ds_bind_arg * $ * ]]
		{
		time1
			[0] UTCTime [[s dba_time1]] OPTIONAL,
		time2
			[1] UTCTime [[s dba_time2]] OPTIONAL,
		random1
			[2] BIT STRING [[x dba_r1.value $ dba_r1.n_bits]] 
			OPTIONAL,
		random2
			[3] BIT STRING [[x dba_r2.value $ dba_r2.n_bits]]
			OPTIONAL
		}
		OPTIONAL <E<parm -> dba_auth_type > DBA_AUTH_SIMPLE>>
			 <D<0>>,
		%E{
			parm->dba_vtmp = parm->dba_passwd;
		%}
	password
		[2] OCTET STRING [[o dba_vtmp $ dba_passwd_len]]
		%D{
		if ((*parm)->dba_vtmp) {
			bcopy((*parm)->dba_vtmp, (*parm)->dba_passwd, (*parm)->dba_passwd_len);
			free((*parm)->dba_vtmp);
			if ((*parm)->dba_auth_type == DBA_AUTH_NONE)
			   (*parm)->dba_auth_type = DBA_AUTH_SIMPLE;
		}
		%}
		OPTIONAL <E<parm->dba_auth_type != DBA_AUTH_NONE>><D<0>>
	}

-- Two temp labels to aid Pepsy parameter passing
TAlgorithmIdentifier [[P struct signature *]]
	::= AlgorithmIdentifier [[ p &parm->alg]]

TBitString [[P struct signature *]]
	::= BIT STRING	[[x parm->encrypted $ parm->n_bits ]]

TokenToSign [[P struct ds_bind_arg *]]
	::=
	SEQUENCE
	{
	algorithm
		[0] TAlgorithmIdentifier [[p dba_sig]],
	name
		[1] DistinguishedName [[p dba_dn]] ,
	time
		[2] UTCTime [[s dba_time1]],
	random
		[3] BIT STRING [[x dba_r1.value $ dba_r1.n_bits]]
	}

Token [[P struct ds_bind_arg *]]
	::=
	SEQUENCE
	{
		TokenToSign [[p *]],
		TAlgorithmIdentifier [[p dba_sig]],
		TBitString [[p dba_sig]]
	}

StrongCredentials [[P struct ds_bind_arg *]]
	::=
	SET
	{
	certificationPath
		[0] CertificationPath [[p dba_cpath]]
		    OPTIONAL,
	bindToken
		[1] Token [[p *]]
	}

Credentials [[P struct ds_bind_arg *]]
	::=
	CHOICE <E< parm->dba_auth_type == DBA_AUTH_STRONG ? 2 : 1 >>
	       <D< (*parm)->dba_auth_type >>
	{
	simple
		[0] SimpleCredentials [[p *]]
		%D{
		   if ((*parm)->dba_time1)
		     (*parm)->dba_auth_type = DBA_AUTH_PROTECTED;
		%},
	strong
		[1] StrongCredentials [[p *]],
	externalProcedure 
		[2] EXTERNAL [[p dba_vtmp]]
	}

-- Pulled up
Versions  ::=  BIT STRING
	{
	v1988(0)
	}

-- Pulled up
SecurityProblem
	::=
	INTEGER
	{
	inappropriateAuthentication(1) ,
	invalidCredentials(2) ,
	insufficientAccessRights(3) ,
	invalidSignature(4) ,
	protectionRequired(5) ,
	noInformation(6)
	}

-- Pulled up
ServiceProblem
	::=
	INTEGER
	{
	busy(1) ,
	unavailable(2) ,
	unwillingToPerform(3) ,
	chainingRequired(4) ,
	unableToProceed(5) ,
	invalidReference(6) ,
	timeLimitExceeded(7) ,
	administrativeLimitExceeded(8) ,
	loopDetected(9) ,
	unavailableCriticalExtension(10) ,
	outOfScope(11) ,
	ditError(12)
	}

EntryInformationSelection [[P struct entryinfoselection *]]
	::=
	SET
	{
	attributeTypes
		CHOICE [[ T struct entryinfoselection * $ *]]
		<D< (*parm)->eis_allattributes>>
		<E< parm->eis_allattributes ? 1 : 2>>
		{
		allAttributes
			[0] NULL,
		select
			[1] SET OF [[ T struct attrcomp * $ eis_select ]] <<attr_link>>
				AttributeType [[p attr_type]]
		}
		%D{
			if ((*parm)->eis_allattributes == 1)
			   (*parm)->eis_allattributes = TRUE;
			else
			   (*parm)->eis_allattributes = FALSE;
		%}
		    -- DEFAULT allAttributes NULL,
		    OPTIONAL <E<parm->eis_allattributes != TRUE>><D<0>>,
	infoTypes
		[2] INTEGER [[i eis_infotypes]]
		{
		attributeTypesOnly(0) ,
		attributeTypesAndValues(1)
		}
		    DEFAULT attributeTypesAndValues
	}

ServiceControls [[P struct svccontrol *]]
	::=
	SET
	{
	%E{
	if (parm->svc_options != 0) {
		parm->svc_len = 5;
		parm->svc_tmp = int2strb_alloc (parm->svc_options,parm->svc_len);
	}
	%}
	options
		[0] BIT STRING [[x svc_tmp $ svc_len]]
		{
		preferChaining(0) ,
		chainingProhibited(1) ,
		localScope(2) ,
		dontUseCopy(3) ,
		dontDereferenceAliases(4)
		}
		%E{
		if (parm->svc_tmp)
			free (parm->svc_tmp);
		%}
		%D{
		if ((*parm)->svc_len) {
			(*parm)->svc_options = strb2int((*parm)->svc_tmp,(*parm)->svc_len);
			free ((*parm)->svc_tmp);
		}
		%}
		-- DEFAULT {},
		OPTIONAL <D<0>> <E<parm->svc_options != 0>>,
	priority
		[1] INTEGER [[i svc_prio]]
		{
		low(0) ,
		medium(1) ,
		high(2)
		}
		DEFAULT medium,
	timeLimit
		[2] INTEGER [[i svc_timelimit]]
		    OPTIONAL <E<parm->svc_timelimit != SVC_NOTIMELIMIT>>
			     <D<0>>,
	sizeLimit
		[3] INTEGER [[i svc_sizelimit]]
		    OPTIONAL <E<parm->svc_sizelimit != SVC_NOSIZELIMIT>>
			     <D<0>>,
	scopeOfReferral
		[4] INTEGER [[i svc_scopeofreferral]]
		{
		dmd(0) ,
		country(1)
		}
		OPTIONAL <E<parm->svc_scopeofreferral != SVC_REFSCOPE_NONE>>
			 <D<0>>
	}

-- Pulled up
ProtectionRequest 
	::=
	INTEGER
	{
	none (0) ,
	signed (1)
	}

-- Pespy temp
DBitString [[P struct random_number * ]] ::=
	BIT STRING [[ x value $ n_bits ]]

SecurityParameters [[P struct security_parms *]]
	::=
	SET
	{
	certificationPath
		[0] CertificationPath [[p sp_path]]
		    OPTIONAL,
	name
		[1] DistinguishedName [[p sp_name]]
		    OPTIONAL,
	time
		[2] UTCTime [[s sp_time]]
		    OPTIONAL,
	random
		[3] TBitString [[p sp_random]]
		    OPTIONAL,
	target
		[4] ProtectionRequest [[i sp_target]]
		    -- OPTIONAL
		    DEFAULT 0
	}

Extension [[P struct extension *]]
	::=
	SET
	{
	identifier
		[0] INTEGER [[i ext_id]] ,
	critical
		[1] BOOLEAN [[b ext_critical]]
		    DEFAULT FALSE,
	item
		[2] ANY DEFINED BY identifier [[a ext_item]]
	}

EntryInformation [[P struct entrystruct *]]
	::=
	SEQUENCE
	{
		DistinguishedName [[p ent_dn]],
		%E{
			if (parm->ent_iscopy == INFO_MASTER)
			   parm->ent_pepsycopy = TRUE;	
			else
			   parm->ent_pepsycopy = FALSE;
		%}
	fromEntry
		BOOLEAN [[b ent_pepsycopy]]
		%D{
			if ( (*parm)->ent_pepsycopy)
			   (*parm)->ent_iscopy = INFO_MASTER;
			else
			   (*parm)->ent_iscopy = INFO_COPY;
		%}
		DEFAULT TRUE,
		SET OF [[ T struct attrcomp * $ ent_attr ]] <<attr_link>>
			CHOICE [[ T struct attrcomp * $ *]] 
			<D<0>>
			<E< parm->attr_value == NULLAV ? 1 : 2>>
			{
				AttributeType [[p attr_type]],
				Attribute [[p *]]
			} OPTIONAL
		}
	    %D{
		   /* Order them */
		   
		   Attr_Sequence as;
		   Attr_Sequence as_next;
		   Attr_Sequence newas = NULLATTR;
		   
		   for (as=(*parm)->ent_attr; as != NULLATTR; as = as_next) {
		       as_next = as->attr_link;
		       as->attr_link = NULLATTR;
		       newas = as_merge (newas,as);
		   }
		   (*parm)->ent_attr = newas;
	    %}

-- Pulled up
LimitProblem
	::=
	INTEGER 
	{
	timeLimitExceeded(0) ,
	sizeLimitExceeded(1) ,
	administrativeLimitExceeded(2)
	}

PartialOutcomeQualifier [[P struct part_outcome *]]
	::=
	SET
	{
	limitProblem
		[0] LimitProblem [[i parm->poq_limitproblem]]
		    OPTIONAL <E<parm->poq_limitproblem != LSR_NOLIMITPROBLEM>> 
			     <D<0>>,					 
	unexplored
		[1] SET OF [[ T struct continuation_ref * $ poq_cref ]] <<cr_next>>
		       ContinuationReference [[p *]]
			    OPTIONAL,

	unavailableCriticalExtensions
		[2] BOOLEAN [[b poq_no_ext]]
		    DEFAULT FALSE
	}

FilterItem [[P struct filter_item *]]
	::=
	CHOICE << fi_type >>
	{
	equality
		[0] AttributeValueAssertion [[p &parm->fi_un.fi_un_ava]],
	substrings 
		[1] ANY [[a &parm->fi_un.fi_un_substrings]] -- never free this
			[[D substring_decode ]] [[E substring_encode]],
			

-- A bit too complex for Pepsy just yet.
--		[1] SEQUENCE 
--		{
--		type
--			AttributeType,
--		strings
--			SEQUENCE OF CHOICE {
--				initial
--					[0] AttributeValue,
--				any
--					[1] AttributeValue,
--				final
--					[2] AttributeValue
--			}
--		} 

	greaterOrEqual
		[2] AttributeValueAssertion [[p &parm->fi_un.fi_un_ava]],
	lessOrEqual
		[3] AttributeValueAssertion [[p &parm->fi_un.fi_un_ava]],
	present
		[4] AttributeType [[p fi_un.fi_un_type]],
	approximateMatch
		[5] AttributeValueAssertion [[p &parm->fi_un.fi_un_ava]]
	}

Filter [[P struct filter *]]
	::=
	CHOICE <<flt_type>>
	{
	item
		[0] FilterItem [[p &parm->flt_un.flt_un_item]],
	and
		[1] SET OF [[ T struct filter * $ parm->flt_un.flt_un_filter ]] <<flt_next>>
			Filter [[p *]],
	or
		[2] SET OF [[ T struct filter * $ parm->flt_un.flt_un_filter ]] <<flt_next>>
			Filter [[p *]],
	not
		[3] Filter [[p parm->flt_un.flt_un_filter]]
	}

-- Pepsy !!!
TAttributeType [[P struct attrcomp *]] 
	::= CHOICE <E<1>><D<0>> {
		    AttributeType [[p attr_type]]
	    }

EntryModification [[P struct entrymod *]]
	::=
	CHOICE <<em_type>>
	{
	addAttribute
		[0] Attribute [[p em_what]],
	removeAttribute
		[1] TAttributeType [[p em_what]],
	addValues
		[2] Attribute [[p em_what]],
	removeValues
		[3] Attribute [[p em_what]]
	}

-- Pulled up
AbandonProblem
	::=
	INTEGER
	{
	noSuchOperation(1) ,
	tooLate(2) ,
	cannotAbandon(3)
	}

-- Pulled up
AttributeProblem
	::=
	INTEGER
	{
	noSuchAttributeOrValue(1) ,
	invalidAttributeSyntax(2) ,
	undefinedAttributeType(3) ,
	inappropriateMatching(4) ,
	constraintViolation(5) ,
	attributeOrValueAlreadyExists(6)
	}

-- Pulled up
NameProblem
	::=
	INTEGER
	{
	noSuchObject(1) ,
	aliasProblem(2) ,
	invalidAttributeSyntax(3) ,
	aliasDereferencingProblem(4)
	}

-- Pulled up
UpdateProblem
	::=
	INTEGER
	{
	namingViolation(1) ,
	objectClassViolation(2) ,
	notAllowedOnNonLeaf(3) ,
	notAllowedOnRDN(4) ,
	entryAlreadyExists(5) ,
	affectsMultipleDSAs(6) ,
	objectClassModificationProhibited(7)
	}

DirectoryBindArgument [[P struct ds_bind_arg *]]
	::=
	SET
	{
	%D{
	   (*parm)->dba_version = DBA_VERSION_V1988;
	%}
	credentials
		[0] Credentials [[p *]]
		    OPTIONAL 
		    <E<  ((parm->dba_auth_type != DBA_AUTH_NONE) || 
			     (parm->dba_dn != NULLDN))  >>
		    <D<0>>,
	    %E{
		    if (parm->dba_version == DBA_VERSION_V1988) {
		       parm->dba_vlen = 1;
		       parm->dba_vtmp = int2strb_alloc(parm->dba_version,1);
		    } else
		      return NOTOK;
	    %}
	versions
		[1] Versions [[x dba_vtmp $ dba_vlen ]]
		    %E{
		    if (parm->dba_vtmp)
			free (parm->dba_vtmp);
		    %}
		    %D{
		        if ( (*parm)->dba_vlen ) {
			   (*parm)->dba_version = strb2int((*parm)->dba_vtmp,(*parm)->dba_vlen);
			   free ((*parm)->dba_vtmp);
			}
		    %}
		    -- DEFAULT {v1988}
		    OPTIONAL <E<parm->dba_version != DBA_VERSION_V1988>><D<0>>
	}

DirectoryBindResult [[P struct ds_bind_arg *]]
	::=
	DirectoryBindArgument [[p *]]

ReadArgumentData [[P struct ds_read_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->rda_eis.eis_allattributes = TRUE;
	(*parm)->rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
	(*parm)->rda_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->rda_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->rda_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->rda_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->rda_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] Name [[p rda_object]],
	selection
		[1] EntryInformationSelection [[p &parm->rda_eis]]
		    -- DEFAULT {},
		    OPTIONAL <D<0>>
			     <E< (
			     (parm->rda_eis.eis_allattributes != TRUE) ||
			     (parm->rda_eis.eis_infotypes != EIS_ATTRIBUTESANDVALUES)
			     ) >>,
		[30] ServiceControls [[p &parm->rda_common.ca_servicecontrol]]
		    -- DEFAULT {},
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->rda_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->rda_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->rda_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->rda_common.ca_servicecontrol.svc_sizelimit != SVC_NOSIZELIMIT) ||
		    (parm->rda_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,
		[29] SecurityParameters [[p rda_common.ca_security]]
		    -- DEFAULT {},
		    OPTIONAL,
	requestor
		[28] DistinguishedName [[p rda_common.ca_requestor]]
		    OPTIONAL,
		[27] OperationProgress [[p &parm->rda_common.ca_progress]]
		    OPTIONAL <D<0>>
		    <E<parm->rda_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->rda_common.ca_aliased_rdns]]
		    OPTIONAL <E<parm->rda_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>>
		    <D<0>>,
	extensions
		[25] SET OF [[ T struct extension * $ rda_common.ca_extensions ]] <<ext_next>>
			Extension [[p *]]
		    OPTIONAL
	}

ReadArgument [[P struct ds_read_arg *]]
	::=
	CHOICE <D<0>>
	       <E< parm->rda_common.ca_sig == (struct signature *) 0 ? 1 : 2>>
	{
		ReadArgumentData [[p * ]] ,
		SEQUENCE [[ T struct ds_read_arg * $ * ]]
		{
			ReadArgumentData [[p * ]] ,
			TAlgorithmIdentifier [[p parm->rda_common.ca_sig]],
			TBitString [[p parm->rda_common.ca_sig]]
		}
	}

ReadResultData [[P struct ds_read_result *]]
	::=
	SET
	{
	entry
		[0] EntryInformation [[p  &parm->rdr_entry]],
		[30] SecurityParameters [[p parm->rdr_common.cr_security ]]
		    OPTIONAL ,
	performer
		[29] DistinguishedName [[p parm->rdr_common.cr_requestor]]
		    OPTIONAL,
	aliasDereferenced
		[28] BOOLEAN [[b rdr_common.cr_aliasdereferenced]]
		    DEFAULT FALSE 
	}

ReadResult [[P struct ds_read_result *]]
	::=
	CHOICE <E<1>><D<0>>
	{
		ReadResultData [[p * ]] ,
		SEQUENCE [[ T struct ds_read_result * $ * ]]
		{
			ReadResultData [[p * ]] ,
			AlgorithmIdentifier [[p rdr_common.cr_alg]],
			BIT STRING [[ x rdr_common.cr_tmp $ rdr_common.cr_len ]]
		}
	}

CompareArgumentData [[P struct ds_compare_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->cma_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->cma_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->cma_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->cma_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->cma_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] Name [[p cma_object]],
	purported
		[1] AttributeValueAssertion [[p &parm->cma_purported]],
		[30] ServiceControls [[p &parm->cma_common.ca_servicecontrol]]
		     -- DEFAULT {} ,
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->cma_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->cma_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->cma_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->cma_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->cma_common.ca_security]]
		     -- DEFAULT {},
		     OPTIONAL,
	requestor
		[28] DistinguishedName [[p parm->cma_common.ca_requestor]]
		     OPTIONAL,
		[27] OperationProgress [[p &parm->cma_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->cma_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->cma_common.ca_aliased_rdns]]
		     OPTIONAL <E<parm->cma_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>><D<0>> ,
	extensions
		[25] SET OF [[T struct extension * $ parm->cma_common.ca_extensions]]
		     <<ext_next>>  Extension [[p *]]
		     OPTIONAL
	}

CompareArgument [[P struct ds_compare_arg *]]
	::=
	CHOICE  <D<0>>
	<E<(parm->cma_common.ca_sig == (struct signature *) 0)? 1:2>>
	{
		CompareArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_compare_arg * $ *]]
		{
			CompareArgumentData [[p *]] ,
			TAlgorithmIdentifier 
				[[p parm->cma_common.ca_sig]],
			TBitString
				[[x parm->cma_common.ca_sig $ parm->cma_common.ca_sig ]]
		}
	}

CompareResultData [[P struct ds_compare_result *]]
	::=
	SET
	{
		DistinguishedName [[p cmr_object]]
		OPTIONAL,
	matched
		[0] BOOLEAN [[b parm->cmr_matched]],
		%E{
			if (parm->cmr_iscopy == INFO_MASTER)
			   parm->cmr_pepsycopy = TRUE;
			else
			   parm->cmr_pepsycopy = FALSE;
		%}
	fromEntry
		[1] BOOLEAN [[b parm->cmr_pepsycopy]]
		%D{
			if ((*parm)->cmr_pepsycopy)
			   (*parm)->cmr_iscopy = INFO_MASTER;
			else
			   (*parm)->cmr_iscopy = INFO_COPY;
		%}
		    DEFAULT TRUE,
		[30] SecurityParameters [[p parm->cmr_common.cr_security ]]
		    OPTIONAL ,
	performer
		[29] DistinguishedName [[p parm->cmr_common.cr_requestor]]
		OPTIONAL,
	aliasDereferenced
		[28] BOOLEAN [[b parm->cmr_common.cr_aliasdereferenced]]
		     DEFAULT FALSE
	}

CompareResult [[P struct ds_compare_result *]]
	::=
	CHOICE <D<0>><E<1>>
	{
		CompareResultData [[p *]] ,
		SEQUENCE [[ T struct ds_compare_result * $ *]]
		{
			CompareResultData [[p *]] ,
			AlgorithmIdentifier [[p cmr_common.cr_alg]],
			BIT STRING [[ x cmr_common.cr_tmp $ cmr_common.cr_len ]]
		}
	}

-- For pepsy as it can't pull up struct not defined here.
TInvokeID ::= INTEGER

AbandonArgument [[P struct ds_abandon_arg *]]
	::=
	SEQUENCE
	{
	invokeID
		[0] TInvokeID [[i parm->aba_invokeid]]
	}

AbandonResult ::= NULL

ListArgumentData [[P struct ds_list_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->lsa_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->lsa_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->lsa_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->lsa_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->lsa_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] Name [[p parm->lsa_object]],
		[30] ServiceControls [[p &parm->lsa_common.ca_servicecontrol]]
		     -- DEFAULT {},
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->lsa_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->lsa_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->lsa_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->lsa_common.ca_servicecontrol.svc_sizelimit != SVC_NOSIZELIMIT) ||
		    (parm->lsa_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->lsa_common.ca_security]]
		     -- DEFAULT {} ,
		     OPTIONAL,
	requestor
		[28] DistinguishedName [[p  parm->lsa_common.ca_requestor]]
		     OPTIONAL,
		[27] OperationProgress [[p &parm->lsa_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->lsa_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->lsa_common.ca_aliased_rdns]]
		     OPTIONAL <E<parm->lsa_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>> <D<0>>
		,
	extensions
		[25] SET OF [[ T struct extension * $ parm->lsa_common.ca_extensions ]]
		     <<ext_next>> Extension [[p *]]
		     OPTIONAL
	}

ListArgument [[P struct ds_list_arg *]]
	::=
	CHOICE <D<0>>
	<E<(parm->lsa_common.ca_sig == (struct signature *) 0)? 1:2>>
	{
		ListArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_list_arg * $ * ]]
		{
			ListArgumentData [[p *]] ,
			TAlgorithmIdentifier [[p parm->lsa_common.ca_sig ]],
			TBitString [[ p parm->lsa_common.ca_sig ]]
		}
	}

ListResultData [[P struct ds_list_result *]]
	::=
	CHOICE <E<1>><D<0>>
	{
	listInfo
		SET [[ T struct ds_list_result * $ * ]]
		{
		DistinguishedName [[p parm->lsr_object]] OPTIONAL,
		subordinates
			[1] SET OF [[T struct subordinate * $ lsr_subordinates ]]
			<<sub_next>>
				SEQUENCE [[ T struct subordinate * $ *]]
				{
					RelativeDistinguishedName [[p sub_rdn]],
				aliasEntry
					[0] BOOLEAN [[b sub_aliasentry]] 
					    DEFAULT FALSE,
				fromEntry
					[1] BOOLEAN [[b sub_copy]]
					    DEFAULT TRUE
				},
	%D{
		(*parm)->lsr_poq.poq_limitproblem = LSR_NOLIMITPROBLEM;
	%}
		partialOutcomeQualifier
			[2] PartialOutcomeQualifier [[p &parm->lsr_poq]]
			     OPTIONAL <E<
				      ((parm->lsr_poq.poq_limitproblem != LSR_NOLIMITPROBLEM) || 
				      (parm->lsr_poq.poq_cref != NULLCONTINUATIONREF))
				      >><D<0>>,
			[30] SecurityParameters [[p parm->lsr_common.cr_security ]]
			    OPTIONAL ,
		performer
			[29] DistinguishedName [[p parm->lsr_common.cr_requestor]]
			OPTIONAL,
		aliasDereferenced
			[28] BOOLEAN [[b parm->lsr_common.cr_aliasdereferenced]]
			     DEFAULT FALSE
		},

	uncorrelatedListInfo
		[0] SET OF [[ T struct ds_list_result * $ lsr_next ]] <<lsr_next>>
			ListResult [[ p * ]]
	}

ListResult [[P struct ds_list_result *]]
	::=
	CHOICE <E<1>><D<0>>
	{
		ListResultData [[p * ]] ,
		SEQUENCE [[ T struct ds_list_result * $ *]]
		{
			ListResultData [[p * ]] ,
			AlgorithmIdentifier [[p parm->lsr_common.cr_alg]],
			BIT STRING [[ x parm->lsr_common.cr_tmp $ parm->lsr_common.cr_len ]]
		}
	}

SearchArgumentData [[P struct ds_search_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->sra_eis.eis_allattributes = TRUE;
	(*parm)->sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;
	(*parm)->sra_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->sra_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->sra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->sra_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->sra_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	baseObject
		[0] Name [[p parm->sra_baseobject]],
	subset
		[1] INTEGER [[i parm->sra_subset]]
		{
		baseObject(0) ,
		oneLevel(1) ,
		wholeSubtree(2)
		}
		DEFAULT baseObject,
	filter
		[2] Filter [[p parm->sra_filter]]
		    OPTIONAL,
	searchAliases
		[3] BOOLEAN [[b parm->sra_searchaliases]]
		    DEFAULT TRUE ,
	selection
		[4] EntryInformationSelection [[p &parm->sra_eis]]
		    -- DEFAULT {},
		    OPTIONAL <D<0>>
			     <E< (
			     (parm->sra_eis.eis_allattributes != TRUE) ||
			     (parm->sra_eis.eis_infotypes != EIS_ATTRIBUTESANDVALUES)
			     ) >>,
		[30] ServiceControls [[p &parm->sra_common.ca_servicecontrol]]
		     -- DEFAULT {},
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->sra_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->sra_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->sra_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->sra_common.ca_servicecontrol.svc_sizelimit != SVC_NOSIZELIMIT) ||
		    (parm->sra_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->sra_common.ca_security]]
		     -- DEFAULT {} ,
		     OPTIONAL,
	requestor
		[28] DistinguishedName [[p  parm->sra_common.ca_requestor]]
		     OPTIONAL,
		[27] OperationProgress [[p &parm->sra_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->sra_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->sra_common.ca_aliased_rdns]]
		     OPTIONAL <E<parm->sra_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>><D<0>>
		,
	extensions
		[25] SET OF [[ T struct extension * $ parm->sra_common.ca_extensions]] 
		     <<ext_next>> Extension [[p *]] 
		     OPTIONAL
	}

SearchArgument [[P struct ds_search_arg *]]
	::=
	CHOICE <D<0>>
	<E<(parm->sra_common.ca_sig == (struct signature *) 0)? 1:2>>
	{
		SearchArgumentData [[p * ]] ,
		SEQUENCE [[ T struct ds_search_arg * $ * ]]
		{
			SearchArgumentData [[p * ]] ,
			TAlgorithmIdentifier [[p parm->sra_common.ca_sig ]],
			TBitString [[ p parm->sra_common.ca_sig ]]
		}
	}


SearchResultData [[P struct ds_search_result *]]
	::=
	CHOICE <D<(*parm)->srr_correlated>>
	<E<((parm->srr_correlated == FALSE) ? 2 : 1)>>
	{
	searchInfo
		SET [[ T struct ds_search_unit * $ srr_un.srr_unit ]]
		{
			DistinguishedName [[p srr_object]]
			    OPTIONAL,
		entries
			[0] SET OF [[ T struct entrystruct * $ srr_entries ]]
			    <<ent_next>> EntryInformation [[p *]],

	%D{
		(*parm)->srr_poq.poq_limitproblem = LSR_NOLIMITPROBLEM;
	%}
		partialOutcomeQualifier
			[2] PartialOutcomeQualifier [[p &parm->srr_poq]]
			    OPTIONAL <E<
				     ((parm->srr_poq.poq_limitproblem != LSR_NOLIMITPROBLEM) || 
				     (parm->srr_poq.poq_cref != NULLCONTINUATIONREF)) 
				     >><D<0>>,
			[30] SecurityParameters [[p srr_common.cr_security ]]
			    OPTIONAL ,
		performer
			[29] DistinguishedName [[p srr_common.cr_requestor]]
			    OPTIONAL,
		aliasDereferenced
			[28] BOOLEAN [[b srr_common.cr_aliasdereferenced]]
			     DEFAULT FALSE
		},
	uncorrelatedSearchInfo
		[0] SET OF [[ T struct ds_search_result * $ srr_un.srr_parts ]]
		    <<srr_next>> SearchResult [[p *]]
	}
%D{
	if ((*parm)->srr_correlated == 2)
	   (*parm)->srr_correlated = 0;
%}


-- Pespy temp
-- If uncorrelated this will fail
SAlgorithmIdentifier [[P struct ds_search_unit *]]
	::= AlgorithmIdentifier [[p srr_common.cr_alg]]

SBitString [[P struct ds_search_unit *]]
	::= BIT STRING [[ x srr_common.cr_tmp $ srr_common.cr_len ]]

SearchResult [[P struct ds_search_result *]]
	::=
	CHOICE <E<1>><D<0>>
	{
		SearchResultData [[p * ]] ,
		SEQUENCE [[ T struct ds_search_result * $ *]]
		{
			SearchResultData [[p * ]] ,
			SAlgorithmIdentifier [[p srr_un.srr_unit ]],
			SBitString [[p srr_un.srr_unit]]
		}
	}

AddEntryArgumentData [[P struct ds_addentry_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->ada_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->ada_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->ada_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->ada_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->ada_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] DistinguishedName [[p ada_object]],
	entry
		[1] SET OF [[ T struct attrcomp * $ ada_entry ]] <<attr_link>> 
		    Attribute [[p *]],
		[30] ServiceControls [[p &parm->ada_common.ca_servicecontrol]]
		     -- DEFAULT {},
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->ada_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->ada_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->ada_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->ada_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->ada_common.ca_security]]
		     -- DEFAULT {} ,
		     OPTIONAL,
	requestor
		[28] DistinguishedName [[p  parm->ada_common.ca_requestor]]
		     OPTIONAL,
		[27] OperationProgress [[p &parm->ada_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->ada_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->ada_common.ca_aliased_rdns]]
		     OPTIONAL <E<parm->ada_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>><D<0>> ,
	extensions
		[25] SET OF [[ T struct extension * $ parm->ada_common.ca_extensions]]
		<<ext_next>> Extension [[p *]] OPTIONAL
	}

AddEntryArgument [[P struct ds_addentry_arg *]]
	::=
	CHOICE <D<0>>
	<E<(parm->ada_common.ca_sig == (struct signature *) 0)? 1:2>>
	{
		AddEntryArgumentData [[p * ]] ,
		SEQUENCE [[ T struct ds_addentry_arg * $ * ]]
		{
			AddEntryArgumentData [[p * ]] ,
			TAlgorithmIdentifier [[p parm->ada_common.ca_sig ]],
			TBitString [[p parm->ada_common.ca_sig ]]
		}
	}

AddEntryResult ::= NULL

RemoveEntryArgumentData [[P struct ds_removeentry_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->rma_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->rma_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->rma_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->rma_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->rma_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] DistinguishedName [[p rma_object]],
		[30] ServiceControls [[p &parm->rma_common.ca_servicecontrol]]
		     -- DEFAULT {},
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->rma_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->rma_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->rma_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->rma_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->rma_common.ca_security]]
		     -- DEFAULT {},
		     OPTIONAL,
	requestor
		[28] DistinguishedName [[p parm->rma_common.ca_requestor]]
		     OPTIONAL,
		[27] OperationProgress [[p &parm->rma_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->rma_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->rma_common.ca_aliased_rdns]]
		     OPTIONAL <E<parm->rma_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>><D<0>>,
	extensions
		[25] SET OF [[ T struct extension * $ parm->rma_common.ca_extensions]]
		<<ext_next>> Extension [[p *]] OPTIONAL
	}

RemoveEntryArgument [[P struct ds_removeentry_arg *]]
	::=
	CHOICE <D<0>>
	<E<(parm->rma_common.ca_sig == (struct signature *) 0)? 1:2>>
	{
		RemoveEntryArgumentData [[p * ]] ,
		SEQUENCE [[ T struct ds_removeentry_arg * $ * ]]
		{
			RemoveEntryArgumentData [[p * ]] ,
			TAlgorithmIdentifier [[p parm->rma_common.ca_sig ]],
			TBitString [[ p parm->rma_common.ca_sig ]]
		}
	}

RemoveEntryResult ::= NULL

ModifyEntryArgumentData [[P struct ds_modifyentry_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->mea_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->mea_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->mea_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->mea_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->mea_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] DistinguishedName [[p parm->mea_object]],
	changes
		[1] SEQUENCE OF [[ T struct entrymod * $ mea_changes]] <<em_next>>
			EntryModification [[p *]],
		[30] ServiceControls [[p &parm->mea_common.ca_servicecontrol]]
		     -- DEFAULT {} ,
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->mea_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->mea_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->mea_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->mea_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->mea_common.ca_security]]
		    -- DEFAULT {} ,
		    OPTIONAL,
	requestor
		[28] DistinguishedName [[p  parm->mea_common.ca_requestor]]
		    OPTIONAL,
		[27] OperationProgress [[p &parm->mea_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->mea_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->mea_common.ca_aliased_rdns]]
		    OPTIONAL <E<parm->mea_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>><D<0>>,
	extensions
		[25] SET OF [[ T struct extension * $ parm->mea_common.ca_extensions]]
		<<ext_next>> Extension [[p *]] OPTIONAL
	}

ModifyEntryArgument [[P struct ds_modifyentry_arg *]]
	::=
	CHOICE <D<0>>
	<E<(parm->mea_common.ca_sig == (struct signature *) 0) ? 1 : 2>>
	{
		ModifyEntryArgumentData [[p *]] ,
		SEQUENCE [[T struct ds_modifyentry_arg * $ * ]]
		{
			ModifyEntryArgumentData [[p *]] ,
			TAlgorithmIdentifier [[p parm->mea_common.ca_sig ]],
			TBitString [[ p parm->mea_common.ca_sig ]]
		}
	}

ModifyEntryResult ::= NULL

ModifyRDNArgumentData [[P struct ds_modifyrdn_arg *]]
	::=
	SET
	{
	%D{
	(*parm)->mra_common.ca_servicecontrol.svc_prio = SVC_PRIO_MED;
	(*parm)->mra_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	(*parm)->mra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;
	(*parm)->mra_common.ca_servicecontrol.svc_scopeofreferral = SVC_REFSCOPE_NONE;
	(*parm)->mra_common.ca_aliased_rdns = CA_NO_ALIASDEREFERENCED;
	%}
	object
		[0] DistinguishedName [[p mra_object]],
	newRDN
		[1] RelativeDistinguishedName [[p mra_newrdn]],
	deleteRDN
		[2] BOOLEAN [[b deleterdn]]
		     DEFAULT FALSE,
		[30] ServiceControls [[p &parm->mra_common.ca_servicecontrol]]
		     -- DEFAULT {},
		    OPTIONAL <D<0>>
		    <E< (
		    (parm->mra_common.ca_servicecontrol.svc_options != 0) ||
		    (parm->mra_common.ca_servicecontrol.svc_prio != SVC_PRIO_MED) ||
		    (parm->mra_common.ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT) ||
		    (parm->mra_common.ca_servicecontrol.svc_scopeofreferral != SVC_REFSCOPE_NONE)
		    ) >>,

		[29] SecurityParameters [[p parm->mra_common.ca_security]]
		    -- DEFAULT {} ,
		    OPTIONAL,
	requestor
		[28] DistinguishedName [[p mra_common.ca_requestor]]
		    OPTIONAL,
		[27] OperationProgress [[p &parm->mra_common.ca_progress]]
		     OPTIONAL <D<0>>
		     <E<parm->mra_common.ca_progress.op_resolution_phase > 1>>,
	aliasedRDNs
		[26] INTEGER [[i parm->mra_common.ca_aliased_rdns]]
		    OPTIONAL <E<parm->mra_common.ca_aliased_rdns != CA_NO_ALIASDEREFERENCED>><D<0>>,
	extensions
		[25] SET OF [[ T struct extension * $ parm->mra_common.ca_extensions]] 
		     <<ext_next>> Extension [[p *]] OPTIONAL
	}

ModifyRDNArgument [[P struct ds_modifyrdn_arg *]]
	::=
	CHOICE <D<0>>
	<E<(parm->mra_common.ca_sig == (struct signature *) 0)? 1:2>>
	{
		ModifyRDNArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_modifyrdn_arg * $ * ]]
		{
			ModifyRDNArgumentData [[p *]] ,
			TAlgorithmIdentifier [[p parm->mra_common.ca_sig ]],
			TBitString [[ p parm->mra_common.ca_sig ]]
		}
	}

ModifyRDNResult ::= NULL

DirectoryBindError [[P struct ds_bind_error *]]
	::=
	SET
	{
		%E{
			if (parm->dbe_version == DBA_VERSION_V1988) {
			   parm->dbe_vlen = 1;
			   parm->dbe_vtmp = int2strb_alloc(parm->dbe_version,1);
			} else
			  return NOTOK;
		%}
	%D{
	   (*parm)->dbe_version = DBA_VERSION_V1988;
	%}
		versions
			[0] Versions [[x dbe_vtmp $ dbe_vlen ]]
			    %E{
			    if (parm->dbe_vtmp)
				free (parm->dbe_vtmp);
			    %}
			    %D{
			    if ((*parm)->dbe_vlen) {
				(*parm)->dbe_version = strb2int((*parm)->dbe_vtmp ,(*parm)->dbe_vlen);
				free ((*parm)->dbe_vtmp);
			    }
			    %}
			    DEFAULT {v1988},
		    
		CHOICE [[T struct ds_bind_error * $ *]] <<dbe_type>>
		{
		serviceError
			[1] ServiceProblem [[i dbe_value]],
		securityError
			[2] SecurityProblem [[i dbe_value]]
		}
	}

AbandonFailedParm [[P struct DSE_abandon_fail *]]
	::=
	SET
	{
	problem
		[0] AbandonProblem [[i DSE_ab_problem]],
	operation
		[1] TInvokeID [[i DSE_ab_invokeid]]
	}

AttributeErrorParm [[P struct DSE_attribute *]]
	::=
	SET
	{
	object
		[0] Name [[p parm->DSE_at_name]],
	problems
		[1] SET OF [[ T struct DSE_at_problem * $ &parm->DSE_at_plist ]] << dse_at_next >>
			SEQUENCE [[ T struct DSE_at_problem * $ *]]
			{
			problem
				[0] AttributeProblem [[i DSE_at_what]],
			type
				[1] AttributeType [[p DSE_at_type]],
			value
				[2] AttributeValue [[p DSE_at_value]]
				    %D{
				    extern AttrT_print ();
				    extern LLog * log_dsap;

				    if ((*parm)->DSE_at_value)
				       if (AttrV_decode ((*parm)->DSE_at_type,
						(*parm)->DSE_at_value) != OK) {
		                          pslog (log_dsap,LLOG_EXCEPTIONS,
				          "AttrV_decode failed",
				          AttrT_print, (caddr_t) (*parm)->DSE_at_type);
				          return NOTOK;
				       }
				    %}
				    OPTIONAL
			}
	}

NameErrorParm [[P struct DSE_name *]]
	::=
	SET
	{
	problem
		[0] NameProblem [[i DSE_na_problem]],
	matched
		[1] Name [[p DSE_na_matched]]
	}

ReferralParm [[P struct DSE_referral *]]
	::=
	SET
	{
	candidate
		[0] ContinuationReference [[p DSE_ref_candidates]]
	}

SecurityErrorParm [[P struct DSE_security *]]
	::=
	SET
	{
	problem
		[0] SecurityProblem [[i DSE_sc_problem]]
	}

ServiceErrorParm [[P struct DSE_service *]]
	::=
	SET
	{
	problem
		[0] ServiceProblem [[i DSE_sv_problem]]
	}

UpdateErrorParm [[P struct DSE_update *]]
	::=
	SET
	{
	problem
		[0] UpdateProblem [[i DSE_up_problem]]
	}


END


%{

/* same as int2strb but with a real buffer */
char   *int2strb_alloc (n, len)
register int    n;
int     len;
{
    register int    i;
    static char *buffer;

    buffer = calloc (1,sizeof (int) + 1);

    for (i = 0; i < len; i++)
	if (n & (1 << i))
	    buffer[i / 8] |= (1 << (7 - (i % 8)));

    return buffer;
}


#ifndef lint

#undef encode_DAS_TokenToSign
int	encode_DAS_TokenToSign(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_TokenToSign *parm;
{
  return (enc_f(_ZTokenToSignDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_ReadArgumentData
int	encode_DAS_ReadArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_ReadArgumentData *parm;
{
  return (enc_f(_ZReadArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_CompareArgumentData
int	encode_DAS_CompareArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_CompareArgumentData *parm;
{
  return (enc_f(_ZCompareArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}
#undef encode_DAS_ListArgumentData
int	encode_DAS_ListArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_ListArgumentData *parm;
{
  return (enc_f(_ZListArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_SearchArgumentData
int	encode_DAS_SearchArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_SearchArgumentData *parm;
{
  return (enc_f(_ZSearchArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_AddEntryArgumentData
int	encode_DAS_AddEntryArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_AddEntryArgumentData *parm;
{
  return (enc_f(_ZAddEntryArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_RemoveEntryArgumentData
int	encode_DAS_RemoveEntryArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_RemoveEntryArgumentData *parm;
{
  return (enc_f(_ZRemoveEntryArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_ModifyEntryArgumentData
int	encode_DAS_ModifyEntryArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_ModifyEntryArgumentData *parm;
{
  return (enc_f(_ZModifyEntryArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#undef encode_DAS_ModifyRDNArgumentData
int	encode_DAS_ModifyRDNArgumentData(pe, top, len, buffer, parm)
PE     *pe;
int	top,
	len;
char   *buffer;
struct type_DAS_ModifyRDNArgumentData *parm;
{
  return (enc_f(_ZModifyRDNArgumentDataDAS, &_ZDAS_mod, pe, top, len, buffer,
		(char *) parm));
}

#endif 

%}
