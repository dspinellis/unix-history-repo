-- do.py - manually augmented DistributedOperations module

-- $Header: /f/osi/dsap/x500as/RCS/do.py,v 7.2 91/03/09 11:53:54 mrose Exp $
--
-- $Log:	do.py,v $
-- Revision 7.2  91/03/09  11:53:54  mrose
-- update
-- 
-- Revision 7.1  91/02/22  09:21:56  mrose
-- Interim 6.8
-- 
-- Revision 7.0  90/12/06  07:34:44  mrose
-- *** empty log message ***
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


DO
	{
	joint-iso-ccitt
	ds(5)
	modules(1)
	distributedOperations(3)
	}

DEFINITIONS ::=

PREFIXES encode decode print

BEGIN

-- EXPORTS
--	ContinuationReference ,
--	OperationProgress ,
-- 	DSAReferralParm;

IMPORTS
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

	AlgorithmIdentifier
		FROM AF
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			authenticationFramework(7)
			}

	ReadArgument ,
	ReadResult ,
	CompareArgument ,
	CompareResult ,
	AbandonArgument ,
	AbandonResult ,
	ListArgument ,
	ListResult ,
	SearchArgument ,
	SearchResult ,
	AddEntryArgument ,
	AddEntryResult ,
	RemoveEntryArgument ,
	RemoveEntryResult ,
	ModifyEntryArgument ,
	ModifyEntryResult ,
	ModifyRDNArgument ,
	ModifyRDNResult ,
	SecurityParameters
		FROM DAS
			{
			joint-iso-ccitt
			ds(5)
			modules(1)
			directoryAbstractService(2)
			};

ChainedReadArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedReadArgumentData [[p *]] ,
		SEQUENCE [[T struct ds_op_arg * $ * ]]
		{
			ChainedReadArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedReadArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] ReadArgument [[p &parm->dca_dsarg.arg_rd]]
	}

ChainedReadResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}
	CHOICE <<dcr_choice>>
	{
		ChainedReadResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedReadResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}

ChainedReadResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] ReadResult [[p &parm->dcr_dsres.res_rd]]
	}

ChainedCompareArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedCompareArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ * ]]
		{
			ChainedCompareArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedCompareArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] CompareArgument [[p &parm->dca_dsarg.arg_cm]]
	}

ChainedCompareResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}
	CHOICE <<dcr_choice>>
	{
		ChainedCompareResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedCompareResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}

ChainedCompareResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] CompareResult [[p &parm->dcr_dsres.res_cm]]
	}

ChainedAbandonArgument [[P struct ds_op_arg *]]
	::=
	AbandonArgument [[p &parm->dca_dsarg.arg_ab]]

ChainedAbandonResult [[P struct ds_op_res *]]
	::=
	AbandonResult

ChainedListArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedListArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ * ]]
		{
			ChainedListArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedListArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] ListArgument [[p &parm->dca_dsarg.arg_ls]]
	}

ChainedListResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}
	CHOICE <<dcr_choice>>
	{
		ChainedListResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedListResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
}


ChainedListResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] ListResult [[p &parm->dcr_dsres.res_ls]]
	}

ChainedSearchArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedSearchArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ * ]]
		{
			ChainedSearchArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedSearchArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] SearchArgument [[p &parm->dca_dsarg.arg_sr]]
	}

ChainedSearchResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}
	CHOICE <<dcr_choice>>
	{
		ChainedSearchResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedSearchResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}

ChainedSearchResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] SearchResult [[p &parm->dcr_dsres.res_sr]]
	}

ChainedAddEntryArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedAddEntryArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ * ]]
		{
			ChainedAddEntryArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedAddEntryArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] AddEntryArgument [[p &parm->dca_dsarg.arg_ad]]
	}

ChainedAddEntryResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}	
	CHOICE <<dcr_choice>>
	{
		ChainedAddEntryResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedAddEntryResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}

ChainedAddEntryResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] AddEntryResult [[p dcr_choice]] -- never set for pespy !
	}

ChainedRemoveEntryArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}	
	CHOICE <<dca_choice>>
	{
		ChainedRemoveEntryArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ * ]]
		{
			ChainedRemoveEntryArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedRemoveEntryArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] RemoveEntryArgument [[p &parm->dca_dsarg.arg_rm]]
	}

ChainedRemoveEntryResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}	
	CHOICE <<dcr_choice>>
	{
		ChainedRemoveEntryResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedRemoveEntryResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}	

ChainedRemoveEntryResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] RemoveEntryResult [[p dcr_choice]] -- never set for pespy !
	}

ChainedModifyEntryArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedModifyEntryArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ * ]]
		{
			ChainedModifyEntryArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedModifyEntryArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] ModifyEntryArgument [[p &parm->dca_dsarg.arg_me]]
	}

ChainedModifyEntryResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}
	CHOICE <<dcr_choice>>
	{
		ChainedModifyEntryResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedModifyEntryResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}

ChainedModifyEntryResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] ModifyEntryResult [[p dcr_choice]] -- never set for pespy !
	}

ChainedModifyRDNArgument [[P struct ds_op_arg *]]
	::=
	%E{
		if ( ! parm->dca_choice )
				parm->dca_choice = 1;
	%}
	CHOICE <<dca_choice>>
	{
		ChainedModifyRDNArgumentData [[p *]] ,
		SEQUENCE [[ T struct ds_op_arg * $ *]]
		{
			ChainedModifyRDNArgumentData [[p *]] ,
			AlgorithmIdentifier [[p dca_alg]],
			BIT STRING [[ x dca_bit $ dca_len ]]
		}
	}

ChainedModifyRDNArgumentData [[P struct ds_op_arg *]]
	::=
	SET
	{
		ChainingArgument [[p &parm->dca_charg]] ,
		[0] ModifyRDNArgument [[p &parm->dca_dsarg.arg_mr]]
	}

ChainedModifyRDNResult [[P struct ds_op_res *]]
	::=
	%E{
		if ( ! parm->dcr_choice )
				parm->dcr_choice = 1;
	%}
	CHOICE <<dcr_choice>>
	{
		ChainedModifyRDNResultData [[p *]] ,
		SEQUENCE [[T struct ds_op_res * $ *]]
		{
			ChainedModifyRDNResultData [[p *]] ,
			AlgorithmIdentifier [[p dcr_alg]],
			BIT STRING [[ x dcr_bit $ dcr_len ]]
		}
	}

ChainedModifyRDNResultData [[P struct ds_op_res *]]
	::=
	SET
	{
		ChainingResult [[p &parm->dcr_chres]] ,
		[0] ModifyRDNResult [[p dcr_choice]] -- never set for pespy !
	}

DSAReferralParm [[P struct DSE_referral *]]
	::=
	SET
	{
		[0] ContinuationReference [[p DSE_ref_candidates]] ,
	contextPrefix
		[1] DistinguishedName [[p DSE_ref_prefix]]
		    OPTIONAL
	}

ChainingArgument [[P struct chain_arg *]]
	::=
	SET
	{
	originator
		[0] DistinguishedName [[p cha_originator]]
		    OPTIONAL,
	targetObject
		[1] DistinguishedName [[p cha_target]]
		    OPTIONAL,
	operationProgress
		[2] OperationProgress [[p &parm->cha_progress]]
		    -- DEFAULT {notStarted} ,
		    OPTIONAL <D<0>>
			     <E<parm->cha_progress.op_resolution_phase > 1>>,
	traceInformation
		[3] TraceInformation [[p cha_trace]] ,
	aliasDereferenced
		[4] BOOLEAN [[b cha_aliasderef]]
		    DEFAULT FALSE ,
	aliasedRDNs
		[5] INTEGER [[i cha_aliasedrdns]]
		    OPTIONAL <D<0>>
			     <E<parm->cha_aliasedrdns != CR_NOALIASEDRDNS>>,
	entryOnly
		[11] BOOLEAN [[b cha_entryonly]]
		    DEFAULT FALSE ,
	returnCrossRefs
		[6] BOOLEAN [[b cha_returnrefs]]
		    DEFAULT FALSE ,
	referenceType
		[7] ReferenceType [[i cha_reftype]]
		    DEFAULT superior ,
	info
		[8] DomainInfo [[a cha_domaininfo]]
		    OPTIONAL,
	timeLimit
		[9] UTCTime [[s cha_timelimit]]
		    OPTIONAL,
		[10] SecurityParameters [[p cha_security]]
		    -- DEFAULT {}
		    OPTIONAL
	}

ChainingResult [[P struct chain_res *]]
	::=
	SET
	{
	info
		[0] DomainInfo [[a chr_domaininfo]]
		    OPTIONAL,
	crossReferences
		[1] SEQUENCE OF [[ T struct cross_ref * $ chr_crossrefs]] <<xref_next>>
			CrossReference [[p *]]
		    OPTIONAL,
		[2] SecurityParameters [[p chr_security]]
		    -- DEFAULT {}
		    OPTIONAL
	}

CrossReference [[P struct cross_ref *]]
	::=
	SET
	{
	contextPrefix
		[0] DistinguishedName [[p xref_dn]] ,
	accessPoint
		[1] AccessPoint [[p xref_ap]]
	}

-- This is pulled up as an 'i' type, so no parameters are needed.
ReferenceType
        ::=
        ENUMERATED
        {
        superior(1) ,
        subordinate(2) ,
        cross(3) ,
        nonSpecificSubordinate(4)
        }

TraceInformation [[P struct trace_info *]]
	::=
	SEQUENCE OF [[ T struct trace_info * $ *]] <<ti_next>>
		TraceItem [[p *]]

TraceItem [[P struct trace_info *]]
	::=
	SET
	{
	dsa
                [0] Name [[p ti_dsa]] ,
	targetObject
		[1] Name [[p ti_target]]
		    OPTIONAL,
	operationProgress
		[2] OperationProgress [[p &parm->ti_progress]]
	}

OperationProgress [[P struct op_progress *]]
	::=
        SET
        {
	%E{
		if (parm->op_resolution_phase < 0)
		   parm->op_resolution_phase = 1;
		else if (parm->op_resolution_phase > 3)
		   parm->op_resolution_phase = 3;
	%}
        nameResolutionPhase
                [0] ENUMERATED [[i op_resolution_phase]]
                {
                notStarted(1) ,
                proceeding(2) ,
                completed(3)
                },
        nextRDNToBeResolved
                [1] INTEGER [[i op_nextrdntoberesolved]]
		    OPTIONAL <D<0>>
			     <E< parm->op_nextrdntoberesolved != -1>>
        }

-- Pulled up
DomainInfo ::=	ANY

ContinuationReference [[P continuation_ref * ]]
        ::=
        SET
        {
	%D{
		(*parm)->cr_aliasedRDNs  = CR_RDNRESOLVED_NOTDEFINED;
		(*parm)->cr_rdn_resolved = CR_NOALIASEDRDNS;
		(*parm)->cr_reftype = RT_UNDEFINED;
	%}
        targetObject
                [0] Name [[p cr_name]],
        aliasedRDNs
                [1] INTEGER [[i cr_aliasedRDNs]]
                    OPTIONAL <E< parm->cr_aliasedRDNs != CR_NOALIASEDRDNS>> <D<0>>,
        operationProgress
                [2] OperationProgress [[p &parm->cr_progress]],
        rdnsResolved
                [3] INTEGER [[i cr_rdn_resolved]]
                    OPTIONAL <E< parm->cr_rdn_resolved != CR_RDNRESOLVED_NOTDEFINED>> <D<0>>,
        referenceType
                [4] ReferenceType [[i cr_reftype]]
                    OPTIONAL <E< parm->cr_reftype != RT_UNDEFINED>> <D<0>>,
        accessPoints
                [5] SET OF [[ T struct  access_point * $ cr_accesspoints]] <<ap_next>>
                        AccessPoint [[p *]]
        }

AccessPoint [[P struct access_point *]]
        ::=
        SET
        {
                [0] Name [[p ap_name]],
                [1] ISODEPresentationAddress [[p ap_address ]]
				[[E enc_ipa ]][[D dec_ipa ]]
				
        }

-- Quipu Access point.  Should be in qu.py but easier to put it here.
-- Same as above Access Point, but PresentationAddress is optional.
QAccessPoint [[P struct access_point *]]
        ::=
        SET
        {
                [0] Name [[p ap_name]],
                [1] ISODEPresentationAddress [[p ap_address ]]
				[[E enc_ipa ]][[D dec_ipa]]
		    OPTIONAL <E<parm->ap_address != NULLPA>><D<0>>
        }

-- Pulled up
ISODEPresentationAddress ::= ANY -- DSE.PSAPaddr 

-- Pulled up
InvokeID ::= INTEGER

END

%{

enc_ipa (parm,ppe)
struct access_point *parm;
PE *ppe;
{
	return build_DSE_PSAPaddr (ppe, 0, 0, NULLCP, (char *)parm->ap_address);
}

dec_ipa (parm,pe)
struct access_point ** parm;
PE pe;
{
struct PSAPaddr *psap;
int res;

	psap = (struct PSAPaddr *) smalloc (sizeof *psap);

	if ((res = parse_DSE_PSAPaddr (pe, 0, NULLIP, NULLVP, psap)) == NOTOK) {
	   free ((char *) psap);
	   return res;
	}

	(*parm)->ap_address = psap;
	return res;
}

%}
