/* alias_chk.c - checks aliases from position provided downwards. */
/* A management tool - probably best to run this as a manager. */

#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/manage/RCS/alias_chk.c,v 7.1 91/02/22 09:32:02 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/manage/RCS/alias_chk.c,v 7.1 90/07/27 08:4
 *
 *
 * $Log:	alias_chk.c,v $
 * Revision 7.1  91/02/22  09:32:02  mrose
 * Interim 6.8
 * 
 * Revision 7.0  91/01/24  14:43:41  mrose
 * *** empty log message ***
 * 
 * Revision 7.1  90/07/27  08:47:16  mrose
 * update
 *
 * Revision 7.0  90/06/26  14:52:31  mrose
 * *** empty log message ***
 *
 */

/*
 *				NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include "quipu/entry.h"
#include "quipu/util.h"
#include "quipu/compare.h"
#include "quipu/ds_search.h"
#include "quipu/read.h"

#define ORG_PERSON "thornPerson & quipuObject"
	/* this should probably go elsewhere !!! */

extern  DN      dn;
#define OPT     (!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define RPS     (!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern  char    frompipe;
extern  PS      opt;
extern  PS      rps;
extern	Entry	current_entry;
extern	DN	sequence_dn();
extern	DN	str2dn_aux();
extern	Filter	get_filter();
	char  **NULLARGV = (char **) 0;

call_alias_chk(argc, argv)
int	argc ;
char  **argv ;
{
	struct	ds_search_arg	search_arg;
	struct	DSError		search_error;
	struct	ds_search_result search_result;
	DN	save_dn, tmp_dn = NULLDN;
	char	alias = FALSE ;
	char    verify_alias() ;
	EntryInfo	*ptr ;

	if (argc > 2)
	{
		ps_print(OPT, "Error, too many arguments..\n") ;
		return(NOTOK) ;
	}

	if (argc == 2)
	{
		/* Convert arg 2 to a DN for future use. */

		/* Turn a sequence number back into a DN */
		if (*argv[1] >= '0' && *argv[1] <= '9')
		{
		    /* First convert the number into a dn */
		    tmp_dn = dn_cpy(sequence_dn(atoi(argv[1]))) ;
		}
		else
		{
		    if (*argv[1] == '.')
		    {
		    	ps_print(OPT, "..@ gives me a headache. Ambiguous. Aborting.\n") ;
		    	return(NOTOK) ;
		    }

		    if (*argv[1] == '@')
		    {
		    	tmp_dn = dn_cpy(str2dn(argv[1] + 1)) ;
		    }
		    else
		    {
		    	save_dn = dn_cpy(str2dn_aux(argv[1], &alias)) ;
		    	if (save_dn != NULLDN)
		    	{
			    if (alias)
			    {
				tmp_dn = dn_cpy(save_dn);
			    }
			    else
			    {
				if (dn == NULLDN)
				{
				    tmp_dn = dn_cpy(save_dn) ;
				}
				else
				{
				    tmp_dn = dn_cpy(dn) ;
				    dn_append(tmp_dn, dn_cpy(save_dn));
				}
			    }
			    dn_free(save_dn) ;
			}
		    }
		}
	}

	/* We now have the start location in tmp_dn */
	if (tmp_dn != NULLDN)
	{
		save_dn = dn_cpy(dn) ;	/* save start location for later restore */
		dn_free(dn) ;
		dn = dn_cpy(tmp_dn) ;
		dn_free(tmp_dn) ;
	}
	        
	/* dn should be set to a: either current location, or
			       b: 2nd arg if specified. */

	/* Is dn a leaf or a non_leaf? */
	{
		struct	ds_compare_arg		compare_arg;
		struct	DSError			compare_error;
		struct	ds_compare_result	compare_result;

		if ((argc = service_control (OPT, argc, argv, &compare_arg.cma_common)) == -1)
		    return(NOTOK) ;

		compare_arg.cma_common.ca_servicecontrol.svc_options |= SVC_OPT_DONTDEREFERENCEALIAS ;
		compare_arg.cma_object = dn;
		if (get_ava (&compare_arg.cma_purported, "objectClass", "quipuNonLeafObject") != OK)
		{
		    ps_print(OPT, "Oops, 'objectClass=quipuNonLeafObject' is a bad attribute!\n") ;
		    ps_print(OPT, "This is very bad...\n") ;
		    return(NOTOK) ;
		}

	        if (compare_arg.cma_common.ca_security != (struct security_parms *) 0)
	        {
		        struct	signature	*sign_operation();
		        int	encode_DAS_CompareArgumentData();
			
		        compare_arg.cma_common.ca_sig =
	                    sign_operation((caddr_t)&compare_arg, encode_DAS_CompareArgumentData) ;
	        }

		if (rebind () != OK)
		    return(NOTOK) ;
		
		while (ds_compare (&compare_arg, &compare_error, &compare_result) != DS_OK) 
		{
		    if (dish_error (OPT, &compare_error) == 0)
		    {
			return(NOTOK) ;
		    }
		    compare_arg.cma_object = compare_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
		}

		/* If the object is a leaf then check it out and exit... */
		if ( compare_result.cmr_matched == FALSE )
		{
			struct	DSError		read_error;
			struct	ds_read_result	read_result;
			struct	ds_read_arg	read_arg;
			
			read_arg.rda_eis.eis_allattributes = TRUE ;
			read_arg.rda_eis.eis_select = NULLATTR ;
			read_arg.rda_eis.eis_infotypes = TRUE ;

			if (service_control (OPT, argc, argv, &read_arg.rda_common) == -1)
			{
				return(-1) ;
			}
			read_arg.rda_common.ca_servicecontrol.svc_options 
				|= SVC_OPT_DONTDEREFERENCEALIAS ;
			read_arg.rda_object = dn;

			/* Strong authentication */
			if (read_arg.rda_common.ca_security !=
				(struct security_parms *) 0)
			{
			struct signature *sign_operation();
			int encode_DAS_ReadArgumentData();

			read_arg.rda_common.ca_sig =
				sign_operation((caddr_t)&read_arg,
					encode_DAS_ReadArgumentData);
			}
			while (ds_read (&read_arg, &read_error, &read_result) != DS_OK) {
				if (dish_error (OPT, &read_error) == 0)
					return (-2);
				read_arg.rda_object = read_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
			}

			if ( verify_alias(&read_result.rdr_entry) != OK )
			{
				  ps_print(OPT, "Bad Alias.\n") ;
			}
			return(OK) ;
		}
	}

	/* Else search the subtree below this current 
	 * position for ALL aliases, and check each one. */

	/* Sort out the search filter for this. */
	search_arg.sra_baseobject = dn ;
	search_arg.sra_subset = SRA_WHOLESUBTREE ;
	search_arg.sra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT ;
	search_arg.sra_searchaliases = FALSE ;
	search_arg.sra_filter = NULLFILTER ;

	if ((argc = service_control (OPT, argc, argv, &search_arg.sra_common)) == -1)
		return(NOTOK);

	if ((search_arg.sra_filter = get_filter("objectClass=alias")) == NULLFILTER)
 	{
		ps_printf (OPT,"Very bad... Filter wrong. Aborting...\n");
		return(NOTOK) ;
	}
	ps_print(OPT, "Searching... please wait...\n") ;

	if (rebind () != OK)
		return(NOTOK);

	/* Strong authentication */
	if (search_arg.sra_common.ca_security != (struct security_parms *) 0)
	{
		struct signature *sign_operation();
		int encode_DAS_SearchArgumentData();

		search_arg.sra_common.ca_sig = sign_operation((caddr_t)&search_arg, encode_DAS_SearchArgumentData);
	}

	while (ds_search (&search_arg, &search_error, &search_result) != DS_OK) 
	{
		if (dish_error (OPT, &search_error) == 0)
			return(NOTOK);
		search_arg.sra_baseobject = search_error.ERR_REFERRAL.DSE_ref_candidates->cr_name ;
	}

	correlate_search_results (&search_result);
	
	if (search_result.CSR_entries == NULLENTRYINFO)
	{
		ps_printf(OPT, "No aliases found...\n");
		return(OK) ;
	}
	
	for (ptr = search_result.CSR_entries; ptr != NULLENTRYINFO; ptr = ptr->ent_next)
	{
		/* decode it immediately so we only have to do it once. */
		cache_entry (ptr, TRUE, TRUE) ;
		if ( verify_alias(ptr) != OK )
		{
		        ps_print(OPT, "Bad Alias.\n") ;
		}  
	}

	handle_problems (RPS, search_result.CSR_cr,search_result.CSR_limitproblem, TRUE);
	filter_free (search_arg.sra_filter);
	dn_free(dn) ;
	dn = dn_cpy(save_dn) ;
	dn_free(save_dn) ;
	return(OK) ;
}

char
verify_alias(alias_entry)
EntryInfo	*alias_entry ;
{
	static  char	       *nvec[2] = {"search"};	
	struct	DSError		read_error;
	struct	ds_read_result	read_result;
	struct	ds_read_arg	read_arg;
	struct	attrcomp       *tmp_ent_attr ;
		AttributeType	at_ojc ;
		AttributeType	at_aoj ;
		AttributeType	at_acl ;
		AttributeType	at_sa ;
		AttributeType	at_lmt ;
		AttributeType	at_lmb ;
                AttributeType   at_trs ;
                AttributeType   at_c ;
                AttributeType   at_o ;
                AttributeType   at_ou ;
                AttributeType   at_cn ;

		AV_Sequence     object_class_of_object ;
		AV_Sequence     tree_strAVS = NULLAV ;
	        DN              dn_above_alias, trail, dnptr ;
	        char            Name=FALSE, Acl=FALSE, ObjClass=FALSE, AlObjNam=FALSE ;
                char            BackReference=FALSE ;
                char            GoodAlias = OK ;

	at_ojc = AttrT_new("objectClass") ;                  /* objectClass */
	at_aoj = AttrT_new("aliasedObjectName") ;
	at_acl = AttrT_new("acl") ;
	at_sa  = AttrT_new("seeAlso") ;
	at_lmt = AttrT_new("lastModifiedTime") ;
	at_lmb = AttrT_new("lastModifiedBy") ;
	at_trs = AttrT_new("treeStructure") ;
	at_c   = AttrT_new("countryName") ;
	at_o   = AttrT_new("organizationName") ;
	at_ou  = AttrT_new("organizationalUnitName") ;
	at_cn  = AttrT_new("commonName") ;
	nvec[1] = "-compact";
	read_arg.rda_object = NULLDN ;

	ps_print(OPT, "\nFound alias:") ;
	dn_print(OPT, alias_entry->ent_dn, EDBOUT) ;
	ps_print(OPT, "\n") ;

	/* We now have ourselves an entrystruct which is an alias, and we 
	 * have to check various features!!
	 * 1: only the allowed objects.
	 * 2: the seeAlso attribute is correct (ie points to a real entry).
	 * 3: The objectClass of the real entry fits under where the alias
	 *    lives.
	 */

	/* Does the alias have the allowed objects and only the allowed? */

	for (tmp_ent_attr = alias_entry->ent_attr; tmp_ent_attr != NULL; tmp_ent_attr = tmp_ent_attr->attr_link)
	{
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_ojc) == 0)
		{
			ObjClass = TRUE ;
		} else
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_aoj) == 0)
		{
			read_arg.rda_object = dn_cpy ((DN) tmp_ent_attr->attr_value->avseq_av.av_struct);
			AlObjNam = TRUE ;
		} else
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_acl) == 0)
		{
			Acl = TRUE ;
		}
		else
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_lmt) == 0)
		{
		  ;
		}
		else
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_lmb) == 0)
		{
		  ;
		}
		else
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_c) == 0 ||
		    AttrT_cmp(tmp_ent_attr->attr_type, at_o) == 0 ||
		    AttrT_cmp(tmp_ent_attr->attr_type, at_ou) == 0 ||
		    AttrT_cmp(tmp_ent_attr->attr_type, at_cn) == 0)
		{
		        Name = TRUE ;
		}
		else
		{
			ps_print(OPT, "Illegal attribute type: ") ;
			AttrT_print (OPT, tmp_ent_attr->attr_type, EDBOUT) ;
			ps_print(OPT, "\n") ;
			GoodAlias = NOTOK;
		}
	}
		
	if (read_arg.rda_object == NULLDN )
	{
	  ps_print(OPT, "Malformed alias. No alias object name present!\n" ) ;
	  return (NOTOK) ;
	}
	
	if (ObjClass == FALSE)
	{
	  ps_print(OPT, "Object Class missing.\n") ;
	}
	if (AlObjNam == FALSE)
	{
	  ps_print(OPT, "Alias Object name missing.\n") ;
	}
	if (Acl == FALSE)
	{
	  ps_print(OPT, "ACL missing.\n") ;
	}
	if (Name == FALSE)
	{
	  ps_print(OPT, "Name of alias is missing.\n") ;
	}

	GoodAlias = ((ObjClass && AlObjNam && Acl && Name) ? OK : NOTOK) ;
	/* Read the entry that the alias points to.... */
	read_arg.rda_eis.eis_allattributes = TRUE ;
	read_arg.rda_eis.eis_select = NULLATTR ;
	read_arg.rda_eis.eis_infotypes = TRUE ;

	if (service_control (OPT, 0, NULLARGV, &read_arg.rda_common) == -1)
	{
		return(-1) ;
	}
	read_arg.rda_common.ca_servicecontrol.svc_options 
		|= SVC_OPT_DONTDEREFERENCEALIAS ;

	/* Strong authentication */
	if (read_arg.rda_common.ca_security !=
		(struct security_parms *) 0)
	{
	struct signature *sign_operation();
	int encode_DAS_ReadArgumentData();

	read_arg.rda_common.ca_sig =
		sign_operation((caddr_t)&read_arg,
			encode_DAS_ReadArgumentData);
	}
	while (ds_read (&read_arg, &read_error, &read_result) != DS_OK) {
		if (dish_error (OPT, &read_error) == 0)
		{
		        ps_print(OPT, "Can't read ") ;
			dn_print(OPT, read_arg.rda_object, EDBOUT) ;
			ps_print(OPT, "\n") ;
			return (GoodAlias);
		}
		read_arg.rda_object = read_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}

	/* and see if it points back to this alias. 
	 * It should do, but doesn't have to. While we are at it,
	 * collect the objectClass of this entry.
	 */
	for (tmp_ent_attr = read_result.rdr_entry.ent_attr; tmp_ent_attr != NULL; 
	     tmp_ent_attr = tmp_ent_attr->attr_link)
	{
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_sa) == 0)
		{
		        AV_Sequence   tmp_avs = tmp_ent_attr->attr_value;

			for (; tmp_avs != NULL; tmp_avs = tmp_avs->avseq_next)
			{
			  if (dn_cmp((DN) tmp_avs->avseq_av.av_struct, alias_entry->ent_dn))
			      {
				ps_print(OPT, "Alias object correctly points back to alias itself.\n") ;
				BackReference = TRUE ;
			      }
			}
		}
		else
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_ojc) == 0)
		{
		        object_class_of_object = avs_cpy(tmp_ent_attr->attr_value) ;
		}
	}
	if (BackReference == FALSE)
	{
	        ps_print(OPT, "Alias object should point back to alias.\n") ;
	}

	if (object_class_of_object == NULLAV)
	{
	        ps_print(OPT, "Can't find object class of aliased object\n. Assuming OK\n") ;
		return(GoodAlias) ;
	}
	/* Move above the alias in order to find the treeStructure. */
	dn_above_alias = dn_cpy(alias_entry->ent_dn) ;
        for (dnptr = dn_above_alias; dnptr->dn_parent != NULLDN; dnptr = dnptr->dn_parent)
	        trail = dnptr;
        dn_comp_free (dnptr);
        trail->dn_parent = NULLDN;

	/* Now read it... */
	read_arg.rda_object = dn_cpy(dn_above_alias) ;
	/* Strong authentication */
	if (read_arg.rda_common.ca_security !=
		(struct security_parms *) 0)
	{
	struct signature *sign_operation();
	int encode_DAS_ReadArgumentData();

	read_arg.rda_common.ca_sig =
		sign_operation((caddr_t)&read_arg,
			encode_DAS_ReadArgumentData);
	}
	while (ds_read (&read_arg, &read_error, &read_result) != DS_OK) {
		if (dish_error (OPT, &read_error) == 0)
			return (-2);
		read_arg.rda_object = read_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}

	for (tmp_ent_attr = read_result.rdr_entry.ent_attr; tmp_ent_attr != NULL; 
	     tmp_ent_attr = tmp_ent_attr->attr_link)
	{
		if (AttrT_cmp(tmp_ent_attr->attr_type, at_trs) == 0)
		{
		  tree_strAVS = avs_cpy(tmp_ent_attr->attr_value) ;
		}
	}

	if (tree_strAVS == NULLAV)
	{
	  ps_print(OPT, "Tree structure missing - assuming validity.\n") ;
	}
	else
        if (test_schema(tree_strAVS, object_class_of_object) != OK)
	{
	  ps_print(OPT, "Tree structure bad...\n") ;
	  GoodAlias = NOTOK ;
	}
	return (GoodAlias) ;
}

shadow_entry()
{
  ;
}
