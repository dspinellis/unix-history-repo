/* del_alias.c -  */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/manage/RCS/del_alias.c,v 7.3 91/02/22 09:32:03 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/manage/RCS/del_alias.c,v 7.3 91/02/22 09:32:03 mrose Interim $
 *
 *
 * $Log:	del_alias.c,v $
 * Revision 7.3  91/02/22  09:32:03  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/24  14:43:14  mrose
 * update
 * 
 * Revision 7.1  90/07/27  08:47:19  mrose
 * update
 * 
 * Revision 7.0  90/06/26  14:52:33  mrose
 * *** empty log message ***
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include "quipu/util.h"
#include "quipu/entry.h"
#include "quipu/remove.h"
#include "quipu/compare.h"
#include "quipu/modify.h"

extern	DN       dn;
extern  char    frompipe;
#define OPT     (!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define RPS     (!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	PS	opt;
extern	PS	rps;
extern	Entry	current_entry ;

call_del_alias (argc, argv)
int		argc;
char	      **argv;
{
	DN		dnptr, aoj_dn, oj_dn, save_dn;
	DN		trail = NULLDN ;
extern	DN		str2dn_aux() ;
extern	DN		sequence_dn () ;
	PS		str_ps ;
	char	       *str_buffer ;
	char		alias = FALSE ;
	int		mod_failed = 0 ;
	AV_Sequence	aliObjNameAVS ;
        struct          entrymod               *emnew ;
	struct		DSError			compare_error;
	struct		ds_compare_result	compare_result;
	struct		ds_compare_arg		compare_arg;
	struct		ds_removeentry_arg	remove_arg;
	struct		ds_modifyentry_arg	mod_arg;
	struct		DSError			mod_error;
	struct		DSError			error;
	char		objectname[80] ;
	char	       *contact_compare[6] ;
	char	       *contact_showentry[6] ;
	char	       *contact_modify[1] ;
	contact_compare[0] = "compare" ;
	contact_compare[1] = "";
	contact_compare[2] = "-attribute";
	contact_compare[4] = "-noprint";
	contact_compare[5] = "-dontdereferencealias" ;
	contact_showentry[0] = "showentry";
	contact_showentry[1] = "-noshow" ;
	contact_showentry[2] = "-all" ;
	contact_showentry[3] = "-nokey" ;
	contact_showentry[4] = "-dontdereferencealias";
	contact_modify[0] = "modify" ;

	emnew = em_alloc() ;
	str_buffer = (char *) malloc ((unsigned)1000) ;
	contact_showentry[5] = (char *) malloc ((unsigned)strlen(argv[1])) ;
	(void)strcpy(contact_showentry[5], argv[1]+1) ;

	contact_compare[3] = (char *) malloc ((unsigned)strlen("objectClass=alias.")) ;
	(void)strcpy(contact_compare[3], "objectClass=alias") ;

	if ((argc = service_control (OPT, argc, argv, &remove_arg.rma_common)) == -1)
		return ;

	if (argc > 2)
	{
		ps_printf (OPT,"Too many arguments. Aborting...\n");
		Usage (argv[0]);
		return ;
	}

	if (argc == 1)
	{
		ps_printf (OPT,"Delete what???\n") ;
		Usage (argv[0]) ;
		return ;
	}

	(void)strcpy(objectname,argv[1]) ;
	contact_compare[1] = argv[1] ;

	if (service_control (OPT, 6, contact_compare, &compare_arg.cma_common) == -1)
	{
		ps_print(OPT, "Problems with compare service control flags.\n") ;
		return ;
	}

	/* Turn a sequence number back into a DN */
	if (*objectname >= '0' && *objectname <= '9')
	{
		/* First convert the number into a dn */
		oj_dn = dn_cpy(sequence_dn(atoi(objectname))) ;
	}
	else
	{
		if (*objectname == '.')
		{
			ps_print(OPT, "..@ gives me a headache. Ambiguous. Aborting.\n") ;
			return ;
		}
		if (*objectname == '@')
		{
			oj_dn = dn_cpy(str2dn(objectname + 1)) ;
		}
		else
		{
			/*oj_dn = dn_cpy(dn) ;
			 *dn_append(oj_dn, dn_cpy(str2dn(objectname))) ;
			 */
			save_dn = str2dn_aux(objectname,&alias) ;
			if (save_dn != NULLDN)
			{
				if (alias)
				{
					oj_dn = dn_cpy(save_dn);
				} 
				else
				{
					if (dn == NULLDN)
					{
						oj_dn = dn_cpy(save_dn) ;
					}
					else
					{
						oj_dn = dn_cpy(dn) ;
						dn_append (oj_dn,dn_cpy(save_dn));
					}
				}
			}
			dn_free(save_dn) ;
		}
	}

	if (get_ava (&compare_arg.cma_purported, "objectClass", "alias") != OK)
	{
		ps_print(OPT, "Oops, 'objectClass=alias' is a bad attribute!\n") ;
		ps_print(OPT, "This is very bad...\n") ;
		return ;
	}

	save_dn = dn_cpy(dn) ;
	dn = dn_cpy(oj_dn) ;
	compare_arg.cma_object = oj_dn;

	if (rebind () != OK)
		return ;

	/* Strong authentication */
	if (compare_arg.cma_common.ca_security != (struct security_parms *) 0)
	{
	struct signature *sign_operation();
	int encode_DAS_CompareArgumentData();

	compare_arg.cma_common.ca_sig =
		sign_operation((caddr_t)&compare_arg, encode_DAS_CompareArgumentData) ;
	}

	while (ds_compare (&compare_arg, &compare_error, &compare_result) != DS_OK)
	{
		if (dish_error (OPT, &compare_error) == 0)
		{
			return ;
		}
		compare_arg.cma_object = compare_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}

	if (compare_result.cmr_matched == FALSE)
	{
		ps_printf(OPT, "Sorry, object is not an alias. Aborting.\n") ;
		return ;
	}

	call_showentry(5, contact_showentry) ;

	if (current_entry == NULLENTRY)
	{
		(void)fprintf(stderr, "we have no current entry. No wonder!\n") ;
	}
	else
	{
		Attr_Sequence eptr ;
		AttributeType a_t = AttrT_new("aliasedObjectName") ;

		for (eptr = current_entry->e_attributes; eptr != NULLATTR; eptr = eptr->attr_link) 
		{
			if ( AttrT_cmp (eptr->attr_type, a_t) == 0 )
			{
				aliObjNameAVS = avs_cpy(eptr->attr_value);
			}
		}
	}
	if (aliObjNameAVS == NULLAV)
	{
		ps_print(OPT, "Can't find 'aliasedObjectName' attribute type.\n") ;
		ps_print(OPT, "Are you sure that this is an alias? Aborting.\n") ;
		return ;
	}

	/* Now we have the other end of the alias in AttrValue format,
	 * convert it into a DN, so we can modify it.
	 */

	if ((str_ps = ps_alloc(str_open)) == NULLPS)
	{
		ps_printf(OPT, "Ps alloc for your string failed.\n") ;
		return ;
	}
	if (str_setup (str_ps, str_buffer, 998, 1) == NOTOK)
	{
		ps_printf (OPT, "str_setup: %s", ps_error (str_ps -> ps_errno));
		ps_free (str_ps);
		return ;
	}

	avs_print(str_ps, aliObjNameAVS, EDBOUT) ;
	*str_ps->ps_ptr = 0 ;
	ps_free (str_ps) ;
	{
		char	*ptr_local = str_buffer ;
		while(*ptr_local !=0 && *ptr_local != '\n')
			ptr_local++ ;
		*ptr_local = '\0' ;
	}
	aoj_dn = str2dn(str_buffer) ;

	/* We now have converted a string to DN and we now have to form the
	 * attribute "seeAlso=<DN>", put it into the modify attributes
	 * and delete that attribute from the other end of the alias.
	 */

	if (service_control(OPT, 1, contact_modify, &mod_arg.mea_common) == -1)
	{
		ps_printf(OPT, "Del_alias: Badly wrong. Service controls for modify in error...\n") ;
		return ;
	}

	dn_free(dn) ;
	dn = dn_cpy(aoj_dn) ;
	contact_showentry[1] = ( char *) malloc ((unsigned)strlen("-noshow") + 1) ;
	(void)strcpy(contact_showentry[1], "-noshow") ;
	contact_showentry[2] = ( char *) malloc ((unsigned)strlen("-all") + 1) ;
	(void)strcpy(contact_showentry[2], "-all") ;
	contact_showentry[3] = ( char *) malloc ((unsigned)strlen("-nokey") + 1) ;
	(void)strcpy(contact_showentry[3], "-nokey") ;
	contact_showentry[4] = ( char *) malloc ((unsigned)strlen("-dontdereferencealias") + 1) ;
	(void)strcpy(contact_showentry[4], "-dontdereferencealias") ;

	call_showentry(5, contact_showentry) ;
	if (current_entry == NULLENTRY)
	{
		(void)fprintf(stderr, "we have no current entry. No wonder!\n") ;
	}
	else
	{
		Attr_Sequence eptr ;
		AttributeType a_t = AttrT_new("seeAlso") ;

		emnew->em_type = -1 ;
		for (eptr = current_entry->e_attributes; eptr != NULLATTR; eptr = eptr->attr_link) 
		{
			if ( AttrT_cmp (eptr->attr_type, a_t) == 0 )
			{
				if (emnew->em_type == -1)
				{
					emnew->em_type = EM_REMOVEATTRIBUTE ;
				}
				else
				{
					emnew->em_type = EM_REMOVEVALUES ;
				}
			}
		}
		if (emnew->em_type == -1)
		{
			ps_print(OPT, "INVALID set of entries for the alias object\n") ;
			ps_print(OPT, "Aborting...\n") ;
			return ;
		}
	}

	{
		AV_Sequence	new_avs = avs_comp_alloc() ;
		AttributeValue	new_AV = AttrV_alloc() ;

		str_buffer = (char *) malloc ((unsigned)1000) ;
		if ((str_ps = ps_alloc(str_open)) == NULLPS)
		{
			ps_printf(OPT, "Ps alloc for your string failed.\n") ;
			return ;
		}
		if (str_setup (str_ps, str_buffer, 998, 1) == NOTOK)
		{
			ps_printf (OPT, "str_setup: %s", ps_error (str_ps -> ps_errno));
			ps_free (str_ps);
			return ;
		}

		dn_print(str_ps, oj_dn, EDBOUT) ;
		*str_ps->ps_ptr = 0 ;
		ps_free(str_ps) ;

		new_AV = AttrV_cpy(str2AttrV(str_buffer, str2syntax("DN"))) ;
		new_avs = avs_comp_new(AttrV_cpy(new_AV)) ;
		emnew->em_what = as_comp_new(AttrT_new("seeAlso"), new_avs, NULLACL_INFO) ;
	}
	emnew->em_next = NULLMOD ;
	mod_arg.mea_object = aoj_dn;
	mod_arg.mea_changes = emnew ;

	if (rebind () != OK)
		return ;
			
/*
 * If this operation is time-stamped, it may have expired while the user
 * was editing the entry. Re-calculate the time-stamp. Modify is the only
 * dish command where this needs to be done.
 */

	if ((mod_arg.mea_common.ca_security != (struct security_parms *) 0)
		&& (mod_arg.mea_common.ca_security->sp_time != NULLCP))
	{
		char *new_version();

		free(mod_arg.mea_common.ca_security->sp_time);
		mod_arg.mea_common.ca_security->sp_time = new_version();
	}

/* If security parameters are present, take this to mean that strong
 * authentication is required. This disallows 'parms + no signature'
 * (pointless) and 'signature + no parms' (security risk).
 */
	if (mod_arg.mea_common.ca_security != (struct security_parms *) 0)
	{
		int encode_DAS_ModifyEntryArgumentData();
		struct signature *sign_operation();
		mod_arg.mea_common.ca_sig = 
			sign_operation((caddr_t)&mod_arg, 
				encode_DAS_ModifyEntryArgumentData);
	}

	while (ds_modifyentry (&mod_arg, &mod_error) != DS_OK)
	{
		if (dish_error (OPT, &mod_error) == 0)
		{
			ps_print(OPT, "Unable to modify ") ;
			dn_print(OPT, aoj_dn, EDBOUT) ;
			ps_print(OPT, "\nContinuing to delete alias...") ;
			mod_failed = 1 ;
		}
		mod_arg.mea_object = mod_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}
	if (!mod_failed)
	{
		ps_print (RPS, "Modified ");
		dn_print (RPS, aoj_dn, EDBOUT);
		ps_print (RPS, "\n");
		delete_cache (aoj_dn);	/* re-cache when next read */
	}

	dn_free(dn) ;
	dn = dn_cpy(save_dn) ;

	if (move (objectname) == OK)
		argc--;

	remove_arg.rma_object = dn;

	if (rebind () != OK)
		return ;

	while (ds_removeentry (&remove_arg, &error) != DS_OK) 
	{
		if (dish_error (OPT, &error) == 0)
			return ;
		remove_arg.rma_object = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	} 
		
	ps_print (RPS, "Removed ");
	dn_print (RPS, dn, EDBOUT);
	delete_cache (dn);
	for (dnptr = dn; dnptr->dn_parent != NULLDN; dnptr = dnptr->dn_parent)
		trail = dnptr;

	if (trail != NULLDN) 
		trail->dn_parent = NULLDN;
	else
		dn = NULLDN;

	dn_comp_free (dnptr);
	ps_print (RPS, "\n");
}
