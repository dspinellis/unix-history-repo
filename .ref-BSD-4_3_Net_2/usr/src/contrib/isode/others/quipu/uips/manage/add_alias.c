/* add_alias.c -  a mutilated add.c*/

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/manage/RCS/add_alias.c,v 7.3 91/02/22 09:32:00 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/uips/manage/RCS/add_alias.c,v 7.3 91/02/22 09:32:00 mrose Interim $
 *
 *
 * $Log:	add_alias.c,v $
 * Revision 7.3  91/02/22  09:32:00  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/24  14:43:12  mrose
 * update
 * 
 * Revision 7.1  90/07/27  08:47:16  mrose
 * update
 * 
 * Revision 7.0  90/06/26  14:52:31  mrose
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
#include "quipu/dua.h"
#include "quipu/add.h"
#include "quipu/entry.h"
#include "quipu/compare.h"
#include "quipu/modify.h"

#define ORG_PERSON "thornPerson & quipuObject"
	/* this should probably go elsewhere !!! */

extern	DN	dn;
#define OPT     (!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define RPS     (!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt;
extern	PS	rps;
extern	Entry	current_entry;
static	char	new_draft;

call_add_alias (argc, argv)
int             argc;
char          **argv;
{
	DN		oj_dn, aoj_dn ;
	DN		save_dn, dnptr, trail ;
	DN		moddn;
extern	DN		str2dn_aux() ;
extern	DN		sequence_dn() ;
	Entry		entry_ptr;
	FILE           *fd;
	PS		tmp ;
	PS		str_ps;
	char		str_buffer[1000];
	char		fname[128];
	char		alias = FALSE ;

	struct		ds_addentry_arg		add_arg;
	struct		DSError			error;
	struct		DSError			compare_error;
	struct		ds_compare_result	compare_result;
	struct		ds_compare_arg		compare_arg;
	struct		ds_modifyentry_arg	mod_arg;
	struct		DSError			mod_error;
	struct		entrymod	       *emnew ;

	AV_Sequence	objClassAVS ;
	AV_Sequence	treeStrAVS = NULLAV;
	Attr_Sequence   get_attributes();

	extern int	parse_status;

	int		draft_flag = 0;
	char	       *home;
	char		objectname[80] ;
	char		aliasobjectname[160] ;
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
	if (argc < 3)
	{
		ps_printf(OPT, "Not enough arguments.\n") ;
		Usage (argv[0]) ;
		return ;
	}
	contact_showentry[5] = (char *) malloc ((unsigned)strlen(argv[2])+1) ;
	(void)strcpy(contact_showentry[5], argv[2]) ;

	contact_compare[3] = (char *) malloc ((unsigned)strlen("objectClass=alias.")) ;
	(void)strcpy(contact_compare[3], "objectClass=alias") ;

	if (service_control (OPT, 6, contact_compare, &compare_arg.cma_common) == -1)
	{
		ps_print(OPT, "Problems with compare service control flags.\n") ;
		return ;
	}

	if (home = getenv ("HOME"))
	    (void) sprintf (fname, "%s/.dishdraft", home);
	else
	    (void) strcpy (fname, "./.dishdraft");
	new_draft = FALSE;

	if ((argc = service_control (OPT, argc, argv, &add_arg.ada_common)) == -1)
		return ;

	(void)strcpy(objectname, argv[1]) ;
	(void)strcpy(aliasobjectname, argv[2]) ;

	/* Turn a sequence number back into a DN */
	if (*aliasobjectname >= '0' && *aliasobjectname <= '9')
	{
		/* First convert the number into a dn */
		aoj_dn = dn_cpy(sequence_dn(atoi(aliasobjectname))) ;
	}
	else
	{
		if (*aliasobjectname == '.')
		{
			ps_print(OPT, "..@ gives me a headache. Ambiguous. Abort... \n") ;
			return ;
		}

		if (*aliasobjectname == '@')
		{	
			aoj_dn = dn_cpy(str2dn(aliasobjectname + 1)) ;
		}
		else
		{
			/*aoj_dn = dn_cpy(dn) ;
			 *dn_append(aoj_dn, dn_cpy(str2dn(aliasobjectname))) ;
			 */
			save_dn = str2dn_aux(aliasobjectname,&alias) ;
			if (save_dn != NULLDN)
			{
				if (alias)
				{
					aoj_dn = dn_cpy(save_dn);
				} 
				else
				{
					if (dn == NULLDN)
					{
						aoj_dn = dn_cpy(save_dn) ;
					}
					else
					{
						aoj_dn = dn_cpy(dn) ;
						dn_append (aoj_dn,dn_cpy(save_dn));
					}
				}
			}
			dn_free(save_dn) ;
		}
 	}

	if (*objectname >= '0' && *objectname <= '9')
	{
		/* First convert the number into a dn */
		oj_dn = dn_cpy(sequence_dn(atoi(objectname))) ;
	}
	else
	{
		if (*objectname == '.')
		{
			ps_print(OPT, "..@ gives me a headache. Ambiguous. Abort... \n") ;
			return ;
		}

		if (*objectname == '@')
		{	
			oj_dn = dn_cpy(str2dn(objectname + 1)) ;
		}
		else
		{
			oj_dn = dn_cpy(dn) ;
			dn_append(oj_dn, dn_cpy(str2dn(objectname))) ;
		}
 	}

	save_dn = dn_cpy(dn) ;
	dn_free(dn) ;
	dn = dn_cpy(aoj_dn) ;
	ps_print(OPT, "Trying to move to ") ;
	dn_print(OPT, dn, EDBOUT) ;
	ps_print(OPT, ".\n") ;
	if (test_move_dn() != TRUE)
	{
		ps_print(OPT, "Can't move to ") ;
		dn_print(OPT, dn, EDBOUT) ;
		ps_print(OPT, ".\nAborting.\n") ;
		return ;
	}
	compare_arg.cma_object = aoj_dn;
	if (get_ava (&compare_arg.cma_purported, "objectClass", "alias") != OK)
	{
		ps_print(OPT, "Oops, 'objectClass=alias' is a bad attribute!\n") ;
		ps_print(OPT, "This is very bad...\n") ;
		return ;
	}

	if (rebind () != OK)
		return ;

	while (ds_compare (&compare_arg, &compare_error, &compare_result) != DS_OK) 
	{
		if (dish_error (OPT, &compare_error) == 0)
		{
			return ;
		}
		compare_arg.cma_object = compare_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}

	if ( compare_result.cmr_matched == 1 )	/* if <AOJ> is an alias, abort. */
	{
		ps_printf(OPT, "Sorry, %s is an alias.\nAliasing to aliases is illegal.\n", aliasobjectname) ;
		return ;
	}

	/* Now we want to discover the objectClass of our object, and make
	 * sure that this will be OK when we add in the alias. (ie fitting
	 * in with the treeStructure.) 
	 */

	/* stick the aliasobjectname into the cache so we can read 
	 * bits of information from it.
	 */

	call_showentry(6, contact_showentry) ;
	contact_showentry[5] = (char *) malloc ((unsigned)strlen(argv[2])+1) ;
	(void)strcpy(contact_showentry[5], argv[2]) ;

	if (current_entry == NULLENTRY)
	{
		ps_print(OPT, "Can't read ") ;
		dn_print(OPT, dn, EDBOUT) ;
		ps_print(OPT, " for objectClass attribute. Aborting.\n") ;
		return ;
	}
	else
	{
		/* Find the objectClass attribute to compare with the tree
		 * structure of the node we are going to dangle the alias
		 * from. 
		 * While we are at it, find out how many seeAlso attributes
		 * are present, so we can decide whether we need to add
		 * the entire attribute or just another value.
		 */

		Attr_Sequence eptr ;
		AttributeType a_t = AttrT_new("objectClass") ;
		AttributeType a_t2 = AttrT_new("seeAlso") ;

		emnew->em_type = EM_ADDATTRIBUTE ;
		for (eptr = current_entry->e_attributes; eptr != NULLATTR; eptr = eptr->attr_link) 
		{
			if ( AttrT_cmp (eptr->attr_type, a_t) == 0 )
			{
				objClassAVS = avs_cpy(eptr->attr_value);
			}
			if ( AttrT_cmp (eptr->attr_type, a_t2) == 0 )
			{
				emnew->em_type = EM_ADDVALUES ;
			}
		}
	}
	if (objClassAVS == NULLAV)
	{
		ps_print(OPT, "We can't find Object Class.... Aborting.\n") ;
		return ;
	}
	/* We should have got the ObjectClass now, so return to where we were
	 * and move up a level to grab the tree structure */

	dn_free(dn) ;
	dn = dn_cpy(oj_dn) ;

	for (dnptr = dn; dnptr->dn_parent != NULLDN; dnptr = dnptr->dn_parent)
		trail = dnptr;
	dn_comp_free (dnptr);
	trail->dn_parent = NULLDN;

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
		ps_print(OPT, "Can't read ") ;
		dn_print(OPT, dn, EDBOUT) ;
		ps_print(OPT, " for the treeStructure. Aborting.\n") ;
		return ;
	}
	else
	{
		Attr_Sequence eptr ;
		AttributeType a_t = AttrT_new("treeStructure") ;

		for (eptr = current_entry->e_attributes; eptr != NULLATTR; eptr = eptr->attr_link)
		{
			if ( AttrT_cmp (eptr->attr_type, a_t) == 0 )
			{
				treeStrAVS = avs_cpy (eptr->attr_value) ;
			}
		}
	}

	if (treeStrAVS == NULLAV)
	{
		ps_print(OPT, "Tree Structure Missing in ") ;
		dn_print(OPT, dn, EDBOUT) ;
		ps_print(OPT, ".\nAssuming that the add will be valid.\n") ;
	}
	else
	{
		if (test_schema(treeStrAVS, objClassAVS) != OK)
		{
			ps_print(OPT, "Not allowed to add this alias here.\n") ;
			ps_print(OPT, "It would break the directory schema to add this alias here.\n") ;
			return ;
		}
	}

	/* This is where we have to start being rather careful, previously
	 * we have just being reading and checking, but now we start to add
	 * things in, changing in several places. Mistakes => inconsistencies
	 * ie BAD...
	 */

	/* If we reach here, we should have the appropriate arguments,
	 * one is the new object,
	 * the other is an existing non-alias-entry object.
	 * We now write the information to a draft file, and 'whongo'
	 * the alias into the database.
	 */

	/* open the draft file for writing... */

	if ((fd = fopen (fname, "w")) == (FILE *) NULL) 
	{
		ps_printf (OPT, "Can't open draft entry %s\n", fname);
		return ;
	}
	
	(void)fprintf(fd, "aliasedObjectName= ") ;
	if ( ((tmp = ps_alloc (std_open)) != NULLPS) &&
	     (std_setup (tmp, fd) != NOTOK) )
	{
		dn_print(tmp, aoj_dn, EDBOUT) ;
	}
	else
	{
		ps_print(OPT, "Unable to open appropriate ps. Aborting..\n") ;
		return ;
	}
	(void)fprintf(fd, "\nobjectClass= quipuObject & alias & top\n") ;
	(void) fclose(fd) ;

	if (move (objectname) != OK)
	{
		ps_printf (OPT,"Unknown option %s\n",objectname);
		return ;
	}

	/* now parse the files */
	if ((fd = fopen (fname, "r")) == (FILE *) NULL) {
		ps_printf (OPT, "Can't open draft entry %s\n", fname);
		return ;
	}
	entry_ptr = get_default_entry (NULLENTRY);
	entry_ptr->e_attributes = get_attributes (fd);
	(void) fclose (fd);
	if (parse_status != 0)
		return ;
		
	add_arg.ada_object = dn;
	for (moddn = dn ; moddn->dn_parent != NULLDN; moddn=moddn->dn_parent)
		;
	entry_ptr->e_name = rdn_cpy (moddn->dn_rdn);
	add_arg.ada_entry = entry_ptr->e_attributes;

	if (rebind () != OK) {
		entry_free (entry_ptr);
		return ;
	}

	while (ds_addentry (&add_arg, &error) != DS_OK) {
		if (dish_error (OPT, &error) == 0) {
			entry_free (entry_ptr);
			return ;
		}
		add_arg.ada_object = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	} 
	ps_print (RPS, "Added ");
	dn_print (RPS, dn, EDBOUT);
	ps_print (RPS, "\n");
	entry_free (entry_ptr);
	make_old (fname,draft_flag);	

	/* Now we have to add a "seeAlso=<DN>" attribute to the <AOJ> */

	if (service_control(OPT, 1, contact_modify, &mod_arg.mea_common) == -1)
	{
		ps_printf(OPT, "Add_alias: Badly wrong. Service controls for modify in error...\n") ;
		return ;
	}

	dn_free(dn) ;
	dn = dn_cpy(save_dn) ;
	{
		AV_Sequence	new_avs = avs_comp_alloc() ;
		AttributeValue	new_AV = AttrV_alloc() ;
		
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
			ps_print(OPT, "\n") ;
			return ;
		}
		mod_arg.mea_object = mod_error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}
	ps_print (RPS, "Modified ");
	dn_print (RPS, aoj_dn, EDBOUT);
	ps_print (RPS, "\n");
	delete_cache (aoj_dn);	/* re-cache when next read */

	dn_free(dn) ;
	dn = dn_cpy(save_dn) ;
}
