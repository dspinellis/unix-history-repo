/* read.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/read.c,v 7.2 91/02/22 09:40:50 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/read.c,v 7.2 91/02/22 09:40:50 mrose Interim $
 *
 *
 * $Log:	read.c,v $
 * Revision 7.2  91/02/22  09:40:50  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:55:36  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:18  mrose
 * Release 6.0
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
#include "quipu/read.h"
#include "quipu/entry.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern DN       dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern DN       current_dn;
extern Entry    current_entry;
char            value_flag;
char		all_flag;
char		name_flag;
char		show_all_flag;
Attr_Sequence   as_flag = NULLATTR;
Attr_Sequence   tmp_ignore = NULLATTR;
char            flag_show;
char 		key_flag;
char		print_format;

read_cache (argc, argv)
int             argc;
char          **argv;
{
extern char	doneget;

	if (doneget)
		return (argc);

	return (read_cache_aux (argc,argv,TRUE, (CommonArgs *) 0));
}

read_cache_aux (argc, argv, ali, ca)
int             argc;
char          **argv;
char 		ali;
CommonArgs     *ca;
{
	Entry           read_entry;
	int             x = 1;
	char            noread_flag = FALSE;
	char            do_read = FALSE;
	Attr_Sequence 	as;
	extern int	copy_flag;
	int		deref = FALSE;
	struct ds_read_arg read_arg;

	value_flag = TRUE;
	all_flag = TRUE;
	show_all_flag = FALSE;
	if (as_flag != NULLATTR) {
		as_free (as_flag);
		as_flag = NULLATTR;
	}
	flag_show = TRUE;
	key_flag = TRUE;

	if (ca)
	    read_arg.rda_common = *ca;	/* struct copy */
	else
	    if ((argc = service_control (OPT, argc, argv, &read_arg.rda_common)) == -1)
		return (-1);

	if ( (argc = set_read_flags (argc,argv)) == -1)
		return (-1);

	read_arg.rda_eis.eis_infotypes = value_flag;
	read_arg.rda_eis.eis_allattributes = all_flag;	
	read_arg.rda_eis.eis_select = as_flag;

	if (!copy_flag)
		do_read = TRUE;

	for (x=1; x< argc; x++) {
		if (test_arg (argv[x], "-nocache",4))
			do_read = TRUE;
		else if (test_arg (argv[x], "-cache",2)) 
			noread_flag = TRUE;
		else
			continue;
		shuffle_up (argc--,argv,x--);
	}

	if ( ! ali ) 
		read_arg.rda_common.ca_servicecontrol.svc_options |= SVC_OPT_DONTDEREFERENCEALIAS;

        else if ((read_arg.rda_common.ca_servicecontrol.svc_options & SVC_OPT_DONTDEREFERENCEALIAS) == 0)
                deref = TRUE;

	if ((read_entry = local_find_entry (dn, deref)) != NULLENTRY) {

		for (as = as_flag; as!= NULLATTR; as = as->attr_link)
			if (as_find_type (read_entry->e_attributes, as->attr_type) == NULL) 
				do_read = TRUE;
		
		if (value_flag && (!read_entry->e_lock))
			do_read = TRUE;

		if ((read_arg.rda_eis.eis_allattributes == 1) && (!read_entry->e_complete))
			do_read = TRUE;

		current_entry = read_entry;
		dn_free (current_dn);
		current_dn = get_copy_dn (read_entry);
	} else 
		do_read = TRUE;

	if (do_read)
		if (noread_flag)
			if (read_entry == NULLENTRY) {
				ps_print (OPT, "No data in cache, but '-cache' prevents me reading it!\n");
				return (-1);
			} else
				ps_print (OPT, "Read required, but '-cache' specified,\nproceeding using cache...\n");
		else {
			struct DSError  error;
			struct ds_read_result result;
				
			read_arg.rda_object = dn;
				
			if (rebind () != OK)
				return(-2);
				
			/* Strong authentication */
			if (read_arg.rda_common.ca_security != 
				(struct security_parms *) 0)
			{
			struct signature *sign_operation();

			read_arg.rda_common.ca_sig =
				sign_operation((caddr_t)&read_arg,
					_ZReadArgumentDataDAS, &_ZDAS_mod);
			}

			while (ds_read (&read_arg, &error, &result) != DS_OK) {
				if (dish_error (OPT, &error) == 0)
					return (-2);
				read_arg.rda_object = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
			}
				
			if (result.rdr_entry.ent_attr == NULLATTR) {
				ps_print (OPT, "No attributes\n");
				return (-2);
			}
				
			if (result.rdr_common.cr_aliasdereferenced) {
				ps_print (RPS, "(Alias dereferenced)\n");
			}
				
			cache_entry (&(result.rdr_entry), read_arg.rda_eis.eis_allattributes, value_flag);
				
			entryinfo_comp_free (&result.rdr_entry,0);
				
			return (argc);
		}

	return (argc);
}


set_read_flags (argc,argv)
int argc;
char ** argv;
{
register int x;
AttributeType at;
extern char allow_move;

	print_format = READOUT;
	tmp_ignore = NULLATTR;

	for (x = 1; x < argc; x++) {
		if (test_arg (argv[x], "-all",1)) {
			show_all_flag = TRUE;
			all_flag = TRUE;
		} else if (test_arg (argv[x], "-noall",3)) {
			show_all_flag = FALSE;
			all_flag = FALSE;
		} else if (test_arg (argv[x], "-value",1))
			value_flag = EIS_ATTRIBUTESANDVALUES;
		else if (test_arg (argv[x], "-novalue",3))
			value_flag = EIS_ATTRIBUTETYPESONLY;
		else if (test_arg (argv[x], "-show",2)) 
			flag_show = TRUE;
		else if (test_arg (argv[x], "-noshow",4))
			flag_show = FALSE;
		else if (test_arg(argv[x],"-noname",3)) 
			name_flag = FALSE;
		else if (test_arg (argv[x],"-name",2))
			name_flag = TRUE;
		else if (test_arg (argv[x], "-key",1)) 
			key_flag = TRUE;
		else if (test_arg (argv[x], "-nokey",3))
			key_flag = FALSE;
		else if (test_arg (argv[x], "-edb",3))
			print_format = EDBOUT;
		else if (test_arg (argv[x], "-sequence",3)) {
			if (x + 1 == argc) {	
				ps_printf (OPT, "We need a sequence name.\n");
				return (-1);
			} else {
				shuffle_up (argc--, argv, x);
				set_sequence (argv[x]);
			}
		} else if (test_arg (argv[x],"-types",2)) {
			shuffle_up (argc--, argv, x);
			if (x >= argc) {
				ps_printf (OPT,"-types argument missing\n");
				return (-1);
			}
			if ((at = AttrT_new (argv[x])) != NULLAttrT) {
				show_all_flag = TRUE;
				all_flag = FALSE;
				as_flag = as_merge (as_flag,as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
			} else {
				ps_printf (OPT,"Unknown attribute type %s\n",argv[x]);
				return (-1);
			}
			for (x++; x < argc;) {
				if (*argv[x] == '-') 
					break;
				if ((at = AttrT_new (argv[x])) != NULLAttrT) 
					as_flag = as_merge (as_flag,as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
				else
					break;
				shuffle_up (argc--,argv,x);
			}
			x--;
		} else if (test_arg (argv[x],"-notypes",4)) {
			shuffle_up (argc--, argv, x);
			if (x >= argc) {
				ps_printf (OPT,"-notypes argument missing\n");
				return (-1);
			}
			if ((at = AttrT_new (argv[x])) != NULLAttrT) {
				tmp_ignore = as_merge (tmp_ignore,as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
			} else {
				ps_printf (OPT,"Unknown attribute type %s\n",argv[x]);
				return (-1);
			}
			for (x++; x < argc;) {
				if (*argv[x] == '-') 
					break;
				if ((at = AttrT_new (argv[x])) != NULLAttrT) 
					tmp_ignore = as_merge (tmp_ignore,as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
				else
					break;
				shuffle_up (argc--,argv,x);
			}
			x--;
		} else if (test_arg (argv[x],"-proc",3)) {
			short sntx;
			shuffle_up (argc--, argv, x);
			if (x >= argc) {
				ps_printf (OPT,"<syntax> missing\n");
				return (-1);
			}
			if ((sntx = str2syntax (argv[x])) == 0) {
				if (lexequ (argv[x],"ASN") != 0) {
					ps_printf (OPT,"Unknown syntax %s\n",argv[x]);
					return (-1);
				}
			}
			shuffle_up (argc--, argv, x);
			set_av_pe_print (sntx,argv[x]);
		} else if (allow_move) {
			if (move (argv[x]) != OK)
				continue;
		} else 
			continue;

		shuffle_up (argc--,argv,x--);
	}
	return (argc);
}
