/* search.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/search.c,v 7.9 91/02/22 09:40:51 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/search.c,v 7.9 91/02/22 09:40:51 mrose Interim $
 *
 *
 * $Log:	search.c,v $
 * Revision 7.9  91/02/22  09:40:51  mrose
 * Interim 6.8
 * 
 * Revision 7.8  91/02/19  09:21:51  mrose
 * ufn
 * 
 * Revision 7.7  90/10/17  11:55:38  mrose
 * sync
 * 
 * Revision 7.6  90/07/09  14:47:23  mrose
 * sync
 * 
 * Revision 7.5  90/03/15  11:18:31  mrose
 * quipu-sync
 * 
 * Revision 7.4  90/01/11  18:37:44  mrose
 * real-sync
 * 
 * Revision 7.3  89/11/26  14:43:00  mrose
 * sync
 * 
 * Revision 7.2  89/11/26  14:27:15  mrose
 * sync
 * 
 * Revision 7.1  89/11/26  14:25:45  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:20  mrose
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
#include "quipu/ds_search.h"
#include "quipu/list.h"
#include "quipu/entry.h"
#include "quipu/sequence.h"
#include "pepsy.h"
#include "quipu/DAS_pre_defs.h"

extern DN       dn,
		current_dn;

#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern Entry	current_entry;
extern char     flag_show;
DN		rel_dn = NULLDN;

extern char	fred_flag;
extern char	fred_expand;
extern char	fred_long;
extern char	fred_phone;
extern char	fred_sequence;
extern char	fred_subdisplay;

extern Attr_Sequence fred_as (), fred_full ();

Filter          get_filter ();
char           *TidyString ();
char		allow_move = TRUE;

int		csr_compar ();

call_search (argc, argv)
int             argc;
char          **argv;
{
	PS	aps;
	struct ds_search_arg search_arg;
	struct DSError  error;
	struct ds_search_result result;
	DN              save_dn;
	extern int      sizelimit;
	int             x;
	Entry		save_entry;
	char 		rel_flag = TRUE;
	char 		part_flag = TRUE;
	char	       *save_arg = NULLCP;
	extern	char     value_flag ;
	extern	char	all_flag;
	extern	char	key_flag;
	extern	char	name_flag;
	extern	char	doneget;
	extern  char * result_sequence;
	static	char    *nvec[2] = {"search"};
	extern	Attr_Sequence as_flag;
	int		seqno;
	Attr_Sequence   eptr;
	char 		hit_one = FALSE;
	extern	char	search_result;

	search_result = OK;

	value_flag = TRUE;
	all_flag = FALSE;
	name_flag = TRUE;
	if (as_flag != NULLATTR) {
		as_free (as_flag);
		as_flag = NULLATTR;
	}
	flag_show = FALSE;
	key_flag = TRUE;

	search_arg.sra_filter = NULLFILTER;
	search_arg.sra_subset = SRA_ONELEVEL;
	search_arg.sra_common.ca_servicecontrol.svc_sizelimit = sizelimit;
	search_arg.sra_searchaliases = FALSE;
	
 	if ((argc = service_control (OPT, argc, argv, &search_arg.sra_common)) == -1)
		return;

	allow_move = FALSE;
	if ( (argc = set_read_flags (argc,argv)) == -1) {
		allow_move = TRUE;
		return;
	}
	allow_move = TRUE;
	fred_flag = FALSE;
	fred_expand = FALSE;
	fred_long = 2;
	fred_phone = FALSE;
	fred_sequence = TRUE;
	fred_subdisplay = FALSE;

	for (x = 1; x < argc; x++) {
		if (test_arg (argv[x], "-baseobject",1))
			search_arg.sra_subset = SRA_BASEOBJECT;
		else if (test_arg (argv[x], "-singlelevel",2))
			search_arg.sra_subset = SRA_ONELEVEL;
		else if (test_arg (argv[x], "-subtree",2))
			search_arg.sra_subset = SRA_WHOLESUBTREE;
		else if (test_arg (argv[x], "-relative",3)) 
			rel_flag = TRUE;
		else if (test_arg (argv[x], "-norelative",5)) 
			rel_flag = FALSE;
		else if (test_arg (argv[x], "-partial",2))
			part_flag = TRUE;
		else if (test_arg (argv[x], "-nopartial",4))
			part_flag = FALSE;
		else if (test_arg (argv[x], "-hitone",3))
			hit_one = TRUE;
		else if (test_arg (argv[x], "-searchaliases",3)) 
			search_arg.sra_searchaliases = TRUE;
		else if (test_arg (argv[x], "-nosearchaliases",5)) 
			search_arg.sra_searchaliases = FALSE;
		else if (test_arg (argv[x], "-filter",1)) {
			if (x+1 == argc) {
				ps_printf (OPT,"Filter missing\n");
				Usage (argv[0]);
				return;
			}
			if ((search_arg.sra_filter = get_filter (argv[++x])) == NULLFILTER) {
				ps_printf (OPT,"Invalid filter %s\n",argv[x]);
				Usage (argv[0]);
				return;
			}
			shuffle_up (argc--,argv,x--);
		} else if (test_arg (argv[x], "-object",1)) {
			if (move (argv[++x]) != OK) {
				ps_printf (OPT,"Invalid move object %s\n",argv[x]);
				Usage (argv[0]);
				return;
			}
			shuffle_up (argc--,argv,x--);
		} else if (*argv[x] != '-') {
			if (save_arg != NULLCP) {
				ps_printf (OPT,"Need flags to parse argument '%s'!\n",argv[x]);
				Usage (argv[0]);
				return;
			} else 
				save_arg = argv[x];
		}
		else if (test_arg (argv[x], "-fred",4))
			fred_flag = TRUE;
		else if (test_arg (argv[x], "-expand",4))
			fred_expand = TRUE;
		else if (test_arg (argv[x], "-full",4))
			fred_long = TRUE;
		else if (test_arg (argv[x], "-summary",7))
			fred_long = FALSE;
		else if (test_arg (argv[x], "-phone",5))
			fred_phone = TRUE;
		else if (test_arg (argv[x], "-nofredseq",9))
			fred_sequence = FALSE;
		else if (test_arg (argv[x], "-subdisplay",10))
			fred_subdisplay = TRUE;
		else
			continue;  /* a read type flag !!! */
			
		shuffle_up (argc--,argv,x--);
	}

	if (fred_flag)
	    as_flag = as_cpy (fred_long || fred_expand ? fred_full ()
						       : fred_as ());

	if (flag_show && (as_flag == NULLATTR))
		all_flag = TRUE;

	if (save_arg != NULLCP) {
		/* There is an unflagged argument */
		if (search_arg.sra_filter == NULLFILTER) {
			if ((search_arg.sra_filter = get_filter (save_arg)) == NULLFILTER) {
				ps_printf (OPT,"Invalid filter %s\n",save_arg);
				Usage (argv[0]);
				return;
			}
		} else if (move (save_arg) != OK) {
			ps_printf (OPT,"Invalid move object %s\n",save_arg);
			Usage (argv[0]);
			return;
		}
	}
	
	if (search_arg.sra_filter == NULLFILTER) {
		/* set default */
		search_arg.sra_filter = filter_alloc ();
		search_arg.sra_filter->flt_next = NULLFILTER;
		search_arg.sra_filter->flt_type = FILTER_AND;
		search_arg.sra_filter->FUFILT = NULLFILTER;
	}

	if (argc != 1) {
		Usage (argv[0]);
		return;
	}

	if (fred_flag
	        && (save_entry = local_find_entry (dn, FALSE))
	        && save_entry -> e_alias)
	    dn = dn_cpy (save_entry -> e_alias);
	search_arg.sra_eis.eis_infotypes = value_flag;
	search_arg.sra_eis.eis_allattributes = all_flag;
	search_arg.sra_eis.eis_select = as_flag;
	search_arg.sra_baseobject = dn;

	if (rebind () != OK)
		return;

	/* Strong authentication */
	if (search_arg.sra_common.ca_security != (struct security_parms *) 0)
	{
	struct signature *sign_operation();

	search_arg.sra_common.ca_sig =
		sign_operation((caddr_t)&search_arg, _ZSearchArgumentDataDAS, &_ZDAS_mod);
	}

	while (ds_search (&search_arg, &error, &result) != DS_OK) {
		if (dish_error (OPT, &error) == 0)
			return;
		search_arg.sra_baseobject = error.ERR_REFERRAL.DSE_ref_candidates->cr_name;
	}

	correlate_search_results (&result);
			
	if (result_sequence)
		set_sequence (result_sequence);

	if (result.CSR_entries == NULLENTRYINFO) 
		ps_printf (aps = OPT, "Search failed to find anything.\n");
	else {
		EntryInfo      *ptr;

		ptr = result.CSR_entries;
		if (hit_one && result.CSR_entries->ent_next != NULLENTRYINFO) {
#ifndef	SOCKETS
			if (frompipe)			
				search_result = NOTOK;	
#else
			if (frompipe
			    	&& rps -> ps_byteno == 0
			    	&& opt -> ps_byteno == 0
			        && fdx_reset (opt) == OK)   /* MAJOR HACK */
			    (void) (*opt -> ps_writeP) (opt, "3", 1, 0);
#endif
			ps_printf (OPT,"Multiple hits...\n");
		}

		aps = RPS;
		save_dn = dn_cpy(current_dn);
		save_entry = current_entry;
		doneget = TRUE;
		if (rel_flag)
			rel_dn = dn_cpy(dn);

		if (fred_flag) {
		    int	    i,
			    nchild = 0;

		    i = 0;
		    for (ptr = result.CSR_entries;
			     ptr;
			     ptr = ptr -> ent_next) {
			cache_entry (ptr, all_flag, value_flag);

			i++;
		    }

		    if (fred_long == 2)
			if ((fred_subdisplay && fred_expand)
				|| (!fred_subdisplay && !fred_expand))
			    fred_long = i == 1;
			else
			    fred_long = fred_expand;

		    if (i > 1) {
			EntryInfo **base,
				  **bp,
				  **ep;

			ps_printf (RPS, "%d matches found.\n", i);
			(void) ps_flush (RPS);

			if (base = (EntryInfo **) malloc ((unsigned)
							  (i * sizeof *base))){
			    ep = base;
			    for (ptr = result.CSR_entries;
				     ptr;
				     ptr = ptr -> ent_next)
				*ep++ = ptr;

			    qsort ((char *) base, i, sizeof *base, csr_compar);

			    bp = base;
			    ptr = result.CSR_entries = *bp++;
			    while (bp < ep) {
				ptr -> ent_next = *bp;
				ptr = *bp++;
			    }
			    ptr -> ent_next = NULL;

			    free ((char *) base);
			}
		    }

		    if (fred_expand)
			fred_long = fred_subdisplay = TRUE;
		    for (ptr = result.CSR_entries;
			     ptr;
			     ptr = ptr -> ent_next)
			(void) add_sequence (ptr -> ent_dn);
		    set_sequence ("default");
		    for (i = 0, ptr = result.CSR_entries;
			     ptr;
			     ptr = ptr -> ent_next, i++) {
			if (i > 0) {
			    if (fred_expand)
				ps_print (RPS, "-------\n");
			    else
				if (nchild)
				    ps_print (RPS, "\n");
			    (void) ps_flush (RPS);
			}

			nchild = showfred (ptr -> ent_dn, fred_long,
					   fred_subdisplay);
		    }
		} 
		else
		for (ptr = result.CSR_entries; ptr != NULLENTRYINFO; ptr = ptr->ent_next) {
				/* decode it immediately so we only
				   have to do it once. */
			cache_entry (ptr, all_flag, value_flag);
			seqno = add_sequence (ptr->ent_dn);
			if (seqno != 0)
				ps_printf (RPS,"%-3d ",seqno);
			nvec[1] = "-compact";

			if (name_flag)
				call_showname (2, nvec);
			else if (seqno != 0)
				ps_print (RPS,"\n");

			if (flag_show) {
				eptr = ptr->ent_attr;
				for (; eptr != NULLATTR; eptr = eptr->attr_link)
					showattribute (eptr->attr_type);
			} 
		}
		if (rel_dn != NULLDN) {
			dn_free (rel_dn);
			rel_dn = NULLDN;
		}
		dn_free (current_dn);
		current_dn = save_dn;
		current_entry = save_entry;
		entryinfo_free (result.CSR_entries,0);
	}

	handle_problems (aps,result.CSR_cr,result.CSR_limitproblem,part_flag);
	
	dn_free (result.CSR_object);
	crefs_free (result.CSR_cr);
	filter_free (search_arg.sra_filter);
}

static	int  csr_compar (a, b)
EntryInfo **a,
          **b;
{
    int	    i;
    DN	    adn,
	    bdn;
    Entry   ae,
	    be;
    static AttributeType at_surName = NULL;

    if ((ae = local_find_entry ((*a) -> ent_dn, FALSE))
	    && (be = local_find_entry ((*b) -> ent_dn, FALSE))) {
	Attr_Sequence as,
		      bs;

	if (!at_surName && !(at_surName = AttrT_new ("surName")))
	    goto check_rdn;

	for (as = ae -> e_attributes; as; as = as -> attr_link)
	    if (AttrT_cmp (as -> attr_type, at_surName) == 0)
		break;
	if (!as)
	    goto check_rdn;

	for (bs = be -> e_attributes; bs; bs = bs -> attr_link)
	    if (AttrT_cmp (bs -> attr_type, at_surName) == 0)
		break;
	if (!bs)
	    goto check_rdn;

	i = AttrV_cmp (&as -> attr_value -> avseq_av,
		       &bs -> attr_value -> avseq_av);
    }
    else {
check_rdn: ;

	for (adn = (*a) -> ent_dn; adn -> dn_parent; adn = adn -> dn_parent)
	    continue;
	for (bdn = (*b) -> ent_dn; bdn -> dn_parent; bdn = bdn -> dn_parent)
	    continue;

	i = rdn_cmp (adn -> dn_rdn, bdn -> dn_rdn);
    }

    return (i == (-1) || i == 1 ? i : 0);
}

handle_problems (aps,cr,limit,proceed)
PS aps;
ContinuationRef cr;
int limit;
{
	if (! proceed)
		return;

	if (limit != LSR_NOLIMITPROBLEM) {
		ps_print (aps, "(");
		switch (limit) {
		case LSR_TIMELIMITEXCEEDED:
			ps_print (aps, (flag_show
			      ? "Time limit exceeded"
			      : "Partial results only--time limit exceeded"));
			break;
		case LSR_SIZELIMITEXCEEDED:
			ps_print (aps, (flag_show
			      ? "Size limit exceeded"
			      : "Partial results only--size limit exceeded"));
			break;
		default: /* admin limit */
			ps_print (aps, (flag_show
			      ? "Admin limit exceeded"
			      : "Partial results only--admin limit exceeded"));
			break;
		}
		ps_print (aps, ")\n");
		if (! flag_show)
			return;
	}

	if (cr != NULLCONTINUATIONREF) {
		ContinuationRef crptr;
		if (!flag_show) {
			ps_print (aps,"(Partial results only--not all DSAs could be reached)\n");
			return;
		}
		ps_print (aps, "NOTE partial results only:- could not contact following DSA(s):-\n");
		for (crptr=cr; crptr != NULLCONTINUATIONREF; crptr=crptr->cr_next) {
			ps_print (aps,"   ");
			dn_print (aps,crptr->cr_accesspoints->ap_name,EDBOUT);
			ps_print (aps," (holding ");
			dn_print (aps,crptr->cr_name,EDBOUT);
			ps_print (aps,")\n");
		}
	}
			
}
