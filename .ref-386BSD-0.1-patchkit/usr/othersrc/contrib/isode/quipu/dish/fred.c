/* fred.c - DiSH support for FrED */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/dish/RCS/fred.c,v 7.11 91/03/09 11:56:20 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/quipu/dish/RCS/fred.c,v 7.11 91/03/09 11:56:20 mrose Exp $
 *
 *
 * $Log:	fred.c,v $
 * Revision 7.11  91/03/09  11:56:20  mrose
 * update
 * 
 * Revision 7.10  91/02/22  09:40:34  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/02/19  09:21:39  mrose
 * ufn
 * 
 * Revision 7.8  90/10/17  11:55:22  mrose
 * sync
 * 
 * Revision 7.7  90/08/29  15:05:00  mrose
 * foo
 * 
 * Revision 7.6  90/07/09  14:47:11  mrose
 * sync
 * 
 * Revision 7.5  90/04/18  08:49:40  mrose
 * 6.2
 * 
 * Revision 7.4  90/03/15  11:18:27  mrose
 * quipu-sync
 * 
 * Revision 7.3  90/01/11  18:37:38  mrose
 * real-sync
 * 
 * Revision 7.2  89/12/19  16:21:03  mrose
 * sync
 * 
 * Revision 7.1  89/11/26  14:25:47  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:20:08  mrose
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


#include <ctype.h>
#include <stdio.h>
#include "quipu/ds_search.h"
#include "quipu/list.h"
#include "quipu/read.h"
#include "quipu/entry.h"
#include "quipu/ufn.h"
#include "tailor.h"
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#ifndef	X_OK
#define	X_OK	1
#endif
#include <sys/stat.h>


#define	OPT	(!frompipe || rps -> ps_byteno == 0 ? opt : rps)
#define	RPS	(!frompipe || opt -> ps_byteno == 0 ? rps : opt)
extern	char	frompipe;
extern	PS	opt, rps;

extern	char	dad_flag;
#ifndef	NO_STATS
extern	LLog   *dad_log;
#endif

char		fred_flag;
char		fred_expand;
char		fred_list;
char		fred_long;
char		fred_mailbox;
char		fred_phone;
char		fred_photo;
char		fred_sequence;
char		fred_subdisplay;


static short	s_dn;
static short	s_photo;

static AttributeType t_alias;
static AttributeType t_author;
static AttributeType t_authorCN;
static AttributeType t_authorSN;
static AttributeType t_domain;
static AttributeType t_master;
static AttributeType t_mbox;
static AttributeType t_modtime;
static AttributeType t_modwhom;
static AttributeType t_object;
static AttributeType t_othermbox;
static AttributeType t_phone;
static AttributeType t_photo;
static AttributeType t_postal;
static AttributeType t_slave;
static AttributeType t_surname;
static AttributeType t_title;


Entry	fredentry ();
Attr_Sequence fred_as (), fred_full ();

static struct dn_seq *interact ();

PE	grab_pe ();
sntx_table *get_syntax_table ();
struct dn_seq *dn_seq_push ();

/*    FRED BACK-END */

int	call_fred (argc, argv)
int	argc;
char  **argv;
{
    if (argc < 2)
	goto usage;

    fred_init ();

    if (test_arg (argv[1], "-display", 7)) {
	if (argc != 3)
	    goto usage;

	(void) setenv ("DISPLAY", argv[2]);
	if (s_photo)
	    set_av_pe_print (s_photo,
			     strdup (isodefile ("g3fax/Xphoto", 1)));
	return;
    }

    if (test_arg (argv[1], "-dm2dn", 5)) {
	if (argc < 3)
	    goto usage;

	do_dm_match (argc - 2, argv + 2);
	return;
    }

    if (test_arg (argv[1], "-expand", 5)) {
	if (argc < 3)
	    goto usage;

	do_expand (argc - 2, argv + 2);
	return;
    }

    if (test_arg (argv[1], "-ufnrc", 5)) {
	build_ufnrc (argc - 2, argv + 2);
	return;
    }

    if (test_arg (argv[1], "-ufn", 3)) {
	if (argc < 3)
	    goto usage;

	do_ufn_match (argc - 2, argv + 2);
	return;
    }

usage: ;
    Usage (argv[0]);
}

/*    DM2DN SUPPORT */

static	int	dlevel = 0;
static	int	dsa_status;

struct dn_seq *dm2dn_seq ();
struct dn_seq *dm2dn_seq_aux ();
Filter	make_filter ();

/*  */

static	do_dm_match (n, vec)
int	n;
char  **vec;
{
    int	    seqno;
    char   *cp,
	    mbox[BUFSIZ];
    register struct dn_seq *dlist,
			   *dp;

    if (n > 0 && strcmp (*vec, "-list") == 0) {
	n--, vec++;
	fred_list = dad_flag;
    }
    else
	fred_list = FALSE;
    if (n > 0 && strcmp (vec[0], "-phone") == 0) {
	n--, vec++;
	fred_phone = TRUE;
    }
    else
	fred_phone = FALSE;
    if (n > 0 && strcmp (vec[0], "-photo") == 0) {
	n--, vec++;
	fred_photo = dad_flag;
    }
    else
	fred_photo = FALSE;

    if (n != 1) {
	Usage ("fred");
	return;
    }

    if ((cp = index (vec[0], '@')) && cp != vec[0]) {
	(void) strcpy (mbox, vec[0]);
	*cp++ = NULL;
	if (*cp == NULL) {
	    ps_printf (OPT, "Must specify domain in mailbox specification.\n");
	    return;
	}
    }
    else {
	mbox[0] = NULL;
	cp = cp ? ++cp : vec[0];
    }

    if ((dlist = dm2dn_seq (cp)) == NULLDNSEQ) {
	if (dsa_status == OK)
	    ps_printf (OPT, "Unable to resolve domain.\n");
	return;
    }

    if (mbox[0] == NULL) {
	if (fred_list) {
	    done_match (dlist, NULLCP);
	    return;
	}

	for (dp = dlist; dp; dp = dp -> dns_next) {
	    if (seqno = add_sequence (dp -> dns_dn))
		ps_printf (RPS, "%-3d ", seqno);
	    dn_print (RPS, dp -> dns_dn, RDNOUT);
	    ps_printf (RPS, "\n");
	}

	dn_seq_free (dlist);
	return;
    }

    if ((dlist = interact (dlist, NULLDN, cp)) == NULL) {
	ps_printf (OPT, "Unable to resolve mailbox.\n");
	return;
    }

    {
	char	buffer[BUFSIZ];
	struct ds_search_arg search_arg;
	register struct ds_search_arg *sa = &search_arg;
	struct ds_search_result search_result;
	register struct ds_search_result *sr = &search_result;
	struct DSError error;
	register struct DSError *se = &error;
	Filter	fi;
	EntryInfo *ptr;
	register struct dn_seq *result = NULL;
	PS	nps;

	bzero ((char *) sa, sizeof *sa);

	sa -> sra_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
	sa -> sra_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
	sa -> sra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;

	sa -> sra_subset = SRA_WHOLESUBTREE;
	sa -> sra_searchaliases = FALSE;

	sa -> sra_eis.eis_allattributes = FALSE;
	sa -> sra_eis.eis_select = fred_as ();
	sa -> sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;

	if (t_mbox == NULL || t_othermbox == NULL)
	    fatal (-100, "rfc822Mailbox/otherMailbox: invalid attribute type");
	
	fi = make_filter (mbox, t_mbox);
	if (!index (mbox, '*')) {
	    (void) sprintf (buffer, "internet$%s", mbox);
	    fi -> flt_next = make_filter (buffer, t_othermbox);
	    sa -> sra_filter = filter_alloc ();
	    bzero ((char *) sa -> sra_filter, sizeof *sa -> sra_filter);
	    sa -> sra_filter -> flt_type = FILTER_OR;
	    sa -> sra_filter -> FUFILT = fi;
	}
	else
	    sa -> sra_filter = fi;

	if ((nps = ps_alloc (str_open))
	        && str_setup (nps, NULLCP, 0, 0) == NOTOK) {
	    ps_free (nps);
	    nps = NULLPS;
	}

	for (dp = dlist; dp; dp = dp -> dns_next) {
	    if (!dp -> dns_dn -> dn_parent) {
		ps_printf (nps, "Unable to resolve domain %s beyond ", cp);
		ufn_dn_print_aux (nps, dp -> dns_dn, NULLDN, 0);
		ps_print (nps, "\n");
		continue;
	    }

	    sa -> sra_baseobject = dp -> dns_dn;

	    if (rebind () != OK)
		break;

	    while (ds_search (sa, se, sr) != DS_OK) {
		if (dish_error (nps, se) == 0) {
		    dsa_status = NOTOK;
		    goto free_filter;
		}

		sa -> sra_baseobject =
		    	    se -> ERR_REFERRAL.DSE_ref_candidates -> cr_name;
	    }

	    if (sr -> srr_correlated != TRUE)
		correlate_search_results (sr);

	    for (ptr = sr -> CSR_entries; ptr; ptr = ptr -> ent_next) {
		cache_entry (ptr, sa -> sra_eis.eis_allattributes,
			     sa -> sra_eis.eis_infotypes);

		result = dn_seq_push (ptr -> ent_dn, result);
	    }

	    dn_free (sr -> CSR_object);
	    entryinfo_free (sr -> CSR_entries, 0);
	    crefs_free (sr -> CSR_cr);
	}
free_filter: ;
	filter_free (sa -> sra_filter);

	dn_seq_free (dlist);
	if (result == NULL && nps && nps -> ps_byteno) {
	    ps_print (nps, " ");
	    *--nps -> ps_ptr = NULL, nps -> ps_cnt++;

	    ps_print (OPT, nps -> ps_base);

	    nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;
	}
	else
	    done_match (result, NULLCP);
	if (nps)
	    ps_free (nps);
    }
}

/*  */

static struct dn_seq *dm2dn_seq (dm)
char   *dm;
{
    register char *dp;

    for (dp = dm; *dp; dp++)
	if (isupper (*dp))
	    *dp = tolower (*dp);

    dlevel = 0;
    dsa_status = OK;

    return dm2dn_seq_aux (dm, NULLDN, NULLDNSEQ);
}

/*  */

static struct dn_seq *dm2dn_seq_aux (dm, dn, dlist)
char   *dm;
DN	dn;
struct dn_seq *dlist;
{
    register char   *dp;
    struct ds_search_arg search_arg;
    register struct ds_search_arg *sa = &search_arg;
    struct ds_search_result search_result;
    register struct ds_search_result *sr = &search_result;
    struct DSError error;
    register struct DSError *se = &error;

    bzero ((char *) sa, sizeof *sa);

    sa -> sra_common.ca_servicecontrol.svc_options = SVC_OPT_PREFERCHAIN;
    sa -> sra_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
    sa -> sra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;

    sa -> sra_baseobject = dn;
    sa -> sra_subset = SRA_ONELEVEL;
    sa -> sra_searchaliases = FALSE;

    sa -> sra_eis.eis_allattributes = FALSE;
    sa -> sra_eis.eis_select = fred_as ();
    sa -> sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;

    dp = dm;
    for (;;) {
	int	    i;
	EntryInfo  *ptr;
	register filter *fi;

	if ((dsa_status = rebind ()) != OK)
	    return dlist;

	if ((i = strlen (dp)) < dlevel)
	    break;

	sa -> sra_filter = fi = filter_alloc ();

	bzero ((char *) fi, sizeof *fi);
	fi -> flt_type = FILTER_ITEM;
	fi -> FUITEM.fi_type = FILTERITEM_EQUALITY;
	if ((fi -> FUITEM.UNAVA.ava_type = t_domain) == NULL)
	    fatal (-100, "associatedDomain: invalid attribute type");
	fi -> FUITEM.UNAVA.ava_value = str2AttrV (dp, t_domain -> oa_syntax);

	while (ds_search (sa, se, sr) != DS_OK) {
	    if (dish_error (OPT, se) == 0) {
		dsa_status = NOTOK;
		goto free_filter;
	    }

	    sa -> sra_baseobject =
			    se -> ERR_REFERRAL.DSE_ref_candidates -> cr_name;
	}

	if (sr -> srr_correlated != TRUE)
	    correlate_search_results (sr);

	if (sr -> CSR_entries == NULLENTRYINFO) {
	    filter_free (sa -> sra_filter);
	    if (dp = index (dp, '.'))
		dp++;
	    if (dp == NULL)
		break;
	    continue;
	}

	for (ptr = sr -> CSR_entries; ptr; ptr = ptr -> ent_next)
	    cache_entry (ptr, sa -> sra_eis.eis_allattributes,
			 sa -> sra_eis.eis_infotypes);

	if (i > dlevel) {
	    dlevel = i;
	    if (dlist)
		dn_seq_free (dlist), dlist = NULLDNSEQ;
	}

	if (i == dlevel)
	    for (ptr = sr -> CSR_entries; ptr; ptr = ptr -> ent_next) {
		struct dn_seq *dprev = dlist;

		dlist = dm2dn_seq_aux (dm, ptr -> ent_dn, dlist);

		if (dprev == dlist)
		    dlist = dn_seq_push (ptr -> ent_dn, dlist);
		else
		    if (i < dlevel)
			break;
	    }

	dn_free (sr -> CSR_object);
	entryinfo_free (sr -> CSR_entries, 0);
	crefs_free (sr -> CSR_cr);
free_filter: ;
	filter_free (sa -> sra_filter);
	break;
    }

    return dlist;
}

/*  */

static	Filter	make_filter (cp, at)
char   *cp;
AttributeType at;
{
    char   *dp,
	    buffer[BUFSIZ];
    AttributeValue av;
    AV_Sequence	any_end = NULL;
    register Filter fi;

    fi = filter_alloc ();
    bzero ((char *) fi, sizeof *fi);
    fi -> flt_type = FILTER_ITEM;

    at = AttrT_cpy (at);

    if ((dp = index (cp, '*')) == NULL) {
	fi -> FUITEM.fi_type = FILTERITEM_EQUALITY;
	fi -> FUITEM.UNAVA.ava_type = at;
	fi -> FUITEM.UNAVA.ava_value = str2AttrV (cp, at -> oa_syntax);

	return fi;
    }
    if (*cp == '*' && !cp[1]) {
	fi -> FUITEM.fi_type = FILTERITEM_PRESENT;
	fi -> FUITEM.UNTYPE = at;

	return fi;
    }

    fi -> FUITEM.fi_type = FILTERITEM_SUBSTRINGS;
    fi -> FUITEM.UNSUB.fi_sub_type = at;

    (void) strcpy (buffer, cp);
    dp = buffer + (dp - cp);
    cp = buffer;
    *dp++ = NULL;

    if (*cp) {
	av = str2AttrV (cp, at -> oa_syntax);
	fi -> FUITEM.UNSUB.fi_sub_initial = avs_comp_new (av);
    }
    cp = dp;

    if (dp = rindex (cp, '*')) {
	*dp++ = NULL;
	if (*dp) {
	    av = str2AttrV (dp, at -> oa_syntax);
	    fi -> FUITEM.UNSUB.fi_sub_final = avs_comp_new (av);
	}
    }
    else {
	if (*cp) {
	    av = str2AttrV (cp, at -> oa_syntax);
	    fi -> FUITEM.UNSUB.fi_sub_final = avs_comp_new (av);
	}	

	return fi;
    }

    do {
	if (dp = index (cp, '*'))
	    *dp++ = NULL;
	if (*cp) {
	    av = str2AttrV (cp, at -> oa_syntax);

	    if (any_end) {
		any_end -> avseq_next = avs_comp_new (av);
		any_end = any_end -> avseq_next;
	    }
	    else
		fi -> FUITEM.UNSUB.fi_sub_any = any_end = avs_comp_new (av);
	}
    } while (cp = dp);

    return fi;
}

/*    EXPAND SUPPORT */

struct dn_seq	*expand_full (),
    		*expand_partial ();

/*  */

static	do_expand (n, vec)
int	n;
char  **vec;
{
    int	    complete;
    DN	    dn;
    struct dn_seq *result;

    if (n > 0 && strcmp (*vec, "-full") == 0) {
	n--, vec++;
	fred_long = TRUE;
    }
    else
	fred_long = FALSE;

    if (n != 1) {
	Usage ("fred");
	return;
    }

    dn = NULLDN;
    if (strcmp (*vec, "@") && (dn = str2dn (*vec)) == NULLDN) {
	ps_printf (OPT, "Bad DN: %s", *vec);
	return;
    }

    result = fred_long ? expand_full (dn, &complete)
		       : expand_partial (dn, &complete);
    
    dn_free (dn);

    if (result == NULL)
	return;

    fred_list = TRUE;
    done_match (result, complete ? "5" : "3");
}

/*  */

static struct dn_seq *expand_full (dn, complete)
DN	dn;
int    *complete;
{
    struct ds_list_arg list_arg;
    register struct ds_list_arg *la = &list_arg;
    struct ds_list_result list_result;
    register struct ds_list_result *lr = &list_result;
    struct DSError list_error;
    register struct DSError *le = &list_error;
    register struct subordinate *sub;
    register struct list_cache *ptr;
    DN	    adn,
	    newdn;
    struct dn_seq *result = NULLDNSEQ;

    bzero ((char *) la, sizeof *la);
    bzero ((char *) lr, sizeof *lr);
    bzero ((char *) le, sizeof *le);

    la -> lsa_common.ca_servicecontrol.svc_options
			= SVC_OPT_PREFERCHAIN | SVC_OPT_DONTDEREFERENCEALIAS;
    la -> lsa_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
    la -> lsa_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;

    if (ptr = find_list_cache (la -> lsa_object = dn, SVC_NOSIZELIMIT)) {
	sub = ptr -> list_subs;
	*complete = 1;
    }
    else {
	if (rebind () != OK)
	    return NULLDNSEQ;

	for (;;) {
	    if (ds_list (la, le, lr) == DS_OK)
		break;
	    if (dish_error (OPT, le) == NOTOK)
		return NULLDNSEQ;

	    la -> lsa_object =
			    le -> ERR_REFERRAL.DSE_ref_candidates -> cr_name;
	}

	cache_list (lr -> lsr_subordinates, lr -> lsr_limitproblem, dn,
		    SVC_NOSIZELIMIT);

	sub = lr -> lsr_subordinates;
	*complete = lr -> lsr_limitproblem == LSR_NOLIMITPROBLEM;
    }

    newdn = dn_comp_new (rdn_comp_new (NULLAttrT, NULLAttrV));
    if (adn = dn_cpy (dn))
	dn_append (adn, newdn);
    else
	dn_free (adn), adn = newdn;

    for (; sub; sub = sub -> sub_next) {
	rdn_free (newdn -> dn_rdn);
	dn_comp_fill (newdn, rdn_cpy (sub -> sub_rdn));

	result = dn_seq_push (adn, result);
    }

    dn_free (adn);

    subords_free (lr -> lsr_subordinates);

    return result;
}

/*  */

static struct dn_seq *expand_partial (dn, complete)
DN	dn;
int    *complete;
{
    struct ds_search_arg search_arg;
    register struct ds_search_arg *sa = &search_arg;
    struct ds_search_result search_result;
    register struct ds_search_result *sr = &search_result;
    struct DSError error;
    register struct DSError *se = &error;
    Filter	fi;
    EntryInfo *ptr;
    register struct dn_seq *result = NULLDNSEQ;

    bzero ((char *) sa, sizeof *sa);

    sa -> sra_common.ca_servicecontrol.svc_options
			= SVC_OPT_PREFERCHAIN | SVC_OPT_DONTDEREFERENCEALIAS;
    sa -> sra_common.ca_servicecontrol.svc_timelimit = SVC_NOTIMELIMIT;
    sa -> sra_common.ca_servicecontrol.svc_sizelimit = SVC_NOSIZELIMIT;

    sa -> sra_subset = SRA_ONELEVEL;
    sa -> sra_searchaliases = FALSE;

    sa -> sra_eis.eis_allattributes = FALSE;
    sa -> sra_eis.eis_select = fred_as ();
    sa -> sra_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;

    if (t_object == NULL)
	fatal (-100, "objectClass: invalid attribute type");
	
    fi = filter_alloc ();
    bzero ((char *) fi, sizeof *fi);
    fi -> flt_type = FILTER_ITEM;
    fi -> FUITEM.fi_type = FILTERITEM_EQUALITY;
    fi -> FUITEM.UNAVA.ava_type = t_object;
    fi -> FUITEM.UNAVA.ava_value = str2AttrV ("dSA", t_object -> oa_syntax);
    
    sa -> sra_filter = filter_alloc ();
    bzero ((char *) sa -> sra_filter, sizeof *sa -> sra_filter);
    sa -> sra_filter -> flt_type = FILTER_NOT;
    sa -> sra_filter -> FUFILT = fi;

    sa -> sra_baseobject = dn;

    if (rebind () != OK)
	goto free_filter;

    while (ds_search (sa, se, sr) != DS_OK) {
	if (dish_error (OPT, se) == 0) {
	    dsa_status = NOTOK;
	    goto free_filter;
	}

	sa -> sra_baseobject =
	    se -> ERR_REFERRAL.DSE_ref_candidates -> cr_name;
    }

    if (sr -> srr_correlated != TRUE)
	correlate_search_results (sr);

    for (ptr = sr -> CSR_entries; ptr; ptr = ptr -> ent_next) {
	cache_entry (ptr, sa -> sra_eis.eis_allattributes,
		     sa -> sra_eis.eis_infotypes);

	result = dn_seq_push (ptr -> ent_dn, result);
    }
    *complete = sr -> CSR_limitproblem == LSR_NOLIMITPROBLEM;

    dn_free (sr -> CSR_object);
    entryinfo_free (sr -> CSR_entries, 0);
    crefs_free (sr -> CSR_cr);

free_filter: ;
    filter_free (sa -> sra_filter);

    return result;
}

/*    UFNRC SUPPORT */

static	envlist myel = NULLEL;

/*  */

static	build_ufnrc (argc, argv)
int	argc;
char  **argv;
{
    envlist	el;
    register envlist  en,
		     *ep;

    el = NULL, ep = &el;
    while (argc >= 3) {
	register char *cp;
	char   *xp;
	register struct dn_seq **dp;

	if ((en = (envlist) calloc (1, sizeof *en)) == NULL) {
no_mem: ;
	    ps_printf (OPT, "Out of memory!");
	    goto losing;
	}
	*ep = en, ep = &en -> Next;

	if (sscanf (argv[0], "%d", &en -> Lower) != 1
	        || sscanf (argv[1], "%d", &en -> Upper) != 1)
	    goto usage;

	dp = &en -> Dns;
	for (cp = argv[2]; *cp; cp = xp){
	    register struct dn_seq *ds;

	    if (xp = index (cp, '$'))
		*xp++ = NULL;
	    else
		xp = cp + strlen (cp);

	    if ((ds = (struct dn_seq *) calloc (1, sizeof *ds)) == NULL)
		goto no_mem;
	    *dp = ds, dp = &ds -> dns_next;

	    if (*cp != '-' && (ds -> dns_dn = str2dn (cp)) == NULLDN) {
		ps_printf (OPT, "Bad DN in environment: %s", cp);
		goto losing;
	    }
	}

	argc -= 3, argv += 3;
    }
    if (argc > 0) {
usage: ;
	Usage ("fred");
	goto losing;
    }
    
    en = myel, myel = el, el = en;
    /* and fall... */

losing: ;
    for (; el; el = en) {
	en = el -> Next;

	dn_seq_free (el -> Dns);
	free ((char *) el);
    }

    for (el = myel; el; el = el -> Next) {
	ps_printf (RPS, "%d,%d:\t    ", el -> Lower, el -> Upper);
	dn_seq_print (RPS, el -> Dns, EDBOUT);
	ps_printf (RPS, "\n");
    }
}


/*    UFN SUPPORT */

#ifndef	SOCKETS
extern	char	search_result;
#endif
extern	char	remote_prob;
extern	Attr_Sequence
    		ufnas;

/*  */

static	do_ufn_match (n, vec)
int	n;
char  **vec;
{
    struct dn_seq  *dns;

    if (ufn_init () == FALSE) {
	ps_printf (OPT, "UFN initialization fails.\n");
	return;
    }

    if (rebind () != OK)
	return;

    ufn_notify = !frompipe;
    if (n > 0 && strcmp (vec[0], "-list") == 0) {
	n--, vec++;
	fred_list = dad_flag;
    }
    else
	fred_list = FALSE;
    if (n > 0 && strcmp (vec[0], "-mailbox") == 0) {
	static int did_ufnas = 0;

	n--, vec++;
	fred_mailbox = TRUE;

	if (!did_ufnas) {
	    did_ufnas = 1;
	    if (t_mbox == NULL)
		fatal (-100, "rfc822Mailbox: invalid attribute type");
	    
	    ufnas = as_merge (ufnas, as_cpy (fred_as ()));
	}
    }
    else
	fred_mailbox = FALSE;
    if (n > 0 && strcmp (vec[0], "-phone") == 0) {
	n--, vec++;
	fred_phone = TRUE;
    }
    else
	fred_phone = FALSE;
    if (n > 0 && strcmp (vec[0], "-photo") == 0) {
	n--, vec++;
	fred_photo = dad_flag;
    }
    else
	fred_photo = FALSE;
    if (n > 0 && strncmp (vec[0], "-options ", sizeof "-options " - 1) == 0) {
	if (sscanf (vec[0], "-options %x", &ufn_flags) != 1) {
	    ps_printf (OPT, "bad option: %s\n", vec[0]);
	    return;
	}

	n--, vec++;
    }
    else
	ufn_flags = UFN_ALL;

    dns = NULL;
    if (!ufn_match (n, vec, interact, &dns, myel)) {
	ps_printf (OPT, fred_mailbox ? "unable to resolve name"
				     : "Unable to resolve name.\n");
	return;
    }

    if (!fred_mailbox)
	done_match (dns, NULLCP);
    else {
	struct dn_seq  *result;
	Entry	e;
	Attr_Sequence	eptr;

	result = NULL;
	while (dns) {
	    struct dn_seq *tmp;

	    if (!(e = local_find_entry (dns -> dns_dn, FALSE))) {
strip_it: ;
		tmp = dns;
		dns = dns -> dns_next;
		tmp -> dns_next = NULL;
		dn_seq_free (tmp);
		continue;
	    }

	    for (eptr = e -> e_attributes; eptr; eptr = eptr -> attr_link)
		if (AttrT_cmp (eptr -> attr_type, t_mbox) == 0)
		    break;
	    if (!eptr || !eptr -> attr_value)
		goto strip_it;

	    tmp = dns -> dns_next;
	    dns -> dns_next = result;
	    result = dns;
	    dns = tmp;
	}

	if (!(dns = result)) {
none_selected: ;
	    ps_printf (OPT, "search failed\n");
	    return;
	}

	while (dns -> dns_next) {
	    int	i;
	    struct dn_seq *ptr;

	    i = 0;
	    for (ptr = dns; ptr; ptr = ptr -> dns_next)
		i++;
	    dns_sort (&dns, i);

	    if (!(dns = interact (dns, NULLDN, NULLCP)))
		goto none_selected;
	}

	if (!(dns = interact (dns, NULLDN, NULLCP)))
	    goto none_selected;

	if (!(e = local_find_entry (dns -> dns_dn, FALSE))) {
lost_entry: ;
	    ps_printf (OPT, "lost entry\n");
	    dn_seq_free (dns);
	    return;
	}
	for (eptr = e -> e_attributes; eptr; eptr = eptr -> attr_link)
	    if (AttrT_cmp (eptr -> attr_type, t_mbox) == 0)
		break;
	if (!eptr || !eptr -> attr_value)
	    goto lost_entry;

	AttrV_print (RPS, &eptr -> attr_value -> avseq_av, READOUT);
	ps_print (RPS, "\n");
	dn_seq_free (dns);
    }
}

/*  */

static struct dn_seq *interact (dns, dn, s)
struct dn_seq *dns;
DN	dn;
char   *s;
{
    int	    i,
	    j;
    char    buffer[BUFSIZ];
    struct dn_seq *result = NULL,
		  *ptr,
		  *tmp;
    static PS    nps = NULL;

    if (dns == NULL)
	return NULL;
    if (nps == NULLPS
	    && ((nps = ps_alloc (str_open)) == NULLPS
	            || str_setup (nps, NULLCP, 0, 0) == NOTOK)) {
	if (nps)
	    ps_free (nps);
	return dns;
    }

    i = 0;
    for (ptr = dns; ptr; ptr = ptr -> dns_next)
	i++;

    if (i == 1 && s)
	return dns;

    if (fred_list && frompipe && i > 1) {
#define	NCHOICES	25
	register int *ip;
	int	chosen[NCHOICES];
	char   *bp;

	if (i > NCHOICES) {
losing: ;
	    dn_seq_free (dns);
	    return NULLDNSEQ;
	}

	dns_sort (&dns, i);

	for (ptr = dns; ptr; ptr = ptr -> dns_next) {
	    ufn_dn_print_aux (nps, ptr -> dns_dn, dn, 0);
	    ps_print (nps, "$");
	    dn_print (nps, ptr -> dns_dn, EDBOUT);
	    ps_print (nps, " ");
	    *--nps -> ps_ptr = NULL, nps -> ps_cnt++;

	    (void) sprintf (buffer, "l%s\n", nps -> ps_base);

	    nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;

	    send_pipe_aux (buffer);

	    if (read_pipe_aux (buffer, sizeof buffer) < 1) {
		(void) fprintf (stderr, "read failure\n");
		remote_prob = TRUE;
		goto losing;
	    }
	}

	if (s)
	    (void) sprintf (buffer,
			    "LPlease select from the following matches for '%s':\n",
			    s);
	else
	    (void) sprintf (buffer, "LPlease select one of the following:\n");
	send_pipe_aux (buffer);
	if (read_pipe_aux (buffer, sizeof buffer) < 1) {
	    (void) fprintf (stderr, "read failure\n");
	    remote_prob = TRUE;
	    goto losing;
	}

	bzero ((char *) chosen, sizeof chosen);
	for (bp = buffer + 1; *bp; ) {
	    int	    k;
	    
	    while (!isdigit (*bp))
		if (*bp)
		    bp++;
	    if (!*bp || sscanf (bp, "%d", &k) != 1 || k <= 0 || k > NCHOICES)
		break;
	    chosen[--k] = 1;
	    while (isdigit (*bp))
		bp++;
	}

	for (ip = chosen; dns; ip++)
	    if (*ip) {
		tmp = dns -> dns_next;
		dns -> dns_next = result;
		result = dns;
		dns = tmp;
	    }
	    else {
		tmp = dns;
		dns = dns -> dns_next;
		tmp -> dns_next = NULL;
		dn_seq_free (tmp);
	    }

	return result;
    }

    if (i > 10) {
	if (s)
	    (void) sprintf (buffer,
			    "%d imprecise matches for '%s', select from them [y/n] ? ",
			    i, s);
	else
	    (void) sprintf (buffer,
			    "%d imprecise matches, select one of them [y/n] ? ");
	if (yesno (buffer)) {
	    dn_seq_free (dns);
	    return NULLDNSEQ;
	}
    }
    else {
	if (s)
	    (void) sprintf (buffer,
			    "mPlease select from the following %d match%s for '%s':\n",
			    i, i != 1 ? "es" : "", s);
	else
	    (void) sprintf (buffer, "mPlease %s of the following:\n",
			    i > 1 ? "select one" : "confirm use");

	if (frompipe) {
	    send_pipe_aux (buffer);

	    if (read_pipe_aux (buffer, sizeof buffer) < 1) {
		(void) fprintf (stderr, "read failure\n");
		remote_prob = TRUE;
		goto losing;
	    }
	}
	else
	    ps_printf (OPT, "%s", buffer + 1);
    }

    if (i > 1)
	dns_sort (&dns, i);

    j = 1;
    while (dns) {
	ufn_dn_print_aux (nps, dns -> dns_dn, dn, 0);
	ps_print (nps, " [y/n] ? ");
	ps_print (nps, " ");
	*--nps -> ps_ptr = NULL, nps -> ps_cnt++;

	(void) strcpy (buffer, nps -> ps_base);

	nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;

	switch (yesno (buffer)) {
	    case OK:
		tmp = dns -> dns_next;
		dns -> dns_next = result;
		result = dns;
		dns = tmp;
		break;

	    case NOTOK:
	    default:
		tmp = dns;
		dns = dns -> dns_next;
		tmp -> dns_next = NULL;
		dn_seq_free (tmp);
		break;

	    case DONE:
		dn_seq_free (dns);
		goto out;
	}

	if ((j++ % 10) == 0 && dns) {
	    (void) sprintf (buffer, "Continue (%d more) [y/n] ? ", i - j + 1);
	    if (yesno (buffer)) {
		dn_seq_free (dns);
		break;
	    }
	}
    }
out: ;

    return result;
}

/*  */

static	int	dns_compar (a, b)
struct dn_seq **a,
	      **b;
{
    int	    i;
    DN	    adn,
	    bdn;

    for (adn = (*a) -> dns_dn; adn -> dn_parent; adn = adn -> dn_parent)
	continue;
    for (bdn = (*b) -> dns_dn; bdn -> dn_parent; bdn = bdn -> dn_parent)
	continue;

    i = rdn_cmp (adn -> dn_rdn, bdn -> dn_rdn);
    return (i == (-1) || i == 1 ? i : 0);
}


static int	dns_sort (dns, i)
struct dn_seq **dns;
int	i;
{
    register struct dn_seq *ptr;

    if (i == 0)
	for (ptr = *dns; ptr; ptr = ptr -> dns_next)
	    i++;

    if (i > 1) {
	struct dn_seq **base,
		      **bp,
		      **ep;

	if (base = (struct dn_seq **) malloc ((unsigned) (i * sizeof *base))) {
	    ep = base;
	    for (ptr = *dns; ptr; ptr = ptr -> dns_next)
		*ep++ = ptr;

	    qsort ((char *) base, i, sizeof *base, dns_compar);

	    bp = base;
	    ptr = *dns = *bp++;
	    while (bp < ep) {
		ptr -> dns_next = *bp;
		ptr = *bp++;
	    }
	    ptr -> dns_next = NULL;

	    free ((char *) base);
	}
    }
}

/*  */

static	done_match (dns, fancy)
struct dn_seq *dns;
char   *fancy;
{
    int	    i;
    register struct dn_seq *ptr;

    if (dns == NULL) {
	ps_printf (OPT, "Search failed to find anything.\n");
	return;
    }

    i = 0;
    for (ptr = dns; ptr; ptr = ptr -> dns_next)
	i++;

    fred_long = i == 1; 
    fred_expand = fred_subdisplay = FALSE;
    fred_sequence = !fred_list;

    if (i > 1 || fancy) {
	PS    aps;

#ifndef	SOCKETS
	if (frompipe)
	    search_result = NOTOK;
	aps = OPT;
#else
	if (frompipe
	    	&& rps -> ps_byteno == 0
	    	&& opt -> ps_byteno == 0
	        && fdx_reset (opt) == OK) {		/* MAJOR HACK */
	    char   *cp = fancy ? fancy : "3";
	    
	    (void) (*opt -> ps_writeP) (opt, cp, strlen (cp), 0);
	    aps = opt;
	}
	else
	    aps = RPS;
#endif

	ps_printf (aps, "%d matches found.\n", i);
	(void) ps_flush (aps);

	dns_sort (&dns, i);
    }

    for (ptr = dns; ptr; ptr = ptr -> dns_next)
	(void) add_sequence (ptr -> dns_dn);

    if (i == 1
	    && !fancy
	    && fred_list
	    && frompipe
	    && rps -> ps_byteno == 0
	    && opt -> ps_byteno == 0
	    && fdx_reset (rps) == OK) {		/* MAJOR HACK */
	showfredDNs (dns -> dns_dn, fred_long);

	(void) (*rps -> ps_writeP) (rps, "4", 1, 0);
	ufn_dn_print_aux (rps, dns -> dns_dn, NULLDN, 0);
	ps_print (RPS, "$");
	dn_print (RPS, dns -> dns_dn, EDBOUT);
	ps_print (rps, "\n");
	(void) ps_flush (rps);
		
	fred_list = FALSE;
    }

    for (i = 0, ptr = dns; ptr; ptr = ptr -> dns_next, i++) {
	if (fred_list) {
	    ufn_dn_print_aux (RPS, ptr -> dns_dn, NULLDN, 0);
	    ps_print (RPS, "$");
	    dn_print (RPS, ptr -> dns_dn, EDBOUT);
	    ps_print (RPS, "\n");

	    continue;
	}

	if (i > 0)
	    (void) ps_flush (RPS);

	 (void) showfred (ptr -> dns_dn, fred_long, fred_subdisplay);
    }

    dn_seq_free (dns);
}

/*    SHOWENTRY SUPPORT */

extern int postal_indent;
extern int ufn_indent;

static struct template {
    char   *t_name;
    char   *t_prefix;

    int	    t_level;

    AttributeType t_at;    
} default_template[] = {
    "title",				NULL,
		0, NULL,
    "documentTitle",			NULL,
		0, NULL,
#define	LEVEL_POSTAL	0
    "organizationName", 		NULL,
		0, NULL,
    "organizationalUnitName",		NULL,
		0, NULL,
	"roomNumber",			"  Room ",
		0, NULL,
	"streetAddress",		"  ",
		0, NULL,
	"postOfficeBox",		"  POB ",
		0, NULL,
	"physicalDeliveryOfficeName",	"  ",
		0, NULL,
	"stateOrProvinceName",		"  ",
		0, NULL,
	"postalCode", 			"  ",
		0, NULL,

    "postalAddress",			NULL,
		1, NULL,
    "documentVersion",			"Version of: ",
		1, NULL,

    "registeredAddress",		"Registered Address: ",
		2, NULL,
    
    "telephoneNumber",			"Telephone: ",
		3, NULL,
    "mobileTelephoneNumber",		"Mobile:    ",
		3, NULL,
    "pagerTelephoneNumber",		"Pager:     ",
		3, NULL,
    "facsimileTelephoneNumber",		"FAX:       ",
		3, NULL,
    "telexNumber",			"Telex:     ",
		3, NULL,
    "teletexTerminalIdentifier",	"Teletex:   ",
		3, NULL,
    "x121Address",			"X.121:     ",
		3, NULL,
    "internationaliSDNNumber",		"ISDN:      ",
		3, NULL,
    "presentationAddress",		"OSI:       ",
		3, NULL,
    "documentLocation",			"Location:  ",
		3, NULL,

#define	LEVEL_MBOX	4
    "rfc822Mailbox",			"  Mailbox: ",
		4, NULL,
        "otherMailbox",			"  ",
		4, NULL,
	"textEncodedORaddress",		"  ",
		4, NULL,

    "destinationIndicator",		"Destination Indicator:     ",
		5, NULL,
    "preferredDeliveryMethod",		"Preferred Delivery Method: ",
		5, NULL,
    "supportedApplicationContext",      "Supports:  ",
		5, NULL,

    "personalTitle",			NULL,
		6, NULL,
    "description",			NULL,
		6, NULL,

    "info",				"Information: ",
		7, NULL,
    "businessCategory",			"Business:    ",
		7, NULL,
    "localityName",			"Locality:    ",
    		7, NULL,
    "userClass",			"User Class:  ",
		7, NULL,
    "owner",				"Owner:       ",
		7, NULL,
    "documentAuthor",			"Author:      ",
		7, NULL,
#define	LEVEL_AUTHOR	7
    "documentAuthorCommonName",		"Author:      ",
		7, NULL,
    "documentAuthorSurName",		"Author:      ",
		7, NULL,

    "member",				"Members:   ",
		8, NULL,

    "aliasedObjectName",		"Alias to:  ",
		9, NULL,
    "roleOccupant",			"Occupant:  ",
		9, NULL,
    "seeAlso",				"See Also:  ",
		9, NULL,
    "secretary",			"Secretary: ",
		9, NULL,
    "manager",				"Manager:   ",
		9, NULL,

    "homePostalAddress",		"Home Address: ",
		10, NULL,
    "homePhone",			"Home Phone:   ",
		10, NULL,
    "favouriteDrink",			"Drinks:       ",
		10, NULL,
    "photo",				"Picture:      ",
		10, NULL,
#ifdef	sparc
    "audio",				"Audio:        ",
		10, NULL,
#endif

    NULL
};

/*  */

showfred (mydn, islong, subdisplay)
DN	mydn;
char	islong,
	subdisplay;
{
    int	    didtime,
	    hasauthor,
	    haspost,
	    level,
	    nchild,
	    pos,
	    seqno;
    register struct template *t;
    register Attr_Sequence eptr;
    register AV_Sequence avs,
			 avp;
    AttributeType rdn_at,
		  inf_at;
    AttributeValue rdn_av;
    Entry	myentry;
    PS	    ps = NULLPS;
    RDN	    myrdn;

    fred_init ();

    myentry = fredentry (mydn = dn_cpy (mydn), islong);

    pos = RPS -> ps_byteno;

    seqno = fred_sequence ? add_sequence (mydn) : 0;
    if (islong == FALSE) {
	if (seqno)
	    ps_printf (RPS, "%3d. ", seqno);
	else
	    ps_printf (RPS, "     ");
    }

    if (mydn) {
	register DN	adn;
	register RDN	rdn;

	if (islong == FALSE) {
	    ufn_dn_print_aux (RPS, mydn, NULLDN, 0);
	    goto ufn_short;
	}
	for (adn = mydn; adn -> dn_parent; adn = adn -> dn_parent)
	    continue;
	myrdn = adn -> dn_rdn;
	rdn_at = myrdn -> rdn_at, rdn_av = &myrdn -> rdn_av;
	AttrV_print (RPS, rdn_av, EDBOUT);
	for (rdn = myrdn -> rdn_next; rdn; rdn = rdn -> rdn_next) {
	    ps_print (RPS, " + ");
	    AttrV_print (RPS, &rdn -> rdn_av, EDBOUT);
	}
    }
    else {
	myrdn = NULLRDN, rdn_at = NULLAttrT, rdn_av = NULLAttrV;
	ps_print (RPS, "@");
    }

    if (islong == TRUE && seqno)
	ps_printf (RPS, " (%d)", seqno);

    if ((pos += 52 - RPS -> ps_byteno) <= 0)
	pos = 1;

    inf_at = NULLAttrT;
    if (myentry) {
	for (eptr = myentry -> e_attributes; eptr; eptr = eptr -> attr_link)
	    if (!fred_phone && AttrT_cmp (eptr -> attr_type, t_mbox) == 0) {
		inf_at = t_mbox;

		if (avs = eptr -> attr_value) {
		    ps_printf (RPS, "%*s", pos, "");
		    showfredattr (&avs -> avseq_av);
		}
		break;
	    }
	    else
		if (AttrT_cmp (eptr -> attr_type, t_phone) == 0) {
		    inf_at = t_phone;
		    avp = eptr -> attr_value;
		}

	if (inf_at == t_phone && avp) {
	    ps_printf (RPS, "%*s", pos, "");
	    showfredattr (&avp -> avseq_av);
	}
    }

ufn_short: ;
    ps_print (RPS, "\n");

    if (myentry == NULLENTRY)
	goto out;
    if (islong == FALSE)
	goto children;

    if (!fred_photo)
	for (eptr = myentry -> e_attributes; eptr; eptr = eptr -> attr_link)
	    if (AttrT_cmp (eptr -> attr_type, t_photo) == 0) {
		if (eptr -> attr_value) {
		    if (rdn_av
		            && (ps = ps_alloc (str_open)) != NULLPS
		            && str_setup (ps, NULLCP, 0, 0) != NOTOK) {
			register RDN rdn;

			AttrV_print (ps, rdn_av, EDBOUT);
			for (rdn = myrdn -> rdn_next;
			         rdn;
			         rdn = rdn -> rdn_next) {
			    ps_print (ps, "/");
			    AttrV_print (ps, &rdn -> rdn_av, EDBOUT);
			}
			ps_print (ps, " ");
			*--ps -> ps_ptr = NULL, ps -> ps_cnt++;
			(void) setenv ("RDN", ps -> ps_base);
		    }
		    else
			(void) setenv ("RDN", "Photo");
		}
		break;
	    }

    level = 0;
    for (eptr = myentry -> e_attributes; eptr; eptr = eptr -> attr_link)
	if (AttrT_cmp (eptr -> attr_type, rdn_at) == 0) {
	    for (avp = eptr -> attr_value; avp; avp = avp -> avseq_next)
		if (AttrV_cmp (&avp -> avseq_av, rdn_av)) {
		    ps_print (RPS, "     aka: ");
		    showfredattr (&avp -> avseq_av);
		    ps_print (RPS, "\n");
		    level++;
		}

	    break;
	}

    hasauthor = haspost = 0;
    for (eptr = myentry -> e_attributes; eptr; eptr = eptr -> attr_link)
	if (AttrT_cmp (eptr -> attr_type, t_author) == 0) {
	    if (eptr -> attr_value)
		hasauthor = 1;
	}
	else
	    if (AttrT_cmp (eptr -> attr_type, t_postal) == 0) {
		if (eptr -> attr_value)
		    haspost = 1;
	    }
    ps_print (RPS, "\n");

    level = -1;
    for (t = default_template; t -> t_name; t++) {
	if (AttrT_cmp (t -> t_at, rdn_at) == 0)
	    continue;
	if (fred_photo && AttrT_cmp (t -> t_at, t_photo) == 0)
	    continue;

	for (eptr = myentry -> e_attributes;
	         eptr;
	         eptr = eptr -> attr_link)
	    if (AttrT_cmp (eptr -> attr_type, t -> t_at) == 0) {
		int	i;

		if (AttrT_cmp (eptr -> attr_type, inf_at) == 0
		        && (avs = eptr -> attr_value)
		        && !avs -> avseq_next)
		    continue;

		if (t -> t_level == LEVEL_AUTHOR)
		    if (AttrT_cmp (eptr -> attr_type, t_authorCN) == 0) {
			if (hasauthor)
			    continue;
			hasauthor = 1;
		    }
		    else
			if (hasauthor
			        && AttrT_cmp (eptr -> attr_type, t_authorSN)
			    		== 0)
			    continue;

		if (haspost
			&& t -> t_level == LEVEL_POSTAL
			&& AttrT_cmp (eptr -> attr_type, t_title)) {
		    level = t -> t_level + 1;
		    continue;
		}

		if (t -> t_level != level) {
		    if (level >= 0)
			ps_print (RPS, "\n");
		    if ((level = t -> t_level) == LEVEL_MBOX)
			ps_print (RPS, "Mailbox information:\n");
		}
		
		if (t -> t_prefix) {
		    i = strlen (t -> t_prefix);
		    ps_printf (RPS, "%s", t -> t_prefix);
		}
		else
		    i = 0;

		if (avs = eptr -> attr_value) {
		    postal_indent = i;
		    ufn_indent = postal_indent + 2;
		    showfredattr (&avs -> avseq_av);
		    ps_print (RPS, "\n");
		    for (avp = avs -> avseq_next;
			     avp;
			     avp = avp -> avseq_next) {
			if (t -> t_prefix)
			    ps_printf (RPS, "%*s", i, "");
			else
			    ps_print (RPS, "\n");

			showfredattr (&avp -> avseq_av);

			ps_print (RPS, "\n");
		    }
		}
		else
		    ps_print (RPS, "no value?!?\n");

		break;
	    }
    }

    ufn_indent = (sizeof "Modified: " - 1) + 2;
    if (mydn) {
	ps_print (RPS, "\nName:     ");
	ufn_dn_print_aux (RPS, mydn, NULLDN, 1);
	if (seqno)
	    ps_printf (RPS, " (%d)", seqno);
    }

    ps_print (RPS, "\n");

    didtime = 0;
    for (eptr = myentry -> e_attributes; eptr; eptr = eptr -> attr_link)
	if (AttrT_cmp (eptr -> attr_type, t_modtime) == 0) {
	    if (avs = eptr -> attr_value) {
		ps_print (RPS, "Modified: ");
		showfredattr (&avs -> avseq_av);
		ps_print (RPS, "\n");

		didtime = 1;
	    }
	    break;
	}
    if (didtime)
	for (eptr = myentry -> e_attributes; eptr; eptr = eptr -> attr_link)
	    if (AttrT_cmp (eptr -> attr_type, t_modwhom) == 0) {
		if ((avs = eptr -> attr_value)
			&& dn_cmp ((DN) avs -> avseq_av.av_struct, mydn)) {
		    ps_print (RPS, "      by: ");
		    showfredattr (&avs -> avseq_av);
		    ps_print (RPS, "\n");
		}
		break;
	    }

children: ;
    nchild = 0;
    if (subdisplay) {
	struct ds_list_arg list_arg;
	struct ds_list_result list_result;
	struct DSError list_error;
	struct list_cache *ptr;

	(void) ps_flush (RPS);

	(void) service_control (OPT, 0, NULLVP, &list_arg.lsa_common);
	list_arg.lsa_common.ca_servicecontrol.svc_options |=
						SVC_OPT_DONTDEREFERENCEALIAS;

	if (ptr = find_list_cache (list_arg.lsa_object = mydn,
				   SVC_NOSIZELIMIT)) {
	    if (ptr -> list_subs)
		nchild = fred_children (mydn, ptr -> list_subs,
					ptr -> list_problem);

	    goto out;
	}

	if (rebind () != OK)
	    goto out;

	for (;;) {
	    if (ds_list (&list_arg, &list_error, &list_result) == DS_OK)
		break;
	    if (dish_error (OPT, &list_error) == NOTOK)
		goto out;
	    
	    list_arg.lsa_object =
			list_error.ERR_REFERRAL.DSE_ref_candidates -> cr_name;
	}

	if (list_result.lsr_subordinates)
	    nchild = fred_children (mydn, list_result.lsr_subordinates,
			   list_result.lsr_limitproblem);

	cache_list (list_result.lsr_subordinates, list_result.lsr_limitproblem,
		    mydn, SVC_NOSIZELIMIT);
	subords_free (list_result.lsr_subordinates);
    }

out: ;
    if (mydn)
	dn_free (mydn);
    if (ps)
	ps_free (ps);
    postal_indent = -1;
    ufn_indent = -1;

    return nchild;
}

/*  */

static	fred_children (parentdn, ptr, prob)
DN	parentdn;
register struct subordinate *ptr;
int	prob;
{
    int	    i,
	    nchild;
    register struct subordinate *qtr;
    register DN	adn,
		newdn;

    newdn = dn_comp_new (rdn_comp_new (NULLAttrT, NULLAttrV));
    if (adn = dn_cpy (parentdn))
	dn_append (adn, newdn);
    else
	adn = newdn;

    i = 0;
    for (qtr = ptr; qtr; qtr = qtr -> sub_next)
	i++;
    nchild = i;

    if (i > 0)
	ps_printf (RPS, "%d child%s.\n-----\n", i, i != 1 ? "ren" : "");

    for (i = 0; ptr; ptr = ptr -> sub_next, i++) {
	rdn_free (newdn -> dn_rdn);
	dn_comp_fill (newdn, rdn_cpy (ptr -> sub_rdn));
	(void) add_sequence (adn);

	(void) showfred (adn, 0, FALSE);
    }

    dn_free (adn);

    if (prob != LSR_NOLIMITPROBLEM)
	ps_print (RPS, "(Limit problem)\n");

    return nchild;
}

/*  */

static showfredattr (av)
register AttributeValue av;
{
    int	    seqno;

    if (av -> av_syntax == s_dn) {
	ufn_dn_print_aux (RPS, (DN) av -> av_struct, NULLDN, 1);

	if (fred_sequence && (seqno = add_sequence ((DN) av -> av_struct)))
	    ps_printf (RPS, " (%d)", seqno);
    }
    else
	AttrV_print (RPS, av, READOUT);
}

/*    MISC */


static struct pair {
    char   *p_name;
    AttributeType *p_at;
}	pairs[] = {
    "aliasedObjectName",	&t_alias,
    "documentAuthor",		&t_author,
    "documentAuthorCommonName",	&t_authorCN,
    "documentAuthorSurName",	&t_authorSN,
    "associatedDomain",		&t_domain,
    "masterDSA",		&t_master,
    "rfc822Mailbox",		&t_mbox,
    "lastModifiedTime",		&t_modtime,
    "lastModifiedBy",		&t_modwhom,
    "objectClass",		&t_object,
    "otherMailbox",		&t_othermbox,
    "telephoneNumber",		&t_phone,
    "photo",			&t_photo,
    "postalAddress",		&t_postal,
    "slaveDSA",			&t_slave,
    "surName",			&t_surname,
    "title",			&t_title,

    NULL
};
    
/*  */

static	fred_init ()
{
    register struct pair *p;
    register struct template *t;
    static int once_only = 1;

    if (!once_only)
	return;
    once_only = 0;
    
    s_dn = str2syntax ("DN");
    s_photo = str2syntax ("Photo");

    for (p = pairs; p -> p_name; p++)
	*p -> p_at = AttrT_new (p -> p_name);

    for (t = default_template; t -> t_name; t++)
	t -> t_at = AttrT_new (t -> t_name);
}

/*  */

static  Entry fredentry (adn, islong)
DN	adn;
char	islong;
{
    register Entry newentry;

    struct ds_read_arg read_arg;
    struct ds_read_result read_result;
    struct DSError read_error;
    
    if (adn == NULLDN)
	return NULLENTRY;

    if ((newentry = local_find_entry (read_arg.rda_object = adn,
				      FALSE)) == NULLENTRY
	    || !newentry -> e_lock
	    || (islong && !newentry -> e_complete)) {
	if (rebind () != OK)
	    return newentry;

	(void) service_control (OPT, 0, NULLVP, &read_arg.rda_common);
	read_arg.rda_common.ca_servicecontrol.svc_options |=
						SVC_OPT_DONTDEREFERENCEALIAS;

	read_arg.rda_eis.eis_allattributes = FALSE;
	read_arg.rda_eis.eis_select = islong ? fred_full () : fred_as ();
	read_arg.rda_eis.eis_infotypes = EIS_ATTRIBUTESANDVALUES;

	if (ds_read (&read_arg, &read_error, &read_result) != DS_OK) {
#ifdef	notdef
	    dish_error (RPS, &read_error);
#endif
	    return newentry;
	}

	cache_entry (&read_result.rdr_entry, islong ? TRUE : FALSE,
		     read_arg.rda_eis.eis_infotypes);

	entryinfo_comp_free (&read_result.rdr_entry, 0);

	newentry = local_find_entry (adn, FALSE);
    }

    return newentry;
}

/*  */

showfredDNs (dn, islong)
DN	dn;
int	islong;
{
    register Attr_Sequence eptr;
    register AV_Sequence avs;
    Entry    theEntry;
    PS	     nps;

    fred_init ();

    if (!(theEntry = fredentry (dn, islong)))
	return;

    if ((nps = ps_alloc (str_open)) == NULLPS
	    || str_setup (nps, NULLCP, 0, 0) == NOTOK) {
	if (nps)
	    ps_free (nps);

	return;
    }

    for (eptr = theEntry -> e_attributes; eptr; eptr = eptr -> attr_link)
	if (eptr -> attr_type -> oa_syntax == s_dn) {
	    if (AttrT_cmp (eptr -> attr_type, t_master) == 0
		    || AttrT_cmp (eptr -> attr_type, t_slave) == 0)
		continue;

	    for (avs = eptr -> attr_value; avs; avs = avs -> avseq_next) {
		char	buffer[BUFSIZ];
		DN	adn = (DN) avs -> avseq_av.av_struct;

		ufn_dn_print_aux (nps, adn, NULLDN, 0);
		ps_print (nps, "$");
		dn_print (nps, adn, EDBOUT);
		ps_print (nps, " ");
		*--nps -> ps_ptr = NULL, nps -> ps_cnt++;

		(void) sprintf (buffer, "d%s\n", nps -> ps_base);

		nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;

		send_pipe_aux (buffer);

		if (read_pipe_aux (buffer, sizeof buffer) < 1) {
		    (void) fprintf (stderr, "read failure\n");
		    remote_prob = TRUE;
		    goto losing;
		}
	    }
	}
	else
	    if (fred_photo
		    && (eptr -> attr_type -> oa_syntax == s_photo
			    || eptr -> attr_type -> oa_syntax - AV_WRITE_FILE
				    == s_photo)) {
		int	avsno;

		for (avs = eptr -> attr_value, avsno = 1;
		         avs;
		         avs = avs -> avseq_next, avsno++) {
		    int	    cc,
			    i,
			    j;
		    register char *cp,
				  *dp;
		    char    buffer[BUFSIZ],
		    	    faxtopbm[BUFSIZ],
		    	    tmp1[BUFSIZ],
		    	    tmp2[BUFSIZ];
		    AttributeValue av;
		    FILE   *fp;
		    PE	    pe;
		    PS	    ps;
		    struct stat st;

		    if ((av = &avs -> avseq_av) -> av_syntax != AV_FILE)
			av = ((struct file_syntax *) av -> av_struct)
								    -> fs_attr;

		    (void) strcpy (faxtopbm, isodefile ("g3fax/faxtopbm", 1));
		    if (access (faxtopbm, X_OK) == NOTOK) {
#ifndef	NO_STATS
			ll_log (dad_log, LLOG_EXCEPTIONS, "failed",
				"check for X_OK access on %s", faxtopbm);
#endif
			break;
		    }

		    (void) strcpy (tmp1, "/tmp/faxXXXXXX");
		    (void) unlink (mktemp (tmp1));
		    (void) strcpy (tmp2, "/tmp/pbmXXXXXX");
		    (void) unlink (mktemp (tmp2));

		    if ((fp = fopen (tmp1, "w")) == NULL)
			break;

		    if ((ps = ps_alloc (std_open)) == NULLPS
		            || std_setup (ps, fp) == NOTOK) {
			if (ps)
			    ps_free (ps);
			(void) unlink (tmp1);
			(void) fclose (fp);
			break;
		    }
		    (void) pe2ps (ps, pe = grab_pe (av));

		    pe_free (pe);
		    ps_free (ps);

		    (void) fclose (fp);

		    (void) sprintf (buffer, "%s < %s > %s",
				    faxtopbm, tmp1, tmp2);

		    i = system (buffer);

		    (void) unlink (tmp1);

		    if (i
			    || (fp = fopen (tmp2, "r")) == NULL
			    || fstat (fileno (fp), &st) == NOTOK
			    || (cc = st.st_size) == 0) {
			if (!i && fp)
			    (void) fclose (fp);
			(void) unlink (tmp2);

			continue;
		    }
		
		    if ((cp = malloc ((unsigned) (cc))) == NULL) {
			fprintf (stderr, "out of memory\n");
			goto out;
		    }
		    for (dp = cp, j = cc; j > 0; dp += i, j -= i)
			switch (i = fread (dp, sizeof *dp, j, fp)) {
			    case NOTOK:
			        fprintf (stderr, "error reading %s: %s\n",
					   tmp2, sys_errname (errno));
				goto out2;

			    case OK:
				fprintf (stderr, "premature eof reading %s\n",
					   tmp2);
out2: ;
				free (cp);
				goto out;

			    default:
				break;
			}

		    (void) sprintf (buffer, "P%d %s", cc,
				    attr2name (eptr -> attr_type, OIDPART));
		    if (avs != eptr -> attr_value || avs -> avseq_next)
			(void) sprintf (buffer + strlen (buffer), "#%d",
					avsno);

		    ufn_dn_print_aux (nps, dn, NULLDN, 0);
		    ps_print (nps, "$");
		    dn_print (nps, dn, EDBOUT);
		    ps_print (nps, " ");
		    *--nps -> ps_ptr = NULL, nps -> ps_cnt++;

		    (void) sprintf (buffer + strlen (buffer), "$%s\n",
				    nps -> ps_base);

		    nps -> ps_ptr = nps -> ps_base, nps -> ps_cnt = nps -> ps_bufsiz;

		    send_pipe_aux (buffer);

		    if ((i = read_pipe_aux (buffer, sizeof buffer)) < 1) {
			(void) fprintf (stderr, "read failure\n");
			remote_prob = TRUE;
out: ;
			(void) fclose (fp);
			(void) unlink (tmp2);
			goto losing;
		    }
		    else
			if ((i == 1) && (*buffer == 'P')) {
			    remote_prob = FALSE;
			    (void) fclose (fp);
			    (void) unlink (tmp2);
			    break;
			}

		    send_pipe_aux2 (cp, cc);
		    free (cp);

		    if ((i = read_pipe_aux (buffer, sizeof buffer)) < 1) {
			(void) ps_printf (OPT, "read failure\n");
			remote_prob = TRUE;
			goto out;
		    }

		    (void) fclose (fp);
		    (void) unlink (tmp2);
		}
	    }

losing: ;

    ps_free (nps);
}

/*  */

Attr_Sequence fred_as ()
{
    static Attr_Sequence as = NULL;

    if (!as) {
	AttributeType at;

	fred_init ();

	if (at = t_mbox)
	    as = as_merge (as,
			   as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
	if (at = t_phone)
	    as = as_merge (as,
			   as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
	if (at = t_surname)
	    as = as_merge (as,
			   as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
	if (at = t_alias)
	    as = as_merge (as,
			   as_comp_new (AttrT_cpy (at), NULLAV, NULLACL_INFO));
    }
    
    return as;
}

/*  */

Attr_Sequence fred_full ()
{
    static Attr_Sequence as = NULL;

    if (!as) {
	register struct pair *p;
	register struct template *t;
	AttributeType at;
	
	fred_init ();

	for (p = pairs; p -> p_name; p++)
	    if (at = *p -> p_at)
		as = as_merge (as, as_comp_new (AttrT_cpy (at), NULLAV,
						NULLACL_INFO));

	for (t = default_template; t -> t_name; t++)
	    if (at = t -> t_at)
		as = as_merge (as, as_comp_new (AttrT_cpy (at), NULLAV,
						NULLACL_INFO));
    }

    if (t_photo) {
	sntx_table *s;

	if (dad_flag
	    	|| (s_photo
		        && (s = get_syntax_table (s_photo))
			&& s -> s_pe_print))
	    as = as_merge (as, as_comp_new (AttrT_cpy (t_photo), NULLAV,
					    NULLACL_INFO));
	else {
	    int	    i;
	    register Attr_Sequence  ptr,
				   *pptr;
	    
	    for (pptr = &as, ptr = *pptr;
		     ptr;
		     pptr = &ptr -> attr_link, ptr = *pptr) {
		if ((i = AttrT_cmp (ptr -> attr_type, t_photo)) == 0) {
		    *pptr = ptr -> attr_link;
		    as_comp_free (ptr);
		}
		if (i <= 0)
		    break;
	    }
	}
    }

    return as;
}
