/* ufn_parse.c - user-friendly name resolution */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/ufn_parse.c,v 7.3 91/02/22 09:20:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/ufn_parse.c,v 7.3 91/02/22 09:20:42 mrose Interim $
 *
 *
 * $Log:	ufn_parse.c,v $
 * Revision 7.3  91/02/22  09:20:42  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:43:08  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:35:20  mrose
 * sync
 * 
 * Revision 7.0  90/06/11  09:59:42  mrose
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


#include "quipu/ufn.h"
#include "tailor.h"
#include "quipu/list.h"
#include "quipu/ds_search.h"
#include "quipu/connection.h"	/* ds_search uses di_block - include this for lint !!! */
#include "quipu/dua.h"

char ufn_notify = FALSE;
int ufn_flags = UFN_ALL;

extern char PY_pepy[];
#define NOTIFY(x) if (ufn_notify) (void) printf x, (void) putchar('\n'); else ;

AttributeType at_OrgUnit;
AttributeType at_Organisation;
AttributeType at_Locality;
AttributeType at_CountryName;
AttributeType at_FriendlyCountryName;
AttributeType at_CommonName;
AttributeType at_Surname;
AttributeType at_Userid;

AttributeType at_ObjectClass;

Attr_Sequence ufnas = NULL;

extern LLog * log_dsap;

extern Filter strfilter ();
extern Filter ocfilter ();
extern Filter joinfilter ();
extern char * TidyString ();

DNS DNS_append (a,b)
DNS a, b;
{	
DNS c;
	if (a == NULLDNS)
		return b;

	for (c=a; c->dns_next != NULLDNS; c=c->dns_next)
		; /* Nothing */

	c->dns_next = b;

	return a;
}

static Attr_Sequence read_cache (base)
DN base;
{
Entry ptr;

	if ((ptr = local_find_entry (base,FALSE)) != NULLENTRY) 
		return (ptr->e_attributes);

	return NULLATTR;
}

static char exact_match (dn,s) 
DN dn;
char * s;
{
RDN rdn;
	for (; dn->dn_parent != NULLDN; dn=dn->dn_parent)
		; /* Nothing */

	for (rdn = dn->dn_rdn; rdn != NULLRDN; rdn=rdn->rdn_next) {
		if (sub_string (rdn->rdn_av.av_syntax)
		&& (lexequ((char *)rdn->rdn_av.av_struct,s) == 0))
			return TRUE;
	}
	return FALSE;
}

static char good_match (dn,s) 
DN dn;
char * s;
{
Attr_Sequence as;
AV_Sequence avs;

	for (as = read_cache(dn); as != NULLATTR; as=as->attr_link)
		for (avs=as->attr_value; avs!= NULLAV; avs=avs->avseq_next)
			if (sub_string (avs->avseq_av.av_syntax)
				&& (lexequ((char *)avs->avseq_av.av_struct,s) == 0))
					return TRUE;
	return FALSE;
}

dnSelect (s,dlist,interact,el)
char * s;
DNS *dlist;
DNS (* interact) ();
DNS el;
{
DNS exact = NULLDNS;
DNS good  = NULLDNS;
DNS bad   = NULLDNS;
DNS tmp, next = NULLDNS;

	if ((dlist == (DNS *)NULL) || (*dlist == NULLDNS))
		return 2;

	for (tmp= *dlist; tmp != NULLDNS; tmp=next) {
		next = tmp->dns_next;
		if (exact_match(tmp->dns_dn,s)) {
			tmp->dns_next = exact;
			exact = tmp;
		} else if (good_match(tmp->dns_dn,s)) {
			tmp->dns_next = good;
			good = tmp;
		} else {
			tmp->dns_next = bad;
			bad = tmp;
		}
	}


	if (exact) {
		NOTIFY (("Found exact match(es) for '%s'",s));
		*dlist = exact;
		dn_seq_free (good);
		dn_seq_free (bad);
		return TRUE;
	}

	if (good) {
		NOTIFY (("Found good match(es) for '%s'",s));
		*dlist = good;
		dn_seq_free (bad);
		return TRUE;
	}

	good = (*interact)(bad,el->dns_dn,s);
	*dlist = good;
	if (good != NULLDNS)
		return TRUE;
	else
		return 2;	/* back track allowed ! */

}

ufn_search (base, subtree, filt, res, s, interact, el)
DN base;
char subtree;
Filter filt;
DNS * res;
char * s;
DNS (* interact) ();
DNS el;
{
struct ds_search_arg search_arg;
static struct ds_search_result result;
struct DSError err;
static CommonArgs ca = default_common_args;
EntryInfo * ptr;
DNS newdns, r = NULLDNS;

	search_arg.sra_baseobject = base;
	search_arg.sra_filter = filt;
	if (subtree)
		search_arg.sra_subset = SRA_WHOLESUBTREE;
	else	
		search_arg.sra_subset = SRA_ONELEVEL;
	search_arg.sra_searchaliases = TRUE;
	search_arg.sra_common = ca; /* struct copy */
	search_arg.sra_eis.eis_infotypes = TRUE;
	search_arg.sra_eis.eis_allattributes = FALSE;
	search_arg.sra_eis.eis_select = ufnas;

#ifdef	DEBUG
	if (ufn_notify == 2)
	    print_search (base, subtree, filt);
#endif
	if (ds_search (&search_arg, &err, &result) != DS_OK) {
		log_ds_error (&err);
		NOTIFY (("DAP Search returned an error"))
		ds_error_free (&err);
		filter_free (filt);
		return FALSE;
	}

	filter_free (filt);
	correlate_search_results (&result);

	dn_free (result.CSR_object);

	if ( (result.CSR_limitproblem != LSR_NOLIMITPROBLEM) || (result.CSR_cr != NULLCONTINUATIONREF)) {
		crefs_free (result.CSR_cr);

		if ( ! result.CSR_entries) {
			NOTIFY (("Search returned partial results"))
			return FALSE;
		} 
		NOTIFY (("Continuing with partial results !"));
	}

	for (ptr = result.CSR_entries; ptr != NULLENTRYINFO; ptr=ptr->ent_next) {
		cache_entry (ptr, search_arg.sra_eis.eis_allattributes,
			     search_arg.sra_eis.eis_infotypes);
		newdns = dn_seq_alloc();
		newdns->dns_next = r;
		newdns->dns_dn = dn_cpy (ptr->ent_dn);
		r = newdns;
	}
	entryinfo_free (result.CSR_entries,0);

	*res = r;

	return dnSelect (s,res,interact,el);
}

#define	SUBSTRINGS()	((ufn_flags & UFN_WILDHEAD) ? FILTERITEM_SUBSTRINGS \
			 			    : -FILTERITEM_SUBSTRINGS)

static rootSearch (s,interact,el,result)
char * s;
DNS (* interact) ();
DNS el;
DNS * result;
{
    Filter filt, filta, filtb, filtc, filtd, filte, filtf;

    if (check_3166 (s)) {
	filta = strfilter (at_CountryName,s,FILTERITEM_EQUALITY);
	filtb = strfilter (at_FriendlyCountryName,s,FILTERITEM_EQUALITY);
	filtb -> flt_next = filta;
	filtc = strfilter (at_Organisation,s,FILTERITEM_EQUALITY);
	filtc -> flt_next = filtb;
	filt = joinfilter (filtc, FILTER_OR);
    }
    else {
	filta = strfilter (at_FriendlyCountryName,s,SUBSTRINGS ());
	filtc = strfilter (at_Organisation,s,SUBSTRINGS ());
	filtc -> flt_next = filta;
	filte = strfilter (at_Locality,s,SUBSTRINGS ());
	filte -> flt_next = filtc;
	if ((ufn_flags & UFN_APPROX) && !index (s, '*')) {
	    filtb = strfilter (at_FriendlyCountryName,s,FILTERITEM_APPROX);
	    filtb -> flt_next = filte;
	    filtd = strfilter (at_Organisation,s,FILTERITEM_APPROX);
	    filtd -> flt_next = filtb;
	    filtf = strfilter (at_Locality,s,FILTERITEM_APPROX);
	    filtf -> flt_next = filtd;
	    filt = joinfilter (filtf, FILTER_OR);
	}
	else
	    filt = joinfilter (filte, FILTER_OR);
    }

    return ufn_search (NULLDN,FALSE,filt,result,s,interact,el);
}

static char present (d,t)
DN d;
AttributeType t;
{
RDN rdn;

	for (; d != NULLDN; d=d->dn_parent)
	    for (rdn = d -> dn_rdn; rdn; rdn = rdn -> rdn_next)
		if (AttrT_cmp (rdn->rdn_at,t) == 0)
			return TRUE;

	return FALSE;	/* More work ... */
}

static intSearch (base,s,interact,el,result)
DN base;
char * s;
DNS (* interact) ();
DNS el;
DNS * result;
{
Filter filt, filta, filtb, filtc, filtd, filte, filtf, filtg, filth;

	if ( present (base,at_OrgUnit) ) {
		if ((filte = ocfilter ("OrganizationalUnit")) == NULLFILTER)
			return FALSE;

		filta = strfilter (at_OrgUnit,s,SUBSTRINGS ());
		if ((ufn_flags & UFN_APPROX) && !index (s, '*')) {
		    filtb = strfilter (at_OrgUnit,s,FILTERITEM_APPROX);
		    filtb->flt_next = filta;
		    filt = joinfilter (filtb, FILTER_OR);
		}
		else
		    filt = filta;

		filte->flt_next = filt;

		filtf = joinfilter (filte, FILTER_AND);
	} else if ( present (base,at_Organisation) ) {
		if ((filte = ocfilter ("OrganizationalUnit")) == NULLFILTER)
			return FALSE;
		if ((filth = ocfilter ("Locality")) == NULLFILTER) {
		    filter_free (filte);
		    return FALSE;
		}
		filth->flt_next = filte;
		filtg = joinfilter (filth, FILTER_OR);

		filta = strfilter (at_OrgUnit,s,SUBSTRINGS ());
		filtc = strfilter (at_Locality,s,SUBSTRINGS ());
		filtc -> flt_next = filta;
		if ((ufn_flags & UFN_APPROX) && !index (s, '*')) {
		    filtb = strfilter (at_OrgUnit,s,FILTERITEM_APPROX);
		    filtb -> flt_next = filtc;
		    filtd = strfilter (at_Locality,s,FILTERITEM_APPROX);
		    filtd -> flt_next = filtb;
		    filt = joinfilter (filtd, FILTER_OR);
		}
		else
		    filt = joinfilter (filtc, FILTER_OR);

		filtg->flt_next = filt;
		filtf = joinfilter (filtg, FILTER_AND);
	} else if ( present (base,at_Locality) ) {
		if ((filte = ocfilter ("Organization")) == NULLFILTER)
			return FALSE;

		filta = strfilter (at_Organisation,s,SUBSTRINGS ());
		if ((ufn_flags & UFN_APPROX) && !index (s, '*')) {
		    filtb = strfilter (at_Organisation,s,FILTERITEM_APPROX);
		    filtb->flt_next = filta;
		    filt = joinfilter (filtb, FILTER_OR);
		}
		else
		    filt = filta;

		filte->flt_next = filt;

		filtf = joinfilter (filte, FILTER_AND);
	} else {
		if ((filte = ocfilter ("Organization")) == NULLFILTER)
		    return FALSE;
		if ((filth = ocfilter ("Locality")) == NULLFILTER) {
		    filter_free (filte);
		    return FALSE;
		}
		filth -> flt_next = filte;
		filtg = joinfilter (filth, FILTER_OR);

		filta = strfilter (at_Organisation,s,SUBSTRINGS ());
		filtc = strfilter (at_Locality,s,SUBSTRINGS ());
		filtc -> flt_next = filta;
		if ((ufn_flags & UFN_APPROX) && !index (s, '*')) {
		    filtb = strfilter (at_Organisation,s,FILTERITEM_APPROX);
		    filtb -> flt_next = filtc;
		    filtd = strfilter (at_Locality,s,FILTERITEM_APPROX);
		    filtd -> flt_next = filtb;
		    filt = joinfilter (filtd, FILTER_OR);
		}
		else
		    filt = joinfilter (filtc, FILTER_OR);

		filtg->flt_next = filt;
		filtf = joinfilter (filtg, FILTER_AND);
	}

    return ufn_search (base,FALSE,filtf,result,s,interact,el);
}

static leafSearch (base,s,subtree,interact,el,result)
DN base;
char * s;
char subtree;
DNS (* interact) ();
DNS el;
DNS * result;
{
Filter filt, filta, filtb, filtc, filtd, filte, filtf;

	filta = strfilter (at_CommonName,s,SUBSTRINGS ());
	filtc = strfilter (at_Surname,s,SUBSTRINGS ());
	filtc -> flt_next = filta;
	filte = strfilter (at_Userid,s,SUBSTRINGS ());
	filte -> flt_next = filtc;
	if ((ufn_flags & UFN_APPROX) && !index (s, '*')) {
	    filtb = strfilter (at_CommonName,s,FILTERITEM_APPROX);
	    filtb -> flt_next = filte;
	    filtd = strfilter (at_Surname,s,FILTERITEM_APPROX);
	    filtd -> flt_next = filtb;
	    filtf = strfilter (at_Userid,s,FILTERITEM_APPROX);
	    filtf -> flt_next = filtd;
	    filt = joinfilter (filtf, FILTER_OR);
	}
	else
	    filt = joinfilter (filte, FILTER_OR);

	return ufn_search (base,subtree,filt,result,s,interact,el);
}

static keyedSearch (base,t,v,interact,el,result)
DN base;
char * t, *v; 
DNS (* interact) ();
DNS el;
DNS * result;
{
Filter filt, filta, filtb;
AttributeType at;

	if ((at = AttrT_new (t)) == NULLAttrT) {
		NOTIFY (("Invalid Key !"));
		return FALSE;
	}
	if ( ! sub_string (at->oa_syntax)) {
		NOTIFY (("String types only !"));
		return FALSE;
	}

	filta = strfilter (at,v,SUBSTRINGS ());
	if ((ufn_flags & UFN_APPROX) && !index (v, '*')) {
	    filtb = strfilter (at,v,FILTERITEM_APPROX);
	    filtb -> flt_next = filta;
	    filt = joinfilter (filtb, FILTER_OR);
	}
	else
	    filt = filta;

	return ufn_search (base, base && base -> dn_parent, filt, result, v,
			   interact, el);
}


static purportedMatch(base,c,v,interact,el,result)
DN base;
int c;
char ** v;
DNS (* interact) ();
DNS el;
DNS * result;
{
char * s;
DNS root, x, new = NULLDNS;
char * ptr;
int matches;

	s = TidyString (v[c-1]);

	if (c == 1) {

		if ((ptr = index (s,'=')) != NULLCP) {
			*ptr++ = 0;
			matches = keyedSearch (base,SkipSpace(s),SkipSpace(ptr),interact,el,result);
			*(--ptr) = '=';
			return matches;
		} else if (base == NULLDN) {
			matches = rootSearch(s,interact,el,result);
			if (*result != NULLDNS)
				return matches;
			else if (matches == TRUE)
				return TRUE;
			else
				return leafSearch (base,s,FALSE,interact,el,result);
		} else if (base->dn_parent == NULLDN) {
			/* length == 1 */
			matches = intSearch (base,s,interact,el,result);
			if (*result != NULLDNS)
				return matches;
			else if (matches == TRUE)
				return TRUE;
			else
				return leafSearch (base,s,FALSE,interact,el,result);
		} else {
			matches = leafSearch (base,s,TRUE,interact,el,result);
			if (*result != NULLDNS)
				return matches;
			else if (matches == TRUE)
				return TRUE;
			else
				return intSearch (base,s,interact,el,result);
		}
	}

	if ((ptr = index (s,'=')) != NULLCP) {
		*ptr++ = 0;
		if ( ! (matches = keyedSearch (base,SkipSpace(s),SkipSpace(ptr),interact,el,&root))) {
			*(--ptr) = '=';
			return FALSE;
		}
		*(--ptr) = '=';
	} else if (base == NULLDN) {
		if ( ! (matches = rootSearch (s,interact,el,&root)))
			return FALSE;
	} else {
		if ( ! (matches = intSearch (base,s,interact,el,&root)))
			return FALSE;
	}

	for (x = root; x != NULLDNS; x = x->dns_next) {
		if (purportedMatch (x->dns_dn, c-1, v,interact,el,&new)) {
			if (new != NULLDNS)
				*result = DNS_append (*result,new);
		} else
			return FALSE;
	}

	return matches;
}

static envMatch (c,v,el,interact,result)
int c;
char ** v;
DNS el;
DNS (* interact) ();
DNS * result;
{
int res;

	if (el == NULLDNS)
		return TRUE;

	if ( ! ( res = purportedMatch(el->dns_dn,c,v,interact,el,result)))
		return FALSE;
	if (*result != NULLDNS)	
		return res;
	if (res == TRUE)
		return TRUE;		

	return envMatch(c,v,el->dns_next,interact,result);

}

static	friendlyMatch_aux (c,v,el,interact,result)
int c;
char ** v;
envlist el;
DNS (* interact) ();
DNS * result;
{
	if (el == NULLEL)
		return TRUE;

	if ( ( c <= el->Upper) && (c >= el->Lower) ) 
		return envMatch (c,v,el->Dns,interact,result);

	return (friendlyMatch_aux (c,v,el->Next,interact,result));

}

envlist read_envlist()
{
char * home, *p, *ptr;
char ufnrc [LINESIZE];
char * def;
char buffer [LINESIZE];
envlist env, top = NULLEL, trail = NULLEL;
DNS dtail = NULLDNS;
FILE * file;
DN dn;
int i = 0;

	if (home = getenv ("UFNRC"))
		(void) strcpy (ufnrc, home);
	else 
		if (home = getenv ("HOME"))
			(void) sprintf (ufnrc, "%s/.ufnrc", home);
		else
			(void) strcpy (ufnrc, "./.ufnrc");

	if ((file = fopen (ufnrc,"r")) == 0) {
		def = isodefile("ufnrc",0);
		if ((file = fopen(def,"r")) == 0) {
			(void) sprintf (PY_pepy,"Can't open '%s' or '%s'",ufnrc,def);
			return NULLEL;
		}
	}

	while (fgets (buffer, LINESIZE, file) != 0) {
		i++;
		p = buffer;
		if (( *p == '#') || (*p == '\0') || (*p == '\n'))
			continue; 	/* ignore comments and blanks */

		if (isspace (*p)) {
			/* part of current environment */
			if (!dtail) {
				(void) sprintf (PY_pepy, "Unexpected blank at start of line %d",i);
				(void) fclose (file);
				return NULLEL;
			}
			p = TidyString(p);
			if (*p == '-')
				dn = NULLDN;
			else if ((dn = str2dn (p)) == NULLDN) {
				(void) sprintf (PY_pepy, "Bad DN in environment file line %d",i);
				(void) fclose (file);
				return NULLEL;
			}
			dtail->dns_next = dn_seq_alloc();
			dtail = dtail->dns_next;
			dtail->dns_next = NULLDNS;
			dtail->dns_dn   = dn;
			continue;
		}

		p = TidyString (p);

		if ((ptr = index (p,':')) == NULLCP) {
			(void) sprintf (PY_pepy, "':' missing in environment file line %d",i);
			(void) fclose (file);
			return NULLEL;
		}

		*ptr++ = 0;
		ptr = SkipSpace (ptr);

		if (*ptr == '-')
			dn = NULLDN;
		else if ((dn = str2dn (ptr)) == NULLDN) {
			(void) sprintf (PY_pepy, "Bad DN in environment file line %d",i);
			(void) fclose (file);
			return NULLEL;
		}

		env = (envlist) smalloc (sizeof (*env));
		dtail = env->Dns = dn_seq_alloc();
		dtail->dns_next = NULLDNS;
		dtail->dns_dn   = dn;
		env->Next = NULLEL;
		if (top == NULLEL)
			top = env;
		else 
			trail->Next = env;
		trail = env;

		if ((ptr = index (p,',')) != NULLCP) {
			*ptr++ = 0;
			ptr = SkipSpace(ptr);
			if (*ptr == '+')
				env->Upper = 32767;	/* ~= infinity */
			else 
				env->Upper = atoi (ptr);	/* how to test error ? */
		} else
			env->Upper = 0;
		
		p = SkipSpace(p);

		env->Lower = atoi (p);		/* how to test error ? */

		if ( ! env->Upper)
			env->Upper = env->Lower;

	}

	(void) fclose (file);

	return top;
}


ufn_match (c,v,interact,result,el)
int c;
char ** v;
DNS (* interact) ();
DNS * result;
envlist el;
{
static int inited = FALSE;

	if ( (!ufnas) && !(inited = ufn_init()))
		return inited;
	
	PY_pepy[0] = NULL;
	if (el == NULLEL) {
		if ((el = read_envlist ()) == NULLEL) {
			(void) sprintf (PY_pepy,"Can't read environment");
			return 0;
		}
	}

	return (friendlyMatch_aux (c,v,el,interact,result));
}

ufn_init ()
{
Attr_Sequence as;
int result = TRUE;

	if (ufnas)
	    return result;

	if ((at_ObjectClass = AttrT_new ("ObjectClass")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("ObjectClass attribute unknown"));
	}

	if ((at_OrgUnit	= AttrT_new ("ou")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("ou attribute unknown"));
	}
	if ((at_Organisation = AttrT_new ("o")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("o attribute unknown"));
	}
	if ((at_Locality = AttrT_new ("l")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("l attribute unknown"));
	}
	if ((at_CountryName = AttrT_new ("c")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("c attribute unknown"));
	}
	if ((at_FriendlyCountryName = AttrT_new ("FriendlyCountryName")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("FriendlyCountryName attribute unknown"));
	}
	if ((at_CommonName = AttrT_new (CN_OID)) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("cn attribute unknown"));
	}
	if ((at_Surname	= AttrT_new ("sn")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("sn attribute unknown"));
	}
	if ((at_Userid = AttrT_new ("uid")) == NULLAttrT) {
		result = FALSE;
		LLOG (log_dsap,LLOG_EXCEPTIONS,("uid attribute unknown"));
	}

	ufnas = as_comp_new (at_OrgUnit,NULLAV,NULLACL_INFO);
	as = as_comp_new (at_Organisation,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);
	as = as_comp_new (at_Locality,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);
	as = as_comp_new (at_CountryName,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);
	as = as_comp_new (at_FriendlyCountryName,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);
	as = as_comp_new (at_CommonName,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);
	as = as_comp_new (at_Surname,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);
	as = as_comp_new (at_Userid,NULLAV,NULLACL_INFO);
	ufnas = as_merge (ufnas,as);

	return result;
}

#ifdef	DEBUG

static	print_search (dn, subtree, fi)
DN	dn;
char	subtree;
Filter	fi;
{
static	PS	nps = NULLPS;

    if (nps == NULLPS) {
	if ((nps = ps_alloc (std_open)) == NULL) {
	    (void) fprintf (stderr, "ps_alloc(std_open): you lose\n");
	    return;
	}
	if (std_setup (nps, stdout) == NOTOK) {
	    (void) fprintf (stderr, "std_setup(stdout): you lose\n");
	    ps_free (nps);
	    nps = NULL;
	    return;
	}
    }
    
    ps_printf (nps, "search starting at @");
    dn_print (nps, dn, EDBOUT);
    ps_printf (nps, "(%s) for ", subtree ? "subtree" : "singlelevel");

    print_filter (nps, fi, 0);

    ps_print (nps, "\n\n");
    (void) ps_flush (nps);
}
#endif
