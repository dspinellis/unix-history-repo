/* ufn_aei.c - user-friendly aei lookup */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/ufn_aet.c,v 7.3 91/02/22 09:20:41 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/common/RCS/ufn_aet.c,v 7.3 91/02/22 09:20:41 mrose Interim $
 *
 *
 * $Log:	ufn_aet.c,v $
 * Revision 7.3  91/02/22  09:20:41  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:43:06  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:35:17  mrose
 * sync
 * 
 * Revision 7.0  90/06/20  08:40:43  mrose
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
#include "quipu/list.h"
#include "quipu/ds_search.h"
#include "quipu/connection.h"	/* ds_search uses di_block - include this for lint !!! */
#include "quipu/dua.h"

extern LLog * log_dsap;
extern LLog * addr_log;

extern Filter ocfilter ();
extern Filter joinfilter ();

extern char PY_pepy[];
void	PY_advise ();


static Filter aet_filter (context)
char * context;
{
Filter a,b;

	if ((a = ocfilter ("ApplicationEntity")) == NULLFILTER)
		return NULLFILTER;

	b = filter_alloc ();
	b->flt_next = a;
	b->flt_type = FILTER_ITEM;
	b->FUITEM.fi_type = FILTERITEM_EQUALITY;
	if ((b->FUITEM.UNAVA.ava_type = AttrT_new (APPLCTX_OID)) == NULLAttrT) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("supported application context attribute unknown"))
		return NULLFILTER;
	}
	b->FUITEM.UNAVA.ava_value = str2AttrV(context,
		b->FUITEM.UNAVA.ava_type->oa_syntax);
	if (b->FUITEM.UNAVA.ava_value == NULLAttrV) {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("'%s' unknown OID",context));
		return NULLFILTER;
	}

	return joinfilter (b,FILTER_AND);
}

static aet_search (base, subtree, filt, res)
DN base;
char subtree;
Filter filt;
DNS * res;
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
	search_arg.sra_eis.eis_allattributes = TRUE;
	search_arg.sra_eis.eis_select = NULLATTR;

	if (ds_search (&search_arg, &err, &result) != DS_OK) {
		log_ds_error (&err);
		ds_error_free (&err);
		return FALSE;
	}

	correlate_search_results (&result);

	dn_free (result.CSR_object);

	if ( (result.CSR_limitproblem != LSR_NOLIMITPROBLEM) || (result.CSR_cr != NULLCONTINUATIONREF)) {
		crefs_free (result.CSR_cr);

		if ( ! result.CSR_entries)
			return FALSE;
	}

	for (ptr = result.CSR_entries; ptr != NULLENTRYINFO; ptr=ptr->ent_next) {
		cache_entry (ptr,FALSE,TRUE);
		newdns = dn_seq_alloc();
		newdns->dns_next = r;
		newdns->dns_dn = dn_cpy (ptr->ent_dn);
		r = newdns;
	}
	entryinfo_free (result.CSR_entries,0);

	*res = r;

	return TRUE;
}


aet_match (c,v,interact,result,el,context)
int c;
char ** v;
DNS *result;
DNS (* interact) ();
envlist el;
char * context;
{
DNS ufnr = NULLDNS;
DNS newap = NULLDNS;
DNS apps = NULLDNS;
DNS dns, DNS_append();
Filter filt;
int ok = TRUE;

	if (!ufn_match (c,v,interact,&ufnr,el)) {
		if (PY_pepy[0]) {
		    char buffer[BUFSIZ];

		    (void) sprintf (buffer, "ufn_match failed: %s", PY_pepy);
		    (void) strcpy (PY_pepy, buffer);
		    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
		}
		*result = NULLDNS;
		return FALSE;
	}

	filt = aet_filter(context);

	for (dns = ufnr; dns != NULLDNS; dns=dns->dns_next) {
		newap = NULLDNS;
		if (aet_search (dns->dns_dn,FALSE,filt,&newap))
			apps = DNS_append (apps,newap);
		else
		    ok = FALSE;
	}

	if ((apps == NULLDNS) && ok) {
		/* go deeper */
		for (dns = ufnr; dns != NULLDNS; dns=dns->dns_next) {
			if (dns->dns_dn->dn_parent == NULLDN)
				/* too high for subtree search */
				continue;
			newap = NULLDNS;
			if (aet_search (dns->dns_dn,TRUE,filt,&newap))
				apps = DNS_append (apps,newap);
			else
				ok = FALSE;
		}
	}

	if (!ok && !apps) {
	    PY_advise (NULLCP,
		       "search for applicationEntity supporting \"%s\" failed",
		       context);
	    SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP, ("%s", PY_pepy));
	}

	filter_free (filt);

	dn_seq_free (ufnr);

	*result = apps;

	return ok;
}
