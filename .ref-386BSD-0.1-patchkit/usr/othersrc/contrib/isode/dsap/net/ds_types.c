/* ds_types.c - Bind argument and operation argument type routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/ds_types.c,v 7.2 91/02/22 09:21:11 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/ds_types.c,v 7.2 91/02/22 09:21:11 mrose Interim $
 *
 *
 * $Log:	ds_types.c,v $
 * Revision 7.2  91/02/22  09:21:11  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:43:27  mrose
 * sync
 * 
 * Revision 7.0  90/07/26  14:45:33  mrose
 * *** empty log message ***
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/bind.h"
#include "quipu/common.h"
#include "quipu/dsargument.h"

extern LLog * log_dsap;

bind_arg_free (arg)
struct ds_bind_arg          *arg;
{
	if (arg->dba_time1 != NULLCP)
		free (arg->dba_time1);

	if (arg->dba_time2 != NULLCP)
		free (arg->dba_time2);

	dn_free (arg->dba_dn);

	/* free signature */
	/* free certificate_list */
}

op_arg_free (arg)
struct ds_op_arg * arg;
{
	DLOG(log_dsap, LLOG_TRACE, ("op_arg_free()"));

	if (arg->dca_dsarg.arg_type == -1 )
		/* Already freed - argument part anyway */
		return;

	DLOG(log_dsap, LLOG_TRACE, ("op_arg_free() - for real"));


	switch (arg->dca_dsarg.arg_type)
	{
	    case OP_ABANDON:

		ds_arg_free (&(arg->dca_dsarg));
		break;

	    case OP_GETEDB:
	    case OP_READ:
	    case OP_COMPARE:
	    case OP_LIST:
	    case OP_SEARCH:
	    case OP_ADDENTRY:
	    case OP_REMOVEENTRY:
	    case OP_MODIFYENTRY:
	    case OP_MODIFYRDN:

		ch_arg_free (&(arg->dca_charg));
		ds_arg_free (&(arg->dca_dsarg));
		break;

	    default:
		break;
	}
}

ch_arg_free (arg)
struct chain_arg * arg;
{
	DLOG(log_dsap, LLOG_TRACE, ("ch_arg_free()"));

	dn_free (arg->cha_originator);

	dn_free (arg->cha_target);

	if (arg->cha_domaininfo != NULLPE)
		pe_free (arg->cha_domaininfo);

	if(arg->cha_trace != NULLTRACEINFO)
		trace_info_free (arg->cha_trace);

	if (arg->cha_timelimit != NULLCP)
		free (arg->cha_timelimit);
}

ds_arg_free (arg)
struct DSArgument * arg;
{
	DLOG(log_dsap, LLOG_TRACE, ("ds_arg_free()"));

	if (arg->arg_type == -1 )
		/* Already freed */
		return;

	DLOG(log_dsap, LLOG_TRACE, ("ds_arg_free() for real"));

	/* free argument structure */
	switch (arg->arg_type) {
	case OP_READ:
		read_arg_free (&(arg->arg_rd));
		break;
	case OP_COMPARE:
		compare_arg_free (&(arg->arg_cm));
		break;
	case OP_ABANDON:
		break;
	case OP_LIST:
		list_arg_free (&(arg->arg_ls));
		break;
	case OP_SEARCH:
		search_arg_free (&(arg->arg_sr));
		break;
	case OP_ADDENTRY:
		addentry_arg_free (&(arg->arg_ad));
		break;
	case OP_REMOVEENTRY:		
		removeentry_arg_free (&(arg->arg_rm));
		break;
	case OP_MODIFYENTRY:
		modifyentry_arg_free (&(arg->arg_me));
		break;
	case OP_MODIFYRDN:
		modifyrdn_arg_free (&(arg->arg_mr));
		break;
	case OP_GETEDB:
		getedb_arg_free (&(arg->arg_ge));
		break;
	default:
		break;
	}

	/* indicate as freed */
	arg->arg_type = -1;
}

read_arg_free (arg)
struct ds_read_arg	* arg;
{
	ca_free (&arg->rda_common);
	dn_free (arg->rda_object);
	as_free (arg->rda_eis.eis_select);
}

compare_arg_free (arg)
struct ds_compare_arg	* arg;
{
	ca_free (&arg->cma_common);
	dn_free (arg->cma_object);
	AttrT_free (arg->cma_purported.ava_type);
	AttrV_free (arg->cma_purported.ava_value);
}

list_arg_free (arg)
struct ds_list_arg	* arg;
{
	ca_free (&arg->lsa_common);
	dn_free (arg->lsa_object);
}

search_arg_free (arg)
struct ds_search_arg	* arg;
{
	ca_free (&arg->sra_common);
	dn_free (arg->sra_baseobject);
	as_free (arg->sra_eis.eis_select);
	filter_free (arg->sra_filter);
}

addentry_arg_free (arg)
struct ds_addentry_arg	* arg;
{
	ca_free (&arg->ada_common);
	dn_free (arg->ada_object);
	as_free (arg->ada_entry);
}

removeentry_arg_free (arg)
struct ds_removeentry_arg	* arg;
{
	ca_free (&arg->rma_common);
	dn_free (arg->rma_object);
}

modifyentry_arg_free (arg)
struct ds_modifyentry_arg	* arg;
{
	ca_free (&arg->mea_common);
	dn_free (arg->mea_object);
	ems_free (arg->mea_changes);
}

modifyrdn_arg_free (arg)
struct ds_modifyrdn_arg	* arg;
{
	ca_free (&arg->mra_common);
	dn_free (arg->mra_object);
	rdn_free (arg->mra_newrdn);
}

getedb_arg_free (arg)
struct getedb_arg	* arg;
{
	dn_free (arg->ga_entry);
	free ((char *)arg->ga_version);
}

op_res_free (res)
struct ds_op_res * res;
{
	DLOG(log_dsap, LLOG_TRACE, ("op_res_free()"));

	if (res->dcr_dsres.result_type == -1 )
		/* Already freed - result part anyway */
		return;

	DLOG(log_dsap, LLOG_TRACE, ("op_res_free() - for real"));

	switch (res->dcr_dsres.result_type)
	{
	    case OP_ABANDON:
	    case OP_GETEDB:

		ds_res_free (&(res->dcr_dsres));
		break;

	    case OP_READ:
	    case OP_COMPARE:
	    case OP_LIST:
	    case OP_SEARCH:
	    case OP_ADDENTRY:
	    case OP_REMOVEENTRY:
	    case OP_MODIFYENTRY:
	    case OP_MODIFYRDN:

		ch_res_free (&(res->dcr_chres));
		ds_res_free (&(res->dcr_dsres));
		break;

	    default:
		break;
	}

}

ch_res_free (res)
struct chain_res	* res;
{
	DLOG(log_dsap, LLOG_TRACE, ("ch_res_free()"));

	/* free chain bits */
	if (res->chr_domaininfo != NULLPE)
		pe_free (res->chr_domaininfo);

	cross_refs_free (res->chr_crossrefs);
}

ds_res_free (res)
struct DSResult	* res;
{
	DLOG(log_dsap, LLOG_TRACE, ("ds_res_free()"));

	if (res->result_type == -1 )
		/* Already freed */
		return;

	DLOG(log_dsap, LLOG_TRACE, ("ds_res_free() - for real"));

	switch (res->result_type) {
	case OP_READ:
/*
		dn_free (&res->res_rd.rdr_common.cr_requestor);
*/
		entryinfo_comp_free (&res->res_rd.rdr_entry,0);
		break;
	case OP_COMPARE:
/*
		dn_free (&res->res_cm.cmr_common.cr_requestor);
*/
		dn_free (res->res_cm.cmr_object);
		break;
	case OP_LIST:
/*
		dn_free (&res->res_ls.lsr_common.cr_requestor);
*/
		dn_free (res->res_ls.lsr_object);
		subords_free (res->res_ls.lsr_subordinates);
		crefs_free (res->res_ls.lsr_cr);
		break;
	case OP_SEARCH:
		search_result_free(&(res->res_sr));
		break;
	case OP_GETEDB:
		break;	/* don't free EDB */
	default:
		break;
	}

	res->result_type = -1;
}

trace_info_free (ti)
struct trace_info * ti;
{
	DLOG(log_dsap, LLOG_TRACE, ("trace_info_free()"));

	if (ti == NULLTRACEINFO)
		return;
	dn_free (ti->ti_target);
	dn_free (ti->ti_dsa);
	trace_info_free (ti->ti_next);
	free( (char *) ti);

}

ca_free (ca)
CommonArgs * ca;
{
	DLOG(log_dsap, LLOG_TRACE, ("ca_free()"));

	dn_free (ca->ca_requestor);
	extension_free (ca->ca_extensions);
}

extension_free (ext)
struct extension * ext;
{
	DLOG(log_dsap, LLOG_TRACE, ("extension_free()"));

	for (; ext != NULLEXT; ext=ext->ext_next) {
		if (ext->ext_item != NULLPE)
			pe_free (ext->ext_item);
		free ((char *) ext);
	}
}

cross_refs_free (xref)
struct cross_ref * xref;
{
	DLOG(log_dsap, LLOG_TRACE, ("cross_refs_free()"));

	if (xref == NULLXREF)
		return;
	dn_free (xref->xref_dn);
	aps_free (xref->xref_ap);
	cross_refs_free (xref->xref_next);
	free ((char *)xref);
}

/* Copy routines */

int	  ds_arg_dup (src, tgt)
struct DSArgument * src;
struct DSArgument * tgt;
{
	DLOG(log_dsap, LLOG_TRACE, ("ds_arg_dup()"));

	if (src->arg_type == -1 )
	{
		/* Has been freed */
		LLOG (log_dsap, LLOG_DEBUG, ("ds_arg_dup(): src already freed"));
		return (NOTOK);
	}

	switch (tgt->arg_type = src->arg_type) {
	case OP_READ:
		return (read_arg_dup (&(src->arg_rd), &(tgt->arg_rd)));
	case OP_COMPARE:
		return (compare_arg_dup (&(src->arg_cm), &(tgt->arg_cm)));
	case OP_ABANDON:
		return (abandon_arg_dup (&(src->arg_ab), &(tgt->arg_ab)));
	case OP_LIST:
		return (list_arg_dup (&(src->arg_ls), &(tgt->arg_ls)));
	case OP_SEARCH:
		return (search_arg_dup (&(src->arg_sr), &(tgt->arg_sr)));
	case OP_ADDENTRY:
		return (addentry_arg_dup (&(src->arg_ad), &(tgt->arg_ad)));
	case OP_REMOVEENTRY:		
		return (removeentry_arg_dup (&(src->arg_rm), &(tgt->arg_rm)));
	case OP_MODIFYENTRY:
		return (modifyentry_arg_dup (&(src->arg_me), &(tgt->arg_me)));
	case OP_MODIFYRDN:
		return (modifyrdn_arg_dup (&(src->arg_mr), &(tgt->arg_mr)));
	case OP_GETEDB:
		return (getedb_arg_dup (&(src->arg_ge), &(tgt->arg_ge)));
	default:
		LLOG(log_dsap, LLOG_DEBUG, ("ds_arg_dup(): unknown arg type %d", src->arg_type));
		return (NOTOK);
	}
}

int	  read_arg_dup (src, tgt)
struct ds_read_arg	* src;
struct ds_read_arg	* tgt;
{
	if (ca_dup (&(src->rda_common), &(tgt->rda_common)) != OK)
	    return (NOTOK);

	if (src->rda_object == NULLDN)
		tgt->rda_object = NULLDN;
	else if ((tgt->rda_object = dn_cpy (src->rda_object)) == NULLDN)
		return (NOTOK);

	if (eis_dup (&(src->rda_eis), &(tgt->rda_eis)) != OK)
	    return (NOTOK);

	return (OK);
}

int	compare_arg_dup (src, tgt)
struct ds_compare_arg	* src;
struct ds_compare_arg	* tgt;
{
	if (ca_dup (&(src->cma_common), &(tgt->cma_common)) != OK)
	    return (NOTOK);

	if (src->cma_object == NULLDN)
		tgt->cma_object = NULLDN;
	else if ((tgt->cma_object = dn_cpy (src->cma_object)) == NULLDN)
		return (NOTOK);

	if (ava_dup (&(src->cma_purported), &(tgt->cma_purported)) != OK)
	    return (NOTOK);

	return (OK);
}

int	abandon_arg_dup (src, tgt)
struct ds_abandon_arg	* src;
struct ds_abandon_arg	* tgt;
{
	tgt->aba_invokeid = src->aba_invokeid;

	return (OK);
}

int	  list_arg_dup (src, tgt)
struct ds_list_arg	* src;
struct ds_list_arg	* tgt;
{
	if (ca_dup (&(src->lsa_common), &(tgt->lsa_common)) != OK)
	    return (NOTOK);

	if (src->lsa_object == NULLDN)
		tgt->lsa_object = NULLDN;
	else if ((tgt->lsa_object = dn_cpy (src->lsa_object)) == NULLDN)
		return (NOTOK);

	return OK;
}

int	  search_arg_dup (src, tgt)
struct ds_search_arg	* src;
struct ds_search_arg	* tgt;
{
struct filter	* filter_cpy();

	if (ca_dup (&(src->sra_common), &(tgt->sra_common)) != OK)
	    return (NOTOK);

	if (src->sra_baseobject = NULLDN)
		tgt->sra_baseobject = NULLDN;
	else if ((tgt->sra_baseobject = dn_cpy (src->sra_baseobject)) == NULLDN)
		return (NOTOK);

	tgt->sra_subset = src->sra_subset;

	if (src->sra_filter == NULLFILTER)
		tgt->sra_filter = NULLFILTER;
	else if ((tgt->sra_filter = filter_cpy (src->sra_filter)) == NULLFILTER)
		return (NOTOK);

	tgt->sra_searchaliases = src->sra_searchaliases;

	if (eis_dup (&(src->sra_eis), &(tgt->sra_eis)) != OK)
	    return (NOTOK);

	return (OK);
}

int	  addentry_arg_dup (src, tgt)
struct ds_addentry_arg	* src;
struct ds_addentry_arg	* tgt;
{
	if (ca_dup (&(src->ada_common), &(tgt->ada_common)) != OK)
	    return (NOTOK);

	if (src->ada_object == NULLDN)
		tgt->ada_object = NULLDN;
	else if ((tgt->ada_object = dn_cpy (src->ada_object)) == NULLDN)
		return (NOTOK);

	if (src->ada_entry = NULLATTR)
		tgt->ada_entry = NULLATTR;
	else if ((tgt->ada_entry = as_cpy (src->ada_entry)) == NULLATTR)
		return (NOTOK);

	return (OK);
}

int	  removeentry_arg_dup (src, tgt)
struct ds_removeentry_arg	* src;
struct ds_removeentry_arg	* tgt;
{
	if (ca_dup (&(src->rma_common), &(tgt->rma_common)) != OK)
	    return (NOTOK);

	if (src->rma_object == NULLDN)
		tgt->rma_object = NULLDN;
	else if ((tgt->rma_object = dn_cpy (src->rma_object)) == NULLDN)
		return (NOTOK);

	return (OK);
}

int	  modifyentry_arg_dup (src, tgt)
struct ds_modifyentry_arg	* src;
struct ds_modifyentry_arg	* tgt;
{
struct entrymod	* ems_cpy();

	if (ca_dup (&(src->mea_common), &(tgt->mea_common)) != OK)
	    return (NOTOK);

	if (src->mea_object == NULLDN)
		tgt->mea_object = NULLDN;
	else if ((tgt->mea_object = dn_cpy (src->mea_object)) == NULLDN)
		return (NOTOK);

	if (src->mea_changes == (struct entrymod *) NULL)
		tgt->mea_changes = (struct entrymod *) NULL;
	else if ((tgt->mea_changes = ems_cpy (src->mea_changes)) == (struct entrymod *) NULL)
		return (NOTOK);

	return (OK);
}

int	  modifyrdn_arg_dup (src, tgt)
struct ds_modifyrdn_arg	* src;
struct ds_modifyrdn_arg	* tgt;
{
	if (ca_dup (&(src->mra_common), &(tgt->mra_common)) != OK)
	    return (NOTOK);

	if (src->mra_object == NULLDN)
		tgt->mra_object = NULLDN;
	else if ((tgt->mra_object = dn_cpy (src->mra_object)) == NULLDN)
		return (NOTOK);

	if (src->mra_newrdn == NULLRDN)
		tgt->mra_newrdn = NULLRDN;
	else if ((tgt->mra_newrdn = rdn_cpy (src->mra_newrdn)) == NULLRDN)
		return (NOTOK);

	tgt->deleterdn = src->deleterdn;

	return (OK);
}

int	  getedb_arg_dup (src, tgt)
struct getedb_arg	* src;
struct getedb_arg	* tgt;
{
	if (src->ga_entry == NULLDN)
		tgt->ga_entry = NULLDN;
	else if ((tgt->ga_entry = dn_cpy (src->ga_entry)) == NULLDN)
		return (NOTOK);

	tgt->ga_version = strdup (src->ga_version);
	tgt->get_next = NULL_GETARG;
	return (OK);
}

int	  ca_dup (src, tgt)
struct common_args	* src;
struct common_args	* tgt;
{
struct security_parms	* secp_cpy ();
struct signature	* sig_cpy ();
struct extension	* ext_cpy ();

	tgt->ca_servicecontrol = src->ca_servicecontrol; /* struct copy */

	if (src->ca_requestor = NULLDN)
		tgt->ca_requestor = NULLDN;
	else if ((tgt->ca_requestor = dn_cpy (src->ca_requestor)) == NULLDN)
		return (NOTOK);

	tgt->ca_progress = src->ca_progress; /* struct copy */

	tgt->ca_aliased_rdns = src->ca_aliased_rdns;

	if (src->ca_security = (struct security_parms *) NULL)
		tgt->ca_security = (struct security_parms *) NULL;
	else if ((tgt->ca_security = secp_cpy (src->ca_security)) == (struct security_parms *) NULL)
		return (NOTOK);

	if (src->ca_sig == (struct signature *) NULL)
		tgt->ca_sig = (struct signature *) NULL;
	else if ((tgt->ca_sig = sig_cpy (src->ca_sig)) == (struct signature *) NULL)
		return (NOTOK);

	if (src->ca_extensions == (struct extension *) NULL)
		tgt->ca_extensions = (struct extension *) NULL;
	else if ((tgt->ca_extensions = ext_cpy (src->ca_extensions)) == (struct extension *) NULL)
		return (NOTOK);

	return (OK);
}

struct security_parms	* secp_cpy (sp)
struct security_parms	* sp;
{
struct certificate_list	* cpair_cpy();
struct random_number	* random_cpy ();

	struct security_parms	* ret;

	if (sp == (struct security_parms *) NULL)
		return ((struct security_parms *) NULL);

	if ((ret = (struct security_parms *) smalloc (sizeof (struct security_parms))) == (struct security_parms *) NULL)
		return ((struct security_parms *) NULL);

	if (sp->sp_path == (struct certificate_list *) NULL)
		ret->sp_path = (struct certificate_list *) NULL;
	else if ((ret->sp_path = cpair_cpy (sp->sp_path)) == (struct certificate_list *) NULL)
		return ((struct security_parms *) NULL);

	if (sp->sp_name == NULLDN)
		ret->sp_name = NULLDN;
	else if ((ret->sp_name = dn_cpy (sp->sp_name)) == NULLDN)
		return ((struct security_parms *) NULL);

	ret->sp_time = strdup (sp->sp_time);

	if (sp->sp_random == (struct random_number *) NULL)
		ret->sp_random = (struct random_number *) NULL;
	else if ((ret->sp_random = random_cpy (sp->sp_random)) == (struct random_number *) NULL)
		return ((struct security_parms *) NULL);

	ret->sp_target = sp->sp_target;

	return (ret);
}

struct random_number	* random_cpy (rand)
struct random_number	* rand;
{
	struct random_number	* ret;

	if (rand == (struct random_number *) NULL)
		return ((struct random_number *) NULL);

	if ((ret = (struct random_number *) smalloc (sizeof (struct random_number))) == (struct random_number *) NULL)
		return ((struct random_number *) NULL);

	ret->n_bits = rand->n_bits;

	ret->value = strdup (rand->value);

	return (ret);
}

struct signature	* sig_cpy (sig)
struct signature	* sig;
{
	struct signature	* ret;

	if (sig == (struct signature *) NULL)
		return ((struct signature *) NULL);

	if ((ret = (struct signature *) smalloc (sizeof (struct signature))) == (struct signature *) NULL)
		return ((struct signature *) NULL);

	(void) alg_cpy (ret->alg, sig->alg);

	if (sig->encoded == NULLPE)
		ret->encoded = NULLPE;
	else
		ret->encoded = pe_cpy (sig->encoded);

	ret->n_bits = sig->n_bits;

	ret->encrypted = strdup (sig->encrypted);

	return (ret);
}

struct extension	* ext_cpy (ext)
struct extension	* ext;
{
	struct extension	* ret;

	if (ext == (struct extension *) NULL)
		return ((struct extension *) NULL);

	if ((ret = (struct extension *) smalloc (sizeof (struct extension))) == (struct extension *) NULL)
		return ((struct extension *) NULL);

	ret->ext_id = ext->ext_id;

	ret->ext_critical = ext->ext_critical;

	if (ext->ext_item == NULLPE)
		ret->ext_item = NULLPE;
	else
		ret->ext_item = pe_cpy (ext->ext_item);

	if (ext->ext_next == (struct extension *) NULL)
		ret->ext_next = (struct extension *) NULL;
	else if ((ret->ext_next = ext_cpy (ext->ext_next)) == (struct extension *) NULL)
		return ((struct extension *) NULL);

	return (ret);
}

struct filter	* filter_cpy(flt)
struct filter	* flt;
{
	struct filter	* ret;

	if (flt == (struct filter *) NULL)
		return ((struct filter *) NULL);

	if ((ret = (struct filter *) smalloc (sizeof (struct filter))) == (struct filter *) NULL)
		return ((struct filter *) NULL);

	switch (ret->flt_type = flt->flt_type)
	{
	case FILTER_ITEM:
		if (filter_item_dup (&(flt->flt_un.flt_un_item), &(ret->flt_un.flt_un_item)) != OK)
			return ((struct filter *) NULL);
		break;

	case FILTER_AND:
	case FILTER_OR:
	case FILTER_NOT:
		if (flt->flt_un.flt_un_filter == (struct filter *) NULL)
			ret->flt_un.flt_un_filter = (struct filter *) NULL;
		else if ((ret->flt_un.flt_un_filter = filter_cpy (flt->flt_un.flt_un_filter)) == (struct filter *) NULL)
			return ((struct filter *) NULL);
		break;

	default:
		DLOG (log_dsap, LLOG_DEBUG, ("filter_cpy(): unknown filter type %d", flt->flt_type));
		return ((struct filter *) NULL);
	}

	if (flt->flt_next == (struct filter *) NULL)
                ret->flt_next = (struct filter *) NULL;
	else if ((ret->flt_next = filter_cpy (flt->flt_next)) == (struct filter *) NULL)
		return ((struct filter *) NULL);

	return (ret);
}

int	  filter_item_dup (src, tgt)
struct filter_item	* src;
struct filter_item	* tgt;
{
	switch (tgt->fi_type = src->fi_type)
	{
	case FILTERITEM_EQUALITY:
	case FILTERITEM_GREATEROREQUAL:
	case FILTERITEM_LESSOREQUAL:
	case FILTERITEM_APPROX:
		if (ava_dup (&(src->fi_un.fi_un_ava), &(tgt->fi_un.fi_un_ava)) != OK)
			return (NOTOK);
		break;

	case FILTERITEM_PRESENT:
		if (src->fi_un.fi_un_type == NULLAttrT)
			tgt->fi_un.fi_un_type = NULLAttrT;
		else if ((tgt->fi_un.fi_un_type = AttrT_cpy (src->fi_un.fi_un_type)) == NULLAttrT)
			return (NOTOK);
		break;

	case FILTERITEM_SUBSTRINGS:
		if (fi_sub_dup (&(src->fi_un.fi_un_substrings), &(tgt->fi_un.fi_un_substrings)) != OK)
			return (NOTOK);
		break;

	default:
		DLOG (log_dsap, LLOG_DEBUG, ("filter_item_dup(): unknown filter_item type %d", src->fi_type));
		return (NOTOK);
	}

	tgt->fi_ifp = src->fi_ifp;

	return (OK);
}

int	  fi_sub_dup (src, tgt)
Filter_Substrings	* src;
Filter_Substrings	* tgt;
{
	if (src->fi_sub_type == NULLAttrT)
		tgt->fi_sub_type = NULLAttrT;
	else if ((tgt->fi_sub_type = AttrT_cpy (src->fi_sub_type)) == NULLAttrT)
		return (NOTOK);


	if (src->fi_sub_initial == NULLAV)
		tgt->fi_sub_initial = NULLAV;
	else if ((tgt->fi_sub_initial = avs_cpy (src->fi_sub_initial)) == NULLAV)
		return (NOTOK);

	if (src->fi_sub_any == NULLAV)
		tgt->fi_sub_any = NULLAV;
	else if ((tgt->fi_sub_any = avs_cpy (src->fi_sub_any)) == NULLAV)
		return (NOTOK);

	if (src->fi_sub_final == NULLAV)
		tgt->fi_sub_final = NULLAV;
	else if ((tgt->fi_sub_final = avs_cpy (src->fi_sub_final)) == NULLAV)
		return (NOTOK);

	tgt->fi_sub_match = strdup (src->fi_sub_match);

	return (OK);
}

struct entrymod	* ems_cpy(em)
struct entrymod	* em;
{
	struct entrymod	* ret;

	if (em == (struct entrymod *) NULL)
		return ((struct entrymod *) NULL);

	if ((ret = (struct entrymod *) smalloc (sizeof (struct entrymod))) == (struct entrymod *) NULL)
		return ((struct entrymod *) NULL);

	ret->em_type = em->em_type;

	if (em->em_what == NULLATTR)
		ret->em_what = NULLATTR;
	else if ((ret->em_what = as_cpy (em->em_what)) == NULLATTR)
		 return ((struct entrymod *) NULL);

	if (em->em_next == (struct entrymod *) NULL)
		ret->em_next = (struct entrymod *) NULL;
	else if ((ret->em_next = ems_cpy (em->em_next)) == (struct entrymod *) NULL)
		return ((struct entrymod *) NULL);

	return (ret);
}

int	  eis_dup(src, tgt)
struct entryinfoselection	* src;
struct entryinfoselection	* tgt;
{
	tgt->eis_allattributes = src->eis_allattributes;

        if (src->eis_select == NULLATTR)
                tgt->eis_select = NULLATTR;
        else if ((tgt->eis_select = as_cpy (src->eis_select)) == NULLATTR)
                 return (NOTOK);

	tgt->eis_infotypes = src->eis_infotypes;

	return (OK);
}

int	  ava_dup(src, tgt)
struct ava	* src;
struct ava	* tgt;
{
	if (src->ava_type == NULLAttrT)
		tgt->ava_type = NULLAttrT;
	else if ((tgt->ava_type = AttrT_cpy (src->ava_type)) == NULLAttrT)
		return (NOTOK);

	if (src->ava_value == NULLAttrV)
		tgt->ava_value = NULLAttrV;
	else if ((tgt->ava_value = AttrV_cpy (src->ava_value)) == NULLAttrV)
		return (NOTOK);

	return (OK);
}

