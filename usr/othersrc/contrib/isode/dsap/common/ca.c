/* ca.c - General Directory Name routines */

#ifndef lint
static char *rcsid = "$Header: /f/osi/dsap/common/RCS/ca.c,v 7.3 91/02/22 09:18:43 mrose Interim $";
#endif

/*
 * $Header: /f/osi/dsap/common/RCS/ca.c,v 7.3 91/02/22 09:18:43 mrose Interim $
 *
 *
 * $Log:	ca.c,v $
 * Revision 7.3  91/02/22  09:18:43  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:41:24  mrose
 * sync
 * 
 * Revision 7.1  89/12/19  16:19:13  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:41:55  mrose
 * Release 6.0
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
#include "quipu/common.h"
#include "quipu/dsargument.h"

DN mydsadn = NULLDN;	

struct common_args      * get_ca_ref(dsarg)
struct ds_op_arg        * dsarg;
{
    struct common_args  * ca;

    switch(dsarg->dca_dsarg.arg_type)
    {
    case OP_READ :
	ca = &(dsarg->dca_dsarg.arg_rd.rda_common);
    break;
    case OP_COMPARE :
	ca = &(dsarg->dca_dsarg.arg_cm.cma_common);
    break;
    case OP_ABANDON :
	ca = NULL_COMMONARG;
    break;
    case OP_LIST :
	ca = &(dsarg->dca_dsarg.arg_ls.lsa_common);
    break;
    case OP_SEARCH :
	ca = &(dsarg->dca_dsarg.arg_sr.sra_common);
    break;
    case OP_ADDENTRY :
	ca = &(dsarg->dca_dsarg.arg_ad.ada_common);
    break;
    case OP_REMOVEENTRY :
	ca = &(dsarg->dca_dsarg.arg_rm.rma_common);
    break;
    case OP_MODIFYENTRY :
	ca = &(dsarg->dca_dsarg.arg_me.mea_common);
    break;
    case OP_MODIFYRDN :
	ca = &(dsarg->dca_dsarg.arg_mr.mra_common);
    break;
    case OP_GETEDB :
	ca = NULL_COMMONARG;
    break;
    }
    return(ca);
}

cha_loopdetected(cha)
struct chain_arg	* cha;
{
    struct trace_info	  ti_elem_s;
    struct trace_info	* ti_elem = &(ti_elem_s);

    ti_elem->ti_dsa = mydsadn;
    ti_elem->ti_target = cha->cha_target;
    ti_elem->ti_progress.op_resolution_phase = cha->cha_progress.op_resolution_phase;
    ti_elem->ti_progress.op_nextrdntoberesolved = cha->cha_progress.op_nextrdntoberesolved;

    return(ti_is_elem(ti_elem, cha->cha_trace));
}

ti_is_elem(ti, ti_list)
struct trace_info	* ti;
struct trace_info	* ti_list;
{
    struct trace_info	* tip;

    for(tip = ti_list; tip!=NULLTRACEINFO; tip=tip->ti_next)
    {
	if(dn_cmp(ti->ti_dsa, tip->ti_dsa) != 0)
	    continue;

	if(dn_cmp(ti->ti_target, tip->ti_target) != 0)
	    continue;

	if(ti->ti_progress.op_resolution_phase == tip->ti_progress.op_resolution_phase)
	    return(1);

	if(ti->ti_progress.op_nextrdntoberesolved == tip->ti_progress.op_nextrdntoberesolved)
	    return(1);
    }

    return(0);
}

struct trace_info	* ti_cpy(ti)
struct trace_info	* ti;
{
    struct trace_info	* ret_ti;

    if(ti == NULLTRACEINFO)
	return(NULLTRACEINFO);

    ret_ti = (struct trace_info *) malloc(sizeof(struct trace_info));

    ret_ti->ti_target = dn_cpy(ti->ti_target);
    ret_ti->ti_dsa = dn_cpy(ti->ti_dsa);
    ret_ti->ti_progress = ti->ti_progress;
    ret_ti->ti_next = ti_cpy(ti->ti_next);

    return(ret_ti);
}

