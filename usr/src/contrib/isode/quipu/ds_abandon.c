/* ds_abandon.c -  */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/ds_abandon.c,v 7.2 91/02/22 09:38:40 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/ds_abandon.c,v 7.2 91/02/22 09:38:40 mrose Interim $
 *
 *
 * $Log:	ds_abandon.c,v $
 * Revision 7.2  91/02/22  09:38:40  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:35  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:03  mrose
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


#include "quipu/util.h"
#include "quipu/attrvalue.h"
#include "quipu/abandon.h"
#include "quipu/dsp.h"
#include "quipu/ds_error.h"
#include "quipu/connection.h"

extern LLog * log_dsap;
/* ARGSUSED */

do_ds_abandon (arg, error)
    struct ds_abandon_arg       *arg;
    struct DSError              *error;
{
	DLOG (log_dsap,LLOG_TRACE,("ds_abandon"));

	error->dse_type = DSE_ABANDON_FAILED;
	error->ERR_ABANDON_FAIL.DSE_ab_problem = DSE_AB_CANNOTABANDON;
	error->ERR_ABANDON_FAIL.DSE_ab_invokeid = 0;
	return (NOTOK);
}

perform_abandon(tk)
struct task_act	* tk;
{
    struct task_act	* tk_tmp;
    struct task_act	**tk_p;
    int			  ab_id = tk->tk_dx.dx_arg.dca_dsarg.arg_ab.aba_invokeid;
    struct DSError	* err = &(tk->tk_resp.di_error.de_err);

    DLOG(log_dsap, LLOG_TRACE, ("perform_abandon"));

    tk_p = &(tk->tk_conn->cn_tasklist);
    for(tk_tmp = (*tk_p); tk_tmp!=NULLTASK; tk_tmp=tk_tmp->tk_next)
    {
	if(tk_tmp->tk_dx.dx_id == ab_id)
	    break;

	tk_p = &(tk_tmp->tk_next);
    }
    if(tk_tmp == NULLTASK)
    {
	LLOG(log_dsap, LLOG_NOTICE, ("perform_abandon - cannot find task to abandon"));
	err->dse_type = DSE_ABANDON_FAILED;
	err->ERR_ABANDON_FAIL.DSE_ab_problem = DSE_AB_NOSUCHOPERATION;
	err->ERR_ABANDON_FAIL.DSE_ab_invokeid = ab_id;
	return(NOTOK);
    }
    else
    {
	DLOG(log_dsap, LLOG_DEBUG, ("perform_abandon - found task to abandon"));

	/* Slice out task to abandon */
	(*tk_p) = tk_tmp->tk_next;

	if(task_abandon(tk_tmp) != OK)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("perform_abandon - task_abandon NOTOK"));
	    err->dse_type = DSE_ABANDON_FAILED;
	    err->ERR_ABANDON_FAIL.DSE_ab_problem = DSE_AB_CANNOTABANDON;
	    err->ERR_ABANDON_FAIL.DSE_ab_invokeid = ab_id;
	    return(NOTOK);
	}
	else
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("perform_abandon - task_abandon OK"));
	    tk->tk_result = &(tk->tk_resp.di_result.dr_res);
	    tk->tk_result->dcr_dsres.result_type = OP_ABANDON;
	    return(OK);
	}
    }
}

task_abandon(tk)
struct task_act	* tk;
{
    struct oper_act	* on;

    DLOG(log_dsap, LLOG_TRACE, ("task_abandon"));

    for(on = tk->tk_operlist; on != NULLOPER; on = on->on_next_task)
    {
	on->on_state = ON_ABANDONED;
	on->on_task = NULLTASK;
    }

    tk->tk_resp.di_error.de_err.dse_type = DSE_ABANDONED;
    task_error(tk);

    return(OK);
}

