/* task_act.c - routines to manipulate task activity blocks */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/task_act.c,v 7.2 91/02/22 09:39:57 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/task_act.c,v 7.2 91/02/22 09:39:57 mrose Interim $
 *
 *
 * $Log:	task_act.c,v $
 * Revision 7.2  91/02/22  09:39:57  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:50  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:11  mrose
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
#include "quipu/connection.h"

extern LLog * log_dsap;

struct task_act *task_alloc()
{
    struct task_act	* tk_ret;

    tk_ret = (struct task_act *) calloc(1,sizeof(struct task_act));

    tk_ret->tk_result = &(tk_ret->tk_resp.di_result.dr_res);
    tk_ret->tk_error = &(tk_ret->tk_resp.di_error.de_err);

    return(tk_ret);
}

task_free(tk)
struct task_act	* tk;
{
    DLOG(log_dsap, LLOG_TRACE, ("task_free()"));

    op_arg_free (&(tk->tk_dx.dx_arg));

    if (tk->tk_resp.di_type == DI_ERROR)
        ds_error_free (tk->tk_error);
    else if (tk->tk_resp.di_type == DI_RESULT)
	if (tk->tk_conn->cn_ctx == DS_CTX_X500_DAP)
	    ds_res_free (&(tk->tk_result->dcr_dsres));
	else
	    op_res_free (tk->tk_result);

    free((char *)tk);
}

/*
*  Extract task from list held by the connection it was received on.
*/
task_conn_extract(tk)
struct task_act	* tk;
{
    struct task_act	* tk_tmp;
    struct task_act	**tk_p;

	DLOG(log_dsap, LLOG_TRACE, ("task_conn_extract()"));

    tk_p = &(tk->tk_conn->cn_tasklist);
    for(tk_tmp = (*tk_p); tk_tmp!=NULLTASK; tk_tmp=tk_tmp->tk_next)
    {
	if(tk_tmp == tk)
	    break;

	tk_p = &(tk_tmp->tk_next);
    }
    if(tk_tmp == NULLTASK)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("task_conn_extract: task lost from connections list."));
    }
    else
    {
	(*tk_p) = tk->tk_next;
    }
}

task_extract(tk)
struct task_act	* tk;
{
    struct oper_act	* on;

    DLOG (log_dsap,LLOG_TRACE, ("task_extract()"));
    if(tk == NULLTASK)
    {
	LLOG (log_dsap,LLOG_FATAL,("Attempted to extract NULLTASK!!"));
	return;
    }

    for(on = tk->tk_operlist; on!=NULLOPER; on = on->on_next_task)
    {
	oper_task_extract(on);
	if(on->on_conn == NULLCONN)
	    oper_free(on);
    }

    task_free(tk);
    DLOG (log_dsap,LLOG_TRACE, ("task block extracted"));
}

task_log(tk)
struct task_act	* tk;
{
    struct oper_act	* on;

    DLOG (log_dsap,LLOG_NOTICE, ("Task id = %d, state = %d, prio = %d.", 
				tk->tk_dx.dx_id, tk->tk_state, tk->tk_prio));
    if(tk->tk_operlist != NULLOPER)
	DLOG (log_dsap,LLOG_DEBUG, ("Task-Opers:"));
    for(on=tk->tk_operlist; on != NULLOPER; on = on->on_next_task)
	oper_log(on);
}

