/* oper_act.c - routines to handle operation activity blocks */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/oper_act.c,v 7.3 91/02/22 09:39:32 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/oper_act.c,v 7.3 91/02/22 09:39:32 mrose Interim $
 *
 *
 * $Log:	oper_act.c,v $
 * Revision 7.3  91/02/22  09:39:32  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:54:29  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:46:21  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:50  mrose
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

struct oper_act *oper_alloc()
{
    struct oper_act	* on_ret;

    on_ret = (struct oper_act *) calloc(1,sizeof(struct oper_act));

    on_ret->on_arg = &(on_ret->on_req);

    on_ret->on_relay = TRUE;	/* Relay unless reason not to. */

    return(on_ret);
}

oper_free(on)
struct oper_act *on;
{
extern struct oper_act * pending_ops;

	DLOG(log_dsap, LLOG_TRACE, ("oper_free()"));
	on->on_state = -1;

/*
	if (on->on_req.dca_charg.cha_trace != (struct trace_info *)NULL)
*/

		if (on->on_type == ON_TYPE_SUBTASK) 
			ch_arg_free (&on->on_req.dca_charg);
		else
			op_arg_free (&on->on_req);

	/* Not the best place to do this - but it will catch everything */
        if (on->on_next_task && ((on->on_type == ON_TYPE_GET_EDB) ||
				 (on->on_type == ON_TYPE_SHADOW))) {
		pending_ops = on->on_next_task;
		get_edb_ops = NULLOPER;
	}

	free((char *)on);
}

oper_extract(on)
struct oper_act	* on;
{
	DLOG(log_dsap, LLOG_TRACE, ("oper_extract()"));

    if(on->on_conn != NULLCONN)
	oper_conn_extract(on);

    if(on->on_task != NULLTASK)
	oper_task_extract(on);

    oper_free(on);
}

oper_conn_extract(on)
struct oper_act	* on;
{
    /*
    * Extract the operation activity block from the list held by its
    * connection.
    */
    struct oper_act	* on_tmp;
    struct oper_act	**on_p;

	DLOG(log_dsap, LLOG_TRACE, ("oper_conn_extract()"));

    if(on == NULLOPER)
    {
	LLOG (log_dsap,LLOG_FATAL, ("oper_conn_extract: Cannot extract NULLOPER"));
	return;
	/* This is an implementation error */
    }

    if(on->on_conn == NULLCONN)
    {
	LLOG (log_dsap,LLOG_EXCEPTIONS, ("oper_conn_extract: already extracted"));
	/* This operation must have already been extracted for some reason. */
	return;
    }

    on_p = &(on->on_conn->cn_operlist);
    for(on_tmp=(*on_p); on_tmp!=NULLOPER; on_tmp=on_tmp->on_next_conn)
    {
	if(on_tmp == on)
	    break;

	on_p = &(on_tmp->on_next_conn);
    }
    if(on_tmp == NULLOPER)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("oper_conn_extract: oper not on connections list!"));
    }
    else
    {
	(*on_p) = on_tmp->on_next_conn;
    }

    on->on_conn = NULLCONN; /* Shows that this has been conn_extracted */
}

oper_task_extract(on)
struct oper_act	* on;
{
    /*
    * Extract this operation from the list held by its task.
    */
    struct oper_act	* on_tmp;
    struct oper_act	**on_p;

	DLOG(log_dsap, LLOG_TRACE, ("oper_task_extract()"));

    if(on == NULLOPER)
    {
	LLOG (log_dsap,LLOG_FATAL, ("oper_task_extract: Cannot extract NULLOPER"));
	return;
	/* This is an implementation error */
    }

    if(on->on_task == NULLTASK)
    {
	/* Must have been extracted previously. */
	if (on->on_state != ON_ABANDONED)
		LLOG (log_dsap,LLOG_EXCEPTIONS, ("oper_task_extract: oper has no task"));
	return;
    }

    on_p = &(on->on_task->tk_operlist);
    for(on_tmp=(*on_p); on_tmp!=NULLOPER; on_tmp=on_tmp->on_next_task)
    {
	if(on_tmp == on)
	    break;

	on_p = &(on_tmp->on_next_task);
    }
    if(on_tmp == NULLOPER)
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Oper not on tasks list"));
    }
    else
    {
	(*on_p) = on_tmp->on_next_task;
    }

    if (on->on_dsas != NULL_DI_BLOCK)
	di_desist (on->on_dsas);

    on->on_dsas = NULL_DI_BLOCK;

    on->on_task = NULLTASK; /* Shows that this has been task_extracted */
}

oper_log(on)
struct oper_act	* on;
{
    DLOG (log_dsap,LLOG_DEBUG, ("Oper id = %d, state = %d, type = %d",
	on->on_id, on->on_state, on->on_type));
}
