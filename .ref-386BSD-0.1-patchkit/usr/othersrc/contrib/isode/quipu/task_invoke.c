/* task_invoke.c - deal with invocation of an operation over a connection */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/task_invoke.c,v 7.2 91/02/22 09:39:59 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/task_invoke.c,v 7.2 91/02/22 09:39:59 mrose Interim $
 *
 *
 * $Log:	task_invoke.c,v $
 * Revision 7.2  91/02/22  09:39:59  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:53  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:13  mrose
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


/* LINTLIBRARY */

#include "rosap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern	LLog	* log_dsap;
extern time_t	  timenow;
extern time_t 	  admin_time;
extern UTC	  str2utct();
struct task_act *       task_alloc();
struct common_args	* get_ca_ref();

int	  task_invoke(conn, dx)
register	struct connection	* conn;
register	struct DSAPinvoke       * dx;
{
    time_t		  timer;
    struct task_act	* task;
    struct extension	* ext;
    struct common_args	* ca;

    DLOG(log_dsap, LLOG_TRACE, ("task_invoke()"));

    for(task=conn->cn_tasklist; task!=NULLTASK; task=task->tk_next)
	if(task->tk_dx.dx_id == dx->dx_id)
	    break;

    if(task != NULLTASK)
    {
	DLOG(log_dsap, LLOG_TRACE, ("Duplicate invocation identifier %d", dx->dx_id));
	send_ro_ureject(conn->cn_ad, &(dx->dx_id), ROS_IP_DUP);
	return(NOTOK);
    }

    task = task_alloc();

    DLOG(log_dsap, LLOG_TRACE, ("Operation Invoked"));

    task->tk_conn = conn;
    task->tk_state = TK_ACTIVE;
    task->tk_dx = (*dx);		/* struct copy */

    if(task->tk_dx.dx_arg.dca_dsarg.arg_type == OP_ABANDON)
    {
	DLOG(log_dsap, LLOG_NOTICE, ("Abandon received"));
#ifndef NO_STATS
	log_x500_event (&(task->tk_dx.dx_arg.dca_dsarg),task->tk_conn->cn_ctx,NULLDN,NULLDN,task->tk_conn->cn_ad);
#endif
	if(perform_abandon(task) == OK)
	{
	    task_result(task);
	}
	else
	{
	    task_error(task);
	}
	task_free(task);
	return(OK);
    }

    if(task->tk_dx.dx_arg.dca_dsarg.arg_type == OP_GETEDB)
    {
	DLOG(log_dsap, LLOG_TRACE, ("GetEDB received"));
	task->tk_dx.dx_arg.dca_charg.cha_originator = dn_cpy(task->tk_conn->cn_dn);
    }

    if((ca = get_ca_ref(&task->tk_dx.dx_arg)) != NULL_COMMONARG)
    {
	switch(ca->ca_servicecontrol.svc_prio)
	{
	case SVC_PRIO_LOW:
	    task->tk_prio = DSA_PRIO_LOW;
	    break;
	case SVC_PRIO_MED:
	    task->tk_prio = DSA_PRIO_MED;
	    break;
	case SVC_PRIO_HIGH:
	    task->tk_prio = DSA_PRIO_HIGH;
	    break;
	default:
	    DLOG(log_dsap, LLOG_EXCEPTIONS, ("Impossibly svc_prio = %d",
		    ca->ca_servicecontrol.svc_prio));
	    task->tk_prio = DSA_PRIO_LOW;
	    break;
	}

	for(ext = ca->ca_extensions; ext!=NULLEXT; ext=ext->ext_next)
	{
	    /* Check for unavailable critical extension */
	    if(ext->ext_critical)
		break;
	}

	if(ext != NULLEXT)
	{
	    task->tk_resp.di_type = DI_ERROR;
	    task->tk_resp.di_error.de_err.dse_type = DSE_SERVICEERROR;
	    task->tk_resp.di_error.de_err.dse_un.dse_un_service.DSE_sv_problem = DSE_SV_UNAVAILABLECRITICALEXTENSION;
	    task_error(task);
	    task_free(task);
	    return(NOTOK);
	}
    }
    else
    {
	/* Logic warning: No common args => low prio */
	task->tk_prio = DSA_PRIO_LOW;
    }

    /* Check for loop */
    if(conn->cn_ctx == DS_CTX_X500_DAP)
    {
       	if(ca != NULL_COMMONARG)
	{
	    if(ca->ca_servicecontrol.svc_timelimit == SVC_NOTIMELIMIT)
	    {
		task->tk_timed = FALSE;
		if ((conn->cn_authen == DBA_AUTH_NONE)
		   || (! manager (task->tk_conn->cn_dn)))
		{
			task->tk_timed = 2;
			task->tk_timeout = timenow + admin_time;
		}
	    }
	    else
	    {
		task->tk_timed = TRUE;
		if ((timer = ca->ca_servicecontrol.svc_timelimit) > admin_time)
		{
		   if ((conn->cn_authen == DBA_AUTH_NONE)
		      || (! manager (task->tk_conn->cn_dn)))
		    {
			task->tk_timed = 2;
			timer = admin_time;
		    }
		}
		task->tk_timeout = timenow + timer;
	    }
#ifdef DEBUG
	    if (task->tk_timed)
		DLOG(log_dsap, LLOG_DEBUG, ("CommonArgs timelimit is %d secs", task->tk_timeout - timenow));
#endif
	}
	else
	{
	    task->tk_timed = FALSE;
	}
    }
    else
    {
	struct chain_arg	* cha = &(task->tk_dx.dx_arg.dca_charg);

	if(cha_loopdetected(cha))
	{
	    task->tk_resp.di_type = DI_ERROR;
	    task->tk_resp.di_error.de_err.dse_type = DSE_SERVICEERROR;
	    task->tk_resp.di_error.de_err.dse_un.dse_un_service.DSE_sv_problem = DSE_SV_LOOPDETECT;
	    task_error(task);
	    task_free(task);
	    return(NOTOK);
	}

	if(cha->cha_timelimit == NULLCP)
	{
	    task->tk_timed = 2;
	    task->tk_timeout = timenow + admin_time;

            if(ca != NULL_COMMONARG) 
	    {
	    	if(ca->ca_servicecontrol.svc_timelimit != SVC_NOTIMELIMIT)
		{
		    task->tk_timed = TRUE;
		    if (ca->ca_servicecontrol.svc_timelimit < admin_time)
		    {
			task->tk_timeout = timenow + ca->ca_servicecontrol.svc_timelimit;
		    }
		}
	    }
#ifdef DEBUG
	    if (task->tk_timed)
		DLOG(log_dsap, LLOG_DEBUG, ("DSP CommonArgs timelimit is %d secs", task->tk_timeout - timenow));
#endif
	}
	else
	{
	    UTC	  ut;

	    task->tk_timed = TRUE;
	    ut = str2utct(cha->cha_timelimit, strlen(cha->cha_timelimit));
	    task->tk_timeout = gtime(ut2tm(ut));
	    timer = timenow;
	    if (task->tk_timeout - timer > admin_time)
	    {
		/* DSP -> can't rely on manager() !!! */
		task->tk_timed = 2;
		task->tk_timeout = timer + admin_time;
		DLOG(log_dsap, LLOG_DEBUG, ("Chained timeout (limited) is %s", cha->cha_timelimit));
	    }
	    else 
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("Chained timeout is %s", cha->cha_timelimit));
	    }
	}
    }

    if(task->tk_timed == FALSE)
    {
	DLOG(log_dsap, LLOG_TRACE, ("task has NO timelimit"));
    }
#ifdef DEBUG
    else
    {
	struct UTCtime	  ut;
	struct UTCtime	  ut2;

	DLOG(log_dsap, LLOG_TRACE, ("inv task has timelimit of %ld", task->tk_timeout));
	tm2ut(gmtime(&(task->tk_timeout)), &ut);
	DLOG(log_dsap, LLOG_DEBUG, ("converted timelimit = %s", utct2str(&(ut))));
	tm2ut(gmtime(&(timenow)), &ut2);
	DLOG(log_dsap, LLOG_DEBUG, ("time now = %s", utct2str(&(ut2))));
    }
#endif

    task->tk_next = conn->cn_tasklist;
    conn->cn_tasklist = task;
    task->tk_state = TK_ACTIVE;
    return(OK);
}

