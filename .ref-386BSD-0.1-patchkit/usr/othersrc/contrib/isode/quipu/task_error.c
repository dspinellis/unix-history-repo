/* task_error.c - */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/task_error.c,v 7.2 91/02/22 09:39:58 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/task_error.c,v 7.2 91/02/22 09:39:58 mrose Interim $
 *
 *
 * $Log:	task_error.c,v $
 * Revision 7.2  91/02/22  09:39:58  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:51  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:18:12  mrose
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

#include "rosap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern  LLog    * log_dsap;

void ros_log ();

/*
* The DSA has produced an error for the task, encode the error,
* generate a D-ERROR.REQUEST and update the task block.
*/
task_error(task)
register        struct task_act * task;
{
    int				  result;
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &(di_s);
    struct DSError              * err;
    struct connection		* cn = task->tk_conn;

#ifndef NO_STATS
    extern LLog * log_stat;
    LLOG (log_stat, LLOG_TRACE,("Error sent (%d)",task->tk_conn->cn_ad));
#endif

    if(task == NULLTASK)
    {
	LLOG(log_dsap, LLOG_FATAL, ("Task memerr 2"));
	return;
    }

    err = task->tk_error;
    task->tk_resp.di_type = DI_ERROR;    

    if (log_dsap -> ll_events & LLOG_NOTICE)
	    log_ds_error (task->tk_error);

    /* Return the right sort of referral error */
    if(cn->cn_ctx == DS_CTX_X500_DAP)
    {
	if(err->dse_type == DSE_DSAREFERRAL)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("Changing DSAREFERRAL to REFERRAL (DAP)"));
	    err->dse_type = DSE_REFERRAL;
	}
    }
    else
    {
	if(err->dse_type == DSE_REFERRAL)
	{
	    DLOG(log_dsap, LLOG_DEBUG, ("Changing DSAREFERRAL to REFERRAL"));
	    err->dse_type = DSE_DSAREFERRAL;
	}
    }

    switch (cn->cn_ctx)
    {
	case DS_CTX_X500_DAP:
	    result = DapErrorRequest (cn->cn_ad, task->tk_dx.dx_id, err, di);
	    break;
	case DS_CTX_X500_DSP:
	    result = DspErrorRequest (cn->cn_ad, task->tk_dx.dx_id, err, di);
	    break;
	case DS_CTX_QUIPU_DSP:
	    result = QspErrorRequest (cn->cn_ad, task->tk_dx.dx_id, err, di);
	    break;
	default:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("task_error(): Unknown context %d", cn->cn_ctx));
	    break;
    }

    if (result != OK)
    {
	if(di->di_type == DI_ABORT)
	{
	    LLOG(log_dsap, LLOG_FATAL, ("D-RESULT.REQUEST: fatal reject - fail the connection"));
	    cn->cn_state = CN_FAILED;
	}
    }

    if(cn->cn_state == CN_FAILED)
    {
	DLOG(log_dsap, LLOG_DEBUG, ("task_error(): extracting conn:"));
	conn_log(cn);
	conn_extract(cn);
    }
}
