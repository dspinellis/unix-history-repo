/* conn_dispatch.c - deal with an event on an open connection */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_dispatch.c,v 7.2 91/02/22 09:38:28 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/conn_dispatch.c,v 7.2 91/02/22 09:38:28 mrose Interim $
 *
 *
 * $Log:	conn_dispatch.c,v $
 * Revision 7.2  91/02/22  09:38:28  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:20  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:44  mrose
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


#include "quipu/dsap.h"
#include "tsap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern LLog * log_dsap;

/*
* Something has happened on the association with descriptor "ad".
* Check what type of activity it is and dispatch to an appropriate
* routine to handle the activity.
*/
conn_dispatch(cn)
struct connection	* cn;
{
    int				  result;
    struct DSAPindication      di_s;
    struct DSAPindication      *di = &di_s;
    struct DSAPabort         *da = &(di->di_abort);
    extern void ds_log();

    DLOG (log_dsap,LLOG_TRACE,( "conn_dispatch()"));

    bzero ((char *)di, sizeof di_s);

    result = DWaitRequest(cn->cn_ctx, cn->cn_ad, OK, di);

    if (result == DONE)
    {
	/* TIMER expired */
	return;
    }

    if (result == NOTOK)
    {
        switch(di->di_type)
        {
	case DI_PREJECT:
	    DLOG(log_dsap, LLOG_DEBUG, ("conn_dispatch calling oper_preject"));
	    oper_preject(cn, &(di->di_preject));
	    return;

	case DI_ABORT:
/*
	    ds_log(da, "DWaitRequest");
*/
	    LLOG(log_dsap, LLOG_NOTICE, ("DWaitRequest - abort"));
	    do_ds_unbind (cn);
	    conn_extract(cn);
	    return;

	default:
	    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unknown indication type : %d", di->di_type));
	    return;
	}
    }

    switch(di->di_type)
    {
	case DI_INVOKE:
	    if (task_invoke(cn, &(di->di_invoke)) != OK)
	    {
		LLOG (log_dsap,LLOG_EXCEPTIONS,("task_invoke failed in conn_dispatch"));
	    }
	    break;

	case DI_RESULT:
	    oper_result(cn, di);
	    break;

	case DI_ERROR:
	    oper_error(cn, di);
	    break;

	case DI_FINISH:
	    conn_finish(cn, &(di->di_finish));
	    break;

	default:
	    LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unknown indication type : %d", di->di_type));
	    break;
    }
}

