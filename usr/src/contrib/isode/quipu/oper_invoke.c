/* oper_invoke.c - encode argument and invoke operation */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/oper_invoke.c,v 7.2 91/02/22 09:39:34 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/oper_invoke.c,v 7.2 91/02/22 09:39:34 mrose Interim $
 *
 *
 * $Log:	oper_invoke.c,v $
 * Revision 7.2  91/02/22  09:39:34  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:32  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:52  mrose
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
#ifndef NO_STATS
extern  LLog    * log_stat;
#endif
void ros_log ();

oper_send_invoke(oper)
register        struct oper_act * oper;
{
    int				  result;
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &(di_s);

    DLOG(log_dsap, LLOG_TRACE, ("oper_send_invoke"));

    if(oper == NULLOPER)
    {
	LLOG(log_dsap, LLOG_FATAL, ("Task memerr 3"));
	return(NOTOK);
    }

    if(oper->on_state == ON_ABANDONED)
	return NOTOK;

    /*
    * Genrate an id unique over this connection for this operation.
    */
    oper->on_id = ++(oper->on_conn->cn_op_id);

    switch (oper->on_conn->cn_ctx)
    {
	case DS_CTX_X500_DAP:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("oper_invoke(): DAP context unexpected"));
	    break;
	case DS_CTX_X500_DSP:
	    result = DspInvokeRequest (oper->on_conn->cn_ad, oper->on_id,
			oper->on_arg, di);
	    break;
	case DS_CTX_QUIPU_DSP:
	    result = QspInvokeRequest (oper->on_conn->cn_ad, oper->on_id,
			oper->on_arg, di);
	    break;
	default:
	    LLOG (log_dsap, LLOG_EXCEPTIONS, ("oper_invoke(): Unknown context %d", oper->on_conn->cn_ctx));
	    break;
    }

    if (result != OK)
    {
	if(di->di_type == DI_ABORT)
	{
	    struct connection	* cn;

	    LLOG(log_dsap, LLOG_FATAL, ("D-INVOKE.REQUEST: fatal reject - fail the connection"));
	    oper->on_conn->cn_state = CN_FAILED;
	    cn = oper->on_conn;
	    oper_extract(oper);
	    conn_extract(cn);
	    return(NOTOK);
	}
	else
	{
	    oper->on_state = ON_COMPLETE;
	    oper->on_resp.di_type = DI_PREJECT;
	    oper_fail_wakeup(oper);
	    return(NOTOK);
	}
    }
    else
    {
	DLOG(log_dsap, LLOG_NOTICE, ("D-INVOKE.REQUEST: OK"));
#ifndef NO_STATS
	LLOG(log_stat, LLOG_TRACE, ("Chain (%d)",oper->on_conn->cn_ad));
#endif
	oper->on_state = ON_CHAINED;
	return(OK);
    }
}

