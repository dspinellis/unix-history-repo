/* conn_request.c - Generate DSP BIND from connection block */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_request.c,v 7.6 91/02/22 09:38:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/conn_request.c,v 7.6 91/02/22 09:38:33 mrose Interim $
 *
 *
 * $Log:	conn_request.c,v $
 * Revision 7.6  91/02/22  09:38:33  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/10/17  11:53:28  mrose
 * sync
 * 
 * Revision 7.4  90/07/09  14:45:28  mrose
 * sync
 * 
 * Revision 7.3  89/12/19  16:20:06  mrose
 * sync
 * 
 * Revision 7.2  89/11/27  10:30:09  mrose
 * sync
 * 
 * Revision 7.1  89/11/24  16:21:56  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:55  mrose
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

#include "quipu/dsap.h"
#include "tsap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern time_t	timenow;

struct connection	* conn_alloc();
void			  conn_free();

extern	LLog	* log_dsap;
#ifndef NO_STATS
extern LLog * log_stat;
extern int	dn_print();
#endif

extern  PS      opt;
extern unsigned watchdog_time;
extern unsigned watchdog_delta;

/*
*  conn_request uses the directory bind argument and context
*  set up in the connection block to generate an asynchronous
*  association request.
*  If OK is returned the connection should be linked onto
*  the global list.
*  If NOTOK is returned the connection block should be freed,
*  alerting all the operations requesting it.
*/
conn_request(cn)
register        struct connection       * cn;
{
    struct DSAPconnect		* dc = &(cn->cn_connect.cc_dc);
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &(di_s);
    int				  inv_ret;

    DLOG(log_dsap, LLOG_TRACE, ("conn_request()"));
    LLOG(log_dsap, LLOG_NOTICE, ("conn_request: Calling: %s", paddr2str(&(cn->cn_addr),NULLNA)));

    switch(cn->cn_ctx)
    {
    case DS_CTX_X500_DAP:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Making DAP connections illegal for DSA"));
	return(NOTOK);

    case DS_CTX_X500_DSP:
	LLOG(log_dsap, LLOG_TRACE, ("Making an X500 DSP connection"));
	inv_ret = DspAsynBindRequest (&(cn->cn_addr), &(cn->cn_connect.cc_req),
			0, dc, di, ROS_ASYNC);
	break;

    case DS_CTX_QUIPU_DSP:
	LLOG(log_dsap, LLOG_TRACE, ("Making a QUIPU DSP connection"));
	inv_ret = QspAsynBindRequest (&(cn->cn_addr),
			&(cn->cn_connect.cc_req),
			0, dc, di, ROS_ASYNC);
	break;

    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Unknown connection context"));
	return(NOTOK);
    }

    cn->cn_last_used = timenow;

    switch(inv_ret)
    {
    case NOTOK:
	DLOG(log_dsap, LLOG_NOTICE, ("ASYN BIND REQUEST NOTOK!"));
#ifndef NO_STATS
    	pslog (log_stat, LLOG_NOTICE, "Failed (NOTOK)", dn_print, (caddr_t) cn->cn_dn);
#endif
    	dsa_reliable (cn,FALSE,timenow);
	return(NOTOK);
    case DONE:
	DLOG(log_dsap, LLOG_NOTICE, ("ASYN BIND REQUEST DONE! (ad = %d)", dc->dc_sd));
	cn->cn_ad = dc->dc_sd;
	if (cn->cn_ad == NOTOK)
	{
#ifndef NO_STATS
	    pslog(log_stat, LLOG_NOTICE, "Failed (DONE)", dn_print, (caddr_t) cn->cn_dn);
#endif
	    cn->cn_state = CN_FAILED;
	    dsa_reliable (cn,FALSE,timenow);
	    return(NOTOK);
	}
	if (conn_req_aux(cn) == NOTOK) {
#ifndef NO_STATS
	    pslog(log_stat, LLOG_NOTICE, "Failed (DONE 2)", dn_print, (caddr_t) cn->cn_dn);
#endif
	    dsa_reliable (cn,FALSE,timenow);
	    return NOTOK;
	}
	return OK;
    case CONNECTING_1:
	DLOG (log_dsap,LLOG_NOTICE,("ASYN BIND REQUEST CONNECTING_1 (ad = %d)", dc->dc_sd));
	cn->cn_ad = dc->dc_sd;
	cn->cn_state = CN_CONNECTING1;
#ifndef NO_STATS
    	pslog (log_stat, LLOG_NOTICE, "Trying (CONN_1)", dn_print, (caddr_t) cn->cn_dn);
#endif
	return(OK);

    case CONNECTING_2:
	DLOG (log_dsap,LLOG_NOTICE,("ASYN BIND REQUEST CONNECTING_2 (ad = %d)", dc->dc_sd));
	cn->cn_ad = dc->dc_sd;
	cn->cn_state = CN_CONNECTING2;
#ifndef NO_STATS
    	pslog (log_stat, LLOG_NOTICE, "Trying (CONN_2)", dn_print, (caddr_t) cn->cn_dn);
#endif
	return(OK);

    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Unknown return from DAsynBind : %d", inv_ret));
	return(NOTOK);
    } /* switch inv_ret */
    /* NOTREACHED */
}

/*
* conn_req_aux() is called to complete work started by conn_request().
* Current major complication is how to deal with an undecodable BindResult.
* If OK is returned, the connection is ready for action and any waiting
* operations should be sent.
* If NOTOK is returned the connection needs to be extracted, alerting any
* waiting operations in the process.
*/
conn_req_aux(cn)
register        struct connection       * cn;
{

    switch(cn->cn_connect.cc_dc.dc_result)
    {
	case DS_RESULT:
	    DLOG(log_dsap, LLOG_NOTICE, ("D-BIND.RETRY(ASYNC) RESULT"));
	    cn->cn_state = CN_OPEN;
	break;

	case DS_ERROR:
	    /*
	    * Get the DirectoryBindError
	    */
	    DLOG(log_dsap, LLOG_NOTICE, ("D-BIND.RETRY(ASYNC) ERROR"));
	    cn->cn_state = CN_FAILED;
	    cn->cn_ad = 0;
	break;

	default:
	    cn->cn_state = CN_FAILED;
	    cn->cn_ad = 0;
	    LLOG (log_dsap,LLOG_NOTICE,( "Association rejected"));
/* ADT
	    LLOG (log_dsap,LLOG_NOTICE,( "Association rejected: [%s]",
	      DErrString(??)));
*/
	break;

    } /* switch acc->acc_result */

    if(cn->cn_state == CN_OPEN)
    {
	struct TSAPdisconnect       td_s;
	struct TSAPdisconnect       *td = &td_s;

	if (TSetQueuesOK (cn->cn_ad, 1, td) == NOTOK) 
		td_log (td, "TSetQueuesOK (outgoing)");

	cn->cn_last_used = timenow;
	dsa_reliable (cn,TRUE,cn->cn_last_used);
#ifndef NO_STATS
	    {
		char buf [LINESIZE];
		(void) sprintf (buf,"Bound using %s DSP context (%d)", cn->cn_ctx == DS_CTX_QUIPU_DSP ? "QUIPU" : "X500", cn->cn_ad);
		pslog(log_stat, LLOG_NOTICE, buf, dn_print, (caddr_t) cn->cn_dn);
	    }
#endif
	return(OK);
    }

    return(NOTOK);
}

