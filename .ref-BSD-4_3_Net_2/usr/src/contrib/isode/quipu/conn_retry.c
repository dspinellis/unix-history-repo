/* conn_retry.c - deal with asynchronous A-Associate events */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_retry.c,v 7.2 91/02/22 09:38:35 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/conn_retry.c,v 7.2 91/02/22 09:38:35 mrose Interim $
 *
 *
 * $Log:	conn_retry.c,v $
 * Revision 7.2  91/02/22  09:38:35  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:30  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:56  mrose
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
#include "quipu/util.h"
#include "quipu/connection.h"

extern LLog * log_dsap;
extern time_t timenow;
extern time_t nsap_timeout;

#ifndef NO_STATS
extern LLog * log_stat;
extern int	dn_print();
#endif

struct connection	* conn_alloc();

/*
* Deal with an incoming acceptance of association establishment.
* Return value says whether anything has happened or not.
*/
conn_retry(conn)
struct connection       * conn;
{
    struct DSAPconnect		* dc = &(conn->cn_connect.cc_dc);
    struct DSAPindication      di_s;
    struct DSAPindication      *di = &di_s;
    struct oper_act		* on;
    struct oper_act		* onext;
    struct oper_act		* ont = NULLOPER;
    int 			result;
    int				pstate;
    int				do_next_nsap;

    DLOG (log_dsap,LLOG_TRACE,( "conn_retry()"));

    pstate = conn->cn_state;

    if (timenow - conn->cn_last_used >= nsap_timeout) {
	/* this NSAP has had long enough - try the next one... */
	LLOG (log_dsap,LLOG_NOTICE,("NSAP hanging (%d)...",conn->cn_ad));
	do_next_nsap = 1;
	conn->cn_last_used = timenow; /* restart timer */
    } else {
	do_next_nsap = 0;
    }

    switch (conn->cn_ctx)
    {
    case DS_CTX_X500_DAP:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("DAP context type in conn_retry()"));
	conn_extract(conn);
	return;

    case DS_CTX_X500_DSP:
	result = DspAsynBindRetry (conn->cn_ad, do_next_nsap, dc, di);
	break;

    case DS_CTX_QUIPU_DSP:
	result = QspAsynBindRetry (conn->cn_ad, do_next_nsap, dc, di);
	break;

    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("Unknown context type in conn_retry()"));
	conn_extract(conn);
	return;
    }

    switch(result)
    {
        case CONNECTING_1:
	    DLOG (log_dsap,LLOG_NOTICE,("D-BIND.RETRY CONNECTING_1 (%d)",conn->cn_ad));
	    conn->cn_state = CN_CONNECTING1;
	break;

	case CONNECTING_2:
	    DLOG (log_dsap,LLOG_NOTICE,("D-BIND.RETRY CONNECTING_2 (%d)",conn->cn_ad));
	    conn->cn_state = CN_CONNECTING2;
	break;

	case NOTOK :
	    DLOG (log_dsap,LLOG_NOTICE,( "D-BIND.RETRY NOTOK"));
	    conn->cn_state = CN_FAILED;

#ifndef NO_STATS
	    pslog(log_stat, LLOG_NOTICE, "Failed (RETRY NOTOK)", dn_print, (caddr_t) conn->cn_dn);
#endif
	    dsa_reliable (conn,FALSE,timenow);

	    for(on=conn->cn_operlist; on!=NULLOPER; on=onext)
	    {
		onext = on->on_next_conn;
		/* See if there is another DSA to try... */
		if ((on->on_state != ON_ABANDONED) && (on->on_dsas != NULL_DI_BLOCK)) {	
			LLOG (log_dsap,LLOG_NOTICE,("Trying a different DSA (NOTOK)..."));
			if (oper_chain (on) == OK) {
				if (ont == NULLOPER)
					conn->cn_operlist = onext;
				else
					ont->on_next_conn = onext;
				continue;
			}
		}
		/* No - we can't do it !!! */
		oper_fail_wakeup(on);
		ont = on;
	    }
	    conn_extract(conn);
	break;

	case DONE :
	    DLOG (log_dsap,LLOG_NOTICE,( "D-BIND.RETRY DONE (%d)",conn->cn_ad));
	    if( (conn->cn_ad == NOTOK) || (conn_req_aux(conn) != OK))
	    {
#ifndef NO_STATS
		pslog(log_stat, LLOG_NOTICE, "Failed (RETRY DONE)", dn_print, (caddr_t) conn->cn_dn);
#endif
		dsa_reliable (conn,FALSE,timenow);
		conn->cn_state = CN_FAILED;
	        for(on=conn->cn_operlist; on!=NULLOPER; on=onext)
	        {
			onext = on->on_next_conn;
			/* See if there is another DSA to try... */
			if ((on->on_state != ON_ABANDONED) && (on->on_dsas != NULL_DI_BLOCK)) {	
				LLOG (log_dsap,LLOG_NOTICE,("Trying a different DSA (DONE)..."));
				if (oper_chain (on) == OK) {
					if (ont == NULLOPER)
						conn->cn_operlist = onext;
					else
						ont->on_next_conn = onext;
					continue;
				}
			}
			oper_fail_wakeup(on);
			ont = on;
	        }
		DLOG(log_dsap, LLOG_DEBUG, ("conn_retry calling conn_extract 1"));
		conn_extract(conn);
		return;
	    }

	    for(on=conn->cn_operlist; on!=NULLOPER; on=on->on_next_conn)
	    {
		if (on->on_state == ON_ABANDONED)
			oper_fail_wakeup(on);

		else if (oper_send_invoke(on) != OK) {
			LLOG (log_dsap,LLOG_EXCEPTIONS,("oper_send_invoke failed in conn_retry"));
			oper_log(on);
			oper_fail_wakeup(on);
		}
	    }
	break;

	default :
	    DLOG(log_dsap, LLOG_DEBUG, ("conn_retry calling conn_extract 2"));
	    for(on=conn->cn_operlist; on!=NULLOPER; on=on->on_next_conn)
	    {
		oper_fail_wakeup(on);
	    }
	    conn_extract(conn);
	break;

    } /* switch retry */

    if (pstate != conn->cn_state)
	conn->cn_last_used = timenow;
}
