/* conn_finish.c - deal with request to finish the association */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_finish.c,v 7.3 91/02/22 09:38:29 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/conn_finish.c,v 7.3 91/02/22 09:38:29 mrose Interim $
 *
 *
 * $Log:	conn_finish.c,v $
 * Revision 7.3  91/02/22  09:38:29  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:53:22  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:18:41  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:16:45  mrose
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
#include "quipu/util.h"
#include "quipu/connection.h"

extern  LLog    * log_dsap;

void	  ds_log();
void	  acs_log ();

/* ARGSUSED */
conn_finish(conn, df)
struct connection	* conn;
struct DSAPfinish	* df;
{
    int			  result;
    struct oper_act	* on;
    extern time_t	  conn_timeout, timenow;
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &(di_s);

    DLOG(log_dsap, LLOG_TRACE, ("conn_finish()"));

    /* Can release be negotiated? */
    if (conn->cn_start.cs_ds.ds_start.acs_start.ps_srequirements & SR_NEGOTIATED)
    {
	/* Should release be rejected? */
        for(on=conn->cn_operlist; on!=NULLOPER; on=on->on_next_conn)
	    if (on->on_state == ON_CHAINED)
		break;

        if (on != NULLOPER)
	{
	    /*
	    * See if oper has had time to complete
	    * if so remote DSA has probably lost the operation (never !!!)
	    * else reject the release
	    */

	    if ( timenow - conn->cn_last_used < conn_timeout)
	    {
		result = DUnBindReject (conn->cn_ad, ACS_REJECT,
			    ACR_NOTFINISHED, di);

		if (result != OK)
		{
		    do_ds_unbind(conn);
		    result = DUAbortRequest (conn->cn_ad, di);
		    conn_extract(conn);
	    	} 
	        return;
	    }
	}
    }

    do_ds_unbind(conn);
    result = DUnBindAccept (conn->cn_ad, di);
    if (result != OK) {
	    result = DUAbortRequest (conn->cn_ad, di);
    }
    conn_extract(conn);

}

conn_rel_abort (conn)
struct connection       * conn;
{
    int				  result;
    struct DSAPindication      di_s;
    struct DSAPindication      *di = &di_s;
    struct DSAPabort           *da = &(di->di_abort);

	if (!conn->cn_initiator)
		return;

	LLOG(log_dsap, LLOG_NOTICE, ("conn_rel_abort %d",conn->cn_ad));

	result = DUAbortRequest (conn->cn_ad, di);

	if (result != OK)
	{
		ds_log (da, "DUAbortRequest in conn_rel_abort()");
	}
}

