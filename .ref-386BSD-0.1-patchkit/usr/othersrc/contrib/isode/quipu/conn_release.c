/* conn_release.c - normal association release */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_release.c,v 7.3 91/03/09 11:55:53 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/conn_release.c,v 7.3 91/03/09 11:55:53 mrose Exp $
 *
 *
 * $Log:	conn_release.c,v $
 * Revision 7.3  91/03/09  11:55:53  mrose
 * update
 * 
 * Revision 7.2  91/02/22  09:38:32  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:27  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:48  mrose
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

#include "quipu/dsap.h"
#include "quipu/util.h"
#include "quipu/connection.h"

extern  LLog    * log_dsap;
extern	time_t	  timenow;

struct connection	* conn_alloc();
void			  conn_free();
void			  ds_log ();

conn_release(conn)
struct connection       * conn;
{
    int				  result;
    struct DSAPrelease		  dr_s;
    struct DSAPrelease		* dr = &(dr_s);
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &(di_s);
    struct DSAPabort		* da = &(di->di_abort);

    DLOG(log_dsap, LLOG_TRACE, ("conn_release"));


    DLOG(log_dsap, LLOG_NOTICE, ("D-UNBIND: <%d, normal, OK>", conn->cn_ad));

    result = DUnBindRequest (conn->cn_ad, OK, dr, di);

    switch (result) {
    case NOTOK:
    	do_ds_unbind(conn);
	DLOG(log_dsap, LLOG_TRACE, ("conn_release: DUnBindRequest - NOTOK"));
	ds_log(da, "A-RELEASE.REQUEST");
	break;
   case OK:
	DLOG(log_dsap, LLOG_TRACE, ("conn_release: dr_affirmative = %d", dr->dr_affirmative));
	if (!dr->dr_affirmative)
	{
	    if ((conn->cn_last_release == conn->cn_last_used) 
		&& (conn->cn_initiator)) {
		    LLOG (log_dsap,LLOG_EXCEPTIONS,("conn_release rejected again without activity - Aborting %d",conn->cn_ad));
		    do_ds_unbind(conn);
		    (void) DUAbortRequest (conn->cn_ad, di);
	    } else {
		    LLOG (log_dsap,LLOG_EXCEPTIONS,("conn_release rejected - continuing with association %d",conn->cn_ad));
		    conn->cn_last_release = conn->cn_last_used = timenow;
		    return NOTOK;
	    }
	}
	else
	{
	    do_ds_unbind(conn);
	    DLOG(log_dsap, LLOG_TRACE, ("conn_release: Conn finished!"));
	}
	break;
    case DONE:
	DLOG (log_dsap,LLOG_NOTICE, ("Waiting for release"));
	conn->cn_state = CN_CLOSING;
	conn->cn_last_release = conn->cn_last_used = timenow;
	return NOTOK;
    default:
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("Unexpected return from DUnBindRequest"));
	return NOTOK;
    }

    DLOG(log_dsap, LLOG_DEBUG, ("conn_release calling conn_extract"));
    conn_extract(conn);
    return OK;
}

conn_release_retry(conn)
struct connection       * conn;
{
    int				  result;
    struct DSAPrelease		  dr_s;
    struct DSAPrelease		* dr = &(dr_s);
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &(di_s);
    struct DSAPabort		* da = &(di->di_abort);

    DLOG(log_dsap, LLOG_NOTICE, ("conn_release retry (%d)",conn->cn_ad));

    result = DUnBindRetry (conn->cn_ad, OK, dr, di);

    switch (result) {
    case NOTOK:
    	do_ds_unbind(conn);
	DLOG(log_dsap, LLOG_TRACE, ("conn_release: DUnBindRetry - NOTOK"));
	ds_log(da, "D-UNBIND.REQUEST");
	break;
   case OK:
	DLOG(log_dsap, LLOG_TRACE, ("conn_release: dr_affirmative = %d", dr->dr_affirmative));
	if (!dr->dr_affirmative)
	{
	    if ((conn->cn_last_release == conn->cn_last_used) 
		&& (conn->cn_initiator)) {
		    LLOG (log_dsap,LLOG_EXCEPTIONS,("conn_release rejected again without activity - Aborting %d",conn->cn_ad));
		    do_ds_unbind(conn);
		    (void) DUAbortRequest (conn->cn_ad, di);
	    } else {
		    LLOG (log_dsap,LLOG_EXCEPTIONS,("conn_release rejected - continuing with association %d",conn->cn_ad));
		    conn->cn_last_release = conn->cn_last_used = timenow;
		    return NOTOK;
	    }
	}
	else
	{
	    do_ds_unbind(conn);
	    DLOG(log_dsap, LLOG_TRACE, ("conn_release: Conn finished!"));
	}
	break;
    case DONE:
	DLOG (log_dsap,LLOG_TRACE, ("Still Waiting for release"));
	conn->cn_last_release = conn->cn_last_used = timenow;
	return NOTOK;
    default:
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("Unexpected return from DUnBindRetry"));
	return NOTOK;
    }

    DLOG(log_dsap, LLOG_DEBUG, ("conn_release calling conn_extract"));
    conn_extract(conn);
    return OK;
}
