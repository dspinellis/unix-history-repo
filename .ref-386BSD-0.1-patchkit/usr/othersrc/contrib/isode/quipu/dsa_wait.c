/* dsa_wait.c - accept and process events listened for */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/dsa_wait.c,v 7.6 91/02/22 09:39:12 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/dsa_wait.c,v 7.6 91/02/22 09:39:12 mrose Interim $
 *
 *
 * $Log:	dsa_wait.c,v $
 * Revision 7.6  91/02/22  09:39:12  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/10/17  11:54:09  mrose
 * sync
 * 
 * Revision 7.4  90/07/09  14:46:03  mrose
 * sync
 * 
 * Revision 7.3  90/04/18  08:49:51  mrose
 * 6.2
 * 
 * Revision 7.2  90/03/15  11:19:00  mrose
 * quipu-sync
 * 
 * Revision 7.1  89/12/19  16:20:30  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:25  mrose
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


#include "rosap.h"
#include "tsap.h"
#include "quipu/util.h"
#include "quipu/connection.h"
#include <signal.h>

extern unsigned watchdog_time;
extern unsigned watchdog_delta;

extern LLog * log_dsap;
extern LLog * tsap_log;

extern time_t	time();
time_t	timenow;

dsa_wait(secs)
int	  secs;
{
    int                         vecp = 0;
    char                        *vec[4];
    fd_set                      iads;
    fd_set                      wads;
    int                         nads;
    struct TSAPdisconnect       td_s;
    struct TSAPdisconnect       *td = &td_s;
    struct connection		* cn;
    struct connection		* next_cn;
    char			ibuffer[BUFSIZ];
    char			*ibp;
    char			wbuffer[BUFSIZ];
    char			*wbp;
    SFD 			attempt_restart();
    int				newfd;

    nads = 0;
    FD_ZERO(&iads);
    FD_ZERO(&wads);
    ibp = ibuffer;
    wbp = wbuffer;

/*
    DLOG(log_dsap, LLOG_DEBUG, ("dsa_wait connections:"));
    conn_list_log(connlist);
*/ 

    for(cn=connlist; cn != NULLCONN; cn=cn->cn_next)
    {
	if (cn->cn_state == CN_CONNECTING1)
	{
	    if (cn->cn_ad > 0)
		    FD_SET(cn->cn_ad, &wads);
	    else
		SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
		      ("fd=%d for connection block(1)", cn -> cn_ad));
	    (void) sprintf(wbp, ", %d.", cn->cn_ad);
	    wbp += (strlen(wbp) - 1);
	}
	else
	{
	    if (cn->cn_ad > 0)
		    FD_SET(cn->cn_ad, &iads);
	    else
		SLOG (tsap_log, LLOG_EXCEPTIONS, NULLCP,
		      ("fd=%d for connection block(2)", cn -> cn_ad));
	    (void) sprintf(ibp, ", %d.", cn->cn_ad);
	    ibp += (strlen(ibp) - 1);
	}

	if(cn->cn_ad >= nads)
	    nads = cn->cn_ad + 1;
    }

    if(ibp == ibuffer)
    {
	DLOG (log_dsap, LLOG_DEBUG, ("Listening for new associations"));
    }
    else
    {
	LLOG (log_dsap, LLOG_NOTICE, ("Listening on ads: %s", (ibuffer+1)));
    }

    if(wbp == wbuffer)
    {
	DLOG (log_dsap, LLOG_DEBUG, ("Not making new associations"));
    }
    else
    {
	LLOG (log_dsap, LLOG_NOTICE, ("Making ads: %s", (wbuffer+1)));
    }

    DLOG (log_dsap, LLOG_NOTICE, ("secs: %d; nads: %d; iads 0x%x, wads 0x%x", 
	secs, nads, iads.fds_bits[0], wads.fds_bits[0]));

    if (secs != NOTOK) {
	/* if secs == NOTOK we want to block, otherwise set watchdog, but
	   beware of setting watchdog off accidentally !
	*/
	if (secs > (watchdog_time - watchdog_delta))
		watch_dog_aux ("TNetAccept (long)",(unsigned)secs + watchdog_delta);
	else
		watch_dog ("TNetAccept");
    }

    if(TNetAcceptAux(&vecp, vec, &newfd, NULLTA, nads, &iads, &wads, NULLFD, secs, td) == NOTOK)
    {
	watch_dog_reset();

	td_log (td, "TNetAccept");

/*
 *	if (td -> td_reason == DR_PROTOCOL || td -> td_reason == DR_NETWORK) 
 */
		return;
/*
 *	attempt_restart (NOTOK);
 * 	exit (0);			*/ /* should not be reached */

    }
    watch_dog_reset();

    (void) time (&timenow);

    if (vecp > 0) 
	conn_pre_init (newfd,vecp,vec);

    for(cn = connlist; cn != NULLCONN; cn = next_cn)
    {
	next_cn = cn->cn_next;

	switch(cn->cn_state)
	{
	case CN_CONNECTING1:
	    DLOG(log_dsap, LLOG_TRACE, ("Checking %d", cn->cn_ad));
	    if(FD_ISSET(cn->cn_ad, &wads))
	    {
	        DLOG(log_dsap, LLOG_DEBUG, ("Polling %d", cn->cn_ad));
	        conn_retry(cn);
	    }
	break;

	case CN_CONNECTING2:
	    DLOG (log_dsap, LLOG_TRACE, ("Checking %d (2)", cn ->cn_ad));
	    if (FD_ISSET(cn->cn_ad, &iads))
	    {
		DLOG(log_dsap, LLOG_DEBUG, ("Polling %d (2)", cn->cn_ad));
		conn_retry(cn);
	    }
	break;

	case CN_OPEN:
	    if (FD_ISSET (cn->cn_ad, &iads))
	    {
		DLOG (log_dsap,LLOG_DEBUG,( "Activity on association: %d", cn->cn_ad));
		conn_dispatch(cn);
	    } /* if there is work on this connection */
	break;

	case CN_CLOSING:
	    if (FD_ISSET (cn->cn_ad, &iads)) 
		(void) conn_release_retry(cn);
	break;

	case CN_OPENING:
	    if (FD_ISSET (cn->cn_ad, &iads))
		conn_init(cn);
	break;

	case CN_INDICATED:
	    if (FD_ISSET (cn->cn_ad, &iads))
	    {
		if(cn->cn_start.cs_bind_compare == NULLOPER)
		{
		    LLOG(log_dsap, LLOG_EXCEPTIONS, ("cn_state = INDICATED but no bind_compare operation"));
		}
		else
		{
		    cn->cn_start.cs_bind_compare->on_bind_compare = NULLCONN;
		}
	    }
	    /* FALL THROUGH */
	
	default:
	    if (FD_ISSET (cn->cn_ad, &iads))
	    {
		LLOG (log_dsap,LLOG_EXCEPTIONS,( "Unexpected activity on association %d ... aborting",cn->cn_ad));
		net_send_abort(cn);
		conn_extract(cn);
	    } /* if there is work on this connection */
	break;
	}
    } /* for each connection */

} /* dsa_wait */



