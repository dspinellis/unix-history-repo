/* conn_abort.c - abort association */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/conn_abort.c,v 7.2 91/02/22 09:38:27 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/conn_abort.c,v 7.2 91/02/22 09:38:27 mrose Interim $
 *
 *
 * $Log:	conn_abort.c,v $
 * Revision 7.2  91/02/22  09:38:27  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:53:19  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:16:43  mrose
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

struct connection	* conn_alloc();
void			  conn_free();
void			  ds_log ();

net_send_abort(conn)
register        struct connection       * conn;
{
    int				  result;
    struct DSAPindication      di_s;
    struct DSAPindication      *di = &di_s;
    struct DSAPabort           *da = &(di->di_abort);

    DLOG(log_dsap, LLOG_TRACE, ("net_send_abort"));

    DLOG(log_dsap, LLOG_NOTICE, ("D-ABORT.REQUEST: <%d>", conn->cn_ad));

    result = DUAbortRequest(conn->cn_ad, di);

    if (result != OK)
    {
	ds_log(da, "D-ABORT.REQUEST");
    }
    conn->cn_state = CN_FAILED;
    conn->cn_ad = 0;
}

/* ADT: Needs improving */
/* ARGSUSED */

void	  ds_log (da, str)
struct DSAPabort	* da;
char			* str;
{
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DSAP abort : %s", str));
}

