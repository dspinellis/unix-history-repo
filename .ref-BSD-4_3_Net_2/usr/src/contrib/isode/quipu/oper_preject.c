/* oper_preject.c - deal with preject of an operation */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/oper_preject.c,v 7.2 91/02/22 09:39:35 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/oper_preject.c,v 7.2 91/02/22 09:39:35 mrose Interim $
 *
 *
 * $Log:	oper_preject.c,v $
 * Revision 7.2  91/02/22  09:39:35  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:54:34  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:53  mrose
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

oper_preject(conn, dp)
struct connection	* conn;
struct DSAPpreject	* dp;
{
    struct oper_act *       on;

    DLOG(log_dsap, LLOG_TRACE, ("oper_preject"));

    if (dp->dp_id == -1)
    {
	/* No identified operation to reject! */
	return;
    }

    for(on=conn->cn_operlist; on!=NULLOPER; on=on->on_next_conn)
	if(on->on_id == dp->dp_id)
	    break;

    if(on == NULLOPER)
    {
	LLOG(log_dsap,LLOG_EXCEPTIONS,( "Unlocatable P-REJECT.INDICATION : %d",
	      dp->dp_reason));
    }
    else
    {
	oper_fail_wakeup(on);
    }
}

