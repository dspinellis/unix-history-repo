/* oper_ureject.c - deal with user rejection of an operation */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/oper_ureject.c,v 7.3 91/02/22 09:39:37 mrose Interim $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/oper_ureject.c,v 7.3 91/02/22 09:39:37 mrose Interim $
 *
 *
 * $Log:	oper_ureject.c,v $
 * Revision 7.3  91/02/22  09:39:37  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:54:36  mrose
 * sync
 * 
 * Revision 7.1  90/03/15  11:19:05  mrose
 * quipu-sync
 * 
 * Revision 7.0  89/11/23  22:17:55  mrose
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

extern LLog * log_dsap;

oper_ureject(conn, rou)
struct connection	* conn;
struct RoSAPureject     * rou;
{
    DLOG(log_dsap,LLOG_TRACE,( "oper_ureject"));

    if(rou->rou_noid)
    {
	LLOG(log_dsap,LLOG_EXCEPTIONS,( "Non-specific U-REJECT.INDICATION : %d (%d)",
	  rou->rou_reason,conn->cn_ad));
    }
    else
    {
	struct oper_act *       on;

	for(on=conn->cn_operlist; on!=NULLOPER; on=on->on_next_conn)
	    if(on->on_id == rou->rou_id)
		break;

	if(on == NULLOPER)
	{
	    LLOG(log_dsap,LLOG_EXCEPTIONS,( "Unlocatable U-REJECT.INDICATION : %d (%d)",
	      rou->rou_reason,conn->cn_ad));
	    return;
	}

	if(on->on_state == ON_ABANDONED)
	{
	    LLOG(log_dsap, LLOG_NOTICE, ("oper_result - operation had been abandoned"));
	    oper_extract(on);
	    return;
	}

	oper_fail_wakeup(on);
    }
}

