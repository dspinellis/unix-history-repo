/* oper_error.c - deal with return of error to an operation */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/oper_error.c,v 7.3 91/02/22 09:39:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/oper_error.c,v 7.3 91/02/22 09:39:33 mrose Interim $
 *
 *
 * $Log:	oper_error.c,v $
 * Revision 7.3  91/02/22  09:39:33  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:54:31  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:46:23  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:17:51  mrose
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
#include "quipu/ds_error.h"

extern	LLog	* log_dsap;

oper_error(conn, di)
struct connection	* conn;
struct DSAPindication	* di;
{
    struct DSAPerror	* de = &(di->di_error);
    struct oper_act *   oper;

    DLOG(log_dsap, LLOG_TRACE, ("net_wait_ro_error"));

    for(oper=conn->cn_operlist; oper != NULLOPER; oper=oper->on_next_conn)
	if(oper->on_id == de->de_id)
	    break;

    if(oper == NULLOPER)
    {
	LLOG(log_dsap, LLOG_FATAL, ("oper_error: Cannot locate operation for error"));
	send_ro_ureject(conn->cn_ad, &(de->de_id), ROS_REP_UNRECOG);
	return;
    }

    if(oper->on_state == ON_ABANDONED)
    {
	LLOG(log_dsap, LLOG_NOTICE, ("oper_error: operation had been abandoned"));
	oper_extract(oper);
	return;
    }

    if(!ds_recog_err(de->de_err.dse_type))
    {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("oper_error - Unrecognised error"));
	send_ro_ureject(conn->cn_ad, &(de->de_id), ROS_REP_RECERR);
	oper_fail_wakeup(oper);
    }

    oper->on_resp = (*di); /* struct copy */

    /* Need to check type of operation here! */
    switch(oper->on_type)
    {
    case ON_TYPE_X500:
	task_error_wakeup(oper);
	break;
    case ON_TYPE_SUBTASK:
	subtask_error_wakeup(oper);
	break;
    case ON_TYPE_BIND_COMPARE:
	bind_compare_error_wakeup(oper);
	break;
    case ON_TYPE_GET_DSA_INFO:
	dsa_info_error_wakeup(oper);
	break;
    case ON_TYPE_GET_EDB:
	get_edb_fail_wakeup(oper);
	break;
    case ON_TYPE_SHADOW:
	shadow_fail_wakeup(oper);
	break;
    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("oper_error - on_type invalid"));
	break;
    }

}

