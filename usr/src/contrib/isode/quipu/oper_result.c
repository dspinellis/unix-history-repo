/* oper_result.c - deal with result of an operation */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/oper_result.c,v 7.4 91/02/22 09:39:36 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/oper_result.c,v 7.4 91/02/22 09:39:36 mrose Interim $
 *
 *
 * $Log:	oper_result.c,v $
 * Revision 7.4  91/02/22  09:39:36  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/10/17  11:54:35  mrose
 * sync
 * 
 * Revision 7.2  90/07/09  14:46:25  mrose
 * sync
 * 
 * Revision 7.1  90/04/18  08:49:57  mrose
 * 6.2
 * 
 * Revision 7.0  89/11/23  22:17:54  mrose
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

extern	LLog	* log_dsap;
extern	int	  dn_print();
extern time_t	  timenow;

oper_result(cn, di)
struct connection	* cn;
struct DSAPindication	* di;
{
    struct DSAPresult	* dr = &(di->di_result);
    struct oper_act *   on;

    DLOG(log_dsap, LLOG_TRACE, ("oper_result()"));

    for(on=cn->cn_operlist; on != NULLOPER; on=on->on_next_conn)
    {
	if(on->on_id == dr->dr_id)
	    break;
    }

    if(on == NULLOPER)
    {
	LLOG(log_dsap, LLOG_FATAL, ("Cannot find operation to match result"));
	send_ro_ureject(cn->cn_ad, &(dr->dr_id), ROS_RRP_UNRECOG);
	return;
    }

    if (dr->dr_res.dcr_dsres.result_type != on->on_arg->dca_dsarg.arg_type)
    {
	LLOG(log_dsap, LLOG_NOTICE, ("oper_result - operation had been abandoned"));
	send_ro_ureject(on->on_conn->cn_ad, &(dr->dr_id), ROS_RRP_MISTYPED);
	oper_extract(on);
	return;
    }

    if(on->on_state == ON_ABANDONED)
    {
	LLOG(log_dsap, LLOG_NOTICE, ("oper_result - operation had been abandoned"));
	oper_extract(on);
	return;
    }

    on->on_resp = (*di);	/* struct copy */

    cn->cn_last_used = timenow;

    switch(on->on_type)
    {
    case ON_TYPE_X500:
	task_result_wakeup (on);
	break;
    case ON_TYPE_SUBTASK:
	subtask_result_wakeup (on);
	break;
    case ON_TYPE_BIND_COMPARE:
	bind_compare_result_wakeup(on);
	break;
    case ON_TYPE_GET_DSA_INFO:
	dsa_info_result_wakeup(on);
	break;
    case ON_TYPE_GET_EDB:
    case ON_TYPE_SHADOW:
	on->on_state = ON_COMPLETE;
	break;
    default:
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("oper_result: operation of unknown type"));
	oper_extract(on);
	break;
    }
}

