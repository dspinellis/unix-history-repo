/* ns_ro_ureject.c - */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/task_ureject.c,v 7.1 91/02/22 09:40:04 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/quipu/RCS/task_ureject.c,v 7.1 91/02/22 09:40:04 mrose Interim $
 *
 *
 * $Log:	task_ureject.c,v $
 * Revision 7.1  91/02/22  09:40:04  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:18:17  mrose
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

send_ro_ureject(ad, id_p, urej)
int     ad;
int     *id_p;
int     urej;
{
    struct RoSAPindication      roi_s;
    struct RoSAPindication      *roi = &roi_s;
    struct RoSAPpreject         *rop = &(roi->roi_preject);

    DLOG(log_dsap, LLOG_TRACE, ("send_ro_ureject()"));

    watch_dog("RoURejectRequest");
    if(RoURejectRequest(ad, id_p, urej, ROS_NOPRIO, roi) == NOTOK)
    {
	watch_dog_reset();
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("RO-U-REJECT.REQUEST: %s", 
	  RoErrString(rop->rop_reason)));
	    if(ROS_FATAL(rop->rop_reason) || (rop->rop_reason == ROS_PARAMETER))
	    {
		LLOG(log_dsap, LLOG_FATAL, ("RoUReject fatal PReject"));
	    }
    } else
	watch_dog_reset();

}

