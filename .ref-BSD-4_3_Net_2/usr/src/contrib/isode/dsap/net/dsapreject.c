/* dsapreject.c - DSAP: Reject a remote operations event */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapreject.c,v 7.1 91/02/22 09:21:24 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapreject.c,v 7.1 91/02/22 09:21:24 mrose Interim $
 *
 *
 * $Log:	dsapreject.c,v $
 * Revision 7.1  91/02/22  09:21:24  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:57  mrose
 * *** empty log message ***
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

#include "logger.h"
#include "quipu/util.h"
#include "quipu/dsap.h"


extern LLog	* log_dsap;

/* ARGSUSED */

int	  DRejectRequest (sd, reason, id)
int	  sd;
int	  reason;
int	  id;
{
	int	  result;
	struct RoSAPindication	  roi_s;
	struct RoSAPindication	* roi = &(roi_s);

	DLOG (log_dsap,LLOG_TRACE,( "DRejectRequest()"));

	watch_dog("RoURejectRequest");
	result = RoURejectRequest(sd, &id, reason, ROS_NOPRIO, roi);
	watch_dog_reset();

	if (result == NOTOK)
	{
		LLOG (log_dsap, LLOG_EXCEPTIONS,( "DBindReject: RoURejectRequest failed"));
        }

}

