/* dsaprovider.c - DSAP: Support for DSAP Actions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsaprovider.c,v 7.1 91/02/22 09:21:26 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsaprovider.c,v 7.1 91/02/22 09:21:26 mrose Interim $
 *
 *
 * $Log:	dsaprovider.c,v $
 * Revision 7.1  91/02/22  09:21:26  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:46:00  mrose
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

#include <stdio.h>
#include "quipu/dsap.h"
#include "tailor.h"

/*    BIND interface */

int	  ronot2dsaplose (di, event, rni)
struct DSAPindication	* di;
char			* event;
struct RoNOTindication	* rni;
{

	char	* cp;
	char	  buffer[BUFSIZ];

#ifdef DEBUG

	if (event)
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			((rni->rni_cc > 0) ? "%s: %s [%*.*s]" : "%s: %s",
			event, AcErrString (rni->rni_reason), rni->rni_cc,
			rni->rni_cc, rni->rni_data));
#endif

	(void) sprintf (cp = buffer, " (Error in RO-BIND)");

	if (rni->rni_cc > 0)
		return (dsaplose (di, DA_RO_BIND, NULLCP, "%*.*s%s",
			rni->rni_cc, rni->rni_cc, rni->rni_data, cp));
	else
		return (dsaplose (di, DA_RO_BIND, NULLCP, "%s", cp));
}

/*    ROS interface */

int	  ros2dsaplose (di, event, rop)
struct DSAPindication	* di;
char			* event;
struct RoSAPpreject	* rop;
{
	char	* cp;
	char	  buffer[BUFSIZ];

/*
	if (event)
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			((rop->rop_cc > 0) ? "%s: %s [%*.*s]" : "%s: %s",
			event, AcErrString (rop->rop_reason), rop->rop_cc,
			rop->rop_cc, rop->rop_data));
*/
	(void) sprintf (cp = buffer, " (Error in ROS)");

	if (rop->rop_cc > 0)
		return (dsaplose (di, DA_ROS, NULLCP, "%*.*s%s",
			rop->rop_cc, rop->rop_cc, rop->rop_data, cp));
	else
		return (dsaplose (di, DA_ROS, NULLCP, "%s", cp));
}

int	  ros2dsapreject (di, event, rou)
struct DSAPindication	* di;
char			* event;
struct RoSAPureject	* rou;
{
	char	* cp;
	char	  buffer[BUFSIZ];

	(void) sprintf (cp = buffer, " (Reject at ROS)");

	if (rou->rou_noid)
		return (dsapreject (di, DA_ROS, -1, NULLCP, " no op id, reason: %d%s", rou->rou_reason, cp));
	else
		return (dsapreject (di, DA_ROS, rou->rou_id, NULLCP, " op id %d, reason: %d%s", rou->rou_id, rou->rou_reason, cp));
}

