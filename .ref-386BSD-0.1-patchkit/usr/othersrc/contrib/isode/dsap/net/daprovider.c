/* daprovider.c - DAP: Support for DAP Actions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/daprovider.c,v 7.1 91/02/22 09:21:06 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/daprovider.c,v 7.1 91/02/22 09:21:06 mrose Interim $
 *
 *
 * $Log:	daprovider.c,v $
 * Revision 7.1  91/02/22  09:21:06  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:26  mrose
 * *** empty log message ***
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

#include <stdio.h>
#include "quipu/dap2.h"
#include "tailor.h"

/*    BIND interface */

int	  ronot2daplose (di, event, rni)
struct DAPindication	* di;
char			* event;
struct RoNOTindication	* rni;
{

	char	* cp;
	char	  buffer[BUFSIZ];

/*
	if (event)
		SLOG (addr_log, LLOG_EXCEPTIONS, NULLCP,
			((rni->rni_cc > 0) ? "%s: %s [%*.*s]" : "%s: %s",
			event, AcErrString (rni->rni_reason), rni->rni_cc,
			rni->rni_cc, rni->rni_data));
*/
	(void) sprintf (cp = buffer, " (Error in RO-BIND)");

	if (rni->rni_cc > 0)
		return (daplose (di, DA_RONOT, NULLCP, "%*.*s%s",
			rni->rni_cc, rni->rni_cc, rni->rni_data, cp));
	else
		return (daplose (di, DA_RONOT, NULLCP, "%s", cp));
}

/*    ROS interface */

int	  ros2daplose (di, event, rop)
struct DAPindication	* di;
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
		return (daplose (di, DA_ROS, NULLCP, "%*.*s%s",
			rop->rop_cc, rop->rop_cc, rop->rop_data, cp));
	else
		return (daplose (di, DA_ROS, NULLCP, "%s", cp));
}

int	  ros2dapreject (di, event, rou)
struct DAPindication	* di;
char			* event;
struct RoSAPureject	* rou;
{
	char	* cp;
	char	  buffer[BUFSIZ];

	(void) sprintf (cp = buffer, " (Reject at ROS)");

	if (rou->rou_noid)
		return (dapreject (di, DA_ROS, -1, NULLCP, " no op id, reason: %d%s", rou->rou_reason, cp));
	else
		return (dapreject (di, DA_ROS, rou->rou_id, NULLCP, " op id %d, reason: %d%s", rou->rou_id, rou->rou_reason, cp));
}

