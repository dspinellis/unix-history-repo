/* dsapunbind2.c - DSAP: maps D-UNBIND mapping onto RO-UNBIND */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapunbind2.c,v 7.1 91/02/22 09:21:29 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapunbind2.c,v 7.1 91/02/22 09:21:29 mrose Interim $
 *
 *
 * $Log:	dsapunbind2.c,v $
 * Revision 7.1  91/02/22  09:21:29  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:46:02  mrose
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

#include "quipu/dsap.h"

/*    D-UNBIND.ACCEPT */

/* ARGSUSED */

int	  DUnBindAccept (sd, di)
int			  sd;
struct DSAPindication	* di;
{
	int			  result;
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);

	watch_dog ("RoUnBindResult");
	result = RoUnBindResult (sd, NULLPE, rni);
	watch_dog_reset();

	if (result == NOTOK)
	{
		(void) ronot2dsaplose (di, "D-UNBIND.ACCEPT", rni);
		return (NOTOK);
	}

	return (result);
}

/*    D-UNBIND.REJECT */

/* ARGSUSED */

int	  DUnBindReject (sd, status, reason, di)
int			  sd;
int			  status;
int			  reason;
struct DSAPindication	* di;
{
	int			  result;
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);

	watch_dog ("DUnBindReject");
	result = RoUnBindReject (sd, status, reason, rni);
	watch_dog_reset ();

	if (result == NOTOK)
	{
		(void) ronot2dsaplose (di, "D-UNBIND.REJECT", rni);
		return (NOTOK);
	}

	return (result);
}

