/* dsapunbind1.c - DSAP: Maps D-UNBIND onto RO-UNBIND.REQUEST */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapunbind1.c,v 7.1 91/02/22 09:21:27 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapunbind1.c,v 7.1 91/02/22 09:21:27 mrose Interim $
 *
 *
 * $Log:	dsapunbind1.c,v $
 * Revision 7.1  91/02/22  09:21:27  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:46:01  mrose
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

/*    D-UNBIND.REQUEST */

/* ARGSUSED */

int	  DUnBindRequest (sd, secs, dr, di)
int			  sd;
int			  secs;
struct DSAPrelease	* dr;
struct DSAPindication	* di;
{
	int			  result;
	struct AcSAPrelease	  acr_s;
	struct AcSAPrelease	* acr = &(acr_s);
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);

	watch_dog ("RoUnBindRequest");
	result = RoUnBindRequest (sd, NULLPE, secs, acr, rni);
	watch_dog_reset();

	if (result == OK)
	{
		dr->dr_affirmative = acr->acr_affirmative;
		dr->dr_reason = acr->acr_reason;
		ACRFREE(acr);
		return (OK);
	}

	if (result == NOTOK)
	{
		return (ronot2dsaplose (di, "D-UNBIND.REQUEST", rni));
	}

	return (result);
}

/*    D-UNBIND.RETRY */

/* ARGSUSED */

int	  DUnBindRetry (sd, secs, dr, di)
int			  sd;
int			  secs;
struct DSAPrelease	* dr;
struct DSAPindication	* di;
{
	int			  result;
	struct AcSAPrelease	  acr_s;
	struct AcSAPrelease	* acr = &(acr_s);
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);

	watch_dog ("RoUnBindRetry");
	result = RoUnBindRetry (sd, secs, acr, rni);
	watch_dog_reset();

	if (result == OK)
	{
		dr->dr_affirmative = acr->acr_affirmative;
		dr->dr_reason = acr->acr_reason;
		ACRFREE (acr);
		return (OK);
	}

	if (result == NOTOK)
	{
		return (ronot2dsaplose (di, "D-UNBIND.RETRY", rni));
	}

	return (result);
}

