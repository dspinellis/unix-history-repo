/* dapunbind.c - DAP unbind operation */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dapunbind.c,v 7.1 91/02/22 09:21:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dapunbind.c,v 7.1 91/02/22 09:21:08 mrose Interim $
 *
 *
 * $Log:	dapunbind.c,v $
 * Revision 7.1  91/02/22  09:21:08  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:30  mrose
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

#include "logger.h"
#include "quipu/util.h"
#include "quipu/dap.h"
#include "quipu/dap2.h"

extern	LLog	* log_dsap;

/*
* Seems that this should be a #define
*/

ds_unbind ()
{
    return(dap_unbind(dsap_ad));
}

dap_unbind (ad)
int	  ad;
{
    int				  ret;
    struct DAPrelease         dr_s;
    struct DAPrelease         *dr = &dr_s;
    struct DAPindication      di_s;
    struct DAPindication      *di = &di_s;

    DLOG(log_dsap, LLOG_NOTICE, ("dap_unbind: <%d, normal, nullpe>",
					ad));

    ret = DapUnBindRequest (ad, NOTOK, dr, di);

    if (ret != OK)
    {
	LLOG (log_dsap, LLOG_EXCEPTIONS, ("DapUnBindRequest() failed"));
	return (DS_ERROR_LOCAL);
    }

    if (!dr->dr_affirmative) {
	LLOG(log_dsap, LLOG_EXCEPTIONS, ("DapUnBindRequest failed"));
	return(DS_ERROR_PROVIDER);
    }

    return(DS_OK);
}

/* ARGSUSED */

int	  DapUnBindRequest (sd, secs, dr, di)
int			  sd;
int			  secs;
struct DAPrelease	* dr;
struct DAPindication	* di;
{
	int			  result;
	struct AcSAPrelease	  acr_s;
	struct AcSAPrelease	* acr = &(acr_s);
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);


	result = RoUnBindRequest (sd, NULLPE, secs, acr, rni);

	if (result == OK)
	{
		dr->dr_affirmative = acr->acr_affirmative;
		dr->dr_reason = acr->acr_reason;
		ACRFREE(acr);
		return (OK);
	}

	if (result == NOTOK)
	{
		return (ronot2daplose (di, "D-UNBIND.REQUEST", rni));
	}

	return (result);
}

/*    D-UNBIND.RETRY */

/* ARGSUSED */

int	  DapUnBindRetry (sd, secs, dr, di)
int			  sd;
int			  secs;
struct DAPrelease	* dr;
struct DAPindication	* di;
{
	int			  result;
	struct AcSAPrelease	  acr_s;
	struct AcSAPrelease	* acr = &(acr_s);
	struct RoNOTindication	  rni_s;
	struct RoNOTindication	* rni = &(rni_s);

	result = RoUnBindRetry (sd, secs, acr, rni);

	if (result == OK)
	{
		dr->dr_affirmative = acr->acr_affirmative;
		dr->dr_reason = acr->acr_reason;
		ACRFREE (acr);
		return (OK);
	}

	if (result == NOTOK)
	{
		return (ronot2daplose (di, "D-UNBIND.RETRY", rni));
	}

	return (result);
}

