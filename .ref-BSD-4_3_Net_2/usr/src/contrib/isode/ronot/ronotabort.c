/* ronotabort.c - RONOT: bail-out routine which logs abort to rosap log */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ronot/RCS/ronotabort.c,v 7.2 91/02/22 09:50:29 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ronot/RCS/ronotabort.c,v 7.2 91/02/22 09:50:29 mrose Interim $
 *
 *
 * $Log:	ronotabort.c,v $
 * Revision 7.2  91/02/22  09:50:29  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/26  14:33:53  mrose
 * template
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

#include "tailor.h"
#include "logger.h"
#include "rosap.h"
#include "ronot.h"

/*    RO-ABORT.REQUEST */

/* ARGSUSED */

int	  RoBindUAbort (sd, rni)
int			  sd;
struct RoNOTindication	* rni;
{
	int			  result;
        struct AcSAPindication    aci_s;
        struct AcSAPindication  * aci = &(aci_s);
        struct AcSAPabort       * aca = &(aci->aci_abort);

	LLOG (rosap_log, LLOG_EXCEPTIONS, ("RO-ABORT-BIND.REQUEST called on %d", sd));

	result = AcUAbortRequest (sd, NULLPEP, 0, aci);

	if (result != OK)
	{
	        LLOG (rosap_log, LLOG_EXCEPTIONS, ("RO-ABORT-BIND.REQUEST failed on %d", sd));
		return (acs2ronotlose (rni, "RO-ABORT-BIND.REQUEST", aca));
	}

	return (OK);
}

