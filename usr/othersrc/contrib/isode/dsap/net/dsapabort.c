/* dsapabort.c - DSAP: Lose the binding identified by the descriptor */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/dsap/net/RCS/dsapabort.c,v 7.1 91/02/22 09:21:13 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/dsap/net/RCS/dsapabort.c,v 7.1 91/02/22 09:21:13 mrose Interim $
 *
 *
 * $Log:	dsapabort.c,v $
 * Revision 7.1  91/02/22  09:21:13  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/07/26  14:45:36  mrose
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
#include "quipu/dsap.h"

extern LLog	* log_dsap;

/*    RO-ABORT.REQUEST */

/* ARGSUSED */


int	  DUAbortRequest (sd, di)
int			  sd;
struct DSAPindication	* di;
{
	int			  result;
        struct RoNOTindication    rni_s;
        struct RoNOTindication  * rni = &(rni_s);

	LLOG (log_dsap, LLOG_EXCEPTIONS, ("RO-ABORT-BIND.REQUEST called on %d", sd));

	watch_dog ("RoBindUAbort");
	result = RoBindUAbort (sd, rni);
	watch_dog_reset();

	if (result != OK)
	{
	        LLOG (log_dsap, LLOG_EXCEPTIONS, ("D-ABORT-BIND.REQUEST failed on %d", sd));
		return (ronot2dsaplose (di, "D-ABORT-BIND.REQUEST", rni));
	}

	return (OK);
}

