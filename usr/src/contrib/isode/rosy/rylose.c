/* rylose.c - ROSY: clean-up after association termination */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosy/RCS/rylose.c,v 7.1 91/02/22 09:42:02 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosy/RCS/rylose.c,v 7.1 91/02/22 09:42:02 mrose Interim $
 *
 *
 * $Log:	rylose.c,v $
 * Revision 7.1  91/02/22  09:42:02  mrose
 * Interim 6.8
 * 
 * Revision 6.0  89/03/18  23:42:54  mrose
 * Release 5.0
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
#include "rosy.h"


#define	missingP(p) \
{ \
    if (p == NULL) \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}

/*    clean-up after association termination */

int	RyLose (sd, roi)
int	sd;
struct RoSAPindication *roi;
{
    missingP (roi);

    loseopblk (sd, ROS_DONE);
    losedsblk (sd);

    return OK;
}
