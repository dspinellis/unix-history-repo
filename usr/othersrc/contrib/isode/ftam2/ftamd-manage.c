/* ftamd-manage.c - FTAM responder -- management */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam2/RCS/ftamd-manage.c,v 7.1 91/02/22 09:23:53 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam2/RCS/ftamd-manage.c,v 7.1 91/02/22 09:23:53 mrose Interim $
 *
 *
 * $Log:	ftamd-manage.c,v $
 * Revision 7.1  91/02/22  09:23:53  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:54:29  mrose
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

#include <stdio.h>
#include "ftamsystem.h"

/*    MANAGEMENT */

int	ftam_managementindication (ftg)
struct FTAMgroup *ftg;
{
    struct FTAMgroup    ftms;
    struct FTAMgroup   *ftm = &ftms;
    struct FTAMindication   ftis;
    register struct FTAMindication *fti = &ftis;

    ftam_selection (ftg, ftm);

    if (myfd != NOTOK) {
#ifndef	BRIDGE
	unlock ();
	(void) close (myfd);
#else
	(void) close (myfd);
	(void) ftp_reply ();
#endif
	myfd = NOTOK;
    }

    if (FManageResponse (ftamfd, ftm, fti) == NOTOK)
	ftam_adios (&fti -> fti_abort, "F-MANAGE.RESPONSE");

    FTGFREE (ftg);
}
