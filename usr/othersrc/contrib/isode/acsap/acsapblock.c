/* acsapblock.c - manage association blocks */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapblock.c,v 7.2 91/02/22 09:14:05 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapblock.c,v 7.2 91/02/22 09:14:05 mrose Interim $
 *
 *
 * $Log:	acsapblock.c,v $
 * Revision 7.2  91/02/22  09:14:05  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/24  14:49:54  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:21:46  mrose
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


/* LINTLIBRARY */

#include <stdio.h>
#include "acpkt.h"

/*    DATA */

static int  once_only = 0;
static struct assocblk assocque;
static struct assocblk *ACHead = &assocque;

/*    ASSOCIATION BLOCKS */

struct assocblk  *newacblk () {
    register struct assocblk *acb;

    acb = (struct assocblk   *) calloc (1, sizeof *acb);
    if (acb == NULL)
	return NULL;

    acb -> acb_fd = NOTOK;
    acb -> acb_actno = 1;

    if (once_only == 0) {
	ACHead -> acb_forw = ACHead -> acb_back = ACHead;
	once_only++;
    }

    insque (acb, ACHead -> acb_back);

    return acb;
}

/*  */

freeacblk (acb)
register struct assocblk *acb;
{
    if (acb == NULL)
	return;

    if (acb -> acb_flags & ACB_STICKY) {
	acb -> acb_flags &= ~ACB_STICKY;
	return;
    }

    if (acb -> acb_fd != NOTOK && acb -> acb_uabort)
	if (acb -> acb_flags & ACB_ACS) {
	    if (acb -> acb_flags & ACB_RTS) {/* recurse */
		struct AcSAPindication  acis;

		(void) (*acb -> acb_uabort) (acb -> acb_fd, NULLPEP, 0, &acis);
		return;
	    }
	    else {
		struct PSAPindication   pis;

		(void) (*acb -> acb_uabort) (acb -> acb_fd, NULLPEP, 0, &pis);
	    }
	}
	else {
	    struct SSAPindication   sis;

	    (void) (*acb -> acb_uabort) (acb -> acb_fd, NULLCP, 0, &sis);
	}

    if (acb -> acb_flags & ACB_FINISH)
	ACFFREE (&acb -> acb_finish);

    if (acb -> acb_context)
	oid_free (acb -> acb_context);
    if (acb -> acb_retry)
	pe_free (acb -> acb_retry);

    FREEACB (acb);

    if (acb -> acb_apdu)
	pe_free (acb -> acb_apdu);

    remque (acb);

    free ((char *) acb);
}

/*  */

struct assocblk   *findacblk (sd)
register int sd;
{
    register struct assocblk *acb;

    if (once_only == 0)
	return NULL;

    for (acb = ACHead -> acb_forw; acb != ACHead; acb = acb -> acb_forw)
	if (acb -> acb_fd == sd)
	    return acb;

    return NULL;
}
