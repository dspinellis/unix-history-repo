/* ftamselect.c - FPM: map descriptors */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamselect.c,v 7.2 91/02/22 09:23:14 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamselect.c,v 7.2 91/02/22 09:23:14 mrose Interim $
 *
 *
 * $Log:	ftamselect.c,v $
 * Revision 7.2  91/02/22  09:23:14  mrose
 * Interim 6.8
 * 
 * Revision 7.1  89/12/14  10:04:10  mrose
 * bdt
 * 
 * Revision 7.0  89/11/23  21:53:53  mrose
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
#include <signal.h>
#include "fpkt.h"

/*    map ftam descriptors for select() */

int	FSelectMask (sd, mask, nfds, fti)
int	sd;
fd_set *mask;
int    *nfds;
struct FTAMindication *fti;
{
    SBV     smask;
    register struct ftamblk *fsb;
    struct PSAPindication   pis;
    register struct PSAPabort  *pa = &pis.pi_abort;

    missingP (mask);
    missingP (nfds);
    missingP (fti);

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    if (fsb -> fsb_data.px_ninfo > 0)
	goto waiting;
    if (PSelectMask (fsb -> fsb_fd, mask, nfds, &pis) == NOTOK)
	switch (pa -> pa_reason) {
	    case PC_WAITING: 
waiting: ;
		(void) sigiomask (smask);
		return ftamlose (fti, FS_GEN_WAITING, 0, NULLCP, NULLCP);

	    default: 
		(void) ps2ftamlose (fsb, fti, "PSelectMask", pa);
		freefsblk (fsb);
		(void) sigiomask (smask);
		return NOTOK;
	}

    (void) sigiomask (smask);

    return OK;
}
