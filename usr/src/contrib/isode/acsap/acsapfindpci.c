/* acsapfindpci.c - find PCI for ACSE */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/acsapfindpci.c,v 7.1 91/02/22 09:14:08 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/acsapfindpci.c,v 7.1 91/02/22 09:14:08 mrose Interim $
 *
 *
 * $Log:	acsapfindpci.c,v $
 * Revision 7.1  91/02/22  09:14:08  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:21:49  mrose
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
#define	ACSE
#include "acpkt.h"

/*  */

int	AcFindPCI (sd, pci, aci)
int	sd;
int    *pci;
struct AcSAPindication *aci;
{
    SBV     smask;
    register struct assocblk  *acb;

    missingP (pci);
    missingP (aci);

    smask = sigioblock ();

    if ((acb = findacblk (sd)) == NULL) {
	(void) sigiomask (smask);
	return acsaplose (aci, ACS_PARAMETER, NULLCP,
		"invalid association descriptor");
    }

    *pci = acb -> acb_id;
    
    (void) sigiomask (smask);

    return OK;
}
