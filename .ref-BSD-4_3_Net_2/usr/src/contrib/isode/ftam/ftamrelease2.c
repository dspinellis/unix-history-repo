/* ftamrelease2.c - FPM: respond to release */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamrelease2.c,v 7.1 91/02/22 09:23:11 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamrelease2.c,v 7.1 91/02/22 09:23:11 mrose Interim $
 *
 *
 * $Log:	ftamrelease2.c,v $
 * Revision 7.1  91/02/22  09:23:11  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:50  mrose
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

/*    F-TERMINATE.RESPONSE */

int	FTerminateResponse (sd, sharedASE, charging, fti)
int	sd;
PE	sharedASE;
struct FTAMcharging *charging;
struct FTAMindication *fti;
{
    SBV	    smask;
    int     result;
    register struct ftamblk *fsb;

    if (charging && charging -> fc_ncharge > NFCHRG)
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
			"too many charges");
    missingP (fti);

    smask = sigioblock ();

    ftamFsig (fsb, sd);

    result = FTerminateResponseAux (fsb, sharedASE, charging, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  FTerminateResponseAux (fsb, sharedASE, charging, fti)
register struct ftamblk *fsb;
PE	sharedASE;
struct FTAMcharging *charging;
struct FTAMindication *fti;
{
    int     result;
    PE	    pe;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    register struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__TERMINATE__response *rsp;

    pe = NULLPE;
    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL) {
no_mem: ;
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
out: ;
	if (pe)
	    pe_free (pe);
	if (pdu)
	    free_FTAM_PDU (pdu);
	if (fti -> fti_abort.fta_action == FACTION_PERM)
	    freefsblk (fsb);
	return NOTOK;
    }
    pdu -> offset = type_FTAM_PDU_f__terminate__response;
    if ((rsp = (struct type_FTAM_F__TERMINATE__response *)
			calloc (1, sizeof *rsp)) == NULL)
	goto no_mem;
    pdu -> un.f__terminate__response = rsp;
    if (sharedASE
	    && (rsp -> shared__ASE__information =
			shared2fpm (fsb, sharedASE, fti)) == NULL)
	goto out;
    if (charging
	    && charging -> fc_ncharge > 0
	    && (rsp -> charging = chrg2fpm (fsb, charging, fti)) == NULL)
	goto out;

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			 "error encoding PDU: %s", PY_pepy);
	goto out;
    }

    pe -> pe_context = fsb -> fsb_id;

    fsbtrace (fsb, (fsb -> fsb_fd, "A-RELEASE.RESPONSE",
		    "F-TERMINATE-response", pe, 0));

    result = AcRelResponse (fsb -> fsb_fd, ACS_ACCEPT, ACR_NORMAL, &pe, 1,
			    aci);

    pe_free (pe);
    pe = NULLPE;
    free_FTAM_PDU (pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) acs2ftamlose (fsb, fti, "AcRelResponse", aca);
	goto out;
    }

    fsb -> fsb_fd = NOTOK;
    freefsblk (fsb);

    return OK;
}
