/* ftamaccess1.c - FPM: initiate file access */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamaccess1.c,v 7.1 91/02/22 09:22:33 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamaccess1.c,v 7.1 91/02/22 09:22:33 mrose Interim $
 *
 *
 * $Log:	ftamaccess1.c,v $
 * Revision 7.1  91/02/22  09:22:33  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:20  mrose
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

/*    F-{LOCATE,ERASE}.REQUEST */

int	FAccessRequest (sd, operation, identity, lock, fti)
int	sd;
int	operation;
struct FADUidentity *identity;
int	lock;	/* F-LOCATE.REQUEST only */
struct FTAMindication *fti;
{
    SBV      smask;
    int     result,
	    state;
    register struct ftamblk *fsb;

    switch (operation) {
	case FA_OPS_LOCATE: 
	    state = FSB_LOCATE;
	    break;

	case FA_OPS_ERASE: 
	    state = FSB_ERASE;
	    break;

	default: 
	    return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
		    "bad value for operation parameter");
    }
    missingP (identity);
    missingP (fti);

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    result = FAccessRequestAux (fsb, state, identity, lock, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  FAccessRequestAux (fsb, state, identity, lock, fti)
register struct ftamblk *fsb;
int	state;
struct FADUidentity *identity;
int	lock;
struct FTAMindication *fti;
{
    int     result;
    PE	    pe;
    struct PSAPindication   pis;
    struct PSAPindication  *pi = &pis;
    struct PSAPabort   *pa = &pi -> pi_abort;
    register struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__LOCATE__request *req;

    if (!(fsb -> fsb_flags & FSB_INIT))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "not initiator");
    if (fsb -> fsb_state != FSB_DATAIDLE)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "wrong state");
    if (!(fsb -> fsb_units & FUNIT_ACCESS))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "file access not allowed");

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
			/* F-ERASE-request is identical... */
    pdu -> offset = state != FSB_LOCATE ? type_FTAM_PDU_f__erase__request
					: type_FTAM_PDU_f__locate__request;
    if ((req = (struct type_FTAM_F__LOCATE__request *)
			calloc (1, sizeof *req)) == NULL)
	goto no_mem;
    pdu -> un.f__locate__request = req;
    if ((req -> file__access__data__unit__identity =
			faduid2fpm (fsb, identity, fti)) == NULL)
	goto out;
    if ((fsb -> fsb_units & FUNIT_FADULOCK) && state == FSB_LOCATE) {
	if ((req -> fadu__lock = (struct type_FTAM_FADU__Lock *)
					calloc (1, sizeof *req -> fadu__lock))
	        == NULL)
	    goto no_mem;
	req -> fadu__lock -> parm = lock ? int_FTAM_FADU__Lock_on
					 : int_FTAM_FADU__Lock_off;
    }

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			 "error encoding PDU: %s", PY_pepy);
	goto out;
    }

    pe -> pe_context = fsb -> fsb_id;

    fsbtrace (fsb, (fsb -> fsb_fd, "P-DATA.REQUEST",
		    state == FSB_LOCATE ? "F-LOCATE-request"
					: "F-ERASE-request", pe, 0));

    result = PDataRequest (fsb -> fsb_fd, &pe, 1, pi);

    pe_free (pe);
    pe = NULLPE;
    free_FTAM_PDU (pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) ps2ftamlose (fsb, fti, "PDataRequest", pa);
	goto out;
    }

    fsb -> fsb_state = state;

    return FWaitRequestAux (fsb, NOTOK, fti);
}
