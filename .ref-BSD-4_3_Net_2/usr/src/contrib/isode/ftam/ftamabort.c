/* ftamabort.c - FPM: user abort */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamabort.c,v 7.1 91/02/22 09:22:32 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamabort.c,v 7.1 91/02/22 09:22:32 mrose Interim $
 *
 *
 * $Log:	ftamabort.c,v $
 * Revision 7.1  91/02/22  09:22:32  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:19  mrose
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

/*    F-U-ABORT.REQUEST */

int	FUAbortRequest (sd, action, diag, ndiag, fti)
int	sd;
int	action;
struct FTAMdiagnostic diag[];
int	ndiag;
struct FTAMindication *fti;
{
    SBV	    smask;
    int     result;
    register struct ftamblk *fsb;

    switch (action) {
	case FACTION_SUCCESS: 
	case FACTION_TRANS: 
	case FACTION_PERM: 
	    break;

	default: 
	    return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
		    "bad value for action parameter");
    }
    toomuchP (diag, ndiag, NFDIAG, "diagnostic");

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    result = FAbortRequestAux (fsb, type_FTAM_PDU_f__u__abort__request, action,
			       diag, ndiag, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

int	FAbortRequestAux (fsb, id, action, diag, ndiag, fti)
register struct ftamblk *fsb;
int	id,
	action;
struct FTAMdiagnostic diag[];
int	ndiag;
struct FTAMindication *fti;
{
    int     result;
    PE	    pe;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    register struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__U__ABORT__request *req;

    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	goto carry_on;
    pdu -> offset = id;	/* F-P-ABORT-request is identical... */
    if ((req = (struct type_FTAM_F__U__ABORT__request *)
			calloc (1, sizeof *req)) == NULL)
	goto carry_on;
    pdu -> un.f__u__abort__request = req;
    if ((req -> action__result =
			(struct type_FTAM_Action__Result *)
				   calloc (1, sizeof *req -> action__result))
		    == NULL)
	goto carry_on;
    req -> action__result -> parm = action;
    if (ndiag > 0
	    && (req -> diagnostic =
			diag2fpm (fsb,
				  id == type_FTAM_PDU_f__p__abort__request,
				  diag, ndiag, fti)) == NULL) {
	free_FTAM_PDU (pdu);
	if (fti -> fti_abort.fta_action == FACTION_TRANS)
	    return NOTOK;
	pdu = NULL;
    }
carry_on: ;

    pe = NULLPE;
    if (pdu) {
	result = encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu);

	if (result == NOTOK) {
	    if (pe)
		pe_free (pe), pe = NULLPE;
	}
	else
	    if (pe)
		pe -> pe_context = fsb -> fsb_id;
    }

    fsbtrace (fsb,
	(fsb -> fsb_fd, "A-ABORT.REQUEST",
	 id != type_FTAM_PDU_f__p__abort__request ? "F-U-ABORT-request"
						  : "F-P-ABORT-request",
	 pe, 0));

    result = AcUAbortRequest (fsb -> fsb_fd, pe ? &pe : NULLPEP, pe ? 1 : 0,
	    aci);

    if (pe)
	pe_free (pe);
    if (pdu)
	free_FTAM_PDU (pdu);

    if (result == NOTOK)
	(void) acs2ftamlose (fsb, fti, "AcUAbortRequest", aca);
    else {
	fsb -> fsb_fd = NOTOK;
	result = OK;
    }

    if (id != type_FTAM_PDU_f__p__abort__request)
	freefsblk (fsb);
    else
	if (result == OK)
	    fsb -> fsb_fd = NOTOK;

    return result;
}
