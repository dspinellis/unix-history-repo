/* ftambulk1.c - FPM: initiate bulk data transfer */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftambulk1.c,v 7.1 91/02/22 09:22:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftambulk1.c,v 7.1 91/02/22 09:22:42 mrose Interim $
 *
 *
 * $Log:	ftambulk1.c,v $
 * Revision 7.1  91/02/22  09:22:42  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:27  mrose
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

/*    F-{READ,WRITE}.REQUEST */

int	FReadWriteRequest (sd, operation, identity, context, level, lock, fti)
int	sd;
int	operation;
struct FADUidentity *identity;
int	context,		/* F-READ.REQUEST only */
	level,			/*   .. */
    	lock;
struct FTAMindication *fti;
{
    SBV      smask;
    int     result,
	    state;
    register struct ftamblk *fsb;

    switch (operation) {
	case FA_OPS_READ: 
	    state = FSB_DATAREAD;
	    break;

	case FA_OPS_INSERT: 
	case FA_OPS_REPLACE: 
	case FA_OPS_EXTEND: 
	    state = FSB_DATAWRITE;
	    break;

	default: 
	    return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
		    "bad value for operation parameter");
    }
    missingP (identity);
    if (operation == FA_OPS_READ)
	switch (context) {
	    case FA_ACC_HA: 
	    case FA_ACC_HN: 
	    case FA_ACC_FA: 
	    case FA_ACC_FL: 
	    case FA_ACC_FS: 
	    case FA_ACC_UA: 
	    case FA_ACC_US: 
	        break;

	    default:
		return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
				 "bad value for context parameter");
	}
    missingP (fti);

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    result = FReadWriteRequestAux (fsb, state, operation, identity, context,
			level, lock, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  FReadWriteRequestAux (fsb, state, operation, identity, context,
		level, lock, fti)
register struct ftamblk *fsb;
int	state,
	operation;
struct FADUidentity *identity;
int	context,
	level,
    	lock;
struct FTAMindication *fti;
{
    int     result;
    char   *fpdu;
    PE	    pe;
    struct PSAPindication   pis;
    struct PSAPindication  *pi = &pis;
    struct PSAPabort   *pa = &pi -> pi_abort;
    register struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__READ__request *rd;
    register struct type_FTAM_F__WRITE__request *wr;

    if (!(fsb -> fsb_flags & FSB_INIT))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "not initiator");
    if (fsb -> fsb_state != FSB_DATAIDLE)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "wrong state");

    switch (state) {
	case FSB_DATAREAD:
	    if (!(fsb -> fsb_units & FUNIT_READ))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			    "reading not allowed");
	    break;

	case FSB_DATAWRITE:
	    if (!(fsb -> fsb_units & FUNIT_WRITE))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			    "writing not allowed");
	    break;
    }

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
    if (state != FSB_DATAREAD) {
	pdu -> offset = type_FTAM_PDU_f__write__request;
	if ((wr = (struct type_FTAM_F__WRITE__request *)
	     		calloc (1, sizeof *wr)) == NULL)
	    goto no_mem;
	pdu -> un.f__write__request = wr;
	fpdu = "F-WRITE-request";

	wr -> file__access__data__unit__operation = operation;
	if ((wr -> file__access__data__unit__identity =
			faduid2fpm (fsb, identity, fti)) == NULL)
	    goto out;
	if (fsb -> fsb_units & FUNIT_FADULOCK) {
	    if ((wr -> fadu__lock = (struct type_FTAM_FADU__Lock *)
					calloc (1, sizeof *wr -> fadu__lock))
		    == NULL)
		goto no_mem;
	    wr -> fadu__lock -> parm = lock ? int_FTAM_FADU__Lock_on
					    : int_FTAM_FADU__Lock_off;
	}
    }
    else {
	pdu -> offset = type_FTAM_PDU_f__read__request;
	if ((rd = (struct type_FTAM_F__READ__request *)
	     		calloc (1, sizeof *rd)) == NULL)
	    goto no_mem;
	pdu -> un.f__read__request = rd;
	fpdu = "F-READ-request";

	if ((rd -> file__access__data__unit__identity =
			faduid2fpm (fsb, identity, fti)) == NULL)
	    goto out;
	if ((rd -> access__context =
			(struct type_FTAM_Access__Context *)
	     			calloc (1, sizeof *rd -> access__context))
	        == NULL)
	    goto no_mem;
	if ((rd -> access__context -> access__context = context)
	        == FA_ACC_FL) {
	    rd -> access__context -> optionals |=
					opt_FTAM_Access__Context_level__number;
	    rd -> access__context -> level__number = level;
	}

	if (fsb -> fsb_units & FUNIT_FADULOCK) {
	    if ((rd -> fadu__lock = (struct type_FTAM_FADU__Lock *)
					calloc (1, sizeof *wr -> fadu__lock))
		    == NULL)
		goto no_mem;
	    rd -> fadu__lock -> parm = lock ? int_FTAM_FADU__Lock_on
					    : int_FTAM_FADU__Lock_off;
	}
    }

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			 "error encoding PDU: %s", PY_pepy);
	goto out;
    }

    pe -> pe_context = fsb -> fsb_id;

    fsbtrace (fsb, (fsb -> fsb_fd, "P-DATA.REQUEST", fpdu, pe, 0));

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

    return OK;
}

/*    F-TRANSFER-END.REQUEST */

int	FTransEndRequest (sd, sharedASE, fti)
int	sd;
PE	sharedASE;
struct FTAMindication *fti;
{
    SBV	    smask;
    int     result;
    register struct ftamblk *fsb;

    missingP (fti);

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    result = FTransEndRequestAux (fsb, sharedASE, fti);

    (void) sigiomask (smask);

    return result;
}


/*  */

static int  FTransEndRequestAux (fsb, sharedASE, fti)
register struct ftamblk *fsb;
PE	sharedASE;
struct FTAMindication *fti;
{
    int	    result;
    PE	    pe;
    struct PSAPindication   pis;
    struct PSAPindication  *pi = &pis;
    struct PSAPabort   *pa = &pi -> pi_abort;
    register struct type_FTAM_PDU *pdu;

    if (!(fsb -> fsb_flags & FSB_INIT))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "not initiator");
    if (fsb -> fsb_state != FSB_DATAFIN1)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "wrong state");

    pe = NULLPE;
    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL) {
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
    pdu -> offset = type_FTAM_PDU_f__transfer__end__request;
    if (sharedASE
	    && (pdu -> un.f__transfer__end__request =
			shared2fpm (fsb, sharedASE, fti)) == NULL)
	goto out;

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			 "error encoding PDU: %s", PY_pepy);
	goto out;
    }

    pe -> pe_context = fsb -> fsb_id;
    
    fsbtrace (fsb, (fsb -> fsb_fd, "P-DATA.REQUEST", "F-TRANSFER-END-request",
		pe, 0));

    result = PDataRequest (fsb -> fsb_fd, &pe, 1, pi);

    pe_free (pe);
    pe = NULLPE;
    free_FTAM_PDU (pdu);
    pdu = NULL;


    if (result == NOTOK) {
	(void) ps2ftamlose (fsb, fti, "PDataRequest", pa);
	goto out;
    }

    fsb -> fsb_state = FSB_DATAFIN2;

    return FWaitRequestAux (fsb, NOTOK, fti);
}
