/* ftamgroup2.c - FPM: respond to a grouped transaction */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamgroup2.c,v 7.1 91/02/22 09:22:56 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamgroup2.c,v 7.1 91/02/22 09:22:56 mrose Interim $
 *
 *
 * $Log:	ftamgroup2.c,v $
 * Revision 7.1  91/02/22  09:22:56  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:39  mrose
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

/*    F-{MANAGE,BULK-{BEGIN,END}}.RESPONSE (group) */

int     FManageResponse (sd, ftg, fti)
int     sd;
struct FTAMgroup   *ftg;
struct FTAMindication  *fti;
{
    return FGroupResponse (sd, ftg, FTI_MANAGEMENT, FSB_INITIALIZED, fti);
}


int     FBulkBeginResponse (sd, ftg, fti)
int     sd;
struct FTAMgroup   *ftg;
struct FTAMindication  *fti;
{
    return FGroupResponse (sd, ftg, FTI_BULKBEGIN, FSB_DATAIDLE, fti);
}


int     FBulkEndResponse (sd, ftg, fti)
int     sd;
struct FTAMgroup   *ftg;
struct FTAMindication  *fti;
{
    return FGroupResponse (sd, ftg, FTI_BULKEND, FSB_INITIALIZED, fti);
}

/*    F-GROUP.RESPONSE (group) */

static int  FGroupResponse (sd, ftg, type, state, fti)
int     sd;
struct FTAMgroup   *ftg;
int	type,
	state;
struct FTAMindication  *fti;
{
    SBV	    smask;
    int	    result;
    register struct ftamblk *fsb;

    missingP (ftg);
    missingP (fti);

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    if ((result = frgrpchk (fsb, ftg, type, fti)) != NOTOK)
	result = FGroupResponseAux (fsb, ftg, state, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  FGroupResponseAux (fsb, ftg, state, fti)
register struct ftamblk *fsb;
register struct FTAMgroup  *ftg;
int	state;
struct FTAMindication  *fti;
{
    register int    i;
    int     did_loop,
	    npdu,
	    result,
	    okstate;
    char  **txp,
	   *texts[NPDATA];
    PE	    pe,
	   *pep,
	    info[NPDATA];
    struct PSAPindication   pis;
    struct PSAPindication *pi = &pis;
    struct PSAPabort  *pa = &pi -> pi_abort;
    struct type_FTAM_PDU **pdup,
			  *pdus[NPDATA];

    bzero ((char *) texts, sizeof texts);
    bzero ((char *) info, sizeof info);
    bzero ((char *) pdus, sizeof pdus);

    did_loop = 0;
    if ((result = frgrp2pdus (fsb, ftg, pdus, texts, &npdu, fti)) == NOTOK)
	goto out;
    for (pdup = pdus, pep = info, txp = texts, i = npdu - 1;
	    i >= 0;
	    pdup++, pep++, txp++, i--) {
	pe = NULLPE;
	if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, *pdup) == NOTOK) {
	    result = ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			       "error encoding PDU: %s", PY_pepy);	    
	    goto out;
	}
	(*pep = pe) -> pe_context = fsb -> fsb_id;

	fsbtrace (fsb, (fsb -> fsb_fd, "P-DATA.REQUEST", *txp, pe, 0));
    }
    did_loop = 1;

    result = PDataRequest (fsb -> fsb_fd, info, npdu, pi);

out: ;
    for (pdup = pdus, pep = info, i = NPDATA - 1;
	    i >= 0;
	    pdup++, pep++, i--) {
	if (*pep)
	    pe_free (*pep);
	if (*pdup)
	    free_FTAM_PDU (*pdup);
    }

    if (result == NOTOK) {
	if (did_loop)
	    (void) ps2ftamlose (fsb, fti, "PDataRequest", pa);
	if (fti -> fti_abort.fta_action == FACTION_PERM)
	    freefsblk (fsb);

	return NOTOK;
    }

    switch (state) {
	case FSB_DATAIDLE:
	    if (ftg -> ftg_flags & FTG_SELECT)
		okstate = ftg -> ftg_select.ftse_state;
	    else
		okstate = ftg -> ftg_create.ftce_state;
	    if (okstate != FSTATE_SUCCESS
		    || ftg -> ftg_open.ftop_state != FSTATE_SUCCESS) {
		fsb -> fsb_state = FSB_INITIALIZED;
		break;
	    }			/* else fall */

	default:
	    fsb -> fsb_state = state;
	    break;
    }

    return OK;
}

/*  */

static int  frgrpchk (fsb, ftg, type, fti)
register struct ftamblk *fsb;
register struct FTAMgroup *ftg;
int	type;
struct FTAMindication *fti;
{
    if (fsb -> fsb_flags & FSB_INIT)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "not responder");
    switch (fsb -> fsb_state) {
	case FSB_MANAGEMENT: 
	    if (type != FTI_MANAGEMENT)
		goto wrong_state;
	    if (ftg -> ftg_flags & ~fsb -> fsb_group) {
reply_mismatch: ;
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			   "group reply mismatch; expecting 0x%x, found 0x%x",
			    fsb -> fsb_group, ftg -> ftg_flags);
	    }
	    break;

	case FSB_BULKBEGIN: 
	    if (type != FTI_BULKBEGIN)
		goto wrong_state;
	    if (ftg -> ftg_flags & ~fsb -> fsb_group)
		goto reply_mismatch;
	    break;

	case FSB_BULKEND: 
	    if (type != FTI_BULKEND)
		goto wrong_state;
	    if (ftg -> ftg_flags & ~fsb -> fsb_group)
		goto reply_mismatch;
	    break;

	default: 
    wrong_state: ;
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "wrong state");
    }

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	switch (ftse -> ftse_state) {
	    case FSTATE_SUCCESS: 
	    case FSTATE_FAILURE:
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for select state parameter");
	}
	switch (ftse -> ftse_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for select action parameter");
	}
	if (ftse -> ftse_attrs.fa_present != FA_FILENAME)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "only filename should be present");
	if (ftse -> ftse_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many select diagnostics");
    }

    if (ftg -> ftg_flags & FTG_CREATE) {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;

	switch (ftce -> ftce_state) {
	    case FSTATE_SUCCESS: 
	    case FSTATE_FAILURE: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for create state parameter");
	}
	switch (ftce -> ftce_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for create action parameter");
	}
	if (!(ftce -> ftce_attrs.fa_present & FA_FILENAME))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "filename not present");
	if (!(ftce -> ftce_attrs.fa_present & FA_ACTIONS))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "permitted-actions not present");
	if (!(ftce -> ftce_attrs.fa_present & FA_CONTENTS))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "contents-type not present");
	if (ftce -> ftce_attrs.fa_present & ~FA_CRE_ATTRS)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "illegal attributes present");
	if (!(fsb -> fsb_attrs & FATTR_STORAGE)
		&& (ftce -> ftce_attrs.fa_present & FA_STORAGE))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "storage attributes not permitted");
	if (!(fsb -> fsb_attrs & FATTR_SECURITY)
		&& (ftce -> ftce_attrs.fa_present & FA_SECURITY))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "security attributes not permitted");
	if (ftce -> ftce_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many create diagnostics");
    }

    if (ftg -> ftg_flags & FTG_CLOSE) {
	register struct FTAMclose     *ftcl = &ftg -> ftg_close;
	
	switch (ftcl -> ftcl_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for close action parameter");
	}
	if (ftcl -> ftcl_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many close diagnostics");
    }

    if (ftg -> ftg_flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;

	switch (ftra -> ftra_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for read attribute action parameter");
	}
	if (!(fsb -> fsb_attrs & FATTR_STORAGE)
		&& (ftra -> ftra_attrs.fa_present & FA_STORAGE))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "storage attributes not permitted");
	if (!(fsb -> fsb_attrs & FATTR_SECURITY)
		&& (ftra -> ftra_attrs.fa_present & FA_SECURITY))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "security attributes not permitted");
	if (ftra -> ftra_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many read attribute diagnostics");
    }

    if (ftg -> ftg_flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;

	switch (ftca -> ftca_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for change attribute action parameter");
	}
	if (!(fsb -> fsb_attrs & FATTR_STORAGE)
		&& (ftca -> ftca_attrs.fa_present & FA_STORAGE))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "storage attributes not permitted");
	if (!(fsb -> fsb_attrs & FATTR_SECURITY)
		&& (ftca -> ftca_attrs.fa_present & FA_SECURITY))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "security attributes not permitted");
	if (ftca -> ftca_attrs.fa_present & FA_CONTROL)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "encoding of access-control not supported (yet)");
	if (ftca -> ftca_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many change attribute diagnostics");
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	switch (ftde -> ftde_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for deselect action parameter");
	}
	if (!(fsb -> fsb_flags & FSB_DECHARGE)
		&& ftde -> ftde_charges.fc_ncharge > 0)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "deselect not permitted to include charges");
	if (ftde -> ftde_charges.fc_ncharge > NFCHRG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many deselect charges");
	if (ftde -> ftde_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many deselect diagnostics");
    }

    if (ftg -> ftg_flags & FTG_DELETE) {
	register struct FTAMdelete *ftxe = &ftg -> ftg_delete;

	switch (ftxe -> ftxe_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for delete action parameter");
	}
	if (ftxe -> ftxe_charges.fc_ncharge > NFCHRG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many delete charges");
	if (ftxe -> ftxe_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many delete diagnostics");
    }

    if (ftg -> ftg_flags & FTG_OPEN) {
	register struct FTAMopen *ftop = &ftg -> ftg_open;
	register struct FTAMconcurrency *fc;

	switch (ftop -> ftop_state) {
	    case FSTATE_SUCCESS: 
	    case FSTATE_FAILURE: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for open state parameter");
	}
	switch (ftop -> ftop_action) {
	    case FACTION_SUCCESS: 
	    case FACTION_TRANS: 
	    case FACTION_PERM: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for open action parameter");
	}
	if (ftop -> ftop_contents == NULLOID)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "missing open contents type parameter");
	if (fsb -> fsb_attrs & FATTR_STORAGE) {
	    fc = &ftop -> ftop_conctl;
	    if (fc -> fc_readattrlock != FLOCK_NOTREQD
		    || fc -> fc_chngattrlock != FLOCK_NOTREQD
		    || fc -> fc_deletelock != FLOCK_NOTREQD)
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad settings for open concurrency control");
	}
	if (ftop -> ftop_ndiag > NFDIAG)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "too many open diagnostics");
    }

    return OK;
}

/*  */

static int  frgrp2pdus (fsb, ftg, pdus, texts, npdu, fti)
register struct ftamblk *fsb;
register struct FTAMgroup *ftg;
struct type_FTAM_PDU *pdus[];
char   *texts[];
int    *npdu;
struct FTAMindication *fti;
{
    int     flags,
	    i;
    register struct type_FTAM_PDU *pdu;

    i = 0;

#define	new_pdu(t,o,u,x) \
	register struct t *req; \
 \
	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL) \
	    goto no_mem; \
	pdus[i] = pdu; \
	pdu  -> offset = o; \
	texts[i++] = x; \
	if ((req = (struct t *) calloc (1, sizeof *req)) == NULL) \
	    goto no_mem; \
	pdu -> un.u = req;

#define	new_state(s) \
    if (s != int_FTAM_State__Result_success) { \
	if ((req -> state__result = \
	     		(struct type_FTAM_State__Result *) \
	     			calloc (1, sizeof *req -> state__result)) \
	        == NULL) \
	    goto no_mem; \
	req -> state__result -> parm = s; \
    }

#define	new_action(a) \
    if (a != int_FTAM_Action__Result_success) { \
	if ((req -> action__result = \
	     		(struct type_FTAM_Action__Result *) \
	     			calloc (1, sizeof *req -> action__result)) \
	        == NULL) \
	    goto no_mem; \
	req -> action__result -> parm = a; \
    }

    if ((flags = ftg -> ftg_flags) & FTG_SELECT) {
	if (ftg -> ftg_select.ftse_state == FSTATE_FAILURE)
	    flags &= FTG_BEGIN | FTG_SELECT | FTG_END;
    }
    else
	if (flags & FTG_CREATE) {
	    if (ftg -> ftg_create.ftce_state == FSTATE_FAILURE)
		flags &= FTG_BEGIN | FTG_CREATE | FTG_END;
	}

    if (flags & FTG_BEGIN) {
	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__begin__group__response;
	texts[i++] = "F-BEGIN-GROUP-response";
    }

    if (flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	new_pdu (type_FTAM_F__SELECT__response,
		 type_FTAM_PDU_f__select__response,
		 f__select__response, "F-SELECT-response");
	new_state (ftse -> ftse_state);
	new_action (ftse -> ftse_action);
	if ((req -> attributes = attr2fpm (fsb, &ftse -> ftse_attrs, fti))
	        == NULL)
	    return NOTOK;
	if (ftse -> ftse_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftse -> ftse_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftse -> ftse_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftse -> ftse_diags,
						  ftse -> ftse_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_CREATE) {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;

	new_pdu (type_FTAM_F__CREATE__response,
		 type_FTAM_PDU_f__create__response,
		 f__create__response, "F-CREATE-response");
	new_state (ftce -> ftce_state);
	new_action (ftce -> ftce_action);
	if ((req -> initial__attributes = attr2fpm (fsb, &ftce -> ftce_attrs,
						    fti)) == NULL)
	    return NOTOK;
	if (ftce -> ftce_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftce -> ftce_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftce -> ftce_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftce -> ftce_diags,
						  ftce -> ftce_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_CLOSE) {
	register struct FTAMclose *ftcl = &ftg -> ftg_close;

	new_pdu (type_FTAM_F__CLOSE__response,
		 type_FTAM_PDU_f__close__response,
		 f__close__response, "F-CLOSE-response");
	new_action (ftcl -> ftcl_action);
	if (ftcl -> ftcl_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftcl -> ftcl_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftcl -> ftcl_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftcl -> ftcl_diags,
						  ftcl -> ftcl_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;

	new_pdu (type_FTAM_F__READ__ATTRIB__response,
		 type_FTAM_PDU_f__read__attrib__response,
		 f__read__attrib__response, "F-READ-ATTRIB-response");
	new_action (ftra -> ftra_action);
	if (ftra -> ftra_attrs.fa_present
	        && (req -> attributes = attr2fpm (fsb, &ftra -> ftra_attrs,
						  fti)) == NULL)
	    return NOTOK;
	if (ftra -> ftra_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftra -> ftra_diags,
						  ftra -> ftra_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;

	new_pdu (type_FTAM_F__CHANGE__ATTRIB__response,
		 type_FTAM_PDU_f__change__attrib__response,
		 f__change__attrib__response, "F-CHANGE-ATTRIB-response");
	new_action (ftca -> ftca_action);
	if (ftca -> ftca_attrs.fa_present
	        && (req -> attributes = attr2fpm (fsb, &ftca -> ftca_attrs,
						  fti)) == NULL)
	    return NOTOK;
	if (ftca -> ftca_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftca -> ftca_diags,
						  ftca -> ftca_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_DESELECT) {
	register struct FTAMdeselect   *ftde = &ftg -> ftg_deselect;

	new_pdu (type_FTAM_F__DESELECT__response,
		 type_FTAM_PDU_f__deselect__response,
		 f__deselect__response, "F-DESELECT-response");
	new_action (ftde -> ftde_action);
	if (ftde -> ftde_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftde -> ftde_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftde -> ftde_charges.fc_ncharge
	        && (req -> charging = chrg2fpm (fsb, &ftde -> ftde_charges,
						fti)) == NULL)
	    return NOTOK;
	if (ftde -> ftde_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftde -> ftde_diags,
						  ftde -> ftde_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_DELETE) {
	register struct FTAMdelete *ftxe = &ftg -> ftg_delete;

	new_pdu (type_FTAM_F__DELETE__response,
		 type_FTAM_PDU_f__delete__response,
		 f__delete__response, "F-DELETE-response");
	new_action (ftxe -> ftxe_action);
	if (ftxe -> ftxe_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftxe -> ftxe_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftxe -> ftxe_charges.fc_ncharge
	        && (req -> charging = chrg2fpm (fsb, &ftxe -> ftxe_charges,
						fti)) == NULL)
	    return NOTOK;
	if (ftxe -> ftxe_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftxe -> ftxe_diags,
						  ftxe -> ftxe_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_OPEN) {
	register struct FTAMopen *ftop = &ftg -> ftg_open;
	register struct type_FTAM_Contents__Type__Attribute *proposed;

	new_pdu (type_FTAM_F__OPEN__response,
		 type_FTAM_PDU_f__open__response,
		 f__open__response, "F-OPEN-response");
	new_state (ftop -> ftop_state);
	new_action (ftop -> ftop_action);
	if ((proposed = (struct type_FTAM_Contents__Type__Attribute *)
	     			calloc (1, sizeof *proposed)) == NULL)
	    goto no_mem;
	req -> contents__type = proposed;
	if ((proposed -> document__type__name =
	     		oid_cpy (ftop -> ftop_contents)) == NULLOID)
	    goto no_mem;
	if (proposed -> parameter = ftop -> ftop_parameter)
	    proposed -> parameter -> pe_refcnt++;
	if (conctl_present (&ftop -> ftop_conctl)
	        && (req -> concurrency__control =
		    		conctl2fpm (fsb, &ftop -> ftop_conctl, fti))
	    		== NULL)
	    return NOTOK;
	if (ftop -> ftop_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftop -> ftop_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftop -> ftop_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftop -> ftop_diags,
						  ftop -> ftop_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (flags & FTG_END) {
	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__end__group__response;
	texts[i++] = "F-END-GROUP-response";
    }

    *npdu = i;
    return OK;

#undef	new_pdu
#undef	new_state
#undef	new_action

no_mem: ;
    return ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
}
