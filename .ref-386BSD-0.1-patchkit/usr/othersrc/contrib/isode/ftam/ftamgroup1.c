/* ftamgroup1.c - FPM: initiate a grouped transaction */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamgroup1.c,v 7.1 91/02/22 09:22:54 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamgroup1.c,v 7.1 91/02/22 09:22:54 mrose Interim $
 *
 *
 * $Log:	ftamgroup1.c,v $
 * Revision 7.1  91/02/22  09:22:54  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:53:37  mrose
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

/*    F-{MANAGE,BULK-{BEGIN,END}}.REQUEST (group) */

int     FManageRequest (sd, ftg, fti)
int     sd;
struct FTAMgroup   *ftg;
struct FTAMindication  *fti;
{
    return FGroupRequest (sd, ftg, FTI_MANAGEMENT, FSB_MANAGEMENT, fti);
}


int     FBulkBeginRequest (sd, ftg, fti)
int     sd;
struct FTAMgroup   *ftg;
struct FTAMindication  *fti;
{
    return FGroupRequest (sd, ftg, FTI_BULKBEGIN, FSB_BULKBEGIN, fti);
}


int     FBulkEndRequest (sd, ftg, fti)
int     sd;
struct FTAMgroup   *ftg;
struct FTAMindication  *fti;
{
    return FGroupRequest (sd, ftg, FTI_BULKEND, FSB_BULKEND, fti);
}

/*    F-GROUP.REQUEST (group) */

static int  FGroupRequest (sd, ftg, type, state, fti)
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

    if ((result = figrpchk (fsb, ftg, type, fti)) != NOTOK)
	result = FGroupRequestAux (fsb, ftg, state, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  FGroupRequestAux (fsb, ftg, state, fti)
register struct ftamblk *fsb;
register struct FTAMgroup  *ftg;
int	state;
struct FTAMindication  *fti;
{
    register int    i;
    int     did_loop,
	    npdu,
	    result;
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
    if ((result = figrp2pdus (fsb, ftg, pdus, texts, &npdu, fti)) == NOTOK)
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

    fsb -> fsb_state = state;
    fsb -> fsb_group = ftg -> ftg_flags;
    
    return FWaitRequestAux (fsb, NOTOK, fti);
}

/*  */

static int  figrpchk (fsb, ftg, type, fti)
register struct ftamblk *fsb;
register struct FTAMgroup *ftg;
int	type;
struct FTAMindication *fti;
{
    int     i,
            request;
    register struct FTAMpasswords  *fp;
    register struct FTAMconcurrency *fc;

    if (!(fsb -> fsb_flags & FSB_INIT))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "not initiator");
    switch (fsb -> fsb_state) {
	case FSB_INITIALIZED: 
	    if (type == FTI_BULKEND)
		goto wrong_state;
	    break;

	case FSB_DATAIDLE: 
	    if (type != FTI_BULKEND)
		goto wrong_state;
	    break;

	default: 
    wrong_state: ;
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "wrong state");
    }

    switch (fsb -> fsb_class) {
	case FCLASS_TRANSFER: 
	    if (type != FTI_MANAGEMENT)
		break;
	    goto bad_class;

	case FCLASS_MANAGE: 
	    if (type == FTI_MANAGEMENT)
		break;
	    goto bad_class;

	case FCLASS_TM: 
	case FCLASS_ACCESS: 
	    break;

	default: 
    bad_class: ;
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "file management not permitted");
    }

    if (!(ftg -> ftg_flags & FTG_BEGIN) || !(ftg -> ftg_flags & FTG_END))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP, "must be grouped");

    switch (type) {
	case FTI_MANAGEMENT: 
	case FTI_BULKBEGIN: 
	    if ((ftg -> ftg_flags & (FTG_SELECT | FTG_CREATE)) == 0
		    || (ftg -> ftg_flags & (FTG_SELECT | FTG_CREATE))
		    == (FTG_SELECT | FTG_CREATE))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"missing/duplicate select/create request");
	    if (type == FTI_MANAGEMENT) {
		if (ftg -> ftg_flags & (FTG_OPEN | FTG_CLOSE))
		    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			    "file transfer not permitted");
		goto finish_end;
	    }
	    if (!(ftg -> ftg_flags & FTG_OPEN))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"missing open request");
	    if (ftg -> ftg_flags & (FTG_CLOSE | FTG_DESELECT | FTG_DELETE))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"file management/close not permitted");
	    break;

	case FTI_BULKEND: 
	    if (ftg -> ftg_flags & (FTG_SELECT | FTG_CREATE | FTG_OPEN))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"file management/open not permitted");
	    if ((ftg -> ftg_flags & FTG_CLOSE) == 0)
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"missing close request");

    finish_end: ;
	    if ((ftg -> ftg_flags & (FTG_DESELECT | FTG_DELETE)) == 0
		    || (ftg -> ftg_flags & (FTG_DESELECT | FTG_DELETE))
		    == (FTG_DESELECT | FTG_DELETE))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"missing/duplicate deselect/delete request");
	    break;
    }

    if (!(fsb -> fsb_units & FUNIT_GROUPING)
	    && (ftg -> ftg_flags & (FTG_BEGIN | FTG_END)))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"grouping not permitted");
    if (!(fsb -> fsb_units & FUNIT_LIMITED)
	    && (ftg -> ftg_flags & (FTG_CREATE | FTG_RDATTR | FTG_DELETE)))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"limited file management not permitted");
    if (!(fsb -> fsb_units & FUNIT_ENHANCED)
	    && (ftg -> ftg_flags & FTG_CHATTR))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"enhanced file management not permitted");

    i = 0;

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	if (ftse -> ftse_attrs.fa_present != FA_FILENAME)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "only filename should be present");
	if (ftse -> ftse_access & ~FA_REQ_MASK)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "FADU-Identity groups not permitted");

	request = ftse -> ftse_access;
	fp = &ftse -> ftse_pwds;
	fc = &ftse -> ftse_conctl;

	goto finish_create;
    }

    if (ftg -> ftg_flags & FTG_CREATE) {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;

	switch (ftce -> ftce_override) {
	    case FOVER_FAIL: 
	    case FOVER_SELECT: 
	    case FOVER_WRITE: 
	    case FOVER_DELETE: 
		break;

	    default: 
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for creation mode");
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
	if (ftce -> ftce_access & ~FA_REQ_MASK)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "FADU-Identity groups not permitted");

	request = ftce -> ftce_access;
	fp = &ftce -> ftce_pwds;
	fc = &ftce -> ftce_conctl;

finish_create: ;
	if (!(fsb -> fsb_attrs & FATTR_SECURITY) && passes_present (fp))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "access passwords not permitted");
	if (fsb -> fsb_attrs & FATTR_STORAGE) {
	    if ((!(request & FA_PERM_READ)
			&& fc -> fc_readlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_INSERT)
			&& fc -> fc_insertlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_REPLACE)
			&& fc -> fc_replacelock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_ERASE)
			&& fc -> fc_eraselock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_EXTEND)
			&& fc -> fc_extendlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_READATTR)
			&& fc -> fc_readattrlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_CHNGATTR)
			&& fc -> fc_chngattrlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_DELETE)
			&& fc -> fc_deletelock < FLOCK_PRESENT))
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad settings for select/create concurrency control");
	}

	i++;
    }

    if (ftg -> ftg_flags & FTG_CLOSE) {
	register struct FTAMclose   *ftcl = &ftg -> ftg_close;
	
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

	i++;
    }

    if (ftg -> ftg_flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;

	if (!(fsb -> fsb_attrs & FATTR_STORAGE)
		&& (ftra -> ftra_attrnames & FA_STORAGE))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "storage attributes not permitted");
	if (!(fsb -> fsb_attrs & FATTR_SECURITY)
		&& (ftra -> ftra_attrnames & FA_SECURITY))
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "security attributes not permitted");

	i++;
    }

    if (ftg -> ftg_flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;

	if (ftca -> ftca_attrs.fa_present & ftca -> ftca_attrs.fa_novalue)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "attributes can not be changed to no-value-available");
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

	i++;
    }

    if (ftg -> ftg_flags & FTG_DESELECT)
	i++;

    if (ftg -> ftg_flags & FTG_DELETE)
	i++;

    if (ftg -> ftg_flags & FTG_OPEN) {
	register struct FTAMopen *ftop = &ftg -> ftg_open;
	
	if ((request = ftop -> ftop_mode) & ~FA_MODE_MASK)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad open mode");
#ifdef	notdef
	if (ftop -> ftop_contents == NULLOID)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"missing contents type in open");
#endif
	if (fsb -> fsb_attrs & FATTR_STORAGE) {
	    fc = &ftop -> ftop_conctl;
	    if ((!(request & FA_PERM_READ)
			&& fc -> fc_readlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_INSERT)
			&& fc -> fc_insertlock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_REPLACE)
			&& fc -> fc_replacelock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_ERASE)
			&& fc -> fc_eraselock < FLOCK_PRESENT)
		    || (!(request & FA_PERM_EXTEND)
			&& fc -> fc_extendlock < FLOCK_PRESENT)
		    || fc -> fc_readattrlock != FLOCK_NOTREQD
		    || fc -> fc_chngattrlock != FLOCK_NOTREQD
		    || fc -> fc_deletelock != FLOCK_NOTREQD)
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad settings for open concurrency control");
	}

	i++;
    }

    if (i != ftg -> ftg_threshold)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"threshold mismatch; expecting %d, found %d",
		ftg -> ftg_threshold, i);

    return OK;
}

/*  */

static int  figrp2pdus (fsb, ftg, pdus, texts, npdu, fti)
register struct ftamblk *fsb;
register struct FTAMgroup *ftg;
struct type_FTAM_PDU *pdus[];
char   *texts[];
int    *npdu;
struct FTAMindication *fti;
{
    int     i;
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

#define	new_action(a) \
    if (a != int_FTAM_Action__Result_success) { \
	if ((req -> action__result = \
	     		(struct type_FTAM_Action__Result *) \
	     			calloc (1, sizeof *req -> action__result)) \
	        == NULL) \
	    goto no_mem; \
	req -> action__result -> parm = a; \
    }

    if (ftg -> ftg_flags & FTG_BEGIN) {
	new_pdu (type_FTAM_F__BEGIN__GROUP__request,
		 type_FTAM_PDU_f__begin__group__request,
		 f__begin__group__request, "F-BEGIN-GROUP-request");
	req -> parm = ftg -> ftg_threshold;
    }

    if (ftg -> ftg_flags & FTG_SELECT) {
	register struct FTAMselect *ftse = &ftg -> ftg_select;

	new_pdu (type_FTAM_F__SELECT__request,
		 type_FTAM_PDU_f__select__request,
		 f__select__request, "F-SELECT-request");
	if ((req -> attributes = attr2fpm (fsb, &ftse -> ftse_attrs, fti))
	        == NULL)
	    return NOTOK;
	if ((req -> requested__access = bits2fpm (fsb, frequested_pairs,
						  ftse -> ftse_access, fti))
	        == NULL)
	    return NOTOK;
	if (passes_present (&ftse -> ftse_pwds)
	        && (req -> access__passwords = pass2fpm (fsb,
							 &ftse -> ftse_pwds,
							 fti)) == NULL)
	    return NOTOK;
	if (conctl_present (&ftse -> ftse_conctl)
	        && (req -> concurrency__control =
		    		conctl2fpm (fsb, &ftse -> ftse_conctl, fti))
	    		== NULL)
	    return NOTOK;
	if (ftse -> ftse_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftse -> ftse_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftse -> ftse_account
	        && (req -> account = str2qb (ftse -> ftse_account,
					     strlen (ftse -> ftse_account), 1))
	    		== NULL)
	    goto no_mem;
    }

    if (ftg -> ftg_flags & FTG_CREATE) {
	register struct FTAMcreate *ftce = &ftg -> ftg_create;

	new_pdu (type_FTAM_F__CREATE__request,
		 type_FTAM_PDU_f__create__request,
		 f__create__request, "F-CREATE-request");
	req -> override = ftce -> ftce_override;
	if ((req -> initial__attributes = attr2fpm (fsb, &ftce -> ftce_attrs,
						    fti)) == NULL)
	    return NOTOK;
	if (ftce -> ftce_create) {
	    register struct type_FTAM_Password *p;
	    
	    if ((p = (struct type_FTAM_Password *) calloc (1, sizeof *p))
		    == NULL)
		goto no_mem;
	    req -> create__password = p;
	    p -> offset = type_FTAM_Password_binary;
	    if ((p -> un.binary = str2qb (ftce -> ftce_create,
					  ftce -> ftce_crelen, 1)) == NULL)
		goto no_mem;
	}
	if ((req -> requested__access = bits2fpm (fsb, frequested_pairs,
						  ftce -> ftce_access, fti))
	        == NULL)
	    return NOTOK;
	if (passes_present (&ftce -> ftce_pwds)
	        && (req -> access__passwords = pass2fpm (fsb,
							 &ftce -> ftce_pwds,
							 fti)) == NULL)
	    return NOTOK;
	if (conctl_present (&ftce -> ftce_conctl)
	        && (req -> concurrency__control =
		    		conctl2fpm (fsb, &ftce -> ftce_conctl, fti))
	    		== NULL)
	    return NOTOK;
	if (ftce -> ftce_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftce -> ftce_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (ftce -> ftce_account
	        && (req -> account = str2qb (ftce -> ftce_account,
					     strlen (ftce -> ftce_account), 1))
	    		== NULL)
	    goto no_mem;
    }

    if (ftg -> ftg_flags & FTG_CLOSE) {
	register struct FTAMclose *ftcl = &ftg -> ftg_close;

	new_pdu (type_FTAM_F__CLOSE__request, type_FTAM_PDU_f__close__request,
		 f__close__request, "F-CLOSE-request");
	new_action (ftcl -> ftcl_action);
	if (ftcl -> ftcl_sharedASE
	        && (req -> shared__ASE__information =
		    		shared2fpm (fsb, ftcl -> ftcl_sharedASE, fti))
	    		== NULL)
	    return NOTOK;
	if (ftcl -> ftcl_ndiag > 0
	        && (req -> diagnostic = diag2fpm (fsb, 0, ftcl -> ftcl_diags,
						  ftcl -> ftcl_ndiag, fti))
	    		== NULL)
	    return NOTOK;
    }

    if (ftg -> ftg_flags & FTG_RDATTR) {
	register struct FTAMreadattr   *ftra = &ftg -> ftg_readattr;

	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__read__attrib__request;
	texts[i++] = "F-READ-ATTRIB-request";
	if ((pdu -> un.f__read__attrib__request =
	     		    bits2fpm (fsb, fname_pairs, ftra -> ftra_attrnames,
				      fti)) == NULLPE)
	    return NOTOK;
    }

    if (ftg -> ftg_flags & FTG_CHATTR) {
	register struct FTAMchngattr   *ftca = &ftg -> ftg_chngattr;

	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__change__attrib__request;
	texts[i++] = "F-CHANGE-ATTRIB-request";
	if ((pdu -> un.f__change__attrib__request =
	     		attr2fpm (fsb, &ftca -> ftca_attrs, fti)) == NULL)
	    return NOTOK;
    }

    if (ftg -> ftg_flags & FTG_DESELECT) {
	register struct FTAMdeselect *ftde = &ftg -> ftg_deselect;

	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__deselect__request;
	texts[i++] = "F-DESELECT-request";
	if (ftde -> ftde_sharedASE
	        && (pdu -> un.f__deselect__request =
		    	shared2fpm (fsb, ftde -> ftde_sharedASE, fti)) == NULL)
	    return NOTOK;
    }

    if (ftg -> ftg_flags & FTG_DELETE) {
	register struct FTAMdelete *ftxe = &ftg -> ftg_delete;

	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__delete__request;
	texts[i++] = "F-DELETE-request";
	if (ftxe -> ftxe_sharedASE
	        && (pdu -> un.f__deselect__request =
		    	shared2fpm (fsb, ftxe -> ftxe_sharedASE, fti)) == NULL)
	    return NOTOK;
    }
    
    if (ftg -> ftg_flags & FTG_OPEN) {
	register struct FTAMopen *ftop = &ftg -> ftg_open;

	new_pdu (type_FTAM_F__OPEN__request, type_FTAM_PDU_f__open__request,
		 f__open__request, "F-OPEN-request");
	if (ftop -> ftop_mode != FA_PERM_READ
	        && (req -> processing__mode = bits2fpm (fsb, fmode_pairs,
							ftop -> ftop_mode,
							fti)) == NULL)
	    return NOTOK;
	if ((req -> contents__type =
	     		(struct choice_FTAM_0 *)
	     			calloc (1, sizeof *req -> contents__type))
	        == NULL)
	    goto no_mem;
	if (ftop -> ftop_contents) {
	    register struct type_FTAM_Contents__Type__Attribute *proposed;

	    req -> contents__type -> offset = choice_FTAM_0_proposed;
	    if ((proposed = (struct type_FTAM_Contents__Type__Attribute *)
		 			calloc (1, sizeof *proposed)) == NULL)
		goto no_mem;
	    req -> contents__type -> un.proposed = proposed;
	    if ((proposed -> document__type__name =
				oid_cpy (ftop -> ftop_contents)) == NULLOID)
		goto no_mem;
	    if (proposed -> parameter = ftop -> ftop_parameter)
		proposed -> parameter -> pe_refcnt++;
	}
	else
	    req -> contents__type -> offset = choice_FTAM_0_unknown;
	if (conctl_present (&ftop -> ftop_conctl)
	        && (req -> concurrency__control =
		    		conctl2fpm (fsb, &ftop -> ftop_conctl, fti))
	    		== NULL)
	    return NOTOK;
	if (ftop -> ftop_sharedASE
	        && (req -> shared__ASE__information =
		    	    shared2fpm (fsb, ftop -> ftop_sharedASE, fti)) == NULL)
	    return NOTOK;
	if (fsb -> fsb_units & FUNIT_FADULOCK)
	    req -> enable__fadu__locking = ftop -> ftop_locking
						? int_FTAM_FADU__Lock_on
						: int_FTAM_FADU__Lock_off;
    }

    if (ftg -> ftg_flags & FTG_END) {
	if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	    goto no_mem;
	pdus[i] = pdu;
	pdu -> offset = type_FTAM_PDU_f__end__group__request;
	texts[i++] = "F-END-GROUP-request";
    }

    *npdu = i;
    return OK;

#undef	new_pdu
#undef	new_action

no_mem: ;
    return ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
}
