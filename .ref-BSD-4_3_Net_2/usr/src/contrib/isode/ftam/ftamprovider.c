/* ftamprovider.c - implement the FTAM protocol */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamprovider.c,v 7.6 91/02/22 09:23:05 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamprovider.c,v 7.6 91/02/22 09:23:05 mrose Interim $
 *
 *
 * $Log:	ftamprovider.c,v $
 * Revision 7.6  91/02/22  09:23:05  mrose
 * Interim 6.8
 * 
 * Revision 7.5  91/01/07  12:40:14  mrose
 * update
 * 
 * Revision 7.4  90/11/21  11:30:12  mrose
 * sun
 * 
 * Revision 7.3  90/11/05  13:32:53  mrose
 * update
 * 
 * Revision 7.2  90/08/08  14:12:18  mrose
 * update
 * 
 * Revision 7.1  89/12/14  10:04:00  mrose
 * bdt
 * 
 * Revision 7.0  89/11/23  21:53:45  mrose
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

/*    DATA */

struct pair fclass_pairs[] = {
    FCLASS_UNCONS, bit_FTAM_Service__Class_unconstrained__class,
    FCLASS_MANAGE, bit_FTAM_Service__Class_management__class,
    FCLASS_TRANSFER, bit_FTAM_Service__Class_transfer__class,
    FCLASS_TM, bit_FTAM_Service__Class_transfer__and__management__class,
    FCLASS_ACCESS, bit_FTAM_Service__Class_access__class,

    0, 0
};

struct pair funit_pairs[] = {
    FUNIT_READ, bit_FTAM_Functional__Units_read,
    FUNIT_WRITE, bit_FTAM_Functional__Units_write,
    FUNIT_ACCESS, bit_FTAM_Functional__Units_file__access,
    FUNIT_LIMITED, bit_FTAM_Functional__Units_limited__file__management,
    FUNIT_ENHANCED, bit_FTAM_Functional__Units_enhanced__file__management,
    FUNIT_GROUPING, bit_FTAM_Functional__Units_grouping,
    FUNIT_FADULOCK, bit_FTAM_Functional__Units_fadu__locking,
    FUNIT_RECOVERY, bit_FTAM_Functional__Units_recovery,
    FUNIT_RESTART, bit_FTAM_Functional__Units_restart__data__transfer,

    0, 0
};

struct pair fattr_pairs[] = {
    FATTR_STORAGE, bit_FTAM_Attribute__Groups_storage,
    FATTR_SECURITY, bit_FTAM_Attribute__Groups_security,
    FATTR_PRIVATE, bit_FTAM_Attribute__Groups_private,

    0, 0
};

struct pair fname_pairs[] = {
    FA_FILENAME, bit_FTAM_Attribute__Names_read__filename,
    FA_ACTIONS, bit_FTAM_Attribute__Names_read__permitted__actions,
    FA_CONTENTS, bit_FTAM_Attribute__Names_read__content__types,
    FA_ACCOUNT, bit_FTAM_Attribute__Names_read__storage__account,
    FA_DATE_CREATE, bit_FTAM_Attribute__Names_read__date__and__time__of__creation,
    FA_DATE_MODIFY, bit_FTAM_Attribute__Names_read__date__and__time__of__last__modification,
    FA_DATE_READ, bit_FTAM_Attribute__Names_read__date__and__time__of__last__read__access,
    FA_DATE_ATTR, bit_FTAM_Attribute__Names_read__date__and__time__of__last__attribute__modification,
    FA_ID_CREATE, bit_FTAM_Attribute__Names_read__identity__of__creator,
    FA_ID_MODIFY, bit_FTAM_Attribute__Names_read__identity__of__last__modifier,
    FA_ID_READ, bit_FTAM_Attribute__Names_read__identity__of__last__reader,
    FA_ID_ATTR, bit_FTAM_Attribute__Names_read__identity__of__last__attribute__modifier,
    FA_AVAILABILITY, bit_FTAM_Attribute__Names_read__file__availability,
    FA_FILESIZE, bit_FTAM_Attribute__Names_read__filesize,
    FA_FUTURESIZE, bit_FTAM_Attribute__Names_read__future__filesize,
    FA_CONTROL, bit_FTAM_Attribute__Names_read__access__control,
    FA_LEGAL, bit_FTAM_Attribute__Names_read__legal__qualifications,
    FA_PRIVATE, bit_FTAM_Attribute__Names_read__private__use,

    0, 0
};

struct pair fmode_pairs[] = {
    FA_PERM_READ, bit_FTAM_processing__mode_f__read,
    FA_PERM_INSERT, bit_FTAM_processing__mode_f__insert,
    FA_PERM_REPLACE, bit_FTAM_processing__mode_f__replace,
    FA_PERM_EXTEND, bit_FTAM_processing__mode_f__extend,
    FA_PERM_ERASE, bit_FTAM_processing__mode_f__erase,

    0, 0
};

struct pair frequested_pairs[] = {
    FA_PERM_READ, bit_FTAM_Access__Request_read,
    FA_PERM_INSERT, bit_FTAM_Access__Request_insert,
    FA_PERM_REPLACE, bit_FTAM_Access__Request_replace,
    FA_PERM_EXTEND, bit_FTAM_Access__Request_extend,
    FA_PERM_ERASE, bit_FTAM_Access__Request_erase,
    FA_PERM_READATTR, bit_FTAM_Access__Request_read__attribute,
    FA_PERM_CHNGATTR, bit_FTAM_Access__Request_change__attribute,
    FA_PERM_DELETE, bit_FTAM_Access__Request_delete,

    0, 0
};

struct pair fpermitted_pairs[] = {
    FA_PERM_READ, bit_FTAM_Permitted__Actions__Attribute_read,
    FA_PERM_INSERT, bit_FTAM_Permitted__Actions__Attribute_insert,
    FA_PERM_REPLACE, bit_FTAM_Permitted__Actions__Attribute_replace,
    FA_PERM_EXTEND, bit_FTAM_Permitted__Actions__Attribute_extend,
    FA_PERM_ERASE, bit_FTAM_Permitted__Actions__Attribute_erase,
    FA_PERM_READATTR, bit_FTAM_Permitted__Actions__Attribute_read__attribute,
    FA_PERM_CHNGATTR, bit_FTAM_Permitted__Actions__Attribute_change__attribute,
    FA_PERM_DELETE, bit_FTAM_Permitted__Actions__Attribute_delete__file,
    FA_PERM_TRAV, bit_FTAM_Permitted__Actions__Attribute_traversal,
    FA_PERM_RVTRAV, bit_FTAM_Permitted__Actions__Attribute_reverse__traversal,
    FA_PERM_RANDOM, bit_FTAM_Permitted__Actions__Attribute_random__order,

    0, 0
};


static int  once_only = 0;
static struct ftamblk ftamque;
static struct ftamblk *FSHead = &ftamque;

int	psDATAser (), psTOKENser (), psSYNCser (), psACTIVITYser (),
	psREPORTser (), psFINISHser (), psABORTser ();

/*    F-WAIT.REQUEST (pseudo) */

int	FWaitRequest (sd, secs, fti)
int	sd;
int	secs;
struct FTAMindication *fti;
{
    SBV	    smask;
    int     result;
    register struct ftamblk *fsb;

    missingP (fti);

    smask = sigioblock ();

    ftamPsig (fsb, sd);

    result = FWaitRequestAux (fsb, secs, fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

int	FWaitRequestAux (fsb, secs, fti)
register struct ftamblk *fsb;
int	secs;
struct FTAMindication *fti;
{
    int     result;
    struct PSAPdata pxs;
    register struct PSAPdata   *px = &pxs;
    struct PSAPindication   pis;
    register struct PSAPindication *pi = &pis;

    for (;;) {
	if (fsb -> fsb_data.px_ninfo > 0) {
	    *px = fsb -> fsb_data;	/* struct copy */
	    bzero ((char *) &fsb -> fsb_data, sizeof fsb -> fsb_data);
	    goto do_data;
	}

	switch (result = PReadRequest (fsb -> fsb_fd, px, secs, pi)) {
	    case NOTOK: 
		return doPSabort (fsb, &pi -> pi_abort, fti);

	    case OK: 
do_data: ;
		result = doPSdata (fsb, px, fti);
		break;

	    case DONE: 
		switch (pi -> pi_type) {
		    case PI_TOKEN: 
			result = doPStokens (fsb, &pi -> pi_token, fti);
			break;

		    case PI_SYNC: 
			result = doPSsync (fsb, &pi -> pi_sync, fti);
			break;

		    case PI_ACTIVITY: 
			result = doPSactivity (fsb, &pi -> pi_activity, fti);
			break;

		    case PI_REPORT: 
			result = doPSreport (fsb, &pi -> pi_report, fti);
			break;

		    case PI_FINISH: 
			result = doPSfinish (fsb, &pi -> pi_finish, fti);
			break;

		    default: 
			result = fpktlose (fsb, fti, FS_PRO_LOWFAIL, NULLCP,
				"unknown indication (0x%x) from presentation",
				pi -> pi_type);
			freefsblk (fsb);
			break;
		}
		break;

	    default: 
		result = fpktlose (fsb, fti, FS_PRO_LOWFAIL, NULLCP,
			"unexpected return from PReadRequest=%d", result);
		freefsblk (fsb);
		break;
	}

	switch (result) {
	    case NOTOK: 
		return NOTOK;

	    case OK: 
		break;

	    case DONE: 
		return OK;
	}
    }
}

/*  */

static int  doPSdata (fsb, px, fti)
register struct ftamblk   *fsb;
register struct PSAPdata *px;
struct FTAMindication *fti;
{
    int     next;
    register int    i;
    register PE	pe,
               *pep;
    register struct FTAMgroup  *ftg = &fti -> fti_group;
    struct type_FTAM_PDU *pdu;

    fti -> fti_type = FTI_FINISH;		/* temporary for group */
    bzero ((char *) ftg, sizeof *ftg);
    pdu = NULL;

    next = 0;
    for (pep = px -> px_info, i = px -> px_ninfo - 1; i >= 0; pep++, i--) {
	if ((pe = *pep) == NULLPE)
	    continue;

	if (pe -> pe_context != fsb -> fsb_id)
	    goto got_fadu;

	switch (PE_ID (pe -> pe_class, pe -> pe_id)) {
	    case PE_ID (PE_CLASS_APPL, FADU_NODESCR):
	    case PE_ID (PE_CLASS_APPL, FADU_ENTERTREE):
	    case PE_ID (PE_CLASS_APPL, FADU_EXITREE):
		pe -> pe_context = PE_DFLT_CTX;

got_fadu: ;
		if (next < 0)
		    goto copy_psdu;
		next = 1;
		break;

	    default:
		if (next > 0) {
		    register struct PSAPdata *fx = &fsb -> fsb_data;

copy_psdu: ;
		    fsbtrace (fsb, (fsb -> fsb_id,
				    "queueing possible BDT entries in PSDU",
				    NULLCP, NULLPE, -1));
		    px -> px_ninfo -= (i + 1);
		    do {
			fx -> px_info[fx -> px_ninfo++] = *pep;
			*pep++ = NULL;
		    }
		    while (--i >= 0);
		    break;
		}
		next = -1;
		break;
	}
    }

    if (next > 0) {
	switch (fsb -> fsb_state) {
	    case FSB_DATAREAD: 
		if (!(fsb -> fsb_flags & FSB_INIT)) {
unexpected_fadu: ;
		    (void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
				"unexpected FADU; state=0x%x",
				fsb -> fsb_state);
		    goto out;
		}
		break;

	    case FSB_DATAWRITE: 
		if (fsb -> fsb_flags & FSB_INIT)
		    goto unexpected_fadu;
		break;

	    case FSB_DATACANCEL: 
		fsbtrace (fsb, (fsb -> fsb_fd,
			    "discarding FADU during CANCEL procedure",
			    NULLCP, NULLPE, -1));
		PXFREE (px);
		return OK;

	    default: 
		goto unexpected_fadu;
	}

	fti -> fti_type = FTI_DATA;
	{
	    register struct PSAPdata   *fx = &fti -> fti_data;

	    *fx = *px;		/* struct copy */
	}

	return DONE;
    }

    next = FTG_BEGIN;
    for (pep = px -> px_info, i = px -> px_ninfo - 1; i >= 0; pep++, i--) {
	if ((pe = *pep) == NULLPE)
	    continue;
	if (decode_FTAM_PDU (pe, 1, NULLIP, NULLVP, &pdu) == NOTOK) {
	    (void) fpktlose (fsb, fti, FS_PRO_ERRMSG, NULLCP,
			     "unable to parse PDU: %s", PY_pepy);
	    goto out;
	}
	fsbtrace (fsb, (fsb -> fsb_fd, "P-DATA.INDICATION", "FPDU", pe, 1));

	switch (pdu -> offset) {
	    case type_FTAM_PDU_f__begin__group__request:
		if (fsb -> fsb_flags & FSB_INIT)
		    goto unexpected_fpdu;
		ftg -> ftg_threshold =
		    	    pdu -> un.f__begin__group__request -> parm;
		goto do_begin;

	    case type_FTAM_PDU_f__begin__group__response:
		if (!(fsb -> fsb_flags & FSB_INIT))
		    goto unexpected_fpdu;
	do_begin: ;
		if (!(next & FTG_BEGIN))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_GROUPING)) {
	    no_grouping: ;
		    (void) fpktlose (fsb, fti, FS_PRO_ERRFUNIT, NULLCP,
			    "grouping not permitted");
		    goto out;
		}
		ftg -> ftg_flags |= FTG_BEGIN;
		next = FTG_SELECT | FTG_CREATE | FTG_CLOSE;
		break;

	    case type_FTAM_PDU_f__select__request:
		if ((fsb -> fsb_flags & FSB_INIT) || !(next & FTG_SELECT))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_SELECT;
		{
		    register struct FTAMselect *ftse = &ftg -> ftg_select;
		    register struct type_FTAM_F__SELECT__request *req =
						pdu -> un.f__select__request;

		    if (fpm2attr (fsb, req -> attributes, &ftse -> ftse_attrs,
				  fti) == NOTOK)
			goto out;
		    ftse -> ftse_attrs.fa_present &= FA_SEL_ATTRS;
		    if (fpm2bits (fsb, frequested_pairs,
				  req -> requested__access,
				  &ftse -> ftse_access, fti) == NOTOK)
			goto out;
		    if ((fsb -> fsb_attrs & FATTR_SECURITY)
			    && req -> access__passwords
			    && fpm2pass (fsb, req -> access__passwords,
					 &ftse -> ftse_pwds, fti) == NOTOK)
			goto out;
		    FCINIT (&ftse -> ftse_conctl);
		    if ((fsb -> fsb_attrs & FATTR_STORAGE)
			    && req -> concurrency__control
			    && fpm2conctl (fsb, req -> concurrency__control,
					   &ftse -> ftse_conctl, fti) == NOTOK)
			goto out;
		    if (req -> shared__ASE__information
			    && fpm2shared (fsb,
					   req -> shared__ASE__information,
					   &ftse -> ftse_sharedASE, fti) == NOTOK)
			goto out;
		    if (req -> account
			    && (ftse -> ftse_account = qb2str (req -> account))
					== NULL) {
no_mem: ;
			(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
					 "out of memory");
			goto out;
		    }
		}
		next = FTG_RDATTR | FTG_CHATTR | FTG_OPEN | FTG_DESELECT
		    	| FTG_DELETE;
		break;

	    case type_FTAM_PDU_f__select__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_SELECT))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_SELECT;
		{
		    register struct FTAMselect *ftse = &ftg -> ftg_select;
		    register struct type_FTAM_F__SELECT__response *rsp =
						pdu -> un.f__select__response;

		    ftse -> ftse_state = rsp -> state__result
					    ? rsp -> state__result -> parm
					    : int_FTAM_State__Result_success;
		    ftse -> ftse_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (fpm2attr (fsb, rsp  -> attributes, &ftse -> ftse_attrs,
				  fti) == NOTOK)
			goto out;
		    ftse -> ftse_attrs.fa_present &= FA_SEL_ATTRS;
		    if (rsp -> shared__ASE__information
			    && fpm2shared (fsb,
					   rsp -> shared__ASE__information,
					   &ftse -> ftse_sharedASE, fti) == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftse -> ftse_diags,
					 &ftse -> ftse_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_RDATTR | FTG_CHATTR | FTG_OPEN | FTG_DESELECT
		    	| FTG_DELETE | FTG_END;
		break;

	    case type_FTAM_PDU_f__create__request:
		if ((fsb -> fsb_flags & FSB_INIT) || !(next & FTG_CREATE))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_LIMITED)) {
	no_limited: ;
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "limited file management not permitted");
		    goto out;
		}
		ftg -> ftg_flags |= FTG_CREATE;
		{
		    register struct FTAMcreate *ftce = &ftg -> ftg_create;
		    register struct type_FTAM_F__CREATE__request *req =
						pdu -> un.f__create__request;

		    ftce -> ftce_override = req -> override;
		    if (fpm2attr (fsb, req -> initial__attributes,
				  &ftce -> ftce_attrs, fti) == NOTOK)
			goto out;
		    if ((ftce -> ftce_attrs.fa_present &
			         (FA_FILENAME | FA_ACTIONS | FA_CONTENTS))
			    != (FA_FILENAME | FA_ACTIONS | FA_CONTENTS)) {
			(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			   "missing mandatory parameters in F-CREATE-request");
			goto out;
		    }
		    ftce -> ftce_attrs.fa_present &= FA_CRE_ATTRS;
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftce -> ftce_attrs.fa_present &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftce -> ftce_attrs.fa_present &= ~FA_SECURITY;
		    if (req->create__password) {/* both choices are qbufs... */
			register struct qbuf *qb =
			    		req -> create__password -> un.graphic;

			if ((ftce -> ftce_create = qb2str (qb)) == NULL)
			    goto no_mem;
			ftce -> ftce_crelen = qb -> qb_len;
		    }
		    if (fpm2bits (fsb, frequested_pairs,
				  req -> requested__access,
				  &ftce -> ftce_access, fti) == NOTOK)
			goto out;
		    if ((fsb -> fsb_attrs & FATTR_SECURITY)
			    && req -> access__passwords
			    && fpm2pass (fsb, req -> access__passwords,
					 &ftce -> ftce_pwds, fti) == NOTOK)
			goto out;
		    FCINIT (&ftce -> ftce_conctl);
		    if ((fsb -> fsb_attrs & FATTR_STORAGE)
			    && req -> concurrency__control
			    && fpm2conctl (fsb, req -> concurrency__control,
					   &ftce -> ftce_conctl, fti) == NOTOK)
			goto out;
		    if (req -> shared__ASE__information
			    && fpm2shared (fsb,
					   req -> shared__ASE__information,
					   &ftce -> ftce_sharedASE, fti) == NOTOK)
			goto out;
		    if (req -> account
			    && (ftce -> ftce_account = qb2str (req -> account))
					== NULL)
			goto no_mem;
		}
		next = FTG_RDATTR | FTG_CHATTR | FTG_OPEN | FTG_DESELECT
		    	| FTG_DELETE;
		break;

	    case type_FTAM_PDU_f__create__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_CREATE))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_LIMITED))
		    goto no_limited;
		ftg -> ftg_flags |= FTG_CREATE;
		{
		    register struct FTAMcreate *ftce = &ftg -> ftg_create;
		    register struct type_FTAM_F__CREATE__response *rsp =
						pdu -> un.f__create__response;

		    ftce -> ftce_state = rsp -> state__result
					    ? rsp -> state__result -> parm
					    : int_FTAM_State__Result_success;
		    ftce -> ftce_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (fpm2attr (fsb, rsp -> initial__attributes,
				  &ftce -> ftce_attrs, fti) == NOTOK)
			goto out;
		    ftce -> ftce_attrs.fa_present &= FA_CRE_ATTRS;
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftce -> ftce_attrs.fa_present &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftce -> ftce_attrs.fa_present &= ~FA_SECURITY;
		    if (rsp -> shared__ASE__information
			    && fpm2shared (fsb,
					   rsp -> shared__ASE__information,
					   &ftce -> ftce_sharedASE, fti) == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftce -> ftce_diags,
					 &ftce -> ftce_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_RDATTR | FTG_CHATTR | FTG_OPEN | FTG_DESELECT
		    	| FTG_DELETE | FTG_END;
		break;

	    case type_FTAM_PDU_f__read__attrib__request:
		if ((fsb -> fsb_flags & FSB_INIT) || !(next & FTG_RDATTR))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_LIMITED))
		    goto no_limited;
		ftg -> ftg_flags |= FTG_RDATTR;
		{
		    register struct FTAMreadattr  *ftra = &ftg -> ftg_readattr;
		    register struct type_FTAM_F__READ__ATTRIB__request *req =
					    pdu -> un.f__read__attrib__request;

		    if (fpm2bits (fsb, fname_pairs, req,
				     &ftra -> ftra_attrnames, fti) == NOTOK)
			goto out;
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftra -> ftra_attrnames &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftra -> ftra_attrnames &= ~FA_SECURITY;
		}
		next = FTG_CHATTR | FTG_DESELECT | FTG_DELETE |
			( ftg -> ftg_flags & ( FTG_SELECT | FTG_CREATE ) ?
			  FTG_OPEN : FTG_NULL );
		break;

	    case type_FTAM_PDU_f__read__attrib__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_RDATTR))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_LIMITED))
		    goto no_limited;
		ftg -> ftg_flags |= FTG_RDATTR;
		{
		    register struct FTAMreadattr  *ftra = &ftg -> ftg_readattr;
		    register struct type_FTAM_F__READ__ATTRIB__response *rsp =
					   pdu -> un.f__read__attrib__response;

		    ftra -> ftra_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> attributes
			    && fpm2attr (fsb, rsp -> attributes,
					 &ftra -> ftra_attrs, fti) == NOTOK)
			goto out;
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftra -> ftra_attrs.fa_present &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftra -> ftra_attrs.fa_present &= ~FA_SECURITY;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftra -> ftra_diags,
					 &ftra -> ftra_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_CHATTR | FTG_DESELECT | FTG_DELETE |
			( ftg -> ftg_flags & ( FTG_SELECT | FTG_CREATE ) ?
			  FTG_OPEN : FTG_NULL );
		break;

	    case type_FTAM_PDU_f__change__attrib__request:
		if ((fsb -> fsb_flags & FSB_INIT) || !(next & FTG_CHATTR))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_ENHANCED)) {
	    no_enhanced: ;
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "enhanced file management not permitted");
		    goto out;
		}
		ftg -> ftg_flags |= FTG_CHATTR;
		{
		    register struct FTAMchngattr  *ftca = &ftg -> ftg_chngattr;
		    register struct type_FTAM_F__CHANGE__ATTRIB__request *req =
					  pdu -> un.f__change__attrib__request;

		    if (fpm2attr (fsb, req, &ftca -> ftca_attrs, fti) == NOTOK)
			goto out;
		    if (ftca -> ftca_attrs.fa_present
				& ftca -> ftca_attrs.fa_novalue) {
			(void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				"attributes can not be changed to no value available");
			goto out;
		    }
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftca -> ftca_attrs.fa_present &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftca -> ftca_attrs.fa_present &= ~FA_SECURITY;
		}
		next = FTG_DESELECT | FTG_DELETE |
			( ftg -> ftg_flags & ( FTG_SELECT | FTG_CREATE ) ?
			  FTG_OPEN : FTG_NULL );
		break;

	    case type_FTAM_PDU_f__change__attrib__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_CHATTR))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_ENHANCED))
		    goto no_enhanced;
		ftg -> ftg_flags |= FTG_CHATTR;
		{
		    register struct FTAMchngattr  *ftca = &ftg -> ftg_chngattr;
		    register struct type_FTAM_F__CHANGE__ATTRIB__response *rsp =
					 pdu -> un.f__change__attrib__response;

		    ftca -> ftca_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> attributes
			    && fpm2attr (fsb, rsp -> attributes,
					 &ftca -> ftca_attrs, fti) == NOTOK)
			goto out;
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftca -> ftca_attrs.fa_present &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftca -> ftca_attrs.fa_present &= ~FA_SECURITY;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftca -> ftca_diags,
					 &ftca -> ftca_ndiag, fti) == NOTOK)
			goto out;
		    if (!(fsb -> fsb_attrs & FATTR_STORAGE))
			ftca -> ftca_attrs.fa_present &= ~FA_STORAGE;
		    if (!(fsb -> fsb_attrs & FATTR_SECURITY))
			ftca -> ftca_attrs.fa_present &= ~FA_SECURITY;
		}
		next = FTG_DESELECT | FTG_DELETE |
			( ftg -> ftg_flags & ( FTG_SELECT | FTG_CREATE ) ?
			  FTG_OPEN : FTG_NULL );
		break;

	    case type_FTAM_PDU_f__open__request:
		if ((fsb -> fsb_flags & FSB_INIT) || !(next & FTG_OPEN))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_OPEN;
		{
		    register struct FTAMopen *ftop = &ftg -> ftg_open;
		    register struct type_FTAM_F__OPEN__request *req =
					  pdu -> un.f__open__request;

		    if (req -> processing__mode) {
			if (fpm2bits (fsb, fmode_pairs,
				      req -> processing__mode,
				      &ftop -> ftop_mode, fti) == NOTOK)
			    goto out;
		    }
		    else
			ftop -> ftop_mode = FA_PERM_READ;
		    if (req -> contents__type -> offset
			    == choice_FTAM_0_proposed) {
			register struct type_FTAM_Contents__Type__Attribute
			    *proposed = req -> contents__type -> un.proposed;

			ftop -> ftop_contents =
			    	proposed -> document__type__name;
			proposed -> document__type__name = NULLOID;
			if (proposed -> parameter
			        && (ftop -> ftop_parameter =
				    	pe_cpy (proposed -> parameter))
			    			== NULLPE)
			    goto no_mem;
		    }
		    FCINIT (&ftop -> ftop_conctl);
		    if ((fsb -> fsb_attrs & FATTR_STORAGE)
			    && req -> concurrency__control
			    && fpm2conctl (fsb, req -> concurrency__control,
					   &ftop -> ftop_conctl, fti) == NOTOK)
			goto out;
		    if (req -> shared__ASE__information
			    && fpm2shared (fsb,
					   req -> shared__ASE__information,
					   &ftop -> ftop_sharedASE, fti) == NOTOK)
			goto out;
		    if (fsb -> fsb_units & FUNIT_FADULOCK)
			ftop -> ftop_locking = req -> enable__fadu__locking;
		}
		next = FTG_END;
		break;

	    case type_FTAM_PDU_f__open__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_OPEN))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_OPEN;
		{
		    register struct FTAMopen *ftop = &ftg -> ftg_open;
		    register struct type_FTAM_F__OPEN__response *rsp =
					  pdu -> un.f__open__response;
		    register struct type_FTAM_Contents__Type__Attribute
					*proposed = rsp -> contents__type;

		    ftop -> ftop_state = rsp -> state__result
					    ? rsp -> state__result -> parm
					    : int_FTAM_State__Result_success;
		    ftop -> ftop_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    ftop -> ftop_contents = proposed -> document__type__name;
		    proposed -> document__type__name = NULLOID;
		    if (proposed -> parameter
			    && (ftop -> ftop_parameter =
				    pe_cpy (proposed -> parameter)) == NULLPE)
			goto no_mem;
		    FCINIT (&ftop -> ftop_conctl);
		    if ((fsb -> fsb_attrs & FATTR_STORAGE)
			    && rsp -> concurrency__control
			    && fpm2conctl (fsb, rsp -> concurrency__control,
					   &ftop -> ftop_conctl, fti) == NOTOK)
			goto out;
		    if (rsp -> shared__ASE__information
			    && fpm2shared (fsb,
					   rsp -> shared__ASE__information,
					   &ftop -> ftop_sharedASE, fti) == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftop -> ftop_diags,
					 &ftop -> ftop_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_END;
		break;

	    case type_FTAM_PDU_f__close__request:
		if (fsb -> fsb_flags & FSB_INIT)
		    goto unexpected_fpdu;
		goto do_close;
	    case type_FTAM_PDU_f__close__response:
		if (!(fsb -> fsb_flags & FSB_INIT))
		    goto unexpected_fpdu;
	do_close: ;
		if (!(next & FTG_CLOSE))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_CLOSE;
		{			/* F-CLOSE-response is identical... */
		    register struct FTAMclose *ftcl = &ftg -> ftg_close;
		    register struct type_FTAM_F__CLOSE__request *req =
					  pdu -> un.f__close__request;

		    ftcl -> ftcl_action = req -> action__result
					    ? req -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (req -> shared__ASE__information
			    && fpm2shared (fsb,
					   req -> shared__ASE__information,
					   &ftcl -> ftcl_sharedASE, fti) == NOTOK)
			goto out;
		    if (req -> diagnostic
			    && fpm2diag (fsb, req -> diagnostic,
					 ftcl -> ftcl_diags,
					 &ftcl -> ftcl_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_RDATTR | FTG_CHATTR | FTG_DESELECT | FTG_DELETE;
		break;

	    case type_FTAM_PDU_f__deselect__request:
		if ((fsb -> fsb_flags & FSB_INIT) || !(next & FTG_DESELECT))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_DESELECT;
		{
		    register struct FTAMdeselect  *ftde = &ftg -> ftg_deselect;
		    register struct type_FTAM_F__DESELECT__request *req =
					  pdu -> un.f__deselect__request;

		    if (req
			    && fpm2shared (fsb, req,
					   &ftde -> ftde_sharedASE, fti) == NOTOK)
			goto out;
		}
		next = FTG_END;
		break;

	    case type_FTAM_PDU_f__deselect__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_DESELECT))
		    goto unexpected_fpdu;
		ftg -> ftg_flags |= FTG_DESELECT;
		{
		    register struct FTAMdeselect  *ftde = &ftg -> ftg_deselect;
		    register struct type_FTAM_F__DESELECT__response *rsp =
					  pdu -> un.f__deselect__response;

		    ftde -> ftde_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> shared__ASE__information
			    && fpm2shared (fsb,
					   rsp -> shared__ASE__information,
					   &ftde -> ftde_sharedASE, fti) == NOTOK)
			goto out;
		    if (rsp -> charging
			    && fpm2chrg (fsb, rsp -> charging,
					  &ftde -> ftde_charges, fti) == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftde -> ftde_diags,
					 &ftde -> ftde_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_END;
		break;

	    case type_FTAM_PDU_f__delete__request:
		if (fsb -> fsb_flags & FSB_INIT)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_LIMITED))
		    goto no_limited;
		ftg -> ftg_flags |= FTG_DELETE;
		{
		    register struct FTAMdelete  *ftxe = &ftg -> ftg_delete;
		    register struct type_FTAM_F__DELETE__request *req =
					  pdu -> un.f__delete__request;

		    if (req
			    && fpm2shared (fsb, req,
					   &ftxe -> ftxe_sharedASE, fti) == NOTOK)
			goto out;
		}
		next = FTG_END;
		break;

	    case type_FTAM_PDU_f__delete__response:
		if (!(fsb -> fsb_flags & FSB_INIT) || !(next & FTG_DELETE))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_LIMITED))
		    goto no_limited;
		ftg -> ftg_flags |= FTG_DELETE;
		{
		    register struct FTAMdelete  *ftxe = &ftg -> ftg_delete;
		    register struct type_FTAM_F__DELETE__response *rsp =
					  pdu -> un.f__delete__response;

		    ftxe -> ftxe_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> shared__ASE__information
			    && fpm2shared (fsb,
					   rsp -> shared__ASE__information,
					   &ftxe -> ftxe_sharedASE, fti) == NOTOK)
			goto out;
		    if (rsp -> charging
			    && fpm2chrg (fsb, rsp -> charging,
					  &ftxe -> ftxe_charges, fti) == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftxe -> ftxe_diags,
					 &ftxe -> ftxe_ndiag, fti) == NOTOK)
			goto out;
		}
		next = FTG_END;
		break;

	    case type_FTAM_PDU_f__end__group__request:
		if (fsb -> fsb_flags & FSB_INIT)
		    goto unexpected_fpdu;
		goto do_end;
	    case type_FTAM_PDU_f__end__group__response:
		if (!(fsb -> fsb_flags & FSB_INIT))
		    goto unexpected_fpdu;
	do_end: ;
		if (!(next & FTG_END))
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_GROUPING))
		    goto no_grouping;
		ftg -> ftg_flags |= FTG_END;
		next = 0;
		break;

	    case type_FTAM_PDU_f__locate__request:
		if ((fsb -> fsb_flags & FSB_INIT)
			|| fsb -> fsb_state != FSB_DATAIDLE
		        || next != FTG_BEGIN)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_ACCESS)) {
	no_access: ;
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "file access not permitted");
		    goto out;
		}
		fti -> fti_type = FTI_ACCESS;
		{
		    register struct FTAMaccess *ftac = &fti -> fti_access;
		    register struct type_FTAM_F__LOCATE__request *req =
					  pdu -> un.f__locate__request;

		    ftac -> ftac_operation = FA_OPS_LOCATE;
		    if (fpm2faduid (fsb,
				    req -> file__access__data__unit__identity,
				    &ftac -> ftac_identity, fti) == NOTOK)
			goto out;
		    if (req -> fadu__lock)
			ftac -> ftac_locking = req -> fadu__lock -> parm;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__locate__response:
		if (!(fsb -> fsb_flags & FSB_INIT)
			|| fsb -> fsb_state != FSB_LOCATE
		        || next != FTG_BEGIN)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_ACCESS))
		    goto no_access;
		fti -> fti_type = FTI_ACCESS;
		{
		    register struct FTAMaccess *ftac = &fti -> fti_access;
		    register struct type_FTAM_F__LOCATE__response *rsp =
					  pdu -> un.f__locate__response;

		    ftac -> ftac_operation = FA_OPS_LOCATE;
		    ftac -> ftac_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> file__access__data__unit__identity
			    && fpm2faduid (fsb,
					   rsp -> file__access__data__unit__identity,
					   &ftac -> ftac_identity, fti)
				    == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftac -> ftac_diags,
					 &ftac -> ftac_ndiag, fti) == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__erase__request:
		if ((fsb -> fsb_flags & FSB_INIT)
			|| fsb -> fsb_state != FSB_DATAIDLE
		        || next != FTG_BEGIN)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_ACCESS))
		    goto no_access;
		fti -> fti_type = FTI_ACCESS;
		{
		    register struct FTAMaccess *ftac = &fti -> fti_access;
		    register struct type_FTAM_F__ERASE__request *req =
					  pdu -> un.f__erase__request;

		    ftac -> ftac_operation = FA_OPS_ERASE;
		    if (fpm2faduid (fsb, req, &ftac -> ftac_identity, fti)
			    == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__erase__response:
		if (!(fsb -> fsb_flags & FSB_INIT)
			|| fsb -> fsb_state != FSB_ERASE
		        || next != FTG_BEGIN)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_ACCESS))
		    goto no_access;
		fti -> fti_type = FTI_ACCESS;
		{
		    register struct FTAMaccess *ftac = &fti -> fti_access;
		    register struct type_FTAM_F__ERASE__response *rsp =
					  pdu -> un.f__erase__response;

		    ftac -> ftac_operation = FA_OPS_ERASE;
		    ftac -> ftac_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftac -> ftac_diags,
					 &ftac -> ftac_ndiag, fti) == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__read__request:
		if ((fsb -> fsb_flags & FSB_INIT)
			|| next != FTG_BEGIN
			|| fsb -> fsb_state != FSB_DATAIDLE)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_READ)) {
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "reading not permitted");
		    goto out;
		}
		fti -> fti_type = FTI_READWRITE;
		{
		    register struct FTAMreadwrite *ftrw = &fti -> fti_readwrite;
		    register struct type_FTAM_F__READ__request *req =
						pdu -> un.f__read__request;

		    ftrw -> ftrw_operation = FA_OPS_READ;
		    if (fpm2faduid (fsb,
				    req -> file__access__data__unit__identity,
				    &ftrw -> ftrw_identity, fti) == NOTOK)
			goto out;
		    if ((ftrw -> ftrw_context =
				    req -> access__context -> access__context)
			    == FA_ACC_FL) {
			if (req -> access__context -> optionals
			        & opt_FTAM_Access__Context_level__number)
			    ftrw -> ftrw_level =
				    req -> access__context -> level__number;
			else {
			    (void) fpktlose (fsb, fti, FS_PRO_ERRMSG, NULLCP,
				 "level-number missing for access-context FL");
			    goto out;
			}
		    }
		    else
			ftrw -> ftrw_level = -1;

		    if ((fsb -> fsb_units & FUNIT_FADULOCK)
			    && req -> fadu__lock)
			ftrw -> ftrw_locking = req -> fadu__lock -> parm;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__write__request:
		if ((fsb -> fsb_flags & FSB_INIT)
			|| next != FTG_BEGIN
			|| fsb -> fsb_state != FSB_DATAIDLE)
		    goto unexpected_fpdu;
		if (!(fsb -> fsb_units & FUNIT_WRITE)) {
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "writing not permitted");
		    goto out;
		}
		fti -> fti_type = FTI_READWRITE;
		{
		    register struct FTAMreadwrite *ftrw = &fti -> fti_readwrite;
		    register struct type_FTAM_F__WRITE__request *req =
		    				pdu -> un.f__write__request;

		    ftrw -> ftrw_operation =
				    req -> file__access__data__unit__operation;
		    if (fpm2faduid (fsb,
				    req -> file__access__data__unit__identity,
				    &ftrw -> ftrw_identity, fti) == NOTOK)
			goto out;
		    if ((fsb -> fsb_units & FUNIT_FADULOCK)
			    && req -> fadu__lock)
			ftrw -> ftrw_locking = req -> fadu__lock -> parm;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__data__end__request:
		switch (fsb -> fsb_state) {
		    case FSB_DATAREAD:
			if (!(fsb -> fsb_flags & FSB_INIT))
			    goto unexpected_data_end;
			break;

		    case FSB_DATAWRITE:
			if (fsb -> fsb_flags & FSB_INIT)
			    goto unexpected_data_end;
			break;

		    case FSB_DATACANCEL:
			fsbtrace (fsb,
			    (fsb -> fsb_fd,
			   "discarding F-DATA-END during CANCEL procedure",
				NULLCP, NULLPE, -1));
			free_FTAM_PDU (pdu);
			PXFREE (px);
			return OK;
			
		    default:
unexpected_data_end: ;
			(void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
				    "unexpected data end; state=0x%x",
				    fsb -> fsb_state);
			goto out;
		}
		if (next != FTG_BEGIN)
		    goto unexpected_data_end;

		fti -> fti_type = FTI_DATAEND;
		{
		    register struct FTAMdataend *ftda = &fti -> fti_dataend;
		    register struct type_FTAM_F__DATA__END__request *req =
					pdu -> un.f__data__end__request;

		    ftda -> ftda_action = req -> action__result
					    ?req -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (req -> diagnostic
			    && fpm2diag (fsb, req -> diagnostic,
					 ftda -> ftda_diags,
					 &ftda -> ftda_ndiag, fti) == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__cancel__request:
		switch (fsb -> fsb_state) {
		    case FSB_DATAREAD:
		    case FSB_DATAWRITE:
		    case FSB_DATAFIN1:
		    case FSB_DATAFIN2:
			break;

		    case FSB_DATACANCEL:
			fsb -> fsb_flags |= FSB_COLLIDE;
			break;

		    default:
			goto unexpected_fpdu;
		}
		goto do_cancel;
	    case type_FTAM_PDU_f__cancel__response:
		if (fsb -> fsb_state != FSB_DATACANCEL)
		    goto unexpected_fpdu;
	do_cancel: ;
		if (next != FTG_BEGIN)
		    goto unexpected_fpdu;
		fti -> fti_type = FTI_CANCEL;
		{			/* F-CANCEL-response is identical... */
		    register struct FTAMcancel *ftcn = &fti -> fti_cancel;
		    register struct type_FTAM_F__CANCEL__request *req =
						pdu -> un.f__cancel__request;

		    ftcn -> ftcn_action = req -> action__result
					    ? req -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (req -> shared__ASE__information
			    && fpm2shared (fsb,
					   req -> shared__ASE__information,
					   &ftcn -> ftcn_sharedASE, fti) == NOTOK)
			goto out;
		    if (req -> diagnostic
			    && fpm2diag (fsb, req -> diagnostic,
					 ftcn -> ftcn_diags,
					 &ftcn -> ftcn_ndiag, fti) == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__transfer__end__request:
		if ((fsb -> fsb_flags & FSB_INIT)
			|| next != FTG_BEGIN)
		    goto unexpected_fpdu;
		switch (fsb -> fsb_state) {
		    case FSB_DATAFIN1:
			break;

		    case FSB_DATACANCEL:
			fsbtrace (fsb,
			   (fsb -> fsb_fd,
			   "discarding F-TRANSFER-END during CANCEL procedure",
				NULLCP, NULLPE, -1));
			free_FTAM_PDU (pdu);
			PXFREE (px);
			return OK;
			
		    default:
			goto unexpected_fpdu;
		}
		fti -> fti_type = FTI_TRANSEND;
		{
		    register struct FTAMtransend *ftre = &fti -> fti_transend;
		    register struct type_FTAM_F__TRANSFER__END__request *req =
					   pdu -> un.f__transfer__end__request;

		    if (req
			    && fpm2shared (fsb, req,
					   &ftre -> ftre_sharedASE, fti) == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    case type_FTAM_PDU_f__transfer__end__response:
		if (!(fsb -> fsb_flags & FSB_INIT)
			|| next != FTG_BEGIN
			|| fsb -> fsb_state != FSB_DATAFIN2)
		    goto unexpected_fpdu;
		fti -> fti_type = FTI_TRANSEND;
		{
		    register struct FTAMtransend *ftre = &fti -> fti_transend;
		    register struct type_FTAM_F__TRANSFER__END__response *rsp =
					  pdu -> un.f__transfer__end__response;

		    ftre -> ftre_action = rsp -> action__result
					    ? rsp -> action__result -> parm
					    : int_FTAM_Action__Result_success;
		    if (rsp -> shared__ASE__information
			    && fpm2shared (fsb,
					   rsp -> shared__ASE__information,
					   &ftre -> ftre_sharedASE, fti) == NOTOK)
			goto out;
		    if (rsp -> diagnostic
			    && fpm2diag (fsb, rsp -> diagnostic,
					 ftre -> ftre_diags,
					 &ftre -> ftre_ndiag, fti) == NOTOK)
			goto out;
		}
		next = 0;
		break;

	    default: 
	unexpected_fpdu: ;
		(void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
			"FPDU mismatch; expecting one of 0x%x, found tag %s/%d/0x%x; state=0x%x",
			next, pe_classlist[pe -> pe_class], pe -> pe_form,
			pe -> pe_id, fsb -> fsb_state);
		goto out;
	}

	free_FTAM_PDU (pdu);
	pdu = NULL;
    }

    if (next) {
	(void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
		"missing FPDU(s) in group; expecting one of 0x%x next",
		next);
	goto out;
    }

    switch (fti -> fti_type) {
	case FTI_ACCESS:
	    fsb -> fsb_state = (fsb -> fsb_flags & FSB_INIT) ? FSB_DATAIDLE
				    : fti -> fti_access.ftac_operation
					== FA_OPS_LOCATE ? FSB_LOCATE
							 : FSB_ERASE;
	    goto done;

	case FTI_READWRITE:
	    fsb -> fsb_state = fti -> fti_readwrite.ftrw_operation
				    == FA_OPS_READ ? FSB_DATAREAD
						   : FSB_DATAWRITE;
	    goto done;

	case FTI_DATAEND:
	    fsb -> fsb_state = FSB_DATAFIN1;
	    goto done;

	case FTI_TRANSEND:
	    fsb -> fsb_state = (fsb -> fsb_flags & FSB_INIT) ? FSB_DATAIDLE
					: FSB_DATAFIN2;
	    goto done;

	case FTI_CANCEL:
	    if (fsb -> fsb_flags & FSB_COLLIDE) {
		struct FTAMindication ftis;

		fsbtrace (fsb, (fsb -> fsb_fd, "resolving CANCEL collision",
			    NULLCP, NULLPE, -1));
		(void) FCancelResponseAux (fsb, fsb -> fsb_cancelaction,
					   fsb -> fsb_cancelshared,
					   fsb -> fsb_canceldiags,
					   fsb -> fsb_cancelndiag,
					   &ftis);

		fsb -> fsb_flags &= ~FSB_COLLIDE;
		fsb -> fsb_cancelaction = FACTION_PERM;
		if (fsb -> fsb_cancelshared) {
		    pe_free (fsb -> fsb_cancelshared);
		    fsb -> fsb_cancelshared = NULLPE;
		}
		fsb -> fsb_canceldiags = NULL;
		fsb -> fsb_cancelndiag = 0;
		FTCNFREE (&fti -> fti_cancel);
		PXFREE (px);
		return OK;
	    }

	    if (fsb -> fsb_flags & FSB_CANCEL) {
		fsb -> fsb_flags &= ~FSB_CANCEL;
		fsb -> fsb_state = FSB_DATAIDLE;
	    }
	    else
		fsb -> fsb_state = FSB_DATACANCEL;
	    goto done;

	default:		/* a grouped request */
	    break;
    }

    if (!(fsb -> fsb_flags & FSB_INIT)
	    && ftg -> ftg_threshold != px -> px_ninfo - 2) {
	(void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
		"threshold mismatch; expecting %d, found %d",
		px -> px_ninfo - 2, ftg -> ftg_threshold);
	goto out;
    }

    if (ftg -> ftg_flags & (FTG_SELECT | FTG_CREATE)) {
	if ((fsb -> fsb_flags & FSB_INIT)
		? (fsb -> fsb_state == FSB_MANAGEMENT)
		: (ftg -> ftg_flags & (FTG_DESELECT | FTG_DELETE))) {
	    if (!(fsb -> fsb_flags & FSB_INIT))
		switch (fsb -> fsb_class) {
		    case FCLASS_MANAGE:
		    case FCLASS_TM:
		    case FCLASS_ACCESS:
		        break;

		    default:
		        goto unexpected_group;
		}

	    fti -> fti_type = FTI_MANAGEMENT;

	    if (fsb -> fsb_flags & FSB_INIT) {
		if (ftg -> ftg_flags & ~fsb -> fsb_group) {
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "management reply mismatch; expecting 0x%x, found 0x%x",
			    fsb -> fsb_group, ftg -> ftg_flags);
		    goto out;
		}
		fsb -> fsb_state = FSB_INITIALIZED;
	    }
	    else {
		if (fsb -> fsb_state != FSB_INITIALIZED)
		    goto unexpected_group;
		if ((ftg -> ftg_flags & FTG_SELECT)
			? ftg -> ftg_select.ftse_account
			: ftg -> ftg_create.ftce_account)
		    fsb -> fsb_flags |= FSB_DECHARGE;
		else
		    fsb -> fsb_flags &= ~FSB_DECHARGE;
		fsb -> fsb_state = FSB_MANAGEMENT;
		fsb -> fsb_group = ftg -> ftg_flags;
	    }

	    goto done;
	}

	if ((fsb -> fsb_flags & FSB_INIT)
	        ? (fsb -> fsb_state == FSB_BULKBEGIN)
		: (ftg -> ftg_flags & FTG_OPEN)) {
	    if (!(fsb -> fsb_flags & FSB_INIT))
		switch (fsb -> fsb_class) {
		    case FCLASS_TRANSFER: 
		    case FCLASS_TM: 
		    case FCLASS_ACCESS: 
		        break;

		    default: 
		        goto unexpected_group;
		}

	    fti -> fti_type = FTI_BULKBEGIN;

	    if (fsb -> fsb_flags & FSB_INIT) {
		int	state;

		if (ftg -> ftg_flags & ~fsb -> fsb_group) {
		    (void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "bulk transfer reply mismatch; expecting 0x%x found 0x%x",
			    fsb -> fsb_group, ftg -> ftg_flags);
		    goto out;
		}
		if (ftg -> ftg_flags & FTG_SELECT)
		    state = ftg -> ftg_select.ftse_state;
		else
		    state = ftg -> ftg_create.ftce_state;
		if (state != FSTATE_SUCCESS
			|| ftg -> ftg_open.ftop_state != FSTATE_SUCCESS)
		    fsb -> fsb_state = FSB_INITIALIZED;
		else
		    fsb -> fsb_state = FSB_DATAIDLE;
	    }
	    else {
		if (fsb -> fsb_state != FSB_INITIALIZED)
		    goto unexpected_group;
		if ((ftg -> ftg_flags & FTG_SELECT)
			? ftg -> ftg_select.ftse_account
			: ftg -> ftg_create.ftce_account)
		    fsb -> fsb_flags |= FSB_DECHARGE;
		else
		    fsb -> fsb_flags &= ~FSB_DECHARGE;
		fsb -> fsb_state = FSB_BULKBEGIN;
		fsb -> fsb_group = ftg -> ftg_flags;
	    }

	    goto done;
	}

	goto unexpected_group;
    }

    if (ftg -> ftg_flags & FTG_CLOSE) {
	switch (fsb -> fsb_class) {
	    case FCLASS_TRANSFER: 
	    case FCLASS_TM: 
	    case FCLASS_ACCESS: 
		break;

	    default: 
		goto unexpected_group;
	}

	fti -> fti_type = FTI_BULKEND;

	if (fsb -> fsb_flags & FSB_INIT) {
	    if (fsb -> fsb_state != FSB_BULKEND)
		goto unexpected_group;
	    if (ftg -> ftg_flags & ~fsb -> fsb_group) {
		(void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			"bulk transfer reply mismatch; expecting 0x%x found 0x%x",
			fsb -> fsb_group, ftg -> ftg_flags);
		goto out;
	    }
	    fsb -> fsb_state = FSB_INITIALIZED;
	}
	else {
	    if (fsb -> fsb_state != FSB_DATAIDLE)
		goto unexpected_group;
	    fsb -> fsb_state = FSB_BULKEND;
	    fsb -> fsb_group = ftg -> ftg_flags;
	}

	goto done;
    }

unexpected_group: ;
    (void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
		"unexpected group type 0x%x; state=0x%x", ftg -> ftg_flags,
		fsb -> fsb_state);
    goto out;

done: ;
    PXFREE (px);
    return DONE;

out: ;
    if (pdu)
	free_FTAM_PDU (pdu);
    switch (fti -> fti_type) {
	case FTI_ACCESS:
	    FTACFREE (&fti -> fti_access);
	    break;

	case FTI_READWRITE:
	    FTRWFREE (&fti -> fti_readwrite);
	    break;

	case FTI_TRANSEND:
	    FTREFREE (&fti -> fti_transend);
	    break;

	case FTI_CANCEL:
	    FTCNFREE (&fti -> fti_cancel);
	    break;

	case FTI_FINISH:
	case FTI_MANAGEMENT:
	case FTI_BULKBEGIN:
	case FTI_BULKEND:
	    FTGFREE (ftg);
	    break;

	default:
	    break;
    }
    PXFREE (px);

    freefsblk (fsb);
    return NOTOK;
}

/*  */

static int  doPStokens (fsb, pt, fti)
register struct ftamblk   *fsb;
register struct PSAPtoken *pt;
struct FTAMindication *fti;
{
    (void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
	    "unexpected token indication (0x%x)", pt -> pt_type);
    PTFREE (pt);

    freefsblk (fsb);
    return NOTOK;
}

/*  */

static int  doPSsync (fsb, pn, fti)
register struct ftamblk   *fsb;
register struct PSAPsync *pn;
struct FTAMindication *fti;
{
    register int i;
    struct PSAPdata pxs;
    register struct PSAPdata *px = &pxs;

    switch (pn -> pn_type) {
	case SN_RESETCNF:
		break;
	case SN_RESETIND:
	    if (pn -> pn_options == SYNC_ABANDON)
		break;		/* else fall */

	default:
	    (void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
			"unexpected sync indication (0x%x)", pn -> pn_type);
	    PNFREE (pn);

	    freefsblk (fsb);
	    return NOTOK;
    }

    fsb -> fsb_settings = pn -> pn_settings;
#define dotoken(requires,shift,bit,type) \
{ \
    if (fsb -> fsb_srequirements & requires) \
	switch (fsb -> fsb_settings & (ST_MASK << shift)) { \
	    default: \
		fsb -> fsb_settings &= ~(ST_MASK << shift); \
		fsb -> fsb_settings |= ST_INIT_VALUE << shift; \
 \
	    case ST_INIT_VALUE << shift: \
		fsb -> fsb_owned |= bit; \
		fsb -> fsb_avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		fsb -> fsb_avail |= bit; \
		break; \
	} \
}
    dotokens ();
#undef	dotoken

    if (pn -> pn_type == SN_RESETCNF && pn -> pn_ninfo == 0) {
	register struct FTAMcancel *ftcn = &fti -> fti_cancel;

	bzero ((char *) ftcn, sizeof *ftcn);
	ftcn -> ftcn_action = FACTION_SUCCESS;/* what else can be done? */

	fsb -> fsb_flags &= ~FSB_CANCEL;
	fsb -> fsb_state = FSB_DATAIDLE;

	PNFREE (pn);
	return DONE;
    }

    bzero ((char *) px, sizeof *px);
    for (i = pn -> pn_ninfo - 1; i >= 0; i--) {
	px -> px_info[i] = pn -> pn_info[i];
	pn -> pn_info[i] = NULLPE;
    }
    px -> px_ninfo = pn -> pn_ninfo;
    PNFREE (pn);

    return doPSdata (fsb, px, fti);
}

/*  */

static int  doPSactivity (fsb, pv, fti)
register struct ftamblk   *fsb;
register struct PSAPactivity *pv;
struct FTAMindication *fti;
{
    (void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
	    "unexpected activity indication (0x%x)", pv -> pv_type);
    PVFREE (pv);

    freefsblk (fsb);
    return NOTOK;
}

/*  */

static int  doPSreport (fsb, pp, fti)
register struct ftamblk   *fsb;
register struct PSAPreport *pp;
struct FTAMindication *fti;
{
    (void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP,
	    "unexpected exception report indication (0x%x)", pp -> pp_peer);
    PPFREE (pp);

    freefsblk (fsb);
    return NOTOK;
}

/*  */

static int  doPSfinish (fsb, pf, fti)
register struct ftamblk   *fsb;
register struct PSAPfinish *pf;
struct FTAMindication *fti;
{
    PE	    pe;
    struct AcSAPindication  acis;
    register struct AcSAPabort *aca = &acis.aci_abort;
    register struct AcSAPfinish *acf = &acis.aci_finish;
    struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__TERMINATE__request *req;

    pdu = NULL;
    if ((fsb -> fsb_flags & FSB_INIT) || fsb -> fsb_state != FSB_INITIALIZED) {
	(void) fpktlose (fsb, fti, FS_ACS_MGMT, NULLCP,
		"association management botched");
	PFFREE (pf);
	goto out1;
    }

    if (AcFINISHser (fsb -> fsb_fd, pf, &acis) == NOTOK) {
	(void) acs2ftamlose (fsb, fti, "AcFINISHser", aca);
	goto out1;
    }

    if (acf -> acf_ninfo < 1 || (pe = acf -> acf_info[0]) == NULLPE) {
	(void) fpktlose (fsb, fti, FS_PRO_ERR, NULLCP, NULLCP);
	goto out2;
    }

    if (decode_FTAM_PDU (pe, 1, NULLIP, NULLVP, &pdu) == NOTOK) {
	(void) fpktlose (fsb, fti, FS_PRO_ERRMSG, NULLCP,
			 "unable to parse PDU: %s", PY_pepy);
	goto out2;
    }
    if (pdu -> offset != type_FTAM_PDU_f__terminate__request) {
	(void) fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			 "expecting F-TERMINATE-request, got %d",
			 pdu -> offset);
	goto out2;
    }
    req = pdu -> un.f__terminate__request;

    fsbtrace (fsb, (fsb -> fsb_fd, "A-RELEASE.INDICATION",
		"F-TERMINATE-request", pe, 1));

    ACFFREE (acf);

    fti -> fti_type = FTI_FINISH;
    {
	register struct FTAMfinish *ftf = &fti -> fti_finish;

	bzero ((char *) ftf, sizeof *ftf);
	if (req && fpm2shared (fsb, req, &ftf -> ftf_sharedASE, fti) == NOTOK)
	    goto out2;
    }
    fsb -> fsb_flags |= FSB_FINN;

    free_FTAM_PDU (pdu);
    return DONE;

out2: 	;
    ACFFREE (acf);
out1: 	;
    if (pdu)
	free_FTAM_PDU (pdu);
    freefsblk (fsb);
    return NOTOK;
}

/*  */

static int  doPSabort (fsb, pa, fti)
register struct ftamblk *fsb;
register struct PSAPabort *pa;
struct FTAMindication *fti;
{
    struct AcSAPindication  acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    if (!pa -> pa_peer && pa -> pa_reason == PC_TIMER)
	return ftamlose (fti, FS_PRO_TIMEOUT, 0, NULLCP, NULLCP);

    if (AcABORTser (fsb -> fsb_fd, pa, &acis) == NOTOK) {
	(void) acs2ftamlose (fsb, fti, "AcABORTser", aca);
	fsb -> fsb_fd = NOTOK;
	freefsblk (fsb);

	return NOTOK;
    }

    return acs2ftamabort (fsb, aca, fti);
}

/*  */

static int  psDATAser (sd, px)
int	sd;
register struct PSAPdata *px;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPSdata (fsb, px, fti) != OK)
	(*handler) (sd, fti);
}

/*  */

static int  psTOKENser (sd, pt)
int	sd;
register struct PSAPtoken *pt;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPStokens (fsb, pt, fti) != OK)
	(*handler) (sd, fti);
}

/*  */

static int  psSYNCser (sd, pn)
int	sd;
register struct PSAPsync *pn;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPSsync (fsb, pn, fti) != OK)
	(*handler) (sd, fti);
}

/*  */

static int  psACTIVITYser (sd, pv)
int	sd;
register struct PSAPactivity *pv;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPSactivity (fsb, pv, fti) != OK)
	(*handler) (sd, fti);
}

/*  */

static int  psREPORTser (sd, pp)
int	sd;
register struct PSAPreport *pp;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPSreport (fsb, pp, fti) != OK)
	(*handler) (sd, fti);
}

/*  */

static int  psFINISHser (sd, pf)
int	sd;
struct PSAPfinish *pf;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPSfinish (fsb, pf, fti) != OK)
	(*handler) (sd, fti);
}

/*  */

static int  psABORTser (sd, pa)
int	sd;
register struct PSAPabort *pa;
{
    IFP	    handler;
    register struct ftamblk   *fsb;
    struct FTAMindication  ftis;
    register struct FTAMindication *fti = &ftis;

    if ((fsb = findfsblk (sd)) == NULL)
	return;
    handler = fsb -> fsb_indication;

    if (doPSabort (fsb, pa, fti) != OK)
	(*handler) (sd, fti);
}

/*    define vector for INDICATION events */

#define	e(i)	(indication ? (i) : NULLIFP)


int	FSetIndications (sd, indication, fti)
int	sd;
IFP	indication;
struct FTAMindication *fti;
{
    SBV     smask;
    register struct ftamblk *fsb;
    struct PSAPindication   pis;
    register struct PSAPabort  *pa = &pis.pi_abort;

    missingP (fti);

    _iosignals_set = 1;
    smask = sigioblock ();

    ftamPsig (fsb, sd);

    if (fsb -> fsb_indication = indication)
	fsb -> fsb_flags |= FSB_ASYN;
    else
	fsb -> fsb_flags &= ~FSB_ASYN;

    if (PSetIndications (fsb -> fsb_fd, e (psDATAser), e (psTOKENser),
		e (psSYNCser), e (psACTIVITYser), e (psREPORTser),
		e (psFINISHser), e (psABORTser), &pis) == NOTOK) {
	fsb -> fsb_flags &= ~FSB_ASYN;
	switch (pa -> pa_reason) {
	    case PC_WAITING: 
		(void) sigiomask (smask);
		return ftamlose (fti, FS_GEN_WAITING, 0, NULLCP, NULLCP);

	    default: 
		(void) ps2ftamlose (fsb, fti, "PSetIndications", pa);
		freefsblk (fsb);
		(void) sigiomask (smask);
		return NOTOK;
	}
    }
    (void) sigiomask (smask);

    return OK;
}

#undef	e

/*    AcSAP interface */

int	acs2ftamlose (fsb, fti, event, aca)
register struct ftamblk *fsb;
struct FTAMindication *fti;
char   *event;
register struct AcSAPabort *aca;
{
    int     observer,
            reason;
    char   *cp,
            buffer[BUFSIZ];

    if (fsb && fsb -> fsb_trace && event) {
	cp = buffer;
	(void) sprintf (cp, "%s: %s", event, AcErrString (aca -> aca_reason));
	if (aca -> aca_cc > 0) {
	    cp += strlen (cp);
	    (void) sprintf (cp, " [%*.*s]", aca -> aca_cc, aca -> aca_cc,
		    aca -> aca_data);
	}

	fsbtrace (fsb, (fsb -> fsb_fd, buffer, NULLCP, NULLPE, -1));
    }

    cp = "";
    switch (aca -> aca_reason) {
	case ACS_ADDRESS: 
	    reason = FS_PRO_LOWADDR;
	    (void) sprintf (cp = buffer, " (%s)", AcErrString (ACS_ADDRESS));
	    break;

	case ACS_REFUSED: 
	    reason = FS_PRO_LOWFAIL;
	    (void) sprintf (cp = buffer, " (%s)", AcErrString (ACS_REFUSED));
	    break;

	default: 
	    (void) sprintf (cp = buffer, " (%s at association control)",
		    AcErrString (aca -> aca_reason));
	case ACS_CONGEST: 
	case ACS_PARAMETER: 
	case ACS_OPERATION: 
	case ACS_PRESENTATION: 
	    reason = FS_PRO_LOWFAIL;
	    break;
    }

    if (fsb) {
	if (fsb -> fsb_flags & FSB_INIT)
	    observer = EREF_IFPM;
	else
	    observer = EREF_RFPM;
    }
    else
	observer = EREF_NONE;

    if (aca -> aca_cc > 0)
	return ftamoops (fti, reason, ACS_FATAL (aca -> aca_reason), observer,
		EREF_NONE, NULLCP, "%*.*s%s", aca -> aca_cc, aca -> aca_cc,
		aca -> aca_data, cp);
    else
	return ftamoops (fti, reason, ACS_FATAL (aca -> aca_reason), observer,
		EREF_NONE, NULLCP, "%s", *cp ? cp + 1 : cp);
}

/*  */

int	acs2ftamabort (fsb, aca, fti)
register struct ftamblk *fsb;
register struct AcSAPabort *aca;
struct FTAMindication *fti;
{
    int     peer;
    PE	    pe;
    register struct FTAMabort  *fta = &fti -> fti_abort;
    struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__U__ABORT__request *req;

    pdu = NULL;
    if (aca -> aca_source != ACA_USER) {
	(void) acs2ftamlose (fsb, fti, NULLCP, aca);
	goto out;
    }

    if (aca -> aca_ninfo < 1 || (pe = aca -> aca_info[0]) == NULLPE) {
	(void) ftamlose (fti, FS_PRO_ERR, 1, NULLCP, NULLCP);
	goto out;
    }

    if (decode_FTAM_PDU (pe, 1, NULLIP, NULLVP, &pdu) == NOTOK) {
	(void) ftamlose (fti, FS_PRO_ERRMSG, 1, NULLCP,
			 "unable to parse PDU: %s", PY_pepy);
	goto out;
    }
    switch (pdu -> offset) {
	case type_FTAM_PDU_f__u__abort__request:
	    peer = 1;
	    req = pdu -> un.f__u__abort__request;
	    break;

	case type_FTAM_PDU_f__p__abort__request:
	    peer = 0;	/* F-P-ABORT-request is identical... */
	    req = pdu -> un.f__u__abort__request;
	    break;

	default:
	    (void) ftamlose (fti, FS_PRO_ERRPROC, 1, NULLCP,
			     "expecting F-{U,P}-ABORT-request, got %d",
			     pdu -> offset);
	    goto out;
    }
    fsbtrace (fsb, (fsb -> fsb_fd, "A-ABORT.INDICATION",
		    pdu -> offset != type_FTAM_PDU_f__u__abort__request
		    	? "F-P-ABORT-request" : "F-U-ABORT-request",
		    pe, 1));

    fti -> fti_type = FTI_ABORT;

    fta -> fta_peer = peer;
    fta -> fta_action = req -> action__result ? req -> action__result -> parm
					      :int_FTAM_Action__Result_success;
    if (req -> diagnostic)
	(void) fpm2diag (fsb, req -> diagnostic, fta -> fta_diags,
			 &fta -> fta_ndiag, fti);

out: ;
    ACAFREE (aca);
    if (pdu)
	free_FTAM_PDU (pdu);

    fsb -> fsb_fd = NOTOK;
    freefsblk (fsb);

    return NOTOK;
}

/*    PSAP interface */

int	ps2ftamlose (fsb, fti, event, pa)
register struct ftamblk *fsb;
struct FTAMindication *fti;
char   *event;
register struct PSAPabort *pa;
{
    int     observer,
            reason;
    char   *cp,
            buffer[BUFSIZ];

    if (fsb && fsb -> fsb_trace && event) {
	cp = buffer;
	(void) sprintf (cp, "%s: %s", event, PErrString (pa -> pa_reason));
	if (pa -> pa_cc > 0) {
	    cp += strlen (cp);
	    (void) sprintf (cp, " [%*.*s]", pa -> pa_cc, pa -> pa_cc,
		    pa -> pa_data);
	}

	fsbtrace (fsb, (fsb -> fsb_fd, buffer, NULLCP, NULLPE, -1));
    }

    cp = "";
    switch (pa -> pa_reason) {
	case PC_PARAMETER: 
	case PC_OPERATION: 
	default:
	    (void) sprintf (cp = buffer, " (%s at presentation)",
		    PErrString (pa -> pa_reason));
	case PC_CONGEST: 
	case PC_SESSION: 
	    reason = FS_PRO_LOWFAIL;
	    break;
    }

    if (fsb) {
	if (fsb -> fsb_flags & FSB_INIT)
	    observer = EREF_IFPM;
	else
	    observer = EREF_RFPM;
    }
    else
	observer = EREF_NONE;

    if (pa -> pa_cc > 0)
	return ftamoops (fti, reason, PC_FATAL (pa -> pa_reason), observer,
		EREF_NONE, NULLCP, "%*.*s%s", pa -> pa_cc, pa -> pa_cc,
		pa -> pa_data, cp);
    else
	return ftamoops (fti, reason, PC_FATAL (pa -> pa_reason), observer,
		EREF_NONE, NULLCP, "%s", *cp ? cp + 1 : cp);
}

/*    INTERNAL */

struct ftamblk *newfsblk () {
    register struct ftamblk *fsb;

    fsb = (struct ftamblk  *) calloc (1, sizeof *fsb);
    if (fsb == NULL)
	return NULL;

    fsb -> fsb_fd = NOTOK;

    if (once_only == 0) {
	FSHead -> fsb_forw = FSHead -> fsb_back = FSHead;
	once_only++;
    }

    insque (fsb, FSHead -> fsb_back);

    return fsb;
}

/*  */

freefsblk (fsb)
register struct ftamblk *fsb;
{
    register int    i;
    register struct PSAPcontext *pp;
    register struct FTAMcontent *fcont;

    if (fsb == NULL)
	return;

    if (fsb -> fsb_fd != NOTOK) {
	struct AcSAPindication  acis;

	fsbtrace (fsb, (fsb -> fsb_fd, "A-ABORT.REQUEST(discard)",
		NULLCP, NULLPE, 0));

	(void) AcUAbortRequest (fsb -> fsb_fd, NULLPEP, 0, &acis);
    }

    if (fsb -> fsb_context)
	oid_free (fsb -> fsb_context), fsb -> fsb_context = NULLOID;

    for (pp = fsb -> fsb_contexts.pc_ctx, i = fsb -> fsb_contexts.pc_nctx - 1;
	    i >= 0; 
	    pp++, i--) {
	if (pp -> pc_asn)
	    oid_free (pp -> pc_asn);
    }
    fsb -> fsb_contexts.pc_nctx = 0;

    for (fcont = fsb -> fsb_contents.fc_contents,
	 	i = fsb -> fsb_contents.fc_ncontent - 1;
	     i >= 0;
	     fcont++, i--) {
	if (fcont -> fc_dtn)
	    oid_free (fcont -> fc_dtn);
    }
    fsb -> fsb_contents.fc_ncontent = 0;

    PXFREE (&fsb -> fsb_data);

    if (fsb -> fsb_cancelshared)
	pe_free (fsb -> fsb_cancelshared);

    remque (fsb);

    free ((char *) fsb);
}

/*  */

struct ftamblk   *findfsblk (sd)
register int sd;
{
    register struct ftamblk *fsb;

    if (once_only == 0)
	return NULL;

    for (fsb = FSHead -> fsb_forw; fsb != FSHead; fsb = fsb -> fsb_forw)
	if (fsb -> fsb_fd == sd)
	    return fsb;

    return NULL;
}
