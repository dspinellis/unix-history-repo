/* ftamrespond.c - FPM: responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftamrespond.c,v 7.5 91/02/22 09:23:12 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftamrespond.c,v 7.5 91/02/22 09:23:12 mrose Interim $
 *
 *
 * $Log:	ftamrespond.c,v $
 * Revision 7.5  91/02/22  09:23:12  mrose
 * Interim 6.8
 * 
 * Revision 7.4  91/01/07  12:40:20  mrose
 * update
 * 
 * Revision 7.3  90/11/11  10:01:10  mrose
 * touch-up
 * 
 * Revision 7.2  90/11/05  13:32:59  mrose
 * update
 * 
 * Revision 7.1  90/11/04  19:15:10  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:53:51  mrose
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
#include "fpkt.h"

/*    F-INITIALIZE.INDICATION */

int	FInit (vecp, vec, fts, tracing, fti)
int	vecp;
char  **vec;
struct FTAMstart *fts;
IFP	tracing;
struct FTAMindication *fti;
{
    register int    i;
    PE	    pe = NULLPE;
    struct AcSAPstart acss;
    register struct AcSAPstart *acs = &acss;
    register struct PSAPstart *ps = &acs -> acs_start;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    register struct ftamblk *fsb;
    struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__INITIALIZE__request *req;
    register struct type_FTAM_F__INITIALIZE__response *rsp;
    
    missingP (vec);
    missingP (fts);
    missingP (fti);

    if ((fsb = newfsblk ()) == NULL)
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP, "out of memory");
    fsb -> fsb_trace = tracing;

    bzero ((char *) fts, sizeof *fts);

    pdu = NULL;
    
    if (AcInit (vecp, vec, acs, aci) == NOTOK) {
	(void) acs2ftamlose (fsb, fti, "AcInit", aca);
	goto out1;
    }

    fsb -> fsb_fd = acs -> acs_sd;

    fsb -> fsb_srequirements = ps -> ps_srequirements;
    fsb -> fsb_srequirements &= ~SR_RESYNC;	/* XXX */
    if (!(fsb -> fsb_srequirements & (SR_MINORSYNC | SR_RESYNC)))
	ps -> ps_isn = SERIAL_NONE;

    fsb -> fsb_settings = ps -> ps_settings;
#define dotoken(requires,shift,bit,type) \
{ \
    if (fsb -> fsb_srequirements & requires) \
	switch (fsb -> fsb_settings & (ST_MASK << shift)) { \
	    case ST_INIT_VALUE << shift: \
		fsb -> fsb_avail |= bit; \
		break; \
 \
	    case ST_RESP_VALUE << shift: \
		fsb -> fsb_owned |= bit; \
		fsb -> fsb_avail |= bit; \
		break; \
 \
	    default: \
		(void) ftamoops (fti, FS_PRO_ERRPROC, 1, EREF_RFPM, \
			    EREF_IFPM, NULLCP, \
			    "%s token management botched", type); \
		goto out2; \
	} \
}
	dotokens ();
#undef	dotoken
    if (fsb -> fsb_owned != 0) {
	(void) ftamoops (fti, FS_PRO_ERRPROC, 1, EREF_RFPM, EREF_IFPM, NULLCP,
		"token management botched");
	goto out2;
    }
    fsb -> fsb_ssn = ps -> ps_isn;
    fsb -> fsb_ssdusize = ps -> ps_ssdusize;
    fsb -> fsb_connect = ps -> ps_connect;	/* struct copy */
    fsb -> fsb_prequirements = ps -> ps_prequirements;

    if (acs -> acs_ninfo < 1 || (pe = acs -> acs_info[0]) == NULLPE) {
	(void) ftamoops (fti, FS_PRO_ERR, 1, EREF_RFPM, EREF_IFPM, NULLCP,
		NULLCP);
	goto out2;
    }

    if (decode_FTAM_PDU (pe, 1, NULLIP, NULLVP, &pdu) == NOTOK) {
	(void) ftamoops (fti, FS_PRO_ERRMSG, 1, EREF_RFPM, EREF_RFPM,
			 NULLCP, "unable to parse PDU: %s", PY_pepy);
	goto out3;
    }
    if (pdu -> offset != type_FTAM_PDU_f__initialize__request) {
	(void) ftamoops (fti, FS_PRO_ERRPROC, 1, EREF_RFPM, EREF_RFPM,
			 NULLCP, "expecting F-INITIALIZE-request, got %d",
			 pdu -> offset);
	goto out3;
    }
    req = pdu -> un.f__initialize__request;

    fsbtrace (fsb, (fsb -> fsb_fd, "A-ASSOCIATE.INDICATION",
		"F-INITIALIZE-request", pe, 1));

    fsb -> fsb_id = pe -> pe_context;

    if (req -> presentation__context__management) {
	if (!(fsb -> fsb_prequirements & PR_MANAGEMENT)) {
	    req -> presentation__context__management = 0;
	    fsb -> fsb_prequirements &= ~PR_RESTORATION;
	}
    }
    else
	fsb -> fsb_prequirements &= ~(PR_MANAGEMENT | PR_RESTORATION);
    if (req -> service__class) {
	if (fpm2bits (fsb, fclass_pairs, req -> service__class,
		      &fsb -> fsb_class, fti) == NOTOK)
	    goto out3;
    }
    else
	fsb -> fsb_class = FCLASS_TRANSFER;

    if (fpm2bits (fsb, funit_pairs, req -> functional__units,
		  &fsb -> fsb_units, fti) == NOTOK)
	goto out3;
					/* conservative... */
    if (fsb -> fsb_class & FCLASS_TRANSFER)
	fsb -> fsb_units |= FUNITS_TRANSFER;
    if (fsb -> fsb_class & FCLASS_TM)
	fsb -> fsb_units |= FUNITS_TM;
    if (fsb -> fsb_class & FCLASS_ACCESS)
	fsb -> fsb_units |= FUNITS_ACCESS;
    if (fsb -> fsb_class & FCLASS_MANAGE)
	fsb -> fsb_units |= FUNITS_MANAGE;
    if (!(fsb -> fsb_class &=
	    (FCLASS_TRANSFER | FCLASS_TM | FCLASS_MANAGE | FCLASS_ACCESS))) {
	(void) ftamoops (fti, FS_ACS_CLASS, 1, EREF_RFPM, EREF_IFPM,
			 NULLCP, NULLCP);
	goto out3;
    }
    if (!(fsb -> fsb_units & FUNIT_LIMITED)
	    && (fsb -> fsb_units & FUNIT_ENHANCED)) {
	(void) ftamoops (fti, FS_PRO_ERRFUNIT, 1, EREF_RFPM, EREF_IFPM,
		 NULLCP,
		 "enhanced-file-management requires limited-file-management");
	goto out3;
    }
    if (!(fsb -> fsb_units & FUNIT_GROUPING)) {	/* XXX: should be OPTIONAL */
	(void) ftamoops (fti, FS_PRO_ERRFUNIT, 1, EREF_RFPM, EREF_IFPM,
			 NULLCP,
			 "insufficient functional units for service class");
	goto out3;
    }
    if (req -> attribute__groups
	    && fpm2bits (fsb, fattr_pairs, req -> attribute__groups,
			 &fsb -> fsb_attrs, fti) == NOTOK)
	goto out3;
#ifdef	notdef
    if ((fsb -> fsb_attrs & FATTR_SECURITY)
	    && !(fsb -> fsb_attrs & FATTR_STORAGE)) {
	(void) ftamoops (fti, FS_ACS_GRP, 1, EREF_RFPM, EREF_IFPM,
			NULLCP, NULLCP);
	goto out3;
    }
#endif

    fts -> fts_sd = fsb -> fsb_fd;
    fts -> fts_callingtitle = acs -> acs_callingtitle;	/* struct copy */
    bzero ((char *) &acs -> acs_callingtitle, sizeof acs -> acs_callingtitle);
    fts -> fts_calledtitle = acs -> acs_calledtitle;	/* struct copy */
    bzero ((char *) &acs -> acs_calledtitle, sizeof acs -> acs_calledtitle);
    if ((fsb -> fsb_context = oid_cpy (acs -> acs_context)) == NULLOID) {
no_mem: ;
	(void) ftamoops (fti, FS_GEN_NOREASON, 1, EREF_RFPM, EREF_RFPM,
			 NULLCP, "out of memory");
	goto out3;
    }
    fts -> fts_context = acs -> acs_context;
    acs -> acs_context = NULLOID;
    fts -> fts_callingaddr = ps -> ps_calling;	/* struct copy */
    fts -> fts_calledaddr = ps -> ps_called;	/* struct copy */
    fts -> fts_manage = (fsb -> fsb_prequirements & PR_MANAGEMENT) ? 1 : 0;
    fts -> fts_class = fsb -> fsb_class;
    fts -> fts_units = fsb -> fsb_units;
    fts -> fts_attrs = fsb -> fsb_attrs;
    if (req -> shared__ASE__information
	    && fpm2shared (fsb, req -> shared__ASE__information,
			   &fts -> fts_sharedASE, fti) == NOTOK)
	goto out3;
    fts -> fts_fqos = fsb -> fsb_fqos = MY_FQOS;

    if (ps -> ps_ctxlist.pc_nctx > 1) {
#define	PC_XXX	(-2)		/* unique code */

	int	acsid;
	register struct type_FTAM_Contents__Type__List *dtn;
	register struct FTAMcontent *fx,
				    *fx2;
	register struct PSAPcontext *px;
	register struct isodocument *id;

	fsb -> fsb_contexts = ps -> ps_ctxlist;/* struct copy */
	bzero ((char *) &ps -> ps_ctxlist, sizeof ps -> ps_ctxlist);

	fx = fts -> fts_contents.fc_contents;

	(void) AcFindPCI (fsb -> fsb_fd, &acsid, aci);

	fx2 = fsb -> fsb_contents.fc_contents;
	fsb -> fsb_contents.fc_ncontent = 0;

	for (px = fsb -> fsb_contexts.pc_ctx,
	             i = fsb -> fsb_contexts.pc_nctx - 1;
	         i >= 0;
	         px++, i--)
	    if (px -> pc_id != fsb -> fsb_id
		    && px -> pc_id != acsid
		    && px -> pc_result == PC_ACCEPT)
		px -> pc_result = PC_XXX;

	for (dtn = req -> contents__type__list; dtn; dtn = dtn -> next) {
	    if ((id = getisodocumentbytype (dtn -> Document__Type__Name))
		    == NULL)
		continue;
	    for (px = fsb -> fsb_contexts.pc_ctx,
		 	i = fsb -> fsb_contexts.pc_nctx - 1;
		     i >= 0;
		     px++, i--) {
		if (px -> pc_id == fsb -> fsb_id
		    	|| px -> pc_id == acsid
			|| oid_cmp (id -> id_abstract, px -> pc_asn))
		    continue;
		break;
	    }

	    if (i < 0)
		continue;

	    if ((fx2 -> fc_dtn = oid_cpy (dtn -> Document__Type__Name))
			== NULLOID
		    || (fx -> fc_dtn = oid_cpy (dtn -> Document__Type__Name))
			    == NULLOID)
		goto no_mem;
	    fx2 -> fc_id = fx -> fc_id = px -> pc_id;
	    if (px -> pc_result == PC_XXX)
		px -> pc_result = PC_ACCEPT;
	    fx2 -> fc_result = fx -> fc_result = px -> pc_result;

	    fx++, fts -> fts_contents.fc_ncontent++;
	    fx2++, fsb -> fsb_contents.fc_ncontent++;
	}

	for (px = fsb -> fsb_contexts.pc_ctx,
		    i = fsb -> fsb_contexts.pc_nctx - 1;
		 i >= 0;
		 px++, i--)
	    if (px -> pc_result == PC_XXX)
		px -> pc_result = PC_REJECTED;

#undef	PC_XXX
    }
    else
	if (req -> contents__type__list) {
	    (void) ftamoops (fti, FS_PRO_ERRPROC, 1, EREF_RFPM, EREF_IFPM,
			NULLCP, "content types management botched");
	    goto out3;
	}
    
    if (req -> initiator__identity
	    && (fts -> fts_initiator = qb2str (req -> initiator__identity))
		    == NULL)
	goto no_mem;
    if (req -> account
	    && (fts -> fts_account = qb2str (req -> account)) == NULL)
	goto no_mem;
    if (req -> filestore__password) {	/* both choices are qbufs... */
	register struct qbuf *qb = req -> filestore__password -> un.graphic;

	if ((fts -> fts_password = qb2str (qb)) == NULL)
	    goto no_mem;
	fts -> fts_passlen = qb -> qb_len;
    }
    fts -> fts_ssdusize = fsb -> fsb_ssdusize;
    fts -> fts_qos = ps -> ps_qos;	/* struct copy */

    free_FTAM_PDU (pdu);
    ACSFREE (acs);

    return OK;

out3: ;
    if (pdu)
	free_FTAM_PDU (pdu);

out2: ;
    ACSFREE (acs);

out1: ;
    pe = NULLPE;
    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu))
	    == NULL)
	goto carry_on;
    pdu -> offset = type_FTAM_PDU_f__initialize__response;
    if ((rsp = (struct type_FTAM_F__INITIALIZE__response *)
	 		calloc (1, sizeof *rsp)) == NULL)
	goto carry_on;
    pdu -> un.f__initialize__response = rsp;
    if (rsp -> state__result = (struct type_FTAM_State__Result *)
				      calloc (1, sizeof *rsp -> state__result))
	rsp -> state__result -> parm = FSTATE_FAILURE;
    if (rsp -> action__result = (struct type_FTAM_Action__Result *)
				     calloc (1, sizeof *rsp -> action__result))
	rsp -> action__result -> parm= FACTION_PERM;
    rsp -> functional__units = bits2fpm (fsb, funit_pairs, 0, fti);
    if (rsp -> ftam__quality__of__service =
		(struct type_FTAM_FTAM__Quality__Of__Service *)
		       calloc (1, sizeof *rsp -> ftam__quality__of__service))
	rsp -> ftam__quality__of__service -> parm = MY_FQOS;
    rsp -> diagnostic = diag2fpm (fsb, 1, fti -> fti_abort.fta_diags, 1, fti);
    rsp -> checkpoint__window = 1;

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) != NOTOK)
	pe -> pe_context = fsb -> fsb_id;

carry_on: ;
    fsbtrace (fsb, (fsb -> fsb_fd, "A-ASSOCIATE.RESPONSE(reject)",
		    "F-INITIALIZE-response", pe, 0));

    (void) AcAssocResponse (acs -> acs_sd, ACS_TRANSIENT, ACS_USER_NOREASON,
		NULLOID, NULLAEI, NULLPA, NULLPC, ps -> ps_defctxresult, 0, 0,
		SERIAL_NONE, 0, &ps -> ps_connect, pe ? &pe : NULLPEP,
		pe ? 1 : 0, aci);
    if (pe)
	pe_free (pe);
    if (pdu)
	free_FTAM_PDU (pdu);

    fsb -> fsb_fd = NOTOK;
    freefsblk (fsb);

    return NOTOK;
}

/*    F-INITIALIZE.RESPONSE */

int	FInitializeResponse (sd, state, action, context, respondtitle,
	respondaddr, manage, class, units, attrs, sharedASE, fqos, contents,
	diag, ndiag, fti)
int	sd;
int	state,
	action,
	manage,
	class,
	units,
	attrs,
	fqos;
OID	context;
AEI	respondtitle;
struct PSAPaddr *respondaddr;
PE	sharedASE;
struct FTAMcontentlist *contents;
struct FTAMdiagnostic diag[];
int	ndiag;
struct FTAMindication *fti;
{
    register int    i;
    int	    result,
	    status;
    PE	    pe;
    register struct FTAMcontentlist *pl;
    register struct FTAMcontent *px;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    register struct ftamblk *fsb;
    register struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__INITIALIZE__response *rsp;

    if ((fsb = findfsblk (sd)) == NULL || (fsb -> fsb_flags & FSB_CONN))
	return ftamlose (fti, FS_GEN_NOREASON, 0, NULLCP,
		"invalid ftam descriptor");

    switch (state) {
	case FSTATE_SUCCESS: 
	    status = ACS_ACCEPT;
	    break;

	case FSTATE_FAILURE: 
	    break;

	default: 
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "bad value for state parameter");
    }
    switch (action) {
	case FACTION_SUCCESS: 
	    if (state == FSTATE_SUCCESS)
		break;
bad_pair: ;
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"bad value for state/action parameters");

	case FACTION_TRANS: 
	    if (state != FSTATE_FAILURE)
		goto bad_pair;
	    status = ACS_TRANSIENT;
	    break;

	case FACTION_PERM: 
	    if (state != FSTATE_FAILURE)
		goto bad_pair;
	    status = ACS_PERMANENT;
	    break;

	default: 
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		    "bad value for action parameter");
    }
#ifdef	notdef
    missingP (context);
    missingP (respondtitle);
    missingP (respondaddr);
#endif
    if (manage) {
	if (!(fsb -> fsb_prequirements & PR_MANAGEMENT))
	    return ftamlose (fti, FS_ACS_CONTEXT, 0, NULLCP, NULLCP);
    }
    else
	fsb -> fsb_prequirements &= ~(PR_MANAGEMENT | PR_RESTORATION);
    switch (class & fsb -> fsb_class) {
	case FCLASS_TRANSFER:
	case FCLASS_ACCESS:
	case FCLASS_MANAGE:
	case FCLASS_TM:
	    fsb -> fsb_class &= class;
	    break;

	default:
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			     "service class mismatch, offered 0x%x received 0x%x",
			     fsb -> fsb_class, class);
    }
    if (units & ~fsb -> fsb_units)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"functional units not open for negotiation");
    if (!(units & FUNIT_LIMITED) && (units & FUNIT_ENHANCED))
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"enhanced-file-management requires limited-file-management");
    if (!((fsb -> fsb_units = units) & (FUNIT_RECOVERY | FUNIT_RESTART))) {
	fsb -> fsb_srequirements &= ~SR_MINORSYNC;
	fsb -> fsb_ssn = SERIAL_NONE;
    }
    switch (fsb -> fsb_class) {
	case FCLASS_TRANSFER:
	    if (!(fsb -> fsb_units & FUNITS_TRANSFER))
		goto not_enough;
	    goto do_trans;
	case FCLASS_TM:
	    if (!(fsb -> fsb_units & FUNITS_TM))
		goto not_enough;
do_trans: ;
	    if (!(fsb -> fsb_units & (FUNIT_READ | FUNIT_WRITE))) {
not_enough: ;
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				 "insufficient functional units for service class");
	    }
	    if (fsb -> fsb_units & (FUNIT_ACCESS | FUNIT_FADULOCK)) {
too_many: ;
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				 "illegal functional units for service class");
	    }
	    break;

	case FCLASS_ACCESS:
	    if (!(fsb -> fsb_units & FUNITS_ACCESS))
		goto not_enough;
	    break;

	case FCLASS_MANAGE:
	    if (!(fsb -> fsb_units & FUNITS_MANAGE))
		goto not_enough;
	    if (fsb -> fsb_units & (FUNIT_READ | FUNIT_WRITE
				        | FUNIT_ACCESS | FUNIT_FADULOCK
				        | FUNIT_RECOVERY | FUNIT_RESTART))
		goto too_many;
	    break;
    }
    if (attrs & ~fsb -> fsb_attrs)
	return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
		"attribute groups not open for negotiation");
#ifdef	notdef
    if ((attrs & FATTR_SECURITY) && !(attrs & FATTR_STORAGE))
	return ftamlose (fti, FS_ACS_GRP, 0, NULLCP, NULLCP);
#endif
    fsb -> fsb_attrs = attrs;
    if (fqos != MY_FQOS)
	return ftamlose (fti, FS_ACS_ROLLBACK, 1, NULLCP,
			 "class-%d-recovery not supported", fqos);
    pl = &fsb -> fsb_contents;
    if (contents) {
	int	acsid;
	register struct FTAMcontent *fx = contents -> fc_contents;

	if (contents -> fc_ncontent != pl -> fc_ncontent)
	    return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			"proposed/resulting content types mismatch");
	
	(void) AcFindPCI (fsb -> fsb_fd, &acsid, aci);
	for (px = pl -> fc_contents, i = pl -> fc_ncontent - 1;
		i >= 0;
		px++, i--) {
	    if (fx -> fc_id != px -> fc_id)
		return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
			    "bad context id %d at offset %d (wanted %d)",
			    fx -> fc_id, fx - contents -> fc_contents,
			    px -> fc_id);
	    switch (fx -> fc_result) {
		case PC_ACCEPT:
		case PC_REJECTED:
		    if (px -> fc_result != PC_ACCEPT) {
invalid_result: ;
			return ftamlose (fti, FS_GEN (fsb), 0, NULLCP,
				    "invalid result %d for content id %d",
				    fx -> fc_result, fx -> fc_id);
		    }
		    px -> fc_result = fx -> fc_result;
		    break;

		default:
		    if (px -> fc_result != fx -> fc_result)
			goto invalid_result;
		    break;
	    }

	    fx++;
	}
    }

    toomuchP (diag, ndiag, NFDIAG, "diagnostic");
    missingP (fti);

    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL) {
no_mem: ;
	result = ftamlose (fti, FS_GEN (fsb), 1, NULLCP, "out of memory");
	goto out;
    }
    pdu -> offset = type_FTAM_PDU_f__initialize__response;
    if ((rsp = (struct type_FTAM_F__INITIALIZE__response *)
	 			calloc (1, sizeof *rsp)) == NULL)
	goto no_mem;
    pdu -> un.f__initialize__response = rsp;
    if (state != int_FTAM_State__Result_success) {
	if ((rsp -> state__result =
	     		(struct type_FTAM_State__Result *)
	     			calloc (1, sizeof *rsp -> state__result))
	        == NULL)
	    goto no_mem;
	rsp -> state__result -> parm = state;
    }
    if (action != int_FTAM_Action__Result_success) {
	if ((rsp -> action__result =
	     		(struct type_FTAM_Action__Result *)
	     			calloc (1, sizeof *rsp -> action__result))
	        == NULL)
	    goto no_mem;
	rsp -> action__result -> parm = action;
    }
    rsp -> presentation__context__management = manage;
    if (fsb -> fsb_class != FCLASS_TRANSFER
		&& (rsp -> service__class = bits2fpm (fsb, fclass_pairs,
						      fsb -> fsb_class, fti))
	    == NULLPE)
	goto out;
    if ((rsp -> functional__units = bits2fpm (fsb, funit_pairs,
					      fsb -> fsb_units, fti))
	    == NULLPE)
	goto out;
    if (fsb -> fsb_attrs
	    && (rsp -> attribute__groups = bits2fpm (fsb, fattr_pairs,
						     attrs, fti)) == NULLPE)
	goto out;
    if (sharedASE
		&& (rsp -> shared__ASE__information =
		    		shared2fpm (fsb, sharedASE, fti)) == NULL)
	goto out;
    if ((rsp -> ftam__quality__of__service =
	 	(struct type_FTAM_FTAM__Quality__Of__Service *)
	 		calloc (1, sizeof *rsp -> ftam__quality__of__service))
	    == NULL)
	goto no_mem;
    rsp -> ftam__quality__of__service -> parm = fsb -> fsb_fqos;
    if (contents) {
	struct type_FTAM_Contents__Type__List *fpm;
	register struct type_FTAM_Contents__Type__List **fpc;
	
	fpc = &rsp -> contents__type__list;
	for (px = pl -> fc_contents, i = pl -> fc_ncontent - 1;
		i >= 0;
		px++, i--)
	    if (px -> fc_result == PC_ACCEPT) {
		if ((fpm = (struct type_FTAM_Contents__Type__List *)
		     		calloc (1, sizeof *fpm)) == NULL)
		    goto no_mem;
		*fpc = fpm;

		if ((fpm -> Document__Type__Name = oid_cpy (px -> fc_dtn))
		        == NULLOID)
		    goto no_mem;
		fpc = &fpm -> next;
	    }
    }

    if (ndiag > 0
	    && (rsp -> diagnostic = diag2fpm (fsb, 0, diag, ndiag, fti))
		    == NULL)
	goto out;
    rsp -> checkpoint__window = 1;

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			 "error encoding PDU: %s", PY_pepy);
	goto out;
    }

    pe -> pe_context = fsb -> fsb_id;

    fsbtrace (fsb, (fsb -> fsb_fd, "A-ASSOCIATE.RESPONSE",
		"F-INITIALIZE-response", pe, 0));

    result = AcAssocResponse (fsb -> fsb_fd, status, status != ACS_ACCEPT
			      ? ACS_USER_NOREASON : ACS_USER_NULL,
		context ? context : fsb -> fsb_context, respondtitle,
		respondaddr, &fsb -> fsb_contexts, PC_ACCEPT,
		fsb -> fsb_prequirements, fsb -> fsb_srequirements,
		fsb -> fsb_ssn, fsb -> fsb_settings, &fsb -> fsb_connect,
		&pe, 1, aci);

    pe_free (pe);
    pe = NULLPE;
    free_FTAM_PDU (pdu);
    pdu = NULL;

    if (result == NOTOK) {
	(void) acs2ftamlose (fsb, fti, "AcAssocResponse", aca);
	goto out;
    }

    fsb -> fsb_flags |= FSB_CONN;
    return OK;
    
out: ;
    if (pe)
	pe_free (pe);
    if (pdu)
	free_FTAM_PDU (pdu);

    pe = NULLPE;
    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu))
	    == NULL)
	goto carry_on;
    pdu -> offset = type_FTAM_PDU_f__initialize__response;
    if ((rsp = (struct type_FTAM_F__INITIALIZE__response *)
	 			calloc (1, sizeof *rsp))
	    == NULL)
	goto carry_on;
    pdu -> un.f__initialize__response = rsp;
    if (rsp -> state__result = (struct type_FTAM_State__Result *)
				      calloc (1, sizeof *rsp -> state__result))
	rsp -> state__result -> parm = FSTATE_FAILURE;
    if (rsp -> action__result = (struct type_FTAM_Action__Result *)
				     calloc (1, sizeof *rsp -> action__result))
	rsp -> action__result -> parm= FACTION_PERM;
    rsp -> functional__units = bits2fpm (fsb, funit_pairs, 0, fti);
    if (rsp -> ftam__quality__of__service =
		(struct type_FTAM_FTAM__Quality__Of__Service *)
		       calloc (1, sizeof *rsp -> ftam__quality__of__service))
	rsp -> ftam__quality__of__service -> parm = MY_FQOS;
    rsp -> diagnostic = diag2fpm (fsb, 1, fti -> fti_abort.fta_diags, 1, fti);
    rsp -> checkpoint__window = 1;

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) != NOTOK)
	pe -> pe_context = fsb -> fsb_id;

carry_on: ;
    fsbtrace (fsb, (fsb -> fsb_fd, "A-ASSOCIATE.RESPONSE(reject)",
		    "F-INITIALIZE-response", pe, 0));

    (void) AcAssocResponse (fsb -> fsb_fd, ACS_TRANSIENT, ACS_USER_NOREASON,
		NULLOID, NULLAEI, NULLPA, NULLPC, PC_ACCEPT, 0, 0, SERIAL_NONE,
		0, &fsb -> fsb_connect, pe ? &pe : NULLPEP, pe ? 1 : 0, aci);
    if (pe)
	pe_free (pe);
    if (pdu)
	free_FTAM_PDU (pdu);

    fsb -> fsb_fd = NOTOK;
    freefsblk (fsb);

    return NOTOK;
}
