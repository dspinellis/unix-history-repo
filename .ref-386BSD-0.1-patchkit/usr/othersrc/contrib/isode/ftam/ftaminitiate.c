/* ftaminitiate.c - FPM: initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/ftam/RCS/ftaminitiate.c,v 7.3 91/02/22 09:23:00 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/ftam/RCS/ftaminitiate.c,v 7.3 91/02/22 09:23:00 mrose Interim $
 *
 *
 * $Log:	ftaminitiate.c,v $
 * Revision 7.3  91/02/22  09:23:00  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/07  12:40:10  mrose
 * update
 * 
 * Revision 7.1  90/11/21  11:30:08  mrose
 * sun
 * 
 * Revision 7.0  89/11/23  21:53:41  mrose
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


#define	FS_CTX		"iso ftam"
#define	FS_ASN		"ftam pci"

/*    F-INITIALIZE.REQUEST */

int	FInitializeRequest (context, callingtitle, calledtitle, callingaddr,
	calledaddr, manage, class, units, attrs, sharedASE, fqos, contents,
	initiator, account, password, passlen, qos, tracing, ftc, fti)
OID	context;
AEI	callingtitle,
	calledtitle;
struct PSAPaddr *callingaddr,
		*calledaddr;
int	manage,
	class,
	units,
	attrs,
	fqos,
	passlen;
PE	sharedASE;
struct FTAMcontentlist *contents;
char   *initiator,
       *account,
       *password;
struct QOStype *qos;
IFP	tracing;
struct FTAMconnect *ftc;
struct FTAMindication *fti;
{
    SBV     smask;
    int     result;

#ifdef	notdef
    missingP (context);
    missingP (callingtitle);
    missingP (calledtitle);
    missingP (callingaddr);
#endif
    missingP (calledaddr);
    if (manage)
	return ftamlose (fti, FS_ACS_CONTEXT, 1, NULLCP, NULLCP);
    if (units & ~MY_FUNIT)
	return ftamlose (fti, FS_ACS_FUNIT, 1, NULLCP, NULLCP);
    if (!(units & FUNIT_LIMITED) && (units & FUNIT_ENHANCED))
	return ftamlose (fti, FS_GEN_NOREASON, 1, NULLCP,
		"enhanced-file-management requires limited-file-management");
    if ((class & FCLASS_TRANSFER
		&& ((units & FUNITS_TRANSFER) != FUNITS_TRANSFER))) {
not_enough: ;
	    return ftamlose (fti, FS_GEN_NOREASON, 1, NULLCP,
			    "insufficient functional units for service class");
    }
    if ((class & FCLASS_TM) && ((units & FUNITS_TM) != FUNITS_TM))
	goto not_enough;
    if ((class & (FCLASS_TRANSFER | FCLASS_TM))
	    && !(units & (FUNIT_READ | FUNIT_WRITE)))
	goto not_enough;
    if ((class & FCLASS_ACCESS) && ((units & FUNITS_ACCESS) != FUNITS_ACCESS))
	goto not_enough;
    if ((class & FCLASS_MANAGE) && ((units & FUNITS_MANAGE) != FUNITS_MANAGE))
	goto not_enough;
    if (!(class &=
		(FCLASS_TRANSFER | FCLASS_TM | FCLASS_MANAGE | FCLASS_ACCESS)))
	return ftamlose (fti, FS_ACS_CLASS, 1, NULLCP, NULLCP);
    if (!(units & FUNIT_LIMITED) && (units & FUNIT_ENHANCED))
	return ftamlose (fti, FS_GEN_NOREASON, 1, NULLCP,
	    "enhanced-file-management requires limited-file-management");
    if (!(units & FUNIT_GROUPING))	/* XXX: should be OPTIONAL */
	    goto not_enough;
    if (attrs & ~MY_FATTR)
	return ftamlose (fti, FS_ACS_GRPSUP, 1, NULLCP, NULLCP);
#ifdef	notdef
    if ((attrs & FATTR_SECURITY) && !(attrs & FATTR_STORAGE))
	return ftamlose (fti, FS_ACS_GRP, 1, NULLCP, NULLCP);
#endif
    if (contents && contents -> fc_ncontent > NFCONT)
	return ftamlose (fti, FS_GEN_NOREASON, 1, NULLCP,
		    "too many content types");
    if (password == NULL)
	passlen = 0;
#ifdef	notdef
    missingP (qos);
#endif
    missingP (fti);

    smask = sigioblock ();

    result = FInitializeRequestAux (context, callingtitle, calledtitle,
	    callingaddr, calledaddr, manage, class, units, attrs, sharedASE, fqos,
	    contents, initiator, account, password, passlen, qos, tracing, ftc,
	    fti);

    (void) sigiomask (smask);

    return result;
}

/*  */

static int  FInitializeRequestAux (context, callingtitle, calledtitle,
	callingaddr, calledaddr, manage, class, units, attrs, sharedASE, fqos,
	contents, initiator, account, password, passlen, qos, tracing, ftc,
	fti)
OID	context;
AEI	callingtitle,
	calledtitle;
struct PSAPaddr *callingaddr,
		*calledaddr;
int	manage,
	class,
	units,
	attrs,
	fqos,
	passlen;
PE	sharedASE;
struct FTAMcontentlist *contents;
char   *initiator,
       *account,
       *password;
struct QOStype *qos;
IFP	tracing;
struct FTAMconnect *ftc;
struct FTAMindication *fti;
{
    register int	i;
    int	    bits,
		rcvd_bits,
	    idc,
	    result,
	    settings;
    long    isn;
    PE	    pe;
    OID	    ctx,
	    pci;
    struct SSAPref *sr;
    register struct PSAPcontext *px;
    struct PSAPctxlist pls;
    register struct PSAPctxlist *pl = &pls;
    struct AcSAPconnect accs;
    register struct AcSAPconnect *acc = &accs;
    register struct PSAPconnect *pc = &acc -> acc_connect;
    struct AcSAPindication acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    register struct FTAMcontent *fx;
    register struct ftamblk *fsb;
    struct type_FTAM_PDU *pdu;
    register struct type_FTAM_F__INITIALIZE__request *req;
    register struct type_FTAM_F__INITIALIZE__response *rsp;

    if ((fsb = newfsblk ()) == NULL)
	return ftamlose (fti, FS_GEN_NOREASON, 1, NULLCP, "out of memory");
    fsb -> fsb_flags |= FSB_INIT;
    fsb -> fsb_trace = tracing;

    ctx = pci = NULLOID, pl -> pc_nctx = 0;
    bzero ((char *) ftc, sizeof *ftc);

    pdu = NULL;

    if (context == NULLOID && (context = ode2oid (FS_CTX)) == NULLOID) {
	result = ftamlose (fti, FS_ACS_MGMT, 1, NULLCP, "%s: unknown", FS_CTX);
	goto out1;
    }
    if ((ctx = oid_cpy (context)) == NULLOID) {
no_mem: ;
	result = ftamlose (fti, FS_GEN_NOREASON, 1, NULLCP, "out of memory");
	goto out1;
    }

    if ((pci = ode2oid (FS_ASN)) == NULLOID) {
	result = ftamlose (fti, FS_ACS_MGMT, 1, NULLCP, "%s: unknown", FS_ASN);
	goto out1;
    }
    if ((pci = oid_cpy (pci)) == NULLOID)
	goto no_mem;

    px = pl -> pc_ctx, pl -> pc_nctx = 0;

    px -> pc_id = fsb -> fsb_id = idc = 1;
    if ((px -> pc_asn = ode2oid (FS_ASN)) == NULLOID) {
	result = ftamlose (fti, FS_ACS_MGMT, 1, NULLCP, "%s: unknown", FS_ASN);
	goto out1;
    }
    if ((px -> pc_asn = oid_cpy (px -> pc_asn)) == NULLOID)
	goto no_mem;
    if ((px -> pc_atn = ode2oid (BER)) == NULLOID) {
	result = ftamlose (fti, FS_ACS_MGMT, 1, NULLCP, "%s: unknown", BER);
	goto out1;
    }
    if ((px -> pc_atn = oid_cpy (px -> pc_atn)) == NULLOID)
	goto no_mem;
    px++, pl -> pc_nctx++;

    if (contents) {
	register struct isodocument *id;

	for (fx = contents -> fc_contents, i = contents -> fc_ncontent - 1;
		i >= 0;
		fx++, i--) {
	    if (fx -> fc_dtn == NULLOID) {
		result = ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			"empty content type at slot %d",
			contents -> fc_ncontent - i - 1);
		goto out1;
	    }

	    if ((id = getisodocumentbytype (fx -> fc_dtn)) == NULL) {
		result = ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			"unknown document type %s at slot %d",
			sprintoid (fx -> fc_dtn),
			contents -> fc_ncontent - i - 1);
		goto out1;
	    }

	    px -> pc_id = (idc += 2);
	    if ((px -> pc_asn = oid_cpy (id -> id_abstract)) == NULLOID)
		goto no_mem;
	    if ((px -> pc_atn = oid_cpy (id -> id_transfer)) == NULLOID)
		goto no_mem;
	    px++, pl -> pc_nctx++;
	}
    }

    if ((pdu = (struct type_FTAM_PDU *) calloc (1, sizeof *pdu)) == NULL)
	goto no_mem;
    pdu -> offset = type_FTAM_PDU_f__initialize__request;
    if ((req = (struct type_FTAM_F__INITIALIZE__request *)
	 			calloc (1, sizeof *req)) == NULL)
	goto no_mem;
    pdu -> un.f__initialize__request = req;
    req -> presentation__context__management = manage;
    if (class != FCLASS_TRANSFER
		&& (req -> service__class = bits2fpm (fsb, fclass_pairs, class,
						      fti)) == NULLPE)
	goto out1;
    if ((req -> functional__units = bits2fpm (fsb, funit_pairs, units, fti))
	    == NULLPE)
	goto out1;
    if (attrs && (req -> attribute__groups = bits2fpm (fsb, fattr_pairs,
						       attrs, fti)) == NULLPE)
	goto out1;
    if (sharedASE
		&& (req -> shared__ASE__information =
		    		shared2fpm (fsb, sharedASE, fti)) == NULL)
	goto out1;
    if ((req -> ftam__quality__of__service =
	 	(struct type_FTAM_FTAM__Quality__Of__Service *)
	 		calloc (1, sizeof *req -> ftam__quality__of__service))
	    == NULL)
	goto no_mem;
#ifdef	lint
    req -> ftam__quality__of__service -> parm = fqos;
#else
    req -> ftam__quality__of__service -> parm = MY_FQOS;
#endif
    if (contents) {
	struct type_FTAM_Contents__Type__List *fpm;
	register struct type_FTAM_Contents__Type__List **fpc;

	fpc = &req -> contents__type__list;
	for (fx = contents -> fc_contents, i = contents -> fc_ncontent - 1;
		i >= 0;
		fx++, i--) {
	    if ((fpm = (struct type_FTAM_Contents__Type__List *)
			    calloc (1, sizeof *fpm)) == NULL)
		goto no_mem;
	    *fpc = fpm;

	    if ((fpm -> Document__Type__Name = oid_cpy (fx -> fc_dtn))
		    == NULLOID)
		goto no_mem;
	    fpc = &fpm -> next;
	}
    }
    if (initiator
	    && (req -> initiator__identity = str2qb (initiator,
						     strlen (initiator), 1))
		    == NULL)
	goto out1;
    if (account
	    && (req -> account = str2qb (account, strlen (account), 1))
		    == NULL)
	goto out1;
    if (password) {
	register struct type_FTAM_Password *p;

	if ((p = (struct type_FTAM_Password *) calloc (1, sizeof *p))
	        == NULL)
	    goto no_mem;
	req -> filestore__password = p;
	p -> offset = type_FTAM_Password_binary;
	if ((p -> un.binary = str2qb (password, passlen, 1)) == NULL)
	    goto no_mem;
    }
    req -> checkpoint__window = 1;

    if (encode_FTAM_PDU (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	(void) ftamlose (fti, FS_GEN (fsb), 1, NULLCP,
			 "error encoding PDU: %s", PY_pepy);
	goto out1;
    }

    pe -> pe_context = fsb -> fsb_id;

    fsb -> fsb_srequirements = SR_DUPLEX | SR_RESYNC;
    fsb -> fsb_srequirements &= ~SR_RESYNC;	/* XXX */
    if (units & (FUNIT_RECOVERY | FUNIT_RESTART))
	fsb -> fsb_srequirements |= SR_MINORSYNC;
    isn = (fsb -> fsb_srequirements & (SR_MINORSYNC | SR_RESYNC)) ? 1L
		: SERIAL_NONE;
    fsb -> fsb_prequirements = manage ? (PR_MANAGEMENT | PR_RESTORATION) : 0;
    settings = 0;
#define dotoken(requires,shift,bit,type) \
{ \
    if (fsb -> fsb_srequirements & requires) \
	settings |= ST_INIT_VALUE << shift; \
}
    dotokens ();
#undef	dotoken

    if ((sr = addr2ref (PLocalHostName ())) == NULL)
	goto no_mem;

    fsbtrace (fsb, (fsb -> fsb_fd, "A-ASSOCIATE.REQUEST",
		"F-INITIALIZE-request", pe, 0));

    result = AcAssocRequest (ctx, callingtitle, calledtitle, callingaddr,
		calledaddr, pl, NULLOID /* pci */, fsb -> fsb_prequirements,
	        fsb -> fsb_srequirements, isn, settings, sr, &pe, 1,
		qos, acc, aci);

    if (result == NOTOK) {
	(void) acs2ftamlose (fsb, fti, "AcAssocRequest", aca);
	goto out1;
    }

    fsb -> fsb_fd = acc -> acc_sd;

    pe_free (pe);
    pe = NULLPE;
    free_FTAM_PDU (pdu);
    pdu = NULL;
    oid_free (ctx);
    ctx = NULLOID;
    oid_free (pci);
    pci = NULLOID;
    for (px = pl -> pc_ctx, i = pl -> pc_nctx - 1; i >= 0; px++, i--) {
	if (px -> pc_asn)
	    oid_free (px -> pc_asn);
	if (px -> pc_atn)
	    oid_free (px -> pc_atn);
    }
	
    if (acc -> acc_ninfo < 1 || (pe = acc -> acc_info[0]) == NULLPE) {
	if (acc -> acc_result != ACS_ACCEPT) {
	    register struct FTAMabort *fta = &fti -> fti_abort;

	    aca -> aca_reason = acc -> acc_result;
	    (void) acs2ftamlose (fsb, fti, "AcAssocRequest(pseudo)", aca);

	    ftc -> ftc_sd = NOTOK;
	    ftc -> ftc_state = FSTATE_FAILURE;
	    ftc -> ftc_action = FACTION_PERM;
	    *ftc -> ftc_diags = *fta -> fta_diags;	/* struct copy */
	    ftc -> ftc_ndiag = fta -> fta_ndiag;

	    result = OK;
	}
	else
	    result = fpktlose (fsb, fti, FS_PRO_ERR, NULLCP, NULLCP);
	goto out2;
    }

    if (decode_FTAM_PDU (pe, 1, NULLIP, NULLVP, &pdu) == NOTOK) {
	result = fpktlose (fsb, fti, FS_PRO_ERRMSG, NULLCP,
			   "unable to parse PDU: %s", PY_pepy);
	goto out2;
    }
    if (pdu -> offset != type_FTAM_PDU_f__initialize__response) {
	result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			   "expecting F-INITIALIZE-response, got %d",
			   pdu -> offset);
	goto out2;
    }
    rsp = pdu -> un.f__initialize__response;

    fsbtrace (fsb,
	(fsb -> fsb_fd, "A-ASSOCIATE.CONFIRMATION", "F-INITIALIZE-response",
		pe, 1));

    ftc -> ftc_state = rsp -> state__result
    			  ? rsp -> state__result -> parm
			  : int_FTAM_State__Result_success;
    ftc -> ftc_action = rsp -> action__result
    			  ? rsp -> action__result -> parm
			  : int_FTAM_Action__Result_success;
    switch (acc -> acc_result) {
	case ACS_ACCEPT:
	    if (ftc -> ftc_state != FSTATE_SUCCESS
		    || ftc -> ftc_action != FACTION_SUCCESS) {
		result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				"state/action mismatch");
		goto out2;
	    }

	    fsb -> fsb_flags |= FSB_CONN;
	    fsb -> fsb_srequirements &= pc -> pc_srequirements;
#define dotoken(requires,shift,bit,type) \
{ \
	    if (fsb -> fsb_srequirements & requires) \
		switch (pc -> pc_settings & (ST_MASK << shift)) { \
		    case ST_INIT_VALUE << shift: \
			fsb -> fsb_owned |= bit; \
			fsb -> fsb_avail |= bit; \
			break; \
 \
		    case ST_RESP_VALUE << shift: \
			fsb -> fsb_avail |= bit; \
			break; \
 \
		    default: \
			result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP, \
					"%s token management botched", type); \
			goto out2; \
		} \
}
	    dotokens ();
#undef	dotoken
	    if (fsb -> fsb_owned != fsb -> fsb_avail) {
		result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
			    "token management botched");
		goto out2;
	    }
	    fsb -> fsb_ssdusize = pc -> pc_ssdusize;

	    pl = &pc -> pc_ctxlist;
	    if (pl -> pc_nctx > 0 && pl -> pc_ctx[0].pc_result != PC_ACCEPT) {
		result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				    "FTAM PCI rejected");
		goto out2;
	    }

	    fsb -> fsb_prequirements &= pc -> pc_prequirements;
	    if (rsp -> presentation__context__management) {
		if (!(fsb -> fsb_prequirements & PR_MANAGEMENT)) {
		    result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				"presentation content management mismatch");
		    goto out2;
		}
	    }
	    else
		fsb -> fsb_prequirements &= ~PR_MANAGEMENT;

		if (rsp -> service__class) {
        rcvd_bits = fpm2bits (fsb, fclass_pairs, rsp -> service__class,
&bits, fti);
        if (rcvd_bits == NOTOK)
            goto out2;
        else if (rcvd_bits > 1) {
                result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
                            "illegal negotiation of service classes");
                goto out2;
            }
        }
	    else
		bits = FCLASS_TRANSFER;
	    switch (fsb -> fsb_class = (bits & class)) {
		case FCLASS_TRANSFER:
		    i = FUNITS_TRANSFER;
		    break;

		case FCLASS_ACCESS:
		    i = FUNITS_ACCESS;
		    break;

		case FCLASS_MANAGE:
		    i = FUNITS_MANAGE;
		    break;

		case FCLASS_TM:
		    i = FUNITS_TM;
		    break;

		case FCLASS_UNCONS:
		    i = FUNITS_UNCONS;
		    break;

		default:
		    result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				       "service class mismatch, offered 0x%x received 0x%x",
				       class, bits);
		    goto out2;
	    }
	    if (fpm2bits (fsb, funit_pairs, rsp -> functional__units, &bits,
			  fti) == NOTOK)
		goto out2;
	    bits |= i;		/* conservative... */
	    if ((fsb -> fsb_units = bits) & ~units) {
		result = fpktlose (fsb, fti, FS_PRO_ERRFUNIT, NULLCP,
				"functional unit mismatch");
		goto out2;
	    }
/* check for illegal and mandatory functional units */
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
			result = fpktlose (fsb, fti, FS_PRO_ERRFUNIT, NULLCP,
	                 "insufficient functional units for service class");
			goto out2;
	        }
		    if (fsb -> fsb_units & (FUNIT_ACCESS | FUNIT_FADULOCK)) {
too_many: ;
			result = fpktlose (fsb, fti, FS_PRO_ERRFUNIT, NULLCP,
					   "illegal functional units for service class");
			goto out2;
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
					        | FUNIT_RECOVERY
					        | FUNIT_RESTART))
			goto too_many;
		    break;
	    }
	    if (rsp -> attribute__groups) {
		if (fpm2bits (fsb, fattr_pairs, rsp -> attribute__groups,
			      &bits, fti) == NOTOK)
		    goto out2;
	    }
	    else
		bits = 0;
	    if (bits & ~attrs) {
		result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				"attribute group mismatch");
		goto out2;
	    }
	    fsb -> fsb_attrs = bits;
	    if (rsp -> ftam__quality__of__service -> parm != MY_FQOS) {
		result = fpktlose (fsb, fti, FS_ACS_ROLLBACK, NULLCP,
				"class-%d-recovery not supported",
				   rsp -> ftam__quality__of__service -> parm);
		goto out2;
	    }
	    fsb -> fsb_fqos = MY_FQOS;

	    ftc -> ftc_sd = fsb -> fsb_fd;
	    break;
	    
	default:
	    ftc -> ftc_sd = NOTOK;
	    if (ftc -> ftc_state == FSTATE_SUCCESS
		    && ftc -> ftc_action == FACTION_SUCCESS) {
		result = fpktlose (fsb, fti, FS_PRO_ERRPROC, NULLCP,
				"state/action mismatch");
		goto out2;
	    }
	    {
		register struct FTAMabort *fta = &fti -> fti_abort;

		bzero ((char *) fta, sizeof *fta);
		fta -> fta_peer = 1;
	    }
	    break;
    }

    ftc -> ftc_respondtitle = acc -> acc_respondtitle;	/* struct copy */
    bzero ((char *) &acc -> acc_respondtitle, sizeof acc -> acc_respondtitle);
    ftc -> ftc_respondaddr = pc -> pc_responding;	/* struct copy */
    ftc -> ftc_context = acc -> acc_context;
    acc -> acc_context = NULLOID;
    ftc -> ftc_manage = (fsb -> fsb_prequirements & PR_MANAGEMENT) ? 1 : 0;
    ftc -> ftc_class = fsb -> fsb_class;
    ftc -> ftc_units = fsb -> fsb_units;
    ftc -> ftc_attrs = fsb -> fsb_attrs;
    if (rsp -> shared__ASE__information
	    && fpm2shared (fsb, rsp -> shared__ASE__information,
			   &ftc -> ftc_sharedASE, fti) == NOTOK)
	goto out2;
    ftc -> ftc_fqos = fsb -> fsb_fqos;

    if (contents) {
	register struct type_FTAM_Contents__Type__List *dtn;
	register struct FTAMcontent *fx2;

	fx2 = ftc -> ftc_contents.fc_contents;

	for (fx = contents -> fc_contents,
		i = contents -> fc_ncontent - 1;
		i >= 0;
		fx++, i--) {
	    for (dtn = rsp -> contents__type__list; dtn; dtn = dtn -> next)
		if (oid_cmp (fx -> fc_dtn, dtn -> Document__Type__Name) == 0)
		    break;
	    if (dtn == NULL)
		continue;

	    px = pl -> pc_ctx + 1 + (fx - contents -> fc_contents);

	    fx2 -> fc_dtn = dtn -> Document__Type__Name;
	    fx2 -> fc_id = px -> pc_id;
	    fx2 -> fc_result = px -> pc_result;

	    fx2++, ftc -> ftc_contents.fc_ncontent++;
	}

	for (dtn = rsp -> contents__type__list; dtn; dtn = dtn -> next) {
	    for (fx2 = ftc -> ftc_contents.fc_contents,
		    i = ftc -> ftc_contents.fc_ncontent - 1;
		    i >= 0;
		    fx2++, i--)
		if (dtn -> Document__Type__Name == fx2 -> fc_dtn) {
		    dtn -> Document__Type__Name = NULLOID;
		    break;
		}
	}
    }
    if (rsp -> diagnostic)
	(void) fpm2diag (fsb, rsp -> diagnostic, ftc -> ftc_diags,
			 &ftc -> ftc_ndiag, fti);
    ftc -> ftc_ssdusize = fsb -> fsb_ssdusize;
    ftc -> ftc_qos = pc -> pc_qos;	/* struct copy */

    free_FTAM_PDU (pdu);
    ACCFREE (acc);

    if (acc -> acc_result != ACS_ACCEPT)
	freefsblk (fsb);

    return OK;
    
out2: ;
    ACCFREE (acc);

out1: ;
    if (pdu)
	free_FTAM_PDU (pdu);
    if (ctx)
	oid_free (ctx);
    if (pci)
	oid_free (pci);
    for (px = pl -> pc_ctx, i = pl -> pc_nctx - 1; i >= 0; px++, i--) {
	if (px -> pc_asn)
	    oid_free (px -> pc_asn);
	if (px -> pc_atn)
	    oid_free (px -> pc_atn);
    }
    freefsblk (fsb);

    return result;
}
