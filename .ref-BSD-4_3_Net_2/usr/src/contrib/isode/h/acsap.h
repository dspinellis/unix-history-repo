/* acsap.h - include file for association control users (AcS-USER) */

/* 
 * $Header: /f/osi/h/RCS/acsap.h,v 7.1 91/02/22 09:24:34 mrose Interim $
 *
 *
 * $Log:	acsap.h,v $
 * Revision 7.1  91/02/22  09:24:34  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:55:39  mrose
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


#ifndef	_AcSAP_
#define	_AcSAP_

#ifndef	_MANIFEST_
#include "manifest.h"
#endif
#ifndef	_GENERAL_
#include "general.h"
#endif

#ifndef	_PSAP2_
#include "psap2.h"		/* definitions for PS-USERs */
#endif

/*  */

#define	NACDATA		3	/* arbitrary */

struct AcSAPstart {		/* A-CONNECT.INDICATION */
    int	    acs_sd;		/* association descriptor */
    
    OID	    acs_context;	/* application context name */

    AEInfo acs_callingtitle;	/* info on calling application-entity */
    AEInfo acs_calledtitle;	/* info on called application-entity */

    struct PSAPstart acs_start;	/* info from P-CONNECT.INDICATION */

				/* initial information from peer */
    int	    acs_ninfo;		/*   number of elements */
    PE	    acs_info[NACDATA];	/*   data */
};
#define	ACSFREE(acs) { \
    register int ACSI; \
 \
    if ((acs) -> acs_context) \
	oid_free ((acs) -> acs_context), (acs) -> acs_context = NULLOID; \
 \
    AEIFREE (&(acs) -> acs_callingtitle); \
    AEIFREE (&(acs) -> acs_calledtitle); \
 \
    PSFREE (&(acs) -> acs_start); \
 \
    for (ACSI = (acs) -> acs_ninfo - 1; ACSI >= 0; ACSI--) \
	if ((acs) -> acs_info[ACSI]) \
	    pe_free ((acs) -> acs_info[ACSI]), \
		(acs) -> acs_info[ACSI] = NULLPE; \
    (acs) -> acs_ninfo = 0; \
}


struct AcSAPconnect {
    int	    acc_sd;		/* association descriptor */
    
    int	    acc_result;		/* result */
#define	ACS_ACCEPT	0	/*   Accepted */
#define	ACS_REJECT	(-1)	/*   Release rejected */
				/*   Rejected by responder: */
#define	ACS_PERMANENT	1	/*     Permanent */
#define	ACS_TRANSIENT	2	/*     Transient */

    int	    acc_diagnostic;	/* source-diagnostic */
				/* service-user */
#define	ACS_USER_NULL	3	/*   null */
#define	ACS_USER_NOREASON 4	/*   no reason given */
#define	ACS_CONTEXT	5	/*   application context name not supported*/
#define	ACS_CALLING_AP_TITLE 6	/*   calling AP title not recognized */
#define	ACS_CALLING_AP_ID 7	/*   calling AP invocation-ID not recognized */
#define	ACS_CALLING_AE_QUAL 8	/*   calling AE qualifier not recognized */
#define	ACS_CALLING_AE_ID 9	/*   calling AE invocation-ID not recognized */
#define	ACS_CALLED_AP_TITLE 10	/*   called AP title not recognized */
#define	ACS_CALLED_AP_ID 11	/*   called AP invocation-ID not recognized */
#define	ACS_CALLED_AE_QUAL 12	/*   called AE qualifier not recognized */
#define	ACS_CALLED_AE_ID 13	/*   called AE invocation-ID not recognized */
				/* service-provider */
#define	ACS_PROV_NULL	14	/*   null */
#define	ACS_PROV_NOREASON 15	/*   no reason given */
#define	ACS_VERSION	16	/*   no common acse version */

				/* begin UNOFFICIAL */
#define	ACS_ADDRESS	17	/* Address unknown */
#define	ACS_REFUSED	18	/* Connect request refused on this network
				   connection */
#define	ACS_CONGEST	19	/* Local limit exceeded */
#define	ACS_PRESENTATION 20	/* Presentation disconnect */
#define	ACS_PROTOCOL	21	/* Protocol error */
#define	ACS_ABORTED	22	/* Peer aborted association */
#define	ACS_PARAMETER	23	/* Invalid parameter */
#define	ACS_OPERATION	24	/* Invalid operation */
#define	ACS_TIMER	25	/* Timer expired */
				/* end UNOFFICIAL */

#define	ACS_FATAL(r)	((r) < ACS_PARAMETER)
#define	ACS_OFFICIAL(r)	((r) < ACS_ADDRESS)

    OID	    acc_context;	/* application context name */

    AEInfo  acc_respondtitle;	/* info on responding application-entity */

    struct PSAPconnect acc_connect;/* info from P-CONNECT.CONFIRMATION */

				/* initial information from peer */
    int	    acc_ninfo;		/*   number of elements */
    PE	    acc_info[NACDATA];	/*   data */
};
#define	ACCFREE(acc) { \
    register int ACCI; \
 \
    if ((acc) -> acc_context) \
	oid_free ((acc) -> acc_context), (acc) -> acc_context = NULLOID; \
 \
    AEIFREE (&(acc) -> acc_respondtitle); \
 \
    PCFREE (&(acc) -> acc_connect); \
 \
    for (ACCI = (acc) -> acc_ninfo - 1; ACCI >= 0; ACCI--) \
	if ((acc) -> acc_info[ACCI]) \
	    pe_free ((acc) -> acc_info[ACCI]), \
		(acc) -> acc_info[ACCI] = NULLPE; \
    (acc) -> acc_ninfo = 0; \
}
	    


struct AcSAPfinish {		/* A-RELEASE.INDICATION */
    int	    acf_reason;		/* reason for release */
#define	ACF_NORMAL	0	/*   normal */
#define	ACF_URGENT	1	/*   urgent */
#define	ACF_USERDEFINED	30	/*   user-defined */

				/* release information from peer */
    int	    acf_ninfo;		/*   number of elements */
    PE	    acf_info[NACDATA];	/*   data */
};
#define	ACFFREE(acf) \
{ \
    register int ACFI; \
 \
    for (ACFI = (acf) -> acf_ninfo - 1; ACFI >= 0; ACFI--) \
	if ((acf) -> acf_info[ACFI]) \
	    pe_free ((acf) -> acf_info[ACFI]), \
		(acf) -> acf_info[ACFI] = NULLPE; \
    (acf) -> acf_ninfo = 0; \
}


struct AcSAPrelease {		/* A-RELEASE.CONFIRMATION */
    int	    acr_affirmative;	/* T   = connection released
				   NIL = request refused */
    
    int	    acr_reason;		/* reason for result */
#define	ACR_NORMAL	0	/*   normal */
#define	ACR_NOTFINISHED	1	/*   not finished */
#define	ACR_USERDEFINED	30	/*   user-defined */

				/* release information from peer */
    int	    acr_ninfo;		/*   number of elements */
    PE	    acr_info[NACDATA];	/*   data */
};
#define	ACRFREE(acr) \
{ \
    register int ACRI; \
 \
    for (ACRI = (acr) -> acr_ninfo - 1; ACRI >= 0; ACRI--) \
	if ((acr) -> acr_info[ACRI]) \
	    pe_free ((acr) -> acr_info[ACRI]), \
		(acr) -> acr_info[ACRI] = NULLPE; \
    (acr) -> acr_ninfo = 0; \
}


struct AcSAPabort {		/* A-{U,P}-ABORT.INDICATION */
    int	    aca_source;		/* abort source */
#define	ACA_USER	0	/*   service-user */
#define	ACA_PROVIDER	1	/*   service-provider */
#define	ACA_LOCAL	2	/*   local ACPM (UNOFFICIAL) */

    int	    aca_reason;		/* same codes as acc_result */
    
				/* abort information from peer */
    int	    aca_ninfo;		/*   number of elements */
    PE	    aca_info[NACDATA];	/*   data */

				/* diagnostics from provider */
#define	ACA_SIZE	512
    int	    aca_cc;		/*   length */
    char    aca_data[ACA_SIZE];	/*   data */
};
#define	ACAFREE(aca) \
{ \
    register int ACAI; \
 \
    for (ACAI = (aca) -> aca_ninfo - 1; ACAI >= 0; ACAI--) \
	if ((aca) -> aca_info[ACAI]) \
	    pe_free ((aca) -> aca_info[ACAI]), \
		(aca) -> aca_info[ACAI] = NULLPE; \
    (aca) -> aca_ninfo = 0; \
}


struct AcSAPindication {
    int	    aci_type;		/* the union element present */
#define	ACI_FINISH	0x00
#define	ACI_ABORT	0x01

    union {
	struct AcSAPfinish aci_un_finish;
	struct AcSAPabort aci_un_abort;
    }	aci_un;
#define	aci_finish	aci_un.aci_un_finish
#define	aci_abort	aci_un.aci_un_abort
};

/*  */

extern char *acsapversion;


int	AcInit ();		/* A-ASSOCIATE.INDICATION */

int	AcAssocResponse ();	/* A-ASSOCIATE.RESPONSE */
				/* A-ASSOCIATE.REQUEST
				   (backwards-compatible) */
#define	AcAssocRequest(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) \
	AcAsynAssocRequest (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,0)
int	AcAsynAssocRequest ();	/* A-(ASYN-)ASSOCIATE.REQUEST */
int	AcAsynRetryRequest ();	/* A-ASYN-RETRY.REQUEST (pseudo) */
int	AcRelRequest ();	/* A-RELEASE.REQUEST */
int	AcRelRetryRequest ();	/* A-RELEASE-RETRY.REQUEST (pseudo) */
int	AcRelResponse ();	/* A-RELEASE.RESPONSE */
int	AcUAbortRequest ();	/* A-ABORT.REQUEST */

int	AcFINISHser ();		/* handle P-RELEASE.INDICATION */
int	AcABORTser ();		/* handle P-{U,P}-ABORT.INDICATION */

int	AcFindPCI ();		/* return PCI used by ACSE */

char   *AcErrString ();		/* return AcSAP error code in string form */

#endif
