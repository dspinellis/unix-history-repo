/* acpkt.h - include file for association control providers (AcS-PROVIDER) */

/* 
 * $Header: /f/osi/h/RCS/acpkt.h,v 7.3 91/02/22 09:24:31 mrose Interim $
 *
 *
 * $Log:	acpkt.h,v $
 * Revision 7.3  91/02/22  09:24:31  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/07/01  21:03:46  mrose
 * pepsy
 * 
 * Revision 7.1  90/01/11  18:35:57  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  21:55:37  mrose
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
#include "acsap.h"		/* definitions for AcS-USERs */
#endif

#ifndef	_PSAP2_
#include "psap2.h"		/* definitions for PS-USERs */
#endif

/*  */

#ifdef	ACSE

#define	AC_ASN		"acse pci version 1"

#define	acsapPsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_OPERATION, NULLCP, \
			    "assocation descriptor not connected"); \
    } \
    if (acb -> acb_flags & ACB_FINN) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_OPERATION, NULLCP, \
			    "association descriptor finishing"); \
    } \
    if (acb -> acb_flags & ACB_RELEASE) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
}

#define	acsapFsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) {\
	(void) sigiomask (smask);\
	return acsaplose (aci, ACS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_OPERATION, NULLCP, \
			    "assocation descriptor not connected"); \
    } \
    if (!(acb -> acb_flags & ACB_FINN)) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_OPERATION, NULLCP, \
			    "association descriptor not finishing"); \
    } \
    if (acb -> acb_flags & ACB_RELEASE) { \
	(void) sigiomask (smask); \
	return acsaplose (aci, ACS_OPERATION, NULLCP, \
			    "release in progress"); \
    } \
}

#define	toomuchP(b,n,m,p) \
{ \
    if (b == NULL) \
	n = 0; \
    else \
	if (n > m) \
	    return acsaplose (aci, ACS_PARAMETER, NULLCP, \
				"too many %s user data elements", p); \
}

#define	missingP(p) \
{ \
    if (p == NULL) \
	return acsaplose (aci, ACS_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}

#ifndef	lint
#ifndef	__STDC__
#define	copyAcSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d/* */_cc = min (i, sizeof d -> d/* */_data)) > 0) \
	bcopy (base, d -> d/* */_data, d -> d/* */_cc); \
}
#else
#define	copyAcSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d##_cc = min (i, sizeof d -> d##_data)) > 0) \
	bcopy (base, d -> d##_data, d -> d##_cc); \
}
#endif
#else
#define	copyAcSAPdata(base,len,d)	bcopy (base, (char *) d, len)
#endif


#define	ACS_USER_BASE	ACS_USER_NULL
#define	ACS_PROV_BASE	ACS_PROV_NULL


int	acpktlose (), acsaplose ();

/*  */

int	ps2acslose ();

struct type_ACS_Association__information *info2apdu ();
int	apdu2info ();
#endif

/*  */

struct assocblk {
    struct assocblk *acb_forw;	/* doubly-linked list */
    struct assocblk *acb_back;	/*   .. */

    int	    acb_fd;		/* session/presentation descriptor */

    short   acb_flags;		/* our state */
#define	ACB_NULL	0x0000
#define	ACB_CONN	0x0001	/* connected */
#define	ACB_FINN	0x0002	/* other side wants to finish */
#define	ACB_INIT	0x0004	/* this side initiated the association */
#define	ACB_ASYN	0x0008	/* asynchronous */
#define	ACB_TURN	0x0010	/* we have the turn */
#define	ACB_TWA		0x0020	/* two-way alternate */
#define	ACB_ACT		0x0040	/* activity in progress */
#define	ACB_PLEASE	0x0080	/* RTS: RT-TURN-PLEASE received
				   ROS on Session: sent S-PLEASE-TOKENS */
#define	ACB_TIMER	0x0100	/* ACTIVITY discarded due to timing
				   constraint */
#define	ACB_ROS		0x0200	/* ROS started association */
#define	ACB_RTS		0x0400	/* RTS   .. */
#define	ACB_ACS		0x0800	/* ACS   .. */
#define	ACB_CLOSING	0x1000	/* waiting to close */
#define	ACB_FINISH	0x2000	/*   .. */
#define	ACB_STICKY	0x4000	/* ROS using RTS (ugh!) */
#define	ACB_RELEASE	0x8000	/* release in progress */

    struct SSAPref acb_connect;	/* session connection reference */

    int	    acb_requirements;	/* session requirements */
    int	    acb_owned;		/* session tokens we own */
    int	    acb_avail;		/* session tokens available */
    int	    acb_settings;	/* initial settings */
    int	    acb_ssdusize;	/* largest atomic SSDU */

    IFP	    acb_uabort;		/* disconnect underlying service */

/* ACSE */
    int	    acb_sversion;	/* session service version number */
    int	    acb_id;		/* ACSE context id */
    OID	    acb_context;	/* application context name */
    int	    acb_offset;		/* offset to ACSE PCI */
				/* negative means at END */
    PE	    acb_retry;		/* final acpkt */
    
/* RTSE */
    int	    acb_rtsid;		/* RTSE context id */
    int	    acb_ckpoint;	/* checkpoint size */
    int	    acb_window;		/* window size */
    int	    acb_actno;		/* sending activity serial number */
    long    acb_ssn;		/* highest serial number sent */
    int	    acb_ack;		/* highest serial number acknowledged */

    IFP	    acb_pturnrequest;	/* RT-TURN-PLEASE.REQUEST */
    IFP	    acb_gturnrequest;	/* RT-TURN-GIVE.REQUEST */
    IFP	    acb_transferequest;	/* RT-TRANSER.REQUEST */
    IFP	    acb_rtwaitrequest;	/* RT-WAIT.REQUEST */
    IFP	    acb_rtsetindications;/* define vectors for INDICATION events */
    IFP	    acb_rtselectmask;	/* map association descriptors for select () */
    IFP	    acb_rtpktlose;	/* protocol-level abort */

    int	    acb_priority;	/* priority of please turn */
    struct AcSAPfinish acb_finish;

    char   *acb_realbase;	/* APDU in transit */
    char   *acb_base;		/*   .. */
    int	    acb_len;		/*   .. */
    
    IFP	    acb_uptrans;	/* upcall for up-transfer */
    IFP	    acb_downtrans;	/* upcall for down-transfer */

    IFP	    acb_rtsindication;	/* rts event handler */

/* ROSE */
    int	    acb_rosid;		/* ROSE (SASE) context id */
    IFP	    acb_putosdu;	/* osdu2acb */
    IFP	    acb_rowaitrequest;	/* RO-WAIT.REQUEST */
    IFP	    acb_ready;		/* get HDX permission */
    IFP	    acb_rosetindications;/* define vectors for INDICATION events */
    IFP	    acb_roselectmask;	/* map association descriptors for select () */
    IFP	    acb_ropktlose;	/* protocol-level abort */
    PE	    (*acb_getosdu) ();	/* for users of THORN... */

    PE	    acb_apdu;		/* APDU buffered */
    
    IFP	    acb_rosindication;	/* ros event handler */
};
#define	NULLACB		((struct assocblk *) 0)

#define	FREEACB(acb) \
{ \
    if ((acb) -> acb_realbase) \
	free ((acb) -> acb_realbase); \
    else \
	if ((acb) -> acb_base) \
	    free ((acb) -> acb_base); \
    (acb) -> acb_realbase = (acb) -> acb_base = NULL, (acb) -> acb_len = 0; \
}


int	freeacblk ();
struct assocblk *newacblk (), *findacblk ();

/*  */

#ifndef	ACSE

				/* PConnect Types */
#define	PCONN_DTS	0	/* Data Transfer Syntax */
#define	PCONN_DATA	1	/* User Data */
#define	  PCONN_DATA_CK	0	/*   Checkpoint Size */
#define	    PCONN_CK_DFLT 0
#define	  PCONN_DATA_WD	1	/*   Window Size */
#define	    PCONN_WD_DFLT 3
#define	  PCONN_DATA_DM	2	/*   Dialogue-mode */
#define	    PCONN_DM_MONO 0	/*     monologue */
#define	    PCONN_DM_TWA  1	/*     two-way alternate */
#define	    PCONN_DM_DFLT PCONN_DM_MONO
#define	  PCONN_DATA_CN	3	/*   Connection Data */
#define	  PCONN_DATA_AP	4	/*   Application Protocol */
#define	    PCONN_AP_DFLT 1


				/* PAccept Types */
#define	PACC_DTS	0	/* Data Transfer Syntax */
#define	PACC_DATA	1	/* User Data */
#define   PACC_DATA_CK  0	/*   Checkpoint Size */
#define	    PACC_CK_DFLT 0
#define   PACC_DATA_WD  1	/*   Window Size */
#define	    PACC_WD_DFLT 3
#define   PACC_DATA_CN  2	/*   Connection Data */


				/* PRefuse Types */
#define	PREF_REASON	0	/* Refuse Reason */


				/* Data Transfer Syntax Types */
#define	DTS_SYNTAX	0	/* IMPLICIT INTEGER */
#define SYN_X409	0	/* x.409 */


				/* Connection Data Types */
#define CN_OPEN		0	/* Open */
#define	CN_RECOVER	1	/* Recover */


				/* Refuse codes */
#define	REFUSE_BUSY	0	/* Busy */
#define	REFUSE_RECOVER	1	/* Cannot recover */
#define	REFUSE_VALIDATE	2	/* Validation failure */
#define	REFUSE_MODE	3	/* Unacceptable dialogue mode */


				/* Abort codes */
#define	ABORT_LSP	0	/* Local system problem */
#define	ABORT_INV	1	/* Invalid parameter */
#define	ABORT_ACT	2	/* Unrecognized activity */
#define	ABORT_TMP	3	/* Temporary problem */
#define	ABORT_PROTO	4	/* Protocol error */
#define	ABORT_PERM	5	/* Permanent problem */
#define	ABORT_USER	6	/* User abort */
#define	ABORT_TRANS	7	/* Transfer completed */
#endif
