/* rtpkt.h - include file for reliable transfer providers (RtS-PROVIDER) */

/* 
 * $Header: /f/osi/h/RCS/rtpkt.h,v 7.1 91/02/22 09:25:03 mrose Interim $
 *
 *
 * $Log:	rtpkt.h,v $
 * Revision 7.1  91/02/22  09:25:03  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  21:55:58  mrose
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


#ifndef	_RtSAP_
#include "rtsap.h"		/* definitions for RtS-USERs */
#endif

#include "acpkt.h"		/* definitions for AcS-PROVIDERs */

#ifndef	_SSAP_
#include "ssap.h"		/* definitions for SS-USERs */
#endif

/*  */

#define	rtsapPsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_RTS)) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_OPERATION, NULLCP, \
			    "not an association descriptor for RTS"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_OPERATION, NULLCP, \
			    "association descriptor not connected"); \
    } \
    if (acb -> acb_flags & ACB_FINN) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_OPERATION, NULLCP, \
			    "association descriptor finishing"); \
    } \
}

#define	rtsapFsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_RTS)) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_OPERATION, NULLCP, \
			    "not an association descriptor for RTS"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_OPERATION, NULLCP, \
			    "association descriptor not connected"); \
    } \
    if (!(acb -> acb_flags & ACB_FINN)) { \
	(void) sigiomask (smask); \
	return rtsaplose (rti, RTS_OPERATION, NULLCP, \
			    "association descriptor not finishing"); \
    } \
}

#define	missingP(p) \
{ \
    if (p == NULL) \
	return rtsaplose (rti, RTS_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}


#ifndef	lint
#ifndef	__STDC__
#define	copyRtSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d/* */_cc = min (i, sizeof d -> d/* */_data)) > 0) \
	bcopy (base, d -> d/* */_data, d -> d/* */_cc); \
}
#else
#define	copyRtSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d##_cc = min (i, sizeof d -> d##_data)) > 0) \
	bcopy (base, d -> d##_data, d -> d##_cc); \
}
#endif
#else
#define	copyRtSAPdata(base,len,d)	bcopy (base, (char *) d, len)
#endif


#define	pylose() \
	rtpktlose (acb, rti, RTS_PROTOCOL, NULLCP, "%s", PY_pepy)


/* would really prefer to determine DEFAULT_CKPOINT dynamically, but can't
   since need to know it *before* doing the A-ASSOCIATE.REQUEST or
   S-CONNECT.REQUEST... */

#define	DEFAULT_CKPOINT	(65518 >> 10)
#define	DEFAULT_WINDOW	PCONN_WD_DFLT


#define	RTS_MYREQUIRE	(SR_EXCEPTIONS | SR_ACTIVITY | SR_HALFDUPLEX \
			    | SR_MINORSYNC)


#define	RT_ASN		"rtse pci version 1"


int	rtpktlose (), rtsaplose ();

/*  */

#define	SetPS2RtService(acb) \
{ \
    (acb) -> acb_pturnrequest = rt2pspturn; \
    (acb) -> acb_gturnrequest = rt2psgturn; \
    (acb) -> acb_transferequest = rt2pstrans; \
    (acb) -> acb_rtwaitrequest = rt2pswait; \
    (acb) -> acb_rtsetindications = rt2psasync; \
    (acb) -> acb_rtselectmask = rt2psmask; \
    (acb) -> acb_rtpktlose = rt2pslose; \
}

int	acs2rtslose (), acs2rtsabort (), ps2rtslose ();
int	rt2pspturn (), rt2psgturn (), rt2pstrans (), rt2pswait (),
	rt2psasync (), rt2psmask (), rt2pslose ();


#define	SetSS2RtService(acb) \
{ \
    (acb) -> acb_pturnrequest = rt2sspturn; \
    (acb) -> acb_gturnrequest = rt2ssgturn; \
    (acb) -> acb_transferequest = rt2sstrans; \
    (acb) -> acb_rtwaitrequest = rt2sswait; \
    (acb) -> acb_rtsetindications = rt2ssasync; \
    (acb) -> acb_rtselectmask = rt2ssmask; \
    (acb) -> acb_rtpktlose = rt2sslose; \
}

int	ss2rtslose (), ss2rtsabort ();
int	rt2sspturn (), rt2ssgturn (), rt2sstrans (), rt2sswait (),
	rt2ssasync (), rt2ssmask (), rt2sslose ();

/*  */
				/* RTORQ apdu */
#define	RTORQ_CKPOINT	0	/* checkpointSize tag */
#define	  RTORQ_CK_DFLT	0	/*   default */
#define	RTORQ_WINDOW	1	/* windowSize tag */
#define	  RTORQ_WD_DFLT	3	/*   default */
#define	RTORQ_DIALOGUE	2	/* dialogueMode tag */
#define	  RTORQ_DM_MONO 0	/*   monologue */
#define	  RTORQ_DM_TWA	1	/*   two-way alternate */
#define	  RTORQ_DM_DFLT	RTORQ_DM_MONO
#define	RTORQ_CONNDATA	3	/* connectionDataRQ tag */
#define	  RTORQ_CD_OPEN 0	/*   open tag */
#define	  RTORQ_CD_RCVR	1	/*   recover tag */

				/* RTOAC apdu */
#define	RTOAC_CKPOINT	0	/* checkpointSize tag */
#define	  RTOAC_CK_DFLT	0	/*   default */
#define	RTOAC_WINDOW	1	/* windowSize tag */
#define	  RTOAC_WD_DFLT	3	/*   default */
#define	RTOAC_CONNDATA	2	/* connectionDataAC */
#define	  RTOAC_CD_OPEN	0	/*   open tag */
#define	  RTOAC_CD_RCVR	1	/*   recover tag */

				/* RTORJ apdu */
#define	RTORJ_REFUSE	0	/* refuseReason tag */
#define	RTORJ_USERDATA	1	/* userDataRJ */

				/* RTAB apdu */
#define	RTAB_REASON	0	/* abortReason tag */
#define	RTAB_REFLECT	1	/* relectedParameter tag */
#define	RTAB_USERDATA	2	/* userDataAB */

extern int rtsap_priority;
