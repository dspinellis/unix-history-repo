/* ropkt.h - include file for remote operation providers (RoS-PROVIDER) */

/* 
 * $Header: /f/osi/h/RCS/ropkt.h,v 7.2 91/02/22 09:24:59 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	ropkt.h,v $
 * Revision 7.2  91/02/22  09:24:59  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:03:53  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  21:55:55  mrose
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


#ifndef	_RoSAP_
#include "rosap.h"		/* definitions for RoS-USERs */
#endif

#include "acpkt.h"		/* definitions for AcS-PROVIDERs */

#ifndef	_RtSAP_
#include "rtsap.h"		/* definitions for RtS-USERs */
#endif

#ifndef _PSAP2_
#include "psap2.h"		/* definitions for PS-USERs */
#endif

#ifndef	_SSAP_
#include "ssap.h"		/* definitions for SS-USERs */
#endif

/*  */

#define	rosapPsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not connected"); \
    } \
    if (acb -> acb_flags & ACB_CLOSING) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_WAITING, NULLCP, NULLCP); \
    } \
    if (acb -> acb_flags & ACB_FINN) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor finishing"); \
    } \
    if (acb -> acb_putosdu == NULLIFP) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not bound"); \
    } \
}

#define	rosapXsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not connected"); \
    } \
    if (acb -> acb_flags & ACB_FINN) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor finishing"); \
    } \
    if (acb -> acb_putosdu == NULLIFP) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not bound"); \
    } \
}

#define	rosapFsig(acb, sd) \
{ \
    if ((acb = findacblk (sd)) == NULL) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "invalid association descriptor"); \
    } \
    if (!(acb -> acb_flags & ACB_ROS)) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "not an association descriptor for ROS"); \
    } \
    if (!(acb -> acb_flags & ACB_CONN)) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not connected"); \
    } \
    if (!(acb -> acb_flags & ACB_FINN)) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not finishing"); \
    } \
    if (acb -> acb_putosdu == NULLIFP) { \
	(void) sigiomask (smask); \
	return rosaplose (roi, ROS_OPERATION, NULLCP, \
			    "association descriptor not bound"); \
    } \
}

#define	missingP(p) \
{ \
    if (p == NULL) \
	return rosaplose (roi, ROS_PARAMETER, NULLCP, \
			    "mandatory parameter \"%s\" missing", "p"); \
}


#ifndef	lint
#ifndef	__STDC__
#define	copyRoSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d/* */_cc = min (i, sizeof d -> d/* */_data)) > 0) \
	bcopy (base, d -> d/* */_data, d -> d/* */_cc); \
}
#else
#define	copyRoSAPdata(base,len,d) \
{ \
    register int i = len; \
    if ((d -> d##_cc = min (i, sizeof d -> d##_data)) > 0) \
	bcopy (base, d -> d##_data, d -> d##_cc); \
}
#endif
#else
#define	copyRoSAPdata(base,len,d)	bcopy (base, (char *) d, len)
#endif


#define	pylose() \
	ropktlose (acb, roi, ROS_PROTOCOL, NULLCP, "%s", PY_pepy)


int	ropktlose (), rosapreject (), rosaplose ();

/*  */

int	acb2osdu ();


int	ro2rtswrite (), ro2rtswait (), ro2rtsready (), ro2rtsasync (),
	ro2rtsmask ();


int	ro2pswrite (), ro2pswait (), ro2psasync (), ro2psmask ();


int	ss2roslose (), ss2rosabort ();
int	ro2sswrite (), ro2sswait (), ro2ssasync (), ro2ssmask (), ro2sslose (),
	ro2ssready ();

/*  */

				/* APDU types */
#define	APDU_INVOKE	1	/* Invoke */
#define	APDU_RESULT	2	/* Return result */
#define	APDU_ERROR	3	/* Return error */
#define	APDU_REJECT	4	/* Reject */

#define	APDU_UNKNOWN	(-1)	/* anything other than the above */


				/* Reject APDU types */
#define	REJECT_GENERAL	0	/* General Problem */
#define	  REJECT_GENERAL_BASE	ROS_GP_UNRECOG
#define	REJECT_INVOKE	1	/* Invoke Problem */
#define	  REJECT_INVOKE_BASE	ROS_IP_DUP
#define	REJECT_RESULT	2	/* Return Result Problem */
#define	  REJECT_RESULT_BASE	ROS_RRP_UNRECOG
#define	REJECT_ERROR	3	/* Return Error Problem */
#define	  REJECT_ERROR_BASE	ROS_REP_UNRECOG
#define	REJECT_COMPLETE	4	/* more Invoke Problem codes */
