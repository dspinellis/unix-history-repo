/* smux.h - SMUX include file */

/* 
 * $Header: /f/osi/snmp/RCS/smux.h,v 1.4 91/02/22 09:44:01 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	smux.h,v $
 * Revision 1.4  91/02/22  09:44:01  mrose
 * Interim 6.8
 * 
 * Revision 1.3  90/10/29  18:38:49  mrose
 * updates
 * 
 * Revision 1.2  90/02/19  15:38:48  mrose
 * one more time
 * 
 * Revision 1.1  90/02/17  10:38:29  mrose
 * smux
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


#ifndef	PEPYPATH
#include <isode/pepy/SNMP-types.h>
#else
#include "SNMP-types.h"
#endif

/*  */

#define	readOnly	int_SNMP_operation_readOnly
#define	readWrite	int_SNMP_operation_readWrite
#define	delete		int_SNMP_operation_delete


#define	goingDown		int_SNMP_ClosePDU_goingDown
#define	unsupportedVersion	int_SNMP_ClosePDU_unsupportedVersion
#define	packetFormat		int_SNMP_ClosePDU_packetFormat
#define	protocolError		int_SNMP_ClosePDU_protocolError
#define	internalError		int_SNMP_ClosePDU_internalError
#define	authenticationFailure	int_SNMP_ClosePDU_authenticationFailure

#define	invalidOperation	(-1)
#define	parameterMissing	(-2)
#define	systemError		(-3)
#define	youLoseBig		(-4)
#define	congestion		(-5)
#define	inProgress		(-6)

extern	integer	smux_errno;
extern	char	smux_info[];

/*  */

int	smux_init ();				/* INIT */
int	smux_simple_open ();			/* (simple) OPEN */
int	smux_close ();				/* CLOSE */
int	smux_register ();			/* REGISTER */
int	smux_response ();			/* RESPONSE */
int	smux_wait ();				/* WAIT */
int	smux_trap ();				/* TRAP */

char   *smux_error ();				/* TEXTUAL ERROR */

/*  */

struct smuxEntry {
    char   *se_name;

    OIDentifier se_identity;
    char   *se_password;

    int	    se_priority;
};

int	setsmuxEntry (), endsmuxEntry ();

struct smuxEntry *getsmuxEntry ();

struct smuxEntry *getsmuxEntrybyname ();
struct smuxEntry *getsmuxEntrybyidentity ();
