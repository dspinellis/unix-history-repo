/* mib.h - MIB realization */

/* 
 * $Header: /f/osi/snmp/RCS/mib.h,v 7.11 91/02/22 09:43:35 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	mib.h,v $
 * Revision 7.11  91/02/22  09:43:35  mrose
 * Interim 6.8
 * 
 * Revision 7.10  91/01/12  21:22:56  mrose
 * update
 * 
 * Revision 7.9  91/01/11  15:34:31  mrose
 * sets
 * 
 * Revision 7.8  91/01/07  12:40:51  mrose
 * update
 * 
 * Revision 7.7  90/12/18  10:13:43  mrose
 * update
 * 
 * Revision 7.6  90/07/09  14:48:52  mrose
 * sync
 * 
 * Revision 7.5  90/05/14  13:28:17  mrose
 * system
 * 
 * Revision 7.4  90/05/13  16:18:13  mrose
 * views
 * 
 * Revision 7.3  90/02/27  18:49:47  mrose
 * unix stuff
 * 
 * Revision 7.2  90/02/17  10:38:21  mrose
 * smux
 * 
 * Revision 7.1  90/01/11  18:34:17  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:13  mrose
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


#include <nlist.h>
#include "SNMP-types.h"
#include "objects.h"
#include "logger.h"


#define	generr(offset)	((offset) == type_SNMP_PDUs_get__next__request \
				    ? NOTOK : int_SNMP_error__status_genErr)

/*  */

#define	sysDescr	"4BSD/ISODE SNMP"
#define	sysObjectID	"fourBSD-isode.3"

/*  */

extern struct nlist nl[];
#define	N_ARPTAB	0
#define	N_ARPTAB_SIZE	1
#define	N_ICMPSTAT	2
#define	N_IFNET		3
#define	N_IPFORWARDING	4
#define	N_IPSTAT	5
#define	N_RTHASHSIZE	6
#define	N_RTHOST	7
#define	N_RTNET		8
#define	N_TCB		9
#define	N_TCPSTAT	10
#define	N_UDB		11
#define	N_UDPSTAT	12
#define	N_RTSTAT	13
#ifdef	BSD44
#define	N_RADIX_NODE_HEAD 14
#define	N_ISO_SYSTYPE	15
#define	N_CLNP_STAT	16
#define	N_ESIS_STAT	17
#endif


int	init_mib (), fin_mib (), set_variable ();
int	getkmem ();

/*  */

extern  int	nd;
extern	int	quantum;

extern	struct timeval
    		my_boottime;

extern	OID	nullSpecific;


void	adios (), advise ();
