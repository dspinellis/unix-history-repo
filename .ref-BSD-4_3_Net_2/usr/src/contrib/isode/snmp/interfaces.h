/* interfaces.h - support for MIB realization of the Interfaces group */

/* 
 * $Header: /f/osi/snmp/RCS/interfaces.h,v 7.6 91/02/22 09:43:26 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	interfaces.h,v $
 * Revision 7.6  91/02/22  09:43:26  mrose
 * Interim 6.8
 * 
 * Revision 7.5  90/12/18  10:13:34  mrose
 * update
 * 
 * Revision 7.4  90/10/23  20:36:22  mrose
 * update
 * 
 * Revision 7.3  90/10/17  14:33:24  mrose
 * update
 * 
 * Revision 7.2  90/03/24  10:54:06  mrose
 * update
 * 
 * Revision 7.1  90/01/11  18:34:08  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:06  mrose
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


#include "internet.h"
#include <net/if.h>
#include <netinet/if_ether.h>		/* to get struct arpcom */
#include "clns.h"

/*  */

extern	int	ifNumber;

struct interface {
    int	    ifn_index;			/* 1..ifNumber */
    int	    ifn_indexmask;		/* 1 << (index - 1) */

    int	    ifn_ready;			/* has an address associated with it */

    struct arpcom ifn_interface;	/* ifnet+physaddr */
    unsigned long ifn_offset;		/* where in kmem */

    char    ifn_descr[IFNAMSIZ];	/* e.g., "lo0" */

    int	    ifn_type;			/* ifType */
    u_long  ifn_speed;			/* ifSpeed */
    int	    ifn_admin;			/* ifAdminStatus */

    struct interface *ifn_next;
};

extern struct interface *ifs;


int	set_interface (), sort_interface ();

/*  */

union sockaddr_un {		/* 'cause sizeof (struct sockaddr_iso) == 32 */
    struct sockaddr	    sa;

    struct sockaddr_in	    un_in;
#ifdef	BSD44
    struct sockaddr_iso	    un_iso;
#endif
};


struct address {
#define	ADR_SIZE	(20 + 1 + 1)	/* object instance */
    unsigned int    adr_instance[ADR_SIZE];
    int	    adr_insize;

    union sockaddr_un adr_address;	/* address */
    union sockaddr_un adr_broadaddr;	/*   broadcast, only if AF_INET */
    union sockaddr_un adr_netmask;	/*   network mask */

    int	    adr_indexmask;		/* mask of interfaces with address */

    struct address *adr_next;
};

extern struct address *afs_inet;
#ifdef	BSD44
extern struct address *afs_iso;
#endif


struct address *find_address (), *get_addrent ();


#if	defined(BSD44) || defined(BSD43_Tahoe) || defined(RT) || defined(MIPS) || defined(ultrix)
#define	NEW_AT
#else
#undef	NEW_AT
#endif
