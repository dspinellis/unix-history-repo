/* isoaddrs.h - ISODE addressing */

/* 
 * $Header: /f/osi/h/RCS/isoaddrs.h,v 7.8 91/02/22 09:24:44 mrose Interim $
 *
 *
 * $Log:	isoaddrs.h,v $
 * Revision 7.8  91/02/22  09:24:44  mrose
 * Interim 6.8
 * 
 * Revision 7.7  90/12/11  10:54:02  mrose
 * lock-and-load
 * 
 * Revision 7.6  90/10/29  18:38:09  mrose
 * updates
 * 
 * Revision 7.5  90/10/15  18:20:37  mrose
 * unused
 * 
 * Revision 7.4  90/07/09  14:37:47  mrose
 * sync
 * 
 * Revision 7.3  90/05/08  08:54:34  mrose
 * touch-up
 * 
 * Revision 7.2  90/03/15  11:17:58  mrose
 * quipu-sync
 * 
 * Revision 7.1  89/11/30  23:54:02  mrose
 * pa2str
 * 
 * Revision 7.0  89/11/23  21:55:46  mrose
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


#ifndef	_ISOADDRS_
#define	_ISOADDRS_

#ifndef	_MANIFEST_
#include "manifest.h"
#endif
#ifndef	_GENERAL_
#include "general.h"
#endif

/*  */

#ifdef	NULLPE
typedef struct AEInfo {		/* "real" directory services! */
    PE	    aei_ap_title;
    PE	    aei_ae_qualifier;

    int	    aei_ap_id;
    int	    aei_ae_id;

    int	    aei_flags;
#define	AEI_NULL	0x00
#define	AEI_AP_ID	0x01
#define	AEI_AE_ID	0x02
}	AEInfo, *AEI;
#define	NULLAEI		((AEI) 0)
#define	AEIFREE(aei) { \
    if ((aei) -> aei_ap_title) \
	pe_free ((aei) -> aei_ap_title), \
	    (aei) -> aei_ap_title = NULLPE; \
    if ((aei) -> aei_ae_qualifier) \
	pe_free ((aei) -> aei_ae_qualifier), \
	    (aei) -> aei_ae_qualifier = NULLPE; \
}

#define	str2aei(d,q)	_str2aei ((d), (q), NULLCP, 0, NULLCP, NULLCP)
#define	str2aeinfo(string,context,interactive,userdn,passwd) \
	_str2aei ((string), NULLCP, (context), (interactive), (userdn), \
		  (passwd))
AEI	_str2aei ();

char   *sprintaei ();
#endif

/*  */

struct NSAPaddr {		/* this structure shouldn't have holes in it */
    long     na_stack;			/* TS-stack */
#define	NA_NSAP	0			/*   native COTS */
#define	NA_TCP	1			/*   RFC1006/TCP */
#define	NA_X25	2			/*   TP0/X.25 */
#define	NA_BRG	3			/*   TP0-bridge */

    long    na_community;		/* internal community # */
#define	SUBNET_REALNS		(-1)	/* hard-wired */
#define	SUBNET_INT_X25		1	
/* (unused)
#define	SUBNET_JANET		2
 */
#define	SUBNET_INTERNET		3
#define	SUBNET_DYNAMIC		100	/* dynamic start here... */

    union {
	struct na_nsap {		/* real network service */
#define	NASIZE	64			/* 20 ought to do it */
	    char    na_nsap_address[NASIZE];
	    char    na_nsap_addrlen;
	}               un_na_nsap;

	struct na_tcp {			/* emulation via RFC1006 */
#define	NSAP_DOMAINLEN	63
	    char    na_tcp_domain[NSAP_DOMAINLEN + 1];

	    u_short na_tcp_port;	/* non-standard TCP port */
	    u_short na_tcp_tset;	/* transport set */
#define	NA_TSET_TCP	0x0001		/*   .. TCP */
#define	NA_TSET_UDP	0x0002	        /*   .. UDP */
	}               un_na_tcp;

	struct na_x25 {			/* X.25 (assume single subnet) */
#define	NSAP_DTELEN	36
	    char    na_x25_dte[NSAP_DTELEN + 1]; /* Numeric DTE + Link */
	    char    na_x25_dtelen;	/* number of digits used */

/* Conventionally, the PID sits at the first head bytes of user data and so
 * should probably not be mentioned specially. A macro might do it, if
 * necessary.
 */

#define	NPSIZE	4
	    char    na_x25_pid[NPSIZE];	/* X.25 protocol id */
	    char    na_x25_pidlen;	/*   .. */

#define	CUDFSIZE 16
	    char    na_x25_cudf[CUDFSIZE];/* call user data field */
	    char    na_x25_cudflen;	/* .. */
/*
 * X25 Facilities field. 
 */
#define	FACSIZE	6
	    char    na_x25_fac[FACSIZE];	/* X.25 facilities */
	    char    na_x25_faclen;		/*   .. */
	}               un_na_x25;
    }               na_un;
#define	na_address	na_un.un_na_nsap.na_nsap_address
#define	na_addrlen	na_un.un_na_nsap.na_nsap_addrlen

#define	na_domain	na_un.un_na_tcp.na_tcp_domain
#define	na_port		na_un.un_na_tcp.na_tcp_port
#define	na_tset		na_un.un_na_tcp.na_tcp_tset

#define	na_dte		na_un.un_na_x25.na_x25_dte
#define	na_dtelen	na_un.un_na_x25.na_x25_dtelen
#define	na_pid		na_un.un_na_x25.na_x25_pid
#define	na_pidlen	na_un.un_na_x25.na_x25_pidlen
#define	na_cudf		na_un.un_na_x25.na_x25_cudf
#define	na_cudflen	na_un.un_na_x25.na_x25_cudflen
#define	na_fac		na_un.un_na_x25.na_x25_fac
#define	na_faclen	na_un.un_na_x25.na_x25_faclen

/* for backwards compatibility... these two will be removed after ISODE 7.0 */
#define	na_type		na_stack
#define	na_subnet	na_community
};
#define	NULLNA			((struct NSAPaddr *) 0)


struct TSAPaddr {
#define	NTADDR	8			/* according to NIST OIW */
    struct NSAPaddr ta_addrs[NTADDR];	/* choice of network addresses */
    int     ta_naddr;

#define	TSSIZE	64
    int	    ta_selectlen;

    union {				/* TSAP selector */
	char    ta_un_selector[TSSIZE];

	u_short ta_un_port;
    }               un_ta;
#define	ta_selector	un_ta.ta_un_selector
#define	ta_port		un_ta.ta_un_port
};
#define	NULLTA			((struct TSAPaddr *) 0)


struct SSAPaddr {
    struct TSAPaddr sa_addr;		/* transport address */

#define	SSSIZE	64
    int	    sa_selectlen;

    union {				/* SSAP selector */
	char    sa_un_selector[SSSIZE];

	u_short sa_un_port;
    }               un_sa;
#define	sa_selector	un_sa.sa_un_selector
#define	sa_port		un_sa.sa_un_port
};
#define	NULLSA			((struct SSAPaddr *) 0)


struct PSAPaddr {
    struct SSAPaddr pa_addr;		/* session address */

#define	PSSIZE	64
    int	    pa_selectlen;

    union {				/* PSAP selector */
	char    pa_un_selector[PSSIZE];

	u_short pa_un_port;
    }               un_pa;
#define	pa_selector	un_pa.pa_un_selector
#define	pa_port		un_pa.pa_un_port
};
#define	NULLPA			((struct PSAPaddr *) 0)

struct PSAPaddr *aei2addr ();	/* application entity title to PSAPaddr */

/*  */

#ifdef	NULLPE
char   *alias2name ();

extern PE    (*acsap_lookup) ();
#endif


#ifdef	NULLOID
struct isoentity {		/* for stub directory service */
    OIDentifier ie_identifier;
    char    *ie_descriptor;

    struct PSAPaddr ie_addr;
};

int	setisoentity (), endisoentity ();

struct isoentity *getisoentity ();

AEI	oid2aei ();
#endif


				/* old-style */
struct PSAPaddr *is2paddr ();	/* service entry to PSAPaddr */
struct SSAPaddr *is2saddr ();	/* service entry to SSAPaddr */
struct TSAPaddr *is2taddr ();	/* service entry to TSAPaddr */

/*  */

struct PSAPaddr *str2paddr ();  /* string encoding to PSAPaddr */
struct SSAPaddr *str2saddr ();  /* string encoding to SSAPaddr */
struct TSAPaddr *str2taddr ();  /* string encoding to TSAPaddr */

#define	paddr2str(pa,na)	_paddr2str ((pa), (na), 0)

char   *_paddr2str ();		/* PSAPaddr to string encoding */
char   *saddr2str ();		/* SSAPaddr to string encoding */
char   *taddr2str ();		/* TSAPaddr to string encoding */

struct NSAPaddr *na2norm ();	/* normalize NSAPaddr */

char   *na2str ();		/* pretty-print NSAPaddr */
char   *pa2str ();		/* pretty-print PSAPaddr */

/*  */

int	isodeserver ();		/* generic server dispatch */

int	iserver_init ();	/* phase 1 */
int	iserver_wait ();	/* phase 2 */
fd_set	iserver_mask ();	/* linkage */

/*  */

/* all of this really should be in "isoqos.h" ... */
   
struct QOStype {
				/* transport QOS */
    int	    qos_reliability;	/* "reliability" element */
#define	HIGH_QUALITY	0
#define	LOW_QUALITY	1

				/* session QOS */
    int	    qos_sversion;	/* session version required */
    int	    qos_extended;	/* extended control */
    int	    qos_maxtime;	/* for SPM response during S-CONNECT */
};
#define	NULLQOS	((struct QOStype *) 0)

#endif
