/* smux.c - SMUX initiator library */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/smux.c,v 7.7 91/02/22 09:43:57 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/smux.c,v 7.7 91/02/22 09:43:57 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	smux.c,v $
 * Revision 7.7  91/02/22  09:43:57  mrose
 * Interim 6.8
 * 
 * Revision 7.6  91/01/12  11:43:21  mrose
 * stuff
 * 
 * Revision 7.5  91/01/07  12:40:52  mrose
 * update
 * 
 * Revision 7.4  90/10/29  18:38:43  mrose
 * updates
 * 
 * Revision 7.3  90/10/23  20:36:25  mrose
 * update
 * 
 * Revision 7.2  90/04/09  08:50:13  mrose
 * update
 * 
 * Revision 7.1  90/02/19  15:38:45  mrose
 * one more time
 * 
 * Revision 7.0  90/02/17  10:36:45  mrose
 * *** empty log message ***
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
#include <varargs.h>
#include "smux.h"
#include "tailor.h"


#include <errno.h>
#include "internet.h"
#ifdef	BSD42
#include <sys/ioctl.h>
#endif

/*    DATA */

integer	smux_errno;
char    smux_info[BUFSIZ];


static	int	sd = NOTOK;
static	PS	ps = NULLPS;

static	struct sockaddr_in in_socket;


static	int	smux_debug = 0;
static	PE	smux_pe = NULL;
static	struct type_SNMP_SMUX__PDUs *smux_pdu;
static	OID	smux_enterprise = NULL;
static struct type_SNMP_NetworkAddress *smux_addr = NULL;
static struct type_SNMP_TimeTicks *smux_stamp = NULL;

static	struct timeval my_boottime;


extern	int	errno;

/*    INIT */

int	smux_init (debug)
int	debug;
{
    int	    onoff;
    register struct sockaddr_in *isock = &in_socket;
    register struct hostent *hp;
    register struct servent *sp;
    static int inited = 0;

    if (!inited) {
	isodetailor ("smux", 0);

	inited = 1;
    }

    smux_debug = debug;
    if (smux_pe)
	pe_free (smux_pe), smux_pe = NULL;
    if (smux_pdu)
	free_SNMP_SMUX__PDUs (smux_pdu), smux_pdu = NULL;
    if (smux_enterprise)
	oid_free (smux_enterprise), smux_enterprise = NULL;
    if (smux_addr)
	free_SNMP_NetworkAddress (smux_addr), smux_addr = NULL;
    if (smux_stamp == NULL
	    && (smux_stamp = (struct type_SNMP_TimeTicks *)
				    calloc (1, sizeof *smux_stamp)) == NULL)
	return smuxlose (congestion, NULLCP, "out of memory");

    bzero ((char *) isock, sizeof *isock);
    if ((hp = gethostbystring ("127.0.0.1")) == NULL)
	return smuxlose (youLoseBig, NULLCP, "%s: unknown host", "127.0.0.1");
    isock -> sin_family = hp -> h_addrtype;
    isock -> sin_port = (sp = getservbyname ("smux", "tcp"))
						       ? sp -> s_port
						       : htons ((u_short) 199);
    inaddr_copy (hp, isock);

    if ((sd = start_tcp_client ((struct sockaddr_in *) NULL, 0)) == NOTOK)
	return smuxlose (systemError, "failed", "start_tcp_client");

    (void) ioctl (sd, FIONBIO, (onoff = 1, (char *) &onoff));

    if (join_tcp_server (sd, isock) == NOTOK)
	switch (errno) {
	    case EINPROGRESS:
	        return sd;

	    case EISCONN:
		break;

	    default:
		(void) smuxlose (systemError, "failed", "join_tcp_server");
		(void) close_tcp_socket (sd);
		return (sd = NOTOK);
	}

    if (smuxalloc () == NOTOK)
	return NOTOK;

    (void) gettimeofday (&my_boottime, (struct timezone *) 0);

    return sd;
}

/*  */

static int  smuxalloc ()
{
    int	    len;

    if ((ps = ps_alloc (fdx_open)) == NULLPS || fdx_setup (ps, sd) == NOTOK) {
	if (ps) {
	    ps_free (ps), ps = NULLPS;
	    (void) smuxlose (youLoseBig, NULLCP, "fdx_setup: %s",
			     ps_error (ps -> ps_errno));
	}
	else
	    (void) smuxlose (youLoseBig, NULLCP, "ps_alloc: failed");

you_lose: ;
	(void) close_tcp_socket (sd);
	return (sd = NOTOK);
    }

    if (getsockname (sd, (struct sockaddr *) &in_socket,
		     (len = sizeof in_socket, &len)) == NOTOK)
	bzero ((char *) &in_socket.sin_addr, 4);
    if ((smux_addr = str2qb ((char *) &in_socket.sin_addr, 4, 1)) == NULL) {
	(void) smuxlose (youLoseBig, NULLCP, "str2qb: failed");

	ps_free (ps), ps = NULLPS;
	goto you_lose;
    }

    return OK;
}

/*    SIMPLE OPEN */

int	smux_simple_open (identity, description, commname, commlen)
OID	identity;
char   *description;
char   *commname;
int	commlen;
{
    int	    result;
    struct type_SNMP_SMUX__PDUs pdu;
    register struct type_SNMP_SimpleOpen *simple;

    if (identity == NULL
	    || description == NULL
	    || (commname == NULL && commlen != 0))
	return smuxlose (parameterMissing, NULLCP, "missing parameter");

    if (sd == NOTOK)
	return smuxlose (invalidOperation, NULLCP, "SMUX not inited");
    if (ps == NULLPS) {
	fd_set	mask;
	register struct sockaddr_in *isock = &in_socket;

	FD_ZERO (&mask);
	FD_SET (sd, &mask);
	if (xselect (sd + 1, NULLFD, &mask, NULLFD, 0) < 1)
	    goto not_yet;

	if (join_tcp_server (sd, isock) == NOTOK)
	    switch (errno) {
	        case EINPROGRESS:
not_yet: ;
    		    return smuxlose (inProgress, NULLCP, NULLCP);

	    case EISCONN:
		break;

	    default:
		(void) smuxlose (systemError, "failed", "join_tcp_server");
		(void) close_tcp_socket (sd);
		return (sd = NOTOK);
	}

	if (smuxalloc () == NOTOK)
	    return NOTOK;
    }

    bzero ((char *) &pdu, sizeof pdu);

    if ((simple = (struct type_SNMP_SimpleOpen *) calloc (1, sizeof *simple))
	== NULL) {
no_mem: ;
        (void) smuxlose (congestion, NULLCP, "out of memory");
	if (simple)
	    free_SNMP_SimpleOpen (simple);

	ps_free (ps), ps = NULLPS;
	(void) close_tcp_socket (sd);
	return (sd = NOTOK);
    }
    pdu.offset = type_SNMP_SMUX__PDUs_simple;
    pdu.un.simple = simple;

    if ((smux_enterprise = oid_cpy (identity)) == NULL)
	goto no_mem;
	
    simple -> version = int_SNMP_version_version__1;
    if ((simple -> identity = oid_cpy (identity)) == NULL
	    || (simple -> description = str2qb (description,
						strlen (description),
						1)) == NULL
	    || (simple -> password = str2qb (commname, commlen, 1)) == NULL)
	goto no_mem;

    result = smuxsend (&pdu);

    free_SNMP_SimpleOpen (simple);

    return result;
}

/*  */

static int  smuxsend (pdu)
struct type_SNMP_SMUX__PDUs *pdu;
{
    int	    result;
    PE	    pe;

    pe = NULLPE;
    if (encode_SNMP_SMUX__PDUs (&pe, 1, 0, NULLCP, pdu) == NOTOK) {
	result = smuxlose (youLoseBig, NULLCP, "encode_SNMP_SMUX__PDUs: %s",
			   PY_pepy);
	goto out;
    }

#ifdef	DEBUG
    if (smux_debug)
	(void) print_SNMP_SMUX__PDUs (pe, 1, NULLIP, NULLVP,
				      (struct type_SNMP_SMUX__PDUs *) 0);
#endif

    if (pe2ps (ps, pe) == NOTOK) {
	result = smuxlose (youLoseBig, NULLCP, "pe2ps: %s",
			   ps_error (ps -> ps_errno));
	goto out;
    }

    result = OK;

out: ;
    if (pe)
	pe_free (pe);

    if (result == NOTOK) {
	ps_free (ps), ps = NULLPS;
	(void) close_tcp_socket (sd);
	return (sd = NOTOK);
    }

    return OK;
}

/*    CLOSE */

int	smux_close (reason)
int	reason;
{
    int	    result;
    struct type_SNMP_SMUX__PDUs pdu;
    register struct type_SNMP_ClosePDU *close;

    if (ps == NULLPS)
	return smuxlose (invalidOperation, NULLCP, "SMUX not opened");

    bzero ((char *) &pdu, sizeof pdu);

    if ((close = (struct type_SNMP_ClosePDU *) calloc (1, sizeof *close))
	    == NULL) {
        result = smuxlose (congestion, NULLCP, "out of memory");
	if (close)
	    free_SNMP_ClosePDU (close);

	ps_free (ps), ps = NULLPS;
	(void) close_tcp_socket (sd);
	return (sd = NOTOK);
    }
    pdu.offset = type_SNMP_SMUX__PDUs_close;
    pdu.un.close = close;

    close -> parm = reason;

    result = smuxsend (&pdu);

    free_SNMP_ClosePDU (close);

    ps_free (ps), ps = NULLPS;
    (void) close_tcp_socket (sd);
    sd = NOTOK;

    if (smux_pe)
	pe_free (smux_pe), smux_pe = NULL;
    if (smux_pdu)
	free_SNMP_SMUX__PDUs (smux_pdu), smux_pdu = NULL;
    if (smux_enterprise)
	oid_free (smux_enterprise), smux_enterprise = NULL;
    if (smux_addr)
	free_SNMP_NetworkAddress (smux_addr), smux_addr = NULL;

    return result;
}

/*    REGISTER */

int	smux_register (subtree, priority, operation)
OID	subtree;
int	priority,
    	operation;
{
    int	    result;
    struct type_SNMP_SMUX__PDUs pdu;
    register struct type_SNMP_RReqPDU *rreq;

    if (subtree == NULL)
	return smuxlose (parameterMissing, NULLCP, "missing parameter");

    if (ps == NULLPS)
	return smuxlose (invalidOperation, NULLCP, "SMUX not opened");

    bzero ((char *) &pdu, sizeof pdu);

    if ((rreq = (struct type_SNMP_RReqPDU *) calloc (1, sizeof *rreq))
	== NULL) {
no_mem: ;
        result = smuxlose (congestion, NULLCP, "out of memory");
	if (rreq)
	    free_SNMP_RReqPDU (rreq);

	ps_free (ps), ps = NULLPS;
	(void) close_tcp_socket (sd);
	return (sd = NOTOK);
    }
    pdu.offset = type_SNMP_SMUX__PDUs_registerRequest;
    pdu.un.registerRequest = rreq;

    if ((rreq -> subtree = oid_cpy (subtree)) == NULLOID)
	goto no_mem;
    rreq -> priority = priority;
    rreq -> operation = operation;

    result = smuxsend (&pdu);

    free_SNMP_RReqPDU (rreq);

    return result;
}

/*    WAIT */

int	smux_wait (event, secs)
struct type_SNMP_SMUX__PDUs **event;
int	secs;
{
    fd_set  mask;
    PE	    pe;

    if (event == NULL)
	return smuxlose (parameterMissing, NULLCP, "missing parameter");
	
    if (ps == NULLPS)
	return smuxlose (invalidOperation, NULLCP, "SMUX not opened");

    FD_ZERO (&mask);
    FD_SET (sd, &mask);
    if (ps_prime (ps, 1) == OK
	    && xselect (sd + 1, &mask, NULLFD, NULLFD, secs) <= OK) {
	errno = EWOULDBLOCK;
	return smuxlose (inProgress, NULLCP, NULLCP);
    }

    if ((pe = ps2pe (ps)) == NULLPE) {
	(void) smuxlose (youLoseBig, NULLCP, "pe2ps: %s",
			 ps_error (ps -> ps_errno));
	goto out;
    }

    if (decode_SNMP_SMUX__PDUs (pe, 1, NULLIP, NULLVP, event) == NOTOK) {
	(void) smuxlose (youLoseBig, NULLCP, "encode_SNMP_SMUX__PDUs: %s",
			 PY_pepy);
	goto out;
    }

#ifdef	DEBUG
    if (smux_debug)
	(void) print_SNMP_SMUX__PDUs (pe, 1, NULLIP, NULLVP,
				      (struct type_SNMP_SMUX__PDUs *) 0);
#endif

    if (smux_pe)
	pe_free (smux_pe);
    smux_pe = pe;
    if (smux_pdu)
	free_SNMP_SMUX__PDUs (smux_pdu);
    smux_pdu = *event;

    if (smux_pdu -> offset == type_SNMP_SMUX__PDUs_close) {
	ps_free (ps), ps = NULLPS;
	(void) close_tcp_socket (sd);
	sd = NOTOK;
    }
    return OK;

out: ;
    if (pe)
	pe_free (pe);

    ps_free (ps), ps = NULLPS;
    (void) close_tcp_socket (sd);
    return (sd = NOTOK);
}

/*    RESPONSE */

int	smux_response (event)
struct type_SNMP_GetResponse__PDU *event;
{
    struct type_SNMP_SMUX__PDUs pdu;

    if (event == NULL)
	return smuxlose (parameterMissing, NULLCP, "missing parameter");

    if (ps == NULLPS)
	return smuxlose (invalidOperation, NULLCP, "SMUX not opened");

    bzero ((char *) &pdu, sizeof pdu);

    pdu.offset = type_SNMP_SMUX__PDUs_get__response;
    pdu.un.get__response = event;

    return smuxsend (&pdu);
}

/*    TRAP */

int	smux_trap (generic, specific, bindings)
int	generic,
	specific;
struct type_SNMP_VarBindList *bindings;
{
    int	    result;
    struct timeval now;
    struct type_SNMP_SMUX__PDUs pdu;
    register struct type_SNMP_Trap__PDU *trap;

    if (ps == NULLPS)
	return smuxlose (invalidOperation, NULLCP, "SMUX not opened");

    bzero ((char *) &pdu, sizeof pdu);

    if ((trap = (struct type_SNMP_Trap__PDU *) calloc (1, sizeof *trap))
	    == NULL) {
        result = smuxlose (congestion, NULLCP, "out of memory");
	if (trap)
	    free_SNMP_Trap__PDU (trap);

	ps_free (ps), ps = NULLPS;
	(void) close_tcp_socket (sd);
	return (sd = NOTOK);
    }
    pdu.offset = type_SNMP_SMUX__PDUs_trap;
    pdu.un.trap = trap;

    trap -> enterprise = smux_enterprise;
    trap -> agent__addr = smux_addr;
    trap -> generic__trap = generic;
    trap -> specific__trap = specific;
    trap -> time__stamp = smux_stamp;
    (void) gettimeofday (&now, (struct timezone *) 0);
    trap -> time__stamp -> parm = (now.tv_sec - my_boottime.tv_sec) * 100
	    				+ ((now.tv_usec - my_boottime.tv_usec)
								      / 10000);
    trap -> variable__bindings = bindings;

    result = smuxsend (&pdu);

    trap -> enterprise = NULL;
    trap -> agent__addr = NULL;
    trap -> time__stamp = NULL;
    trap -> variable__bindings = NULL;

    free_SNMP_Trap__PDU (trap);

    return result;
}

/*    LOSE */

#ifndef	lint
static int  smuxlose (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    smux_errno = va_arg (ap, int);

    asprintf (smux_info, ap);

    va_end (ap);

    return NOTOK;
}
#else
/* VARARGS3 */

static int  smuxlose (reason, what, fmt)
int	reason;
char   *what,
       *fmt;
{
    return smuxlose (reason, what, fmt);
}
#endif

/*  */

static char *errors_up[] = {
    "goingDown",
    "unsupportedVersion",
    "packetFormat",
    "protocolError",
    "internalError",
    "authenticationFailure"
};

static char *errors_down[] = {
    "SMUX error 0",
    "invalidOperation",
    "parameterMissing",
    "systemError",
    "youLoseBig",
    "congestion",
    "inProgress"
};

char   *smux_error (i)
integer	i;
{
    int	    j;
    char  **ap;
    static char buffer[BUFSIZ];

    if (i < 0) {
	ap = errors_down, j = sizeof errors_down / sizeof errors_down[0];
	i = -i;
    }
    else
	ap = errors_up, j = sizeof errors_up / sizeof errors_up[0];
    if (0 <= i && i < j)
	return ap[i];

    (void) sprintf (buffer, "SMUX error %s%d", ap == errors_down ? "-" : "",i);

    return buffer;
}
