/* unixd.c - daemon for UNIX MIB */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/unixd.c,v 7.13 91/03/09 11:57:58 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/unixd.c,v 7.13 91/03/09 11:57:58 mrose Exp $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	unixd.c,v $
 * Revision 7.13  91/03/09  11:57:58  mrose
 * update
 * 
 * Revision 7.12  91/02/22  09:44:55  mrose
 * Interim 6.8
 * 
 * Revision 7.11  91/01/11  15:35:40  mrose
 * sets
 * 
 * Revision 7.10  91/01/08  12:48:55  mrose
 * update
 * 
 * Revision 7.9  90/12/18  10:14:22  mrose
 * update
 * 
 * Revision 7.8  90/10/17  11:57:41  mrose
 * sync
 * 
 * Revision 7.7  90/07/09  14:49:48  mrose
 * sync
 * 
 * Revision 7.6  90/03/06  13:51:01  mrose
 * jch
 * 
 * Revision 7.5  90/02/23  17:48:05  mrose
 * update
 * 
 * Revision 7.4  90/02/19  15:54:09  mrose
 * touch-up
 * 
 * Revision 7.3  90/02/19  15:39:08  mrose
 * one more time
 * 
 * Revision 7.2  90/02/17  17:19:01  mrose
 * touch-up
 * 
 * Revision 7.1  90/02/17  10:42:20  mrose
 * touch-up
 * 
 * Revision 7.0  90/02/17  10:36:48  mrose
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


#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "smux.h"
#include "objects.h"
#include <sys/ioctl.h>
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif
#include "tailor.h"

/*    DATA */

int	debug = 0;
static	int	nbits = FD_SETSIZE;

static LLog	_pgm_log = {
    "unixd.log", NULLCP, NULLCP, LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE,
    LLOG_FATAL, -1, LLOGCLS | LLOGCRT | LLOGZER, NOTOK
};
static	LLog   *pgm_log = &_pgm_log;

static	char   *myname = "unixd";


static	int	smux_fd = NOTOK;
static	int	rock_and_roll = 0;
static	int	dont_bother_anymore = 0;

static	OID	subtree = NULLOID;
static	struct smuxEntry *se = NULL;


static	fd_set	ifds;
static	fd_set	ofds;


void	adios (), advise ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    nfds;

    arginit (argv);
    envinit ();

    FD_ZERO (&ifds);
    FD_ZERO (&ofds);
    nfds = 0;

/* set fd's for other purposes here... */

    for (;;) {
	int	n,
		secs;
	fd_set	rfds,
		wfds;

	secs = NOTOK;

	rfds = ifds;	/* struct copy */
	wfds = ofds;	/*   .. */

	if (smux_fd == NOTOK && !dont_bother_anymore)
	    secs = 5 * 60L;
	else
	    if (rock_and_roll)
		FD_SET (smux_fd, &rfds);
	    else
		FD_SET (smux_fd, &wfds);
	if (smux_fd >= nfds)
	    nfds = smux_fd + 1;

	if ((n = xselect (nfds, &rfds, &wfds, NULLFD, secs)) == NOTOK)
	    adios ("failed", "xselect");

/* check fd's for other purposes here... */

	if (smux_fd == NOTOK && !dont_bother_anymore) {
	    if (n == 0) {
		if ((smux_fd = smux_init (debug)) == NOTOK)
		    advise (LLOG_EXCEPTIONS, NULLCP, "smux_init: %s [%s]",
			    smux_error (smux_errno), smux_info);
		else
		    rock_and_roll = 0;
	    }
	}
	else
	    if (rock_and_roll) {
		if (FD_ISSET (smux_fd, &rfds))
		    doit_smux ();
	    }
	    else
		if (FD_ISSET (smux_fd, &wfds))
		    start_smux ();
    }
}

/*    MISCELLANY */

static	arginit (vec)
char	**vec;
{
    register char  *ap;

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;
    if (strncmp (myname, "smux.", 5) == 0 && myname[5] != NULL)
	myname += 5;

    isodetailor (myname, 0);
    ll_hdinit (pgm_log, myname);

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'd':
		    debug++;
		    continue;

		default: 
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	adios (NULLCP, "usage: %s [switches]", myname);
    }


}

/*  */

static  envinit () {
    int     i,
            sd;
    char    file[BUFSIZ];
    FILE   *fp;

    nbits = getdtablesize ();

    if (debug == 0 && !(debug = isatty (2))) {
	for (i = 0; i < 5; i++) {
	    switch (fork ()) {
		case NOTOK: 
		    sleep (5);
		    continue;

		case OK: 
		    break;

		default: 
		    _exit (0);
	    }
	    break;
	}

	(void) chdir ("/");

	if ((sd = open ("/dev/null", O_RDWR)) == NOTOK)
	    adios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

#ifdef	SETSID
	if (setsid () == NOTOK)
	    advise (LLOG_EXCEPTIONS, "failed", "setsid");
#endif
#ifdef	TIOCNOTTY
	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULLCP);
	    (void) close (sd);
	}
#else
#ifdef	SYS5
	(void) setpgrp ();
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
    }
    else
	ll_dbinit (pgm_log, myname);

#ifndef	sun		/* damn YP... */
    for (sd = 3; sd < nbits; sd++)
	if (pgm_log -> ll_fd != sd)
	    (void) close (sd);
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (pgm_log, myname);

    mibinit ();

    (void) sprintf (file, "/etc/%s.pid", myname);
    if (fp = fopen (file, "w")) {
	(void) fprintf (fp, "%d\n", getpid ());
	(void) fclose (fp);
    }
    
    advise (LLOG_NOTICE, NULLCP, "starting");
}

/*    MIB */

#include <nlist.h>

static	int	kd;
static	int	quantum = 0;
static	int	lastq = -1;

static struct nlist nl[] = {
#define	N_MBSTAT	0
    { "_mbstat" },

    NULL
};


static  mibinit () {
    OT	    ot;
    register struct nlist *nz;

    if ((se = getsmuxEntrybyname ("unixd")) == NULL)
	adios (NULLCP, "no SMUX entry for \"%s\"", "unixd");

    if (readobjects ("unixd.defs") == NOTOK)
	adios (NULLCP, "readobjects: %s", PY_pepy);

    if ((ot = text2obj ("mbuf")) == NULL)
	adios (NULLCP, "text2obj (\"%s\") fails", "mbuf");
    subtree = ot -> ot_name;

    if (nlist ("/vmunix", nl) == NOTOK)
	adios ("/vmunix", "unable to nlist");
    for (nz = nl; nz -> n_name; nz++)
	if (nz -> n_value == 0)
	    advise (LLOG_EXCEPTIONS, NULLCP, "\"%s\" not in /vmunix (warning)",
		    nz -> n_name);

    if ((kd = open ("/dev/kmem", O_RDONLY)) == NOTOK)
	adios ("/vmunix", "unable to read");

    init_unix ();	    /* UNIX-specific enterprise */

    if ((smux_fd = smux_init (debug)) == NOTOK)
	advise (LLOG_EXCEPTIONS, NULLCP, "smux_init: %s [%s]",
		smux_error (smux_errno), smux_info);
    else
	rock_and_roll = 0;
}

/*  */

static	start_smux () {
    if (smux_simple_open (&se -> se_identity, "SMUX UNIX daemon",
			  se -> se_password, strlen (se -> se_password))
	    == NOTOK) {
	if (smux_errno == inProgress)
	    return;

	advise (LLOG_EXCEPTIONS, NULLCP, "smux_simple_open: %s [%s]",
		smux_error (smux_errno), smux_info);
losing: ;
	smux_fd = NOTOK;
	return;
    }
    advise (LLOG_NOTICE, NULLCP, "SMUX open: %s \"%s\"",
	    oid2ode (&se -> se_identity), se -> se_name);
    rock_and_roll = 1;

    if (smux_register (subtree, -1, readOnly) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "smux_register: %s [%s]",
		smux_error (smux_errno), smux_info);
	goto losing;
    }
    advise (LLOG_NOTICE, NULLCP, "SMUX register: readOnly %s in=%d",
	    oid2ode (subtree), -1);
}

/*  */

static	doit_smux () {
    struct type_SNMP_SMUX__PDUs *event;

    if (smux_wait (&event, NOTOK) == NOTOK) {
	if (smux_errno == inProgress)
	    return;

	advise (LLOG_EXCEPTIONS, NULLCP, "smux_wait: %s [%s]",
		smux_error (smux_errno), smux_info);
losing: ;
	smux_fd = NOTOK;
	return;
    }

    switch (event -> offset) {
	case type_SNMP_SMUX__PDUs_registerResponse:
	    {
		struct type_SNMP_RRspPDU *rsp = event -> un.registerResponse;

		if (rsp -> parm == int_SNMP_RRspPDU_failure) {
		    advise (LLOG_NOTICE, NULLCP,
			    "SMUX registration of %s failed",
			    oid2ode (subtree));
		    dont_bother_anymore = 1;
		    (void) smux_close (goingDown);
		    goto losing;
		}
		else
		    advise (LLOG_NOTICE, NULLCP,
			    "SMUX register: readOnly %s out=%d",
			    oid2ode (subtree), rsp -> parm);
	    }
	    if (smux_trap (int_SNMP_generic__trap_coldStart,
			   0, (struct type_SNMP_VarBindList *) 0) == NOTOK) {
		advise (LLOG_EXCEPTIONS, NULLCP, "smux_trap: %s [%s]",
			smux_error (smux_errno), smux_info);
		goto losing;
	    }
	    break;

	case type_SNMP_SMUX__PDUs_get__request:
	case type_SNMP_SMUX__PDUs_get__next__request:
	    get_smux (event -> un.get__request, event -> offset);
	    break;

	case type_SNMP_SMUX__PDUs_close:
	    advise (LLOG_NOTICE, NULLCP, "SMUX close: %s",
		    smux_error (event -> un.close -> parm));
	    goto losing;

	case type_SNMP_SMUX__PDUs_simple:
	case type_SNMP_SMUX__PDUs_registerRequest:
	case type_SNMP_SMUX__PDUs_get__response:
	case type_SNMP_SMUX__PDUs_trap:
	    advise (LLOG_EXCEPTIONS, NULLCP, "unexpectedOperation: %d",
		    event -> offset);
	    (void) smux_close (protocolError);
	    goto losing;

	case type_SNMP_SMUX__PDUs_set__request:
	case type_SNMP_SMUX__PDUs_commitOrRollback:
	    set_smux (event);
	    break;

	default:
	    advise (LLOG_EXCEPTIONS, NULLCP, "badOperation: %d",
		    event -> offset);
	    (void) smux_close (protocolError);
	    goto losing;
    }
}

/*  */

static	get_smux (pdu, offset)
register struct type_SNMP_GetRequest__PDU *pdu;
int	offset;
{
    int	    idx,
	    status;
    object_instance ois;
    register struct type_SNMP_VarBindList *vp;

    quantum = pdu -> request__id;
    idx = 0;
    for (vp = pdu -> variable__bindings; vp; vp = vp -> next) {
	register OI	oi;
	register OT	ot;
	register struct type_SNMP_VarBind *v = vp -> VarBind;

	idx++;

	if (offset == type_SNMP_SMUX__PDUs_get__next__request) {
	    if ((oi = name2inst (v -> name)) == NULLOI
		    && (oi = next2inst (v -> name)) == NULLOI)
		goto no_name;

	    if ((ot = oi -> oi_type) -> ot_getfnx == NULLIFP)
		goto get_next;
	}
	else
	    if ((oi = name2inst (v -> name)) == NULLOI
	            || (ot = oi -> oi_type) -> ot_getfnx == NULLIFP) {
no_name: ;
		pdu -> error__status = int_SNMP_error__status_noSuchName;
		goto out;
	    }

try_again: ;
	switch (ot -> ot_access) {
	    case OT_NONE:
	        if (offset == type_SNMP_SMUX__PDUs_get__next__request)
		    goto get_next;
	        goto no_name;

	    case OT_RDONLY:
		if (offset == type_SNMP_SMUX__PDUs_set__request) {
		    pdu -> error__status = int_SNMP_error__status_noSuchName;
		    goto out;
		}
		break;

	    case OT_RDWRITE:
		break;
	}
		
	switch (status = (*ot -> ot_getfnx) (oi, v, offset)) {
	    case NOTOK:	    /* get-next wants a bump */
get_next: ;
		oi = &ois;
		for (;;) {
		    if ((ot = ot -> ot_next) == NULLOT) {
			pdu -> error__status =
					    int_SNMP_error__status_noSuchName;
			goto out;
		    }
		    oi -> oi_name = (oi -> oi_type = ot) -> ot_name;
		    if (ot -> ot_getfnx)
			goto try_again;
		}

	    case int_SNMP_error__status_noError:
		break;

	    default:
		pdu -> error__status = status;
		goto out;
	}
    }
    idx = 0;

out: ;
    pdu -> error__index = idx;

    if (smux_response (pdu) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP, "smux_response: %s [%s]",
		smux_error (smux_errno), smux_info);
	smux_fd = NOTOK;
    }
}

/*  */

static	set_smux (event)
struct type_SNMP_SMUX__PDUs *event;
{
    switch (event -> offset) {
	case type_SNMP_SMUX__PDUs_set__request:
	    {
		register struct type_SNMP_GetRequest__PDU *pdu =
						    event -> un.get__response;

		pdu -> error__status = int_SNMP_error__status_noSuchName;
		pdu -> error__index = pdu -> variable__bindings ? 1 : 0;

		if (smux_response (pdu) == NOTOK) {
		    advise (LLOG_EXCEPTIONS, NULLCP, "smux_response: %s [%s]",
			    smux_error (smux_errno), smux_info);
		    smux_fd = NOTOK;
		}
	    }
	    break;

	case type_SNMP_SMUX__PDUs_commitOrRollback:
	    {
		struct type_SNMP_SOutPDU *cor = event -> un.commitOrRollback;

		if (cor -> parm == int_SNMP_SOutPDU_commit) {
					/* "should not happen" */
		    (void) smux_close (protocolError);
		    smux_fd = NOTOK;
		}
	    }
	    break;
    }
}

/*    UNIX */

#ifdef	BSD44
#include <sys/param.h>
#endif
#include <sys/mbuf.h>

/*  */

static	struct mbstat mbstat;

/*  */

#define	mbufS		0
#define	mbufClusters	1
#define	mbufFreeClusters 2
#define	mbufDrops	3
#ifdef	BSD44
#define	mbufWaits	4
#define	mbufDrains	5
#endif
#if	!defined(BSD43) && !defined(BSD44)
#define	mbufFrees	6
#endif


static int  o_mbuf (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifvar;
    register struct mbstat *m = &mbstat;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_SMUX__PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1
		    || oid -> oid_elements[oid -> oid_nelem - 1] != 0)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_SMUX__PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((new = oid_extend (oid, 1)) == NULLOID)
		    return NOTOK;
		new -> oid_elements[new -> oid_nelem - 1] = 0;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else
		return NOTOK;
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    if (quantum != lastq) {
	lastq = quantum;

	if (getkmem (nl + N_MBSTAT, (caddr_t) m, sizeof *m) == NOTOK)
	    return (offset == type_SNMP_PDUs_get__next__request ? NOTOK
					: int_SNMP_error__status_genErr);
    }

    switch (ifvar) {
	case mbufS:
	    return o_integer (oi, v, m -> m_mbufs);

	case mbufClusters:
	    return o_integer (oi, v, m -> m_clusters);

	case mbufFreeClusters:
	    return o_integer (oi, v, m -> m_clfree);

	case mbufDrops:
	    return o_integer (oi, v, m -> m_drops);

#ifdef	mbufWaits
	case mbufWaits:
	    return o_integer (oi, v, m -> m_wait);
#endif

#ifdef	mbufDrains
	case mbufDrains:
	    return o_integer (oi, v, m -> m_drain);
#endif

#ifdef	mbufFrees
	case mbufFrees:
	    return o_integer (oi, v, m -> m_mbfree);
#endif

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

#define	mbufType	0
#define	mbufAllocates	1


static int  o_mbufType (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    int	    ifnum,
	    ifvar;
    register struct mbstat *m = &mbstat;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;

    ifvar = (int) ot -> ot_info;
    switch (offset) {
	case type_SNMP_SMUX__PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1)
		return int_SNMP_error__status_noSuchName;
	    if ((ifnum = oid -> oid_elements[oid -> oid_nelem - 1])
		    >= sizeof m -> m_mtypes / sizeof m -> m_mtypes[0])
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_SMUX__PDUs_get__next__request:
again: ;
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		ifnum = 0;

		if ((new = oid_extend (oid, 1)) == NULLOID)
		    return NOTOK;
		new -> oid_elements[new -> oid_nelem - 1] = ifnum;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;

		oid = new;	/* for hack... */
	    }
	    else {
		int	i = ot -> ot_name -> oid_nelem;

		if ((ifnum = oid -> oid_elements[i] + 1)
		        >= sizeof m -> m_mtypes / sizeof m -> m_mtypes[0])
		    return NOTOK;

		oid -> oid_elements[i] = ifnum;
		oid -> oid_nelem = i + 1;
	    }
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    if (quantum != lastq) {
	lastq = quantum;

	if (getkmem (nl + N_MBSTAT, (caddr_t) m, sizeof *m) == NOTOK)
	    return (offset == type_SNMP_PDUs_get__next__request ? NOTOK
					: int_SNMP_error__status_genErr);
    }

/* hack to compress table size... */
    if (offset == type_SNMP_SMUX__PDUs_get__next__request
	    && m -> m_mtypes[ifnum] == 0)
	goto again;

    switch (ifvar) {
	case mbufType:
	    return o_integer (oi, v, ifnum);

	case mbufAllocates:
	    return o_integer (oi, v, m -> m_mtypes[ifnum]);

	default:
	    return int_SNMP_error__status_noSuchName;
    }
}

/*  */

init_unix () {
    register OT	    ot;

    if (ot = text2obj ("mbufS"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufS;
    if (ot = text2obj ("mbufClusters"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufClusters;
    if (ot = text2obj ("mbufFreeClusters"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufFreeClusters;
    if (ot = text2obj ("mbufDrops"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufDrops;
#ifdef	mbufWaits
    if (ot = text2obj ("mbufWaits"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufWaits;
#endif
#ifdef	mbufDrains
    if (ot = text2obj ("mbufDrains"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufDrains;
#endif
#ifdef	mbufFrees
    if (ot = text2obj ("mbufFrees"))
	ot -> ot_getfnx = o_mbuf,
	ot -> ot_info = (caddr_t) mbufFrees;
#endif
    if (ot = text2obj ("mbufType"))
	ot -> ot_getfnx = o_mbufType,
	ot -> ot_info = (caddr_t) mbufType;
    if (ot = text2obj ("mbufAllocates"))
	ot -> ot_getfnx = o_mbufType,
	ot -> ot_info = (caddr_t) mbufAllocates;
}

/*  */

int	getkmem (n, buffer, cc)
struct nlist *n;
caddr_t	buffer;
int	cc;
{
    if (n -> n_value == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP, "\"%s\" not in /vmunix", n -> n_name);
	return NOTOK;
    }
    if (lseek (kd, (long) n -> n_value, L_SET) == NOTOK) {
	advise (LLOG_EXCEPTIONS, "failed", "lseek of 0x%x for \"%s\" in kmem",
		(long) n -> n_value, n -> n_name);
	return NOTOK;
    }
    if (read (kd, buffer, cc) != cc) {
	advise (LLOG_EXCEPTIONS, "failed", "read of \"%s\" from kmem",
		n -> n_name);
	return NOTOK;
    }

    return OK;
}

/*    ERRORS */

#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);
    
    _ll_log (pgm_log, LLOG_FATAL, ap);

    va_end (ap);

    _exit (1);
}
#else
/* VARARGS */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code;
    va_list ap;

    va_start (ap);
    
    code = va_arg (ap, int);

    _ll_log (pgm_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	advise (code, what, fmt)
char   *what,
       *fmt;
int	code;
{
    advise (code, what, fmt);
}
#endif
