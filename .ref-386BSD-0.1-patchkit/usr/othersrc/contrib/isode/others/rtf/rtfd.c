/* rtfd.c - RT-file transfer utility -- responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/rtf/RCS/rtfd.c,v 7.2 91/02/22 09:34:20 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/rtf/RCS/rtfd.c,v 7.2 91/02/22 09:34:20 mrose Interim $
 *
 *
 * $Log:	rtfd.c,v $
 * Revision 7.2  91/02/22  09:34:20  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:48  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:10:49  mrose
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


#include "RTF-types.h"
#include "rtf.h"
#include <pwd.h>


#ifdef	SYS5
struct passwd *getpwnam ();
#endif

#ifndef	ANON
#define	ANON	"ftp"
#endif

/*    DATA */

static int debug = 0;

static char *myname = "rtfd";


static int    fd;
static int    nbytes;
static char  *destination;

int	downtrans (), uptrans ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    guest,
	    sd,
	    result,
	    turn;
    register char *cp,
		  *user;
    register struct passwd *pw;
    struct RtSAPstart   rtss;
    register struct RtSAPstart *rts = &rtss;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort   *rta = &rti -> rti_abort;
    struct type_RTF_Request *req;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    isodetailor (myname, 0);
    if (debug = isatty (fileno (stderr)))
	ll_dbinit (pgm_log, myname);
    else
	ll_hdinit (pgm_log, myname);

    advise (LLOG_NOTICE, NULLCP, "starting");

    if (RtBInit (argc, argv, rts, rti) == NOTOK)
	rts_adios (rta, "(RtB)initialization fails");
    advise (LLOG_NOTICE, NULLCP,
	    "RT-BEGIN.INDICATION: <%d, %s, %s, <%d, %s>, 0x%x>",
	    rts -> rts_sd, rts -> rts_mode == RTS_TWA ? "twa" : "monologue",
	    rts -> rts_turn == RTS_RESPONDER ? "responder" : "initiator",
	    ntohs (rts -> rts_port),
	    saddr2str (&rts -> rts_initiator.rta_addr),
	    rts -> rts_data);

    sd = rts -> rts_sd;

    if (rts -> rts_data == NULLPE) {
	advise (LLOG_EXCEPTIONS, NULLCP, "rejected -- no user-data parameter");
reject: ;
	if (RtBeginResponse (sd, RTS_REJECT, NULLPE, rti) == NOTOK)
	    rts_adios (rta, "RT-BEGIN.RESPONSE (reject)");
	exit (1);
    }

    req = NULL;
    if (decode_RTF_Request (rts -> rts_data, 1, NULLIP, NULLVP, &req)
	    == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"rejected -- error decoding request: %s", PY_pepy);
	goto reject;
    }
    PLOGP (pgm_log,RTF_Request, rts -> rts_data, "Request", 1);

    if (qb_pullup (req -> user) == NOTOK) {
no_mem: ;
        advise (LLOG_EXCEPTIONS, NULLCP, "rejected -- out of memory");
	goto reject;
    }
    if (qb_pullup (req -> password) == NOTOK)
	goto no_mem;
    if ((cp = qb2str (req -> file)) == NULL)
	goto no_mem;

    guest = 0;
    advise (LLOG_NOTICE, NULLCP, "%s: %s \"%s\"",
	    user = req -> user -> qb_forw -> qb_data,
	    (turn = rts -> rts_turn) == RTS_RESPONDER ? "get" : "put", cp);

    if (strcmp (cp, "ANON") == 0 || strcmp (user, ANON) == 0) {
	if ((pw = getpwnam (ANON)) && pw -> pw_uid == 0)
	    pw = NULL;
	guest = 1;
    }
    else
	pw = baduser (NULLCP, user) ? NULL : getpwnam (user);
    if (pw == NULL) {
	advise (LLOG_EXCEPTIONS, NULLCP, "rejected -- no such user");
no_validate: ;
	if (RtBeginResponse (sd, RTS_VALIDATE, NULLPE, rti) == NOTOK)
	    rts_adios (rta, "RT-BEGIN.RESPONSE (validate)");
	exit (1);
    }
    if (*pw -> pw_passwd == NULL
	    || (!guest && strcmp (crypt (req -> password -> qb_forw -> qb_data,
					 pw -> pw_passwd), pw -> pw_passwd))) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"authentication failure for \"%s\"%s requesting %s of \"%s\"",
		user, guest ? " (guest)" : "",
		(turn = rts -> rts_turn) == RTS_RESPONDER ? "get" : "put",
		cp);
	goto no_validate;
    }
    
    if (chdir (pw -> pw_dir) == NOTOK) {
	advise (LLOG_EXCEPTIONS, pw -> pw_dir,
		"unable to change directory to");
no_dice: ;
        if (RtBeginResponse (sd, RTS_BUSY, NULLPE, rti) == NOTOK)
	    rts_adios (rta, "RT-BEGIN.RESPONSE (busy)");
	exit (1);
    }
    if (guest && chroot (pw -> pw_dir) == NOTOK) {
	advise (LLOG_EXCEPTIONS, pw -> pw_dir,
		"unable to change root to");
	goto no_dice;
    }

    (void) setgid (pw -> pw_gid);
#ifndef	SYS5
    (void) initgroups (pw -> pw_name, pw -> pw_gid);
#endif
    (void) setuid (pw -> pw_uid);

    (void) umask (0022);

    if (turn == RTS_RESPONDER) {
	if ((fd = open (cp, O_RDONLY, 0x00)) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, cp, "rejected -- unable to open");
	    goto reject;
	}
	free (cp);
    }
    else
	destination = cp;

    free_RTF_Request (req);

    RTSFREE (rts);

    if (RtBeginResponse (sd, RTS_ACCEPT, NULLPE, rti) == NOTOK)
	rts_adios (rta, "RT-BEGIN.RESPONSE (accept)");

    if (turn == RTS_RESPONDER) {
	if (RtSetDownTrans (sd, downtrans, rti) == NOTOK)
	    rts_adios (rta, "set DownTrans upcall");

	if (RtTransferRequest (sd, NULLPE, NOTOK, rti) == NOTOK)
	    rts_adios (rta, "RT-TRANSFER.REQUEST");

	if (nbytes == 0)
	    advise (LLOG_NOTICE, NULLCP, "transfer complete");
	else
	    timer (nbytes);

	(void) close (fd);
    }
    else
	if (RtSetUpTrans (sd, uptrans, rti) == NOTOK)
	    rts_adios (rta, "set UpTrans upcall");

    for (;;) {
	switch (result = RtWaitRequest (sd, NOTOK, rti)) {
	    case NOTOK:
	    case OK:
	    case DONE:
	        break;

	    default:
		adios (NULLCP, "unknown return from RtWaitRequest=%d",
		       result);
	}

	switch (rti -> rti_type) {
	    case RTI_TURN:
	        {
		    register struct RtSAPturn *rtu = &rti -> rti_turn;

		    if (rtu -> rtu_please) {
			if (RtGTurnRequest (sd, rti) == NOTOK)
			    rts_adios (rta, "RT-TURN-GIVE.REQUEST");
		    }
		    else
			adios (NULLCP, "protocol screw-up");
		}
		continue;

	    case RTI_TRANSFER:
		{
#ifndef	lint
		    register struct RtSAPtransfer *rtt = &rti -> rti_transfer;
#endif

		    if (nbytes == 0)
			advise (LLOG_NOTICE, NULLCP, "transfer complete");
		    else
			timer (nbytes);
		}
		continue;

	    case RTI_ABORT:
		{
		    register struct RtSAPabort *rtb = &rti -> rti_abort;

		    if (rtb -> rta_peer)
			rts_adios (rtb, "RT-U-ABORT.INDICATION");
		    if (RTS_FATAL (rtb -> rta_reason))
			rts_adios (rtb, "RT-P-ABORT.INDICATION");
		    rts_advise (rtb, "RT-P-ABORT.INDICATION");
		}
		break;

	    case RTI_CLOSE:
		{
#ifndef	lint
		    register struct RtSAPclose *rtc = &rti -> rti_close;
#endif

		    advise (LLOG_NOTICE, NULLCP, "RT-END.INDICATION");
		    if (RtEndResponse (sd, rti) == NOTOK)
			rts_adios (rta, "RT-END.RESPONSE");
		}
		break;

	    case RTI_FINISH:
		adios (NULLCP, "unexpected indication type=%d",
		       rti -> rti_type);

	    default:
		adios (NULLCP, "unknown indication type=%d", rti -> rti_type);
	    }
	break;
    }

    exit (0);
}

/*     TRANSFER */

/* ARGSUSED */

static int  downtrans (sd, base, len, size, ssn, ack, rti)
int	sd;
char  **base;
int    *len,
	size;
long	ssn,
	ack;
struct RtSAPindication *rti;
{
    register int    cc;
    int	    n;
    register char *dp,
		  *ep;
    static int bsize;
    static char *bp = NULL;

    if (base == NULLVP) {
#ifdef	DEBUG
	advise (LLOG_DEBUG, NULLCP, "RT-PLEASE.INDICATION: %d", size);
#endif
	return OK;
    }

    if (bp == NULL) {
	struct stat st;

	if (fstat (fd, &st) == NOTOK)
	    return rtsaplose (rti, RTS_TRANSFER, destination,
			      "unable to fstat");
#ifdef	MAXBSIZE
	bsize = st.st_blksize > 0 ? st.st_blksize : BUFSIZ;
#else
	bsize = BUFSIZ;
#endif
	if (size == 0)	/* no checkpointing... */
	    n = st.st_size;
	else
	    if ((n = bsize) > size)
		n = size;
	if ((bp = malloc ((unsigned) n)) == NULL)
	    return rtsaplose (rti, RTS_CONGEST, NULLCP, "out of memory");
#ifdef	DEBUG
	advise (LLOG_DEBUG, NULLCP, "Selecting block size of %d", n);
	advise (LLOG_DEBUG, NULLCP,
		"  based on blksize of %d and RTTR size of %d",
		bsize, size);
#endif
	bsize = n;
	timer (nbytes = 0);
    }

    *base = NULLCP, *len = 0;
    for (ep = (dp = bp) + (cc = bsize); dp < ep; dp += n, cc -= n) {
	switch (n = read (fd, dp, cc)) {
	    case NOTOK:
	        return rtsaplose (rti, RTS_TRANSFER, "failed", "read");

	    default:
		continue;

	    case OK:
		break;
	}
	break;
    }
    if ((cc = dp - bp) > 0) {
	*base = bp, *len = cc;
	nbytes += cc;
    }

    return OK;
}

/*  */

/* ARGSUSED */

static int  uptrans (sd, type, addr, rti)
int	sd;
int	type;
caddr_t	addr;
struct RtSAPindication *rti;
{
    switch (type) {
	case SI_DATA:
	    {
		register struct qbuf *qb = (struct qbuf *) addr;
		register struct qbuf *qp;

		for (qp = qb -> qb_forw; qp != qb; qp = qp -> qb_forw)
		    if (write (fd, qp -> qb_data, qp -> qb_len) !=qp -> qb_len)
			return rtsaplose (rti, RTS_TRANSFER, "failed","write");
		    else
			nbytes += qp -> qb_len;
	    }
	    break;

	case SI_SYNC:
	    {
#ifdef	DEBUG
		register struct SSAPsync *sn = (struct SSAPsync *) addr;

		advise (LLOG_DEBUG, NULLCP, "S-MINOR-SYNC.INDICATION: %ld",
			sn -> sn_ssn);
#endif
	    }
	    break;

	case SI_ACTIVITY:
	    {
		register struct SSAPactivity *sv = (struct SSAPactivity *)addr;

		switch (sv -> sv_type) {
		    case SV_START:
#ifdef	DEBUG
			advise (LLOG_DEBUG, NULLCP,
				"S-ACTIVITY-START.INDICATION");
#endif
		        if ((fd = creat (destination, 0666)) == NOTOK) {
			    advise (LLOG_EXCEPTIONS, destination,
				    "unable to create");
			    return rtsaplose (rti, RTS_TRANSFER, destination,
					      "unable to create");
			}
			timer (nbytes = 0);
		        break;

		    case SV_INTRIND:
		    case SV_DISCIND:
			advise (LLOG_EXCEPTIONS, NULLCP,
				"activity %s: %s",
				sv -> sv_type == SV_INTRIND ? "interrupted"
							    : "discarded",
				SReportString (sv -> sv_reason));
			remove (destination);
			break;

		    case SV_ENDIND:
#ifdef	DEBUG
			advise (LLOG_DEBUG, NULLCP,
				"S-ACTIVITY-END.INDICATION");
#endif
			if (close (fd) == NOTOK)
			    return rtsaplose (rti, RTS_TRANSFER, destination,
					      "close failed on");
			break;

		    default:
			return rtsaplose (rti, RTS_TRANSFER, NULLCP,
				       "unexpected activity indication=0x%x",
				       sv -> sv_type);
		}
	    }
	    break;

	case SI_REPORT:
	    {
		register struct SSAPreport *sp = (struct SSAPreport *) addr;

		if (!sp -> sp_peer)
		    return rtsaplose (rti, RTS_TRANSFER, NULLCP,
			     "unexpected provider-initiated exception report");
		advise (LLOG_EXCEPTIONS, NULLCP,
			"exception: %s", SReportString (sp -> sp_reason));
		remove (destination);
	    }
	    break;

	default:
	    return rtsaplose (rti, RTS_TRANSFER, NULLCP,
			      "unknown uptrans type=0x%x", type);
    }

    return OK;
}

/*  */

static	remove (file)
char   *file;
{
    struct stat st;

    if (stat (file, &st) != NOTOK
	    && (st.st_mode & S_IFMT) == S_IFREG
	    && unlink (file) == NOTOK)
	advise (LLOG_EXCEPTIONS, file, "unable to unlink");
}
