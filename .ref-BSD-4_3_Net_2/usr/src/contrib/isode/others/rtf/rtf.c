/* rtf.c - RT-file transfer utility -- initiator */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/rtf/RCS/rtf.c,v 7.2 91/02/22 09:34:16 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/rtf/RCS/rtf.c,v 7.2 91/02/22 09:34:16 mrose Interim $
 *
 *
 * $Log:	rtf.c,v $
 * Revision 7.2  91/02/22  09:34:16  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/01  21:04:45  mrose
 * pepsy
 * 
 * Revision 7.0  89/11/23  22:10:45  mrose
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
#include "isoservent.h"

/*    DATA */

static char *myname = "rtf";

static char *myprovider = "rtsap";
static char *myentity = "file transfer";

static char *host = NULL;
static char *user = NULL;
static char *password = NULL;
static char *source = NULL;
static char *destination = NULL;
static int   turn = NOTOK;

static int   fd;
static int   nbytes;

int	downtrans (), uptrans ();


char   *getenv ();

/*    MAIN */

/* ARGSUSED */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    result,
	    sd;
    char   *file;
    register struct isoservent *is;
    struct SSAPaddr *sa;
    struct RtSAPaddr rtzs;
    register struct RtSAPaddr *rtz = &rtzs;
    struct RtSAPconnect rtcs;
    register struct RtSAPconnect   *rtc = &rtcs;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort   *rta = &rti -> rti_abort;
    PE	    pe;
    struct type_RTF_Request reqs;
    register struct type_RTF_Request *req = &reqs;

    arginit (argv);
    
    if (turn == RTS_INITIATOR) {
	if ((fd = open (source, O_RDONLY, 0x00)) == NOTOK)
	    adios (source, "unable to open");
	file = destination;
    }
    else
	file = source;

    if ((req -> user = str2qb (user, strlen (user), 1)) == NULL
	    || (req -> password = str2qb (password, strlen (password), 1))
		    == NULL
	    || (req -> file = str2qb (file, strlen (file), 1)) == NULL)
	adios (NULLCP, "out of memory");
    pe = NULLPE;
    if (encode_RTF_Request (&pe, 1, 0, NULLCP, req) == NOTOK)
	adios (NULLCP, "error encoding request: %s", PY_pepy);
    PLOGP (pgm_log,RTF_Request, pe, "Request", 0);

    if ((is = getisoserventbyname (myentity, myprovider)) == NULL)
	adios (NULLCP, "%s/%s: unknown provider/entity pair",
	       myentity, myprovider);
    rtz -> rta_port = is -> is_port;		/* yikes! */
    if ((is = getisoserventbyname ("rts", "ssap")) == NULL)
	adios (NULLCP, "ssap/rts: unknown entity");
    if ((sa = is2saddr (host, NULLCP, is)) == NULLSA)
	adios (NULLCP, "address translation failed");
    rtz -> rta_addr = *sa;	/* struct copy */

    fprintf (stderr, "%s...", host);
    (void) fflush (stderr);
    if (RtBeginRequest (rtz, RTS_TWA, turn, pe, rtc, rti) == NOTOK) {
	fprintf (stderr, "failed\n");
	rts_adios (rta, "RT-BEGIN.REQUEST");
    }

    pe_free (pe);
    qb_free (req -> user);
    qb_free (req -> password);
    qb_free (req -> file);

    if (rtc -> rtc_result != RTS_ACCEPT) {
	fprintf (stderr, "failed\n");
	adios (NULLCP, "association rejected: [%s]",
	       RtErrString (rtc -> rtc_result));
    }
    fprintf (stderr, "connected\n");

    sd = rtc -> rtc_sd;

    RTCFREE (rtc);

    if (turn == RTS_INITIATOR) {
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
    else {
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
			    break;
		    }
		    continue;
		    
		case RTI_TRANSFER:
		    {
#ifndef	lint
			register struct RtSAPtransfer *rtt =
							&rti -> rti_transfer;
#endif

			if (nbytes == 0)
			    advise (LLOG_NOTICE, NULLCP, "transfer complete");
			else
			    timer (nbytes);
			if (RtPTurnRequest (sd, 1, rti) == NOTOK)
			    rts_adios (rta, "RT-TURN-PLEASE.REQUEST");
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
		case RTI_FINISH:
		    adios (NULLCP, "unexpected indication type=%d",
			   rti -> rti_type);

		default:
		    adios (NULLCP, "unknown indication type=%d",
			   rti -> rti_type);
	    }
	    break;
	}
    }

    if (RtEndRequest (sd, rti) == NOTOK)
	rts_adios (rta, "RT-END.REQUEST");

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
	    return rtsaplose (rti, RTS_TRANSFER, source, "unable to fstat");
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
			if (unlink (destination) == NOTOK)
			    advise (LLOG_EXCEPTIONS, destination,
				    "unable to unlink");
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
		if (unlink (destination) == NOTOK)
		    advise (LLOG_EXCEPTIONS, destination, "unable to unlink");
	    }
	    break;

	default:
	    return rtsaplose (rti, RTS_TRANSFER, NULLCP,
			      "unknown uptrans type=0x%x", type);
    }

    return OK;
}

/*  */

static	arginit (vec)
char  **vec;
{
    register char *ap;
    char    prompt[BUFSIZ];

    if (myname = rindex (*vec, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *vec;
    if (strcmp (myname, "rtf") && strcmp (myname, "xrtf"))
	host = myname, myname = "rtf";

    isodetailor (myname, 1);

    ll_hdinit (pgm_log, myname);
    pgm_log -> ll_stat |= LLOGTTY;

    for (vec++; ap = *vec; vec++) {
	if (*ap == '-')
	    switch (*++ap) {
		case 'l':
		    if ((user = *++vec) == NULL || *user == '-')
			adios (NULLCP, "usage: %s -l username", myname);
		    continue;

		default:
		    adios (NULLCP, "-%s: unknown switch", ap);
	    }

	if (host == NULL)
	    host = ap;
	else
	    if (turn == NOTOK) {
		if (strcmp (ap, "get") == 0)
		    turn = RTS_RESPONDER;
		else
		    if (strcmp (ap, "put") == 0)
			turn = RTS_INITIATOR;
		    else
			goto usage;
	    }
	    else
		if (source == NULL)
		    source = ap;
		else
		    if (destination == NULL)
			destination = ap;
		else {
usage: ;
		    adios (NULLCP,
			   "usage: %s host [get|put] source destination",
			   myname);
		}
    }

    if (destination == NULL)
	goto usage;

    if (user == NULL && (user = getenv ("USER")) == NULL)
	user = getenv ("LOGNAME");
    if (strcmp (user, "anon") == 0)
	user = "ANON";
	
    if (password == NULL) {
	if (strcmp (user, "ANON")) {
	    (void) sprintf (prompt, "password (%s:%s): ", host, user);
	    password = getpassword (prompt);
	}
	else
	    password = user ? user : "";
    }
}
