/* post.c - enter messages into the transport system */
#ifndef	lint
static char ident[] = "@(#)$Id: post.c,v 2.23 1993/08/25 17:26:51 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/aliasbr.h"
#include "../h/dropsbr.h"
#include "../zotnet/tws.h"
#ifndef	MMDFMTS
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#else	/* MMDFMTS */
#include "../mts/mmdf/util.h"
#include "../mts/mmdf/mmdf.h"
#endif	/* MMDFMTS */
#include "../zotnet/mts.h"
#ifdef	MHMTS
#ifndef	V7
#include <sys/ioctl.h>
#endif	/* not V7 */
#include <sys/stat.h>
#endif	/* MHMTS */
#ifdef	SENDMTS
#include "../mts/sendmail/smail.h"
#undef	MF
#endif	/* SENDMTS */
#include <signal.h>
#ifdef	MIME
#include "../h/mhn.h"
#endif	/* MIME */
#ifdef LOCALE
#include <locale.h>
#endif


#ifndef	MMDFMTS
#define	uptolow(c)	((isalpha(c) && isupper (c)) ? tolower (c) : (c))
#endif	/* not MMDFMTS */

#define FCCS		10	/* max number of fccs allowed */

/*  */

#ifndef	MIME
#define	MIMEminc(a)	(a)
#else
#define	MIMEminc(a)	0
#endif

#ifndef	TMA
#define	TMAminc(a)	(a)
#else	/* TMA */
#define	TMAminc(a)	0
#endif	/* TMA */

static struct swit switches[] = {
#define	ALIASW	0
    "alias aliasfile", 0,

#define	CHKSW	1
    "check", -5,		/* interface from whom */
#define	NCHKSW	2
    "nocheck", -7,		/* interface from whom */

#define	DEBUGSW	3
    "debug", -5,

#define	DISTSW	4
    "dist", -4,			/* interface from dist */

#define	ENCRSW	5
    "encrypt", TMAminc (-7),
#define	NENCRSW	6
    "noencrypt", TMAminc (-9),

#define	FILTSW	7
    "filter filterfile", 0,
#define	NFILTSW	8
    "nofilter", 0,

#define	FRMTSW	9
    "format", 0,
#define	NFRMTSW	10
    "noformat", 0,

#define	LIBSW	11		/* interface from send, whom */
    "library directory", -7,

#define	MIMESW	12
    "mime", MIMEminc(-4),
#define	NMIMESW	13
    "nomime", MIMEminc(-6),

#define	MSGDSW	14
    "msgid", 0,
#define	NMSGDSW	15
    "nomsgid", 0,

#define	VERBSW	16
    "verbose", 0,
#define	NVERBSW	17
    "noverbose", 0,

#define	WATCSW	18
    "watch", 0,
#define	NWATCSW	19
    "nowatch", 0,

#define	WHOMSW	20	/* interface from whom */
    "whom", -4,		

#define	WIDTHSW	21
    "width columns", 0,

#define	HELPSW	22
    "help", 4,

#define	MAILSW	23
    "mail", -4,
#define	SAMLSW	24
    "saml", -4,
#define	SENDSW	25
    "send", -4,
#define	SOMLSW	26
    "soml", -4,

#define	ANNOSW	27		/* interface from send */
    "idanno number", -6,

#define	DLVRSW	28
    "deliver address-list", -7,

#define	CLIESW	29
    "client host", -6,
#define	SERVSW	30
    "server host", -6,
#define	SNOOPSW	31
    "snoop", -5,

#define	FILLSW	32
    "fill-in file", -7,
#define	FILLUSW	33
    "fill-up", -7,
#define	PARTSW	34
    "partno", -6,

#define	QUEUESW 35
    "queued", -6,

    NULL, 0
};

/*  */

struct headers {
    char   *value;

    unsigned int    flags;
#define	HNOP	0x0000		/* just used to keep .set around */
#define	HBAD	0x0001		/* bad header - don't let it through */
#define	HADR	0x0002		/* header has an address field */
#define	HSUB	0x0004		/* Subject: header */
#define	HTRY	0x0008		/* try to send to addrs on header */
#define	HBCC	0x0010		/* don't output this header */
#define	HMNG	0x0020		/* munge this header */
#define	HNGR	0x0040		/* no groups allowed in this header */
#define	HFCC	0x0080		/* FCC: type header */
#define	HNIL	0x0100		/* okay for this header not to have addrs */
#define	HIGN	0x0200		/* ignore this header */
#define	HDCC	0x0400		/* another undocumented feature */

    unsigned int    set;
#define	MFRM	0x0001		/* we've seen a From: */
#define	MDAT	0x0002		/* we've seen a Date: */
#define	MRFM	0x0004		/* we've seen a Resent-From: */
#define	MVIS	0x0008		/* we've seen sighted addrs */
#define	MINV	0x0010		/* we've seen blind addrs */
};

/*  */

static struct headers  NHeaders[] = {
    "Return-Path", HBAD, 0,
    "Received", HBAD, 0,
    "Reply-To", HADR | HNGR, 0,
    "From", HADR | HNGR, MFRM,
    "Sender", HADR | HBAD, 0,
    "Date", HBAD, 0,
    "Subject", HSUB, 0,
    "To", HADR | HTRY, MVIS,
    "cc", HADR | HTRY, MVIS,
    "Bcc", HADR | HTRY | HBCC | HNIL, MINV,
    "Dcc", HADR | HTRY | HDCC | HNIL, MVIS,	/* sorta cc & bcc combined */
    "Message-ID", HBAD, 0,
    "Fcc", HFCC, 0,

    NULL
};

static struct headers  RHeaders[] = {
    "Resent-Reply-To", HADR | HNGR, 0,
    "Resent-From", HADR | HNGR, MRFM,
    "Resent-Sender", HADR | HBAD, 0,
    "Resent-Date", HBAD, 0,
    "Resent-Subject", HSUB, 0,
    "Resent-To", HADR | HTRY, MVIS,
    "Resent-cc", HADR | HTRY, MVIS,
    "Resent-Bcc", HADR | HTRY | HBCC, MINV,
    "Resent-Message-ID", HBAD, 0,
    "Resent-Fcc", HFCC, 0,
    "Reply-To", HADR, 0,
    "From", HADR | HNGR, MFRM,
#ifdef	MMDFI
    "Sender", HADR | HMNG | HNGR, 0,
#else	/* not MMFDI */
    "Sender", HADR | HNGR, 0,
#endif	/* not MMDFI */
    "Date", HNOP, MDAT,
    "To", HADR | HNIL, 0,
    "cc", HADR | HNIL, 0,
    "Bcc", HADR | HTRY | HBCC | HNIL, 0,
    "Fcc", HIGN, 0,

    NULL
};

/*  */


static short    fccind = 0;	/* index into fccfold[] */
static short    outputlinelen = OUTPUTLINELEN;

static int  pfd = NOTOK;	/* fd to write annotation list to */
static int  myuid= -1;		/* my user id */
static int  mygid= -1;		/* my group id */
static int  recipients = 0;	/* how many people will get a copy */
static int  unkadr = 0;		/* how many of those were unknown */
static int  badadr = 0;		/* number of bad addrs */
static int  badmsg = 0;		/* message has bad semantics */
static int  verbose = 0;	/* spell it out */
static int  format = 1;		/* format addresses */
static int  mime = 0;		/* use MIME-style encapsulations */
static int  msgid = 0;		/* add msgid */
static int  debug = 0;		/* debugging post */
static int  watch = 0;		/* watch the delivery process */
static int  whomsw = 0;		/* we are whom not post */
static int  checksw = 0;	/* whom -check */
static int  linepos=0;		/* putadr()'s position on the line */
static int  nameoutput=0;	/* putadr() has output header name */

static unsigned msgflags = 0;	/* what we've seen */

#define	NORMAL 0
#define	RESENT 1
static int msgstate = NORMAL;

static long tclock = 0L;		/* the time we started (more or less) */

static TYPESIG  (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();

static char tmpfil[BUFSIZ];
static char bccfil[BUFSIZ];

static char from[BUFSIZ];	/* my network address */
static char signature[BUFSIZ];	/* my signature */
static char *filter = NULL;	/* the filter for BCC'ing */
static char *subject = NULL;	/* the subject field for BCC'ing */
static char *fccfold[FCCS];	/* foldernames for FCC'ing */

static struct headers  *hdrtab;	/* table for the message we're doing */

static struct mailname localaddrs={NULL};	/* local addrs */
static struct mailname netaddrs={NULL};		/* network addrs */
static struct mailname uuaddrs={NULL};		/* uucp addrs */
static struct mailname tmpaddrs={NULL};		/* temporary queue */

/*  */

#ifdef	MMDFMTS
static char *submitmode = "m";	/* deliver to mailbox only */
static char submitopts[6] = "vl";/* initial options for submit */
#endif	/* MMDFMTS */

#ifdef	MHMTS
static char *deliver = NULL;

extern char **environ;

TYPESIG	sigser ();
#endif	/* MHMTS */

#ifdef	SENDMTS
static int smtpmode = S_MAIL;
static int snoop = 0;
static char *clientsw = NULL;
static char *serversw = NULL;

extern struct smtp  sm_reply;
#endif	/* SENDMTS */

#ifdef	TMA
#define	post(a,b,c) \
    if (encryptsw) postcipher ((a), (b), (c)); else postplain ((a), (b), (c))

#ifndef	SENDMTS
#define	tmasnoop	0
#else	/* SENDMTS */
#define	tmasnoop	snoop
#endif	/* SENDMTS */
#endif	/* TMA */

static int  encryptsw = 0;	/* encrypt it */


#ifdef	BERK
#undef	WP
#endif

#ifdef MIME
static char	prefix[] = "----- =_aaaaaaaaaa";
static int      find_prefix();
#endif /* MIME */

static int	fill_up = 0;
static char    *fill_in = NULLCP;
static char    *partno = NULLCP;

static	int	queued = 0;

off_t	lseek ();
long	time ();

static	putfmt(), start_headers(), finish_headers(), putgrp(), pl();
static	anno(), make_bcc_file(), verify_all_addresses();
static	chkadr(), do_addresses(), do_text(), do_an_address(), sigon();
static 	sigoff(), p_refile(), fcc(), die(), insert_fcc();
static int	get_header(), putadr(), insert(), annoaux();
#ifdef TMA
static	postplain();
#else
static	post();
#endif	/* !TMA */
/*    MAIN */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     state,
            compnum;
    char   *cp,
           *msg = NULL,
          **argp = argv + 1,
            buf[BUFSIZ],
            name[NAMESZ];
    FILE   *in,
	   *out;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    m_foil (NULLCP);
    mts_init (invo_name);
#ifdef	MMDFMTS
#ifdef	MMDFII
    mmdf_init (invo_name);
#endif	/* MMDFII */
#endif	/* MMDFMTS */

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [switches] file", invo_name);
		    help (buf, switches);
		    done (1);

		case LIBSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    m_foil (cp);
		    continue;

		case ALIASW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
#ifdef	MHMTS
		    if (access (libpath (cp), 04) == NOTOK)
			adios (cp, "unable to read");
#endif	/* MHMTS */
		    if ((state = alias (cp)) != AK_OK)
			adios (NULLCP, "aliasing error in %s - %s",
				cp, akerror (state));
		    continue;

		case CHKSW: 
		    checksw++;
		    continue;
		case NCHKSW: 
		    checksw = 0;
		    continue;

		case DEBUGSW: 
		    debug++;
		    continue;

		case DISTSW:
		    msgstate = RESENT;
		    continue;

		case FILTSW:
		    if (!(filter = *argp++) || *filter == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    mime = 0;
		    continue;
		case NFILTSW:
		    filter = NULL;
		    continue;
		
		case FRMTSW: 
		    format++;
		    continue;
		case NFRMTSW: 
		    format = 0;
		    continue;

		case MIMESW:
#ifdef	MIME
		    mime++;
		    filter = 0;
#endif
		    continue;
		case NMIMESW: 
		    mime = 0;
		    continue;

		case MSGDSW: 
		    msgid++;
		    continue;
		case NMSGDSW: 
		    msgid = 0;
		    continue;

		case VERBSW: 
		    verbose++;
		    continue;
		case NVERBSW: 
		    verbose = 0;
		    continue;

		case WATCSW: 
		    watch++;
		    continue;
		case NWATCSW: 
		    watch = 0;
		    continue;

		case WHOMSW: 
		    whomsw++;
		    continue;

		case WIDTHSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((outputlinelen = atoi (cp)) < 10)
			adios (NULLCP, "impossible width %d", outputlinelen);
		    continue;

		case ENCRSW:
		    encryptsw++;
		    continue;
		case NENCRSW:
		    encryptsw = 0;
		    continue;

		case ANNOSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if ((pfd = atoi (cp)) <= 2)
			adios (NULLCP, "bad argument %s %s", argp[-2], cp);
		    continue;

#ifdef	MMDFMTS
		case MAILSW:
		    submitmode = "m";
		    continue;
		case SOMLSW:	/* for right now, sigh... */
		case SAMLSW:
		    submitmode = "b";
		    continue;
		case SENDSW:
		    submitmode = "y";
		    continue;
#endif	/* MMDFMTS */

#ifndef	MHMTS
		case DLVRSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
#else	/* MHMTS */
		case MAILSW:
		case SAMLSW:
		case SOMLSW:
		case SENDSW:
		    continue;
		case DLVRSW: 
		    if (!(deliver = *argp++) || *deliver == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
#endif	/* MHMTS */

#ifndef	SENDMTS
		case CLIESW:
		case SERVSW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case SNOOPSW:
		    continue;
#else	/* SENDMTS */
		case MAILSW:
		    smtpmode = S_MAIL;
		    continue;
		case SAMLSW:
		    smtpmode = S_SAML;
		    continue;
		case SOMLSW:
		    smtpmode = S_SOML;
		    continue;
		case SENDSW:
		    smtpmode = S_SEND;
		    continue;
		case CLIESW:
		    if (!(clientsw = *argp++) || *clientsw == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case SERVSW:
		    if (!(serversw = *argp++) || *serversw == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case SNOOPSW:
		    snoop++;
		    continue;
#endif	/* SENDMTS */

		case FILLSW:
		    if (!(fill_in = *argp++) || *fill_in == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case FILLUSW:
		    fill_up++;
		    continue;
		case PARTSW:
		    if (!(partno = *argp++) || *partno == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case QUEUESW:
		    queued++;
		    continue;
	    }
	if (msg)
	    adios (NULLCP, "only one message at a time!");
	else
	    msg = cp;
    }

    (void) alias (AliasFile);

/*  */

    if (!msg)
	adios (NULLCP, "usage: %s [switches] file", invo_name);

    if (outputlinelen < 10)
	adios (NULLCP, "impossible width %d", outputlinelen);

#ifdef	MHMTS
    if (access (msg, 04) == NOTOK)
	adios (msg, "unable to read");
#endif	/* MHMTS */
    if ((in = fopen (msg, "r")) == NULL)
	adios (msg, "unable to open");

    start_headers ();
    if (debug) {
	verbose++;
	discard (out = stdout);	/* XXX: reference discard() to help loader */
#ifdef	MHMTS
	if (deliver) {
	    (void) strcpy (tmpfil, msg);
	    putfmt ("To", deliver, out);
	    goto daemon;
	}
#endif	/* MHMTS */
    }
    else
#ifdef	MHMTS
    if (deliver) {
	if ((out = fopen ("/dev/null", "r")) == NULL)
	    adios ("/dev/null", "unable to write");
	(void) strcpy (tmpfil, msg);
	putfmt ("To", deliver, out);
	goto daemon;
    }
    else
#endif	/* MHMTS */
	if (whomsw) {
	    if ((out = fopen (fill_in ? fill_in : "/dev/null", "w")) == NULL)
		adios ("/dev/null", "unable to open");
	}
	else {
	    (void) strcpy (tmpfil, m_scratch ("", m_maildir (invo_name)));
	    if ((out = fopen (tmpfil, "w")) == NULL) {
		(void) strcpy (tmpfil, m_tmpfil (invo_name));
		if ((out = fopen (tmpfil, "w")) == NULL)
		    adios (tmpfil, "unable to create");
	    }
#ifdef	MHMTS
	    (void) chown (tmpfil, myuid, mygid);
#endif	/* MHMTS */
	    (void) chmod (tmpfil, 0600);
	}

/*  */

    hdrtab = msgstate == NORMAL ? NHeaders : RHeaders;

    for (compnum = 1, state = FLD;;) {
	switch (state = m_getfld (state, name, buf, sizeof buf, in)) {
	    case FLD: 
	    case FLDEOF: 
	    case FLDPLUS: 
		compnum++;
		cp = add (buf, NULLCP);
		while (state == FLDPLUS) {
		    state = m_getfld (state, name, buf, sizeof buf, in);
		    cp = add (buf, cp);
		}
		putfmt (name, cp, out);
		free (cp);
		if (state != FLDEOF)
		    continue;
		finish_headers (out);
		break;

	    case BODY: 
	    case BODYEOF: 
		finish_headers (out);
		if (whomsw && !fill_in)
		    break;
		fprintf (out, "\n%s", buf);
		while (state == BODY) {
		    state = m_getfld (state, name, buf, sizeof buf, in);
		    fputs (buf, out);
		}
		break;

	    case FILEEOF: 
		finish_headers (out);
		break;

	    case LENERR: 
	    case FMTERR: 
		adios (NULLCP, "message format error in component #%d",
			compnum);

	    default: 
		adios (NULLCP, "getfld() returned %d", state);
	}
	break;
    }

/*  */

#ifdef	MHMTS
daemon: ;
#endif	/* MHMTS */
    if (pfd != NOTOK)
	anno ();
    (void) fclose (in);
    if (debug) {
	pl ();
	done (0);
    }
    else
	(void) fclose (out);

#ifdef	TMA
    if (encryptsw)
	tmastart (tmasnoop);
#endif	/* TMA */
    if (whomsw) {
	if (!fill_up)
	    verify_all_addresses (1);
	done (0);
    }

#ifdef	MMDFMTS
    (void) strcat (submitopts, submitmode);
    if (watch)
	(void) strcat (submitopts, "nw");
#endif	/* MMDFMTS */
#ifdef	MHMTS
    verify_all_addresses (0);
#endif	/* MHMTS */
    if (encryptsw)
	verify_all_addresses (verbose);
    if (msgflags & MINV) {
	make_bcc_file ();
	if (msgflags & MVIS) {
#ifndef	MHMTS
	    if (!encryptsw)
		verify_all_addresses (verbose);
#endif	/* not MHMTS */
	    post (tmpfil, 0, verbose);
	}
	post (bccfil, 1, verbose);
	(void) unlink (bccfil);
    }
    else
	post (tmpfil, 0, isatty (1));
#ifdef	TMA
    if (encryptsw)
	tmastop ();
#endif	/* TMA */

    p_refile (tmpfil);

#ifdef	MHMTS
    if (!deliver)
#endif	/* MHMTS */
	(void) unlink (tmpfil);

    if (verbose)
	printf (partno ? "Partial Message #%s Processed\n" : "Message Processed\n",
		partno);

    done (0);
}

/*    DRAFT GENERATION */

static putfmt (name, str, out)
register char   *name,
		*str;
register FILE *out;
{
    int     count,
            grp,
            i,
            keep;
    register char  *cp,
                   *pp,
                   *qp;
    char    namep[BUFSIZ];
    register struct mailname   *mp,
                               *np;
    register struct headers *hdr;

    while (*str == ' ' || *str == '\t')
	str++;

    if (msgstate == NORMAL && uprf (name, "resent")) {
	advise (NULLCP, "illegal header line -- %s:", name);
	badmsg++;
	return;
    }

    if ((i = get_header (name, hdrtab)) == NOTOK) {
	fprintf (out, "%s: %s", name, str);
	return;
    }

    hdr = &hdrtab[i];
    if (hdr -> flags & HIGN) {
	if (fill_in)
	    fprintf (out, "%s: %s", name, str);
	return;
    }
    if (hdr -> flags & HBAD) {
	if (fill_in)
	    fprintf (out, "%s: %s", name, str);
	else {
	    advise (NULLCP, "illegal header line -- %s:", name);
	    badmsg++;
	}
	return;
    }
    msgflags |= (hdr -> set & ~(MVIS | MINV));

    if (hdr -> flags & HSUB)
	subject = subject ? add (str, add ("\t", subject)) : getcpy (str);
    if (hdr -> flags & HFCC) {
	if (fill_in) {
	    fprintf (out, "%s: %s", name, str);
	    return;
	}

	if (cp = rindex (str, '\n'))
	    *cp = 0;
	for (cp = pp = str; cp = index (pp, ','); pp = cp) {
	    *cp++ = 0;
	    insert_fcc (hdr, pp);
	}
	insert_fcc (hdr, pp);
	return;
    }

/*  */

    if (!(hdr -> flags & HADR)) {
	fprintf (out, "%s: %s", name, str);
	return;
    }

    tmpaddrs.m_next = NULL;
    for (count = 0; cp = getname (str); count++)
	if (mp = getm (cp, NULLCP, 0, AD_HOST, NULLCP)) {
	    if (tmpaddrs.m_next)
		np -> m_next = mp;
	    else
		tmpaddrs.m_next = mp;
	    np = mp;
	}
	else
	    if (hdr -> flags & HTRY)
		badadr++;
	    else
		badmsg++;

    if (count < 1) {
	if (hdr -> flags & HNIL)
	    fprintf (out, "%s: %s", name, str);
	else {
#ifdef	notdef
	    advise (NULLCP, "%s: field requires at least one address", name);
	    badmsg++;
#endif	/* notdef */
	}
	return;
    }

/*  */

    nameoutput = linepos = 0;
    (void) sprintf (namep, "%s%s",
		    !fill_in && (hdr -> flags & HMNG) ? "Original-" : "",
		    name);

    for (grp = 0, mp = tmpaddrs.m_next; mp; mp = np)
	if (mp -> m_nohost) {	/* also used to test (hdr -> flags & HTRY) */
	    pp = akvalue (mp -> m_mbox);
	    qp = akvisible () ? mp -> m_mbox : "";
	    np = mp;
	    if (np -> m_gname)
		putgrp (namep, np -> m_gname, out, hdr -> flags);
	    while (cp = getname (pp)) {
		if (!(mp = getm (cp, NULLCP, 0, AD_HOST, NULLCP))) {
		    badadr++;
		    continue;
		}
		if (hdr -> flags & HBCC)
		    mp -> m_bcc++;
		if (np -> m_ingrp)
		    mp -> m_ingrp = np -> m_ingrp;
		else
		    if (mp -> m_gname)
			putgrp (namep, mp -> m_gname, out, hdr -> flags);
		if (mp -> m_ingrp)
		    grp++;
#ifdef	MHMTS
		mp -> m_aka = getcpy (np -> m_mbox);
#endif	/* MHMTS */
		if (putadr (namep, qp, mp, out, hdr -> flags))
		    msgflags |= (hdr -> set & (MVIS | MINV));
		else
		    mnfree (mp);
	    }
	    mp = np;
	    np = np -> m_next;
	    mnfree (mp);
	}
	else {
	    if (hdr -> flags & HBCC)
		mp -> m_bcc++;
	    if (mp -> m_gname)
		putgrp (namep, mp -> m_gname, out, hdr -> flags);
	    if (mp -> m_ingrp)
		grp++;
	    keep = putadr (namep, "", mp, out, hdr -> flags);
	    np = mp -> m_next;
	    if (keep) {
		mp -> m_next = NULL;
		msgflags |= (hdr -> set & (MVIS | MINV));
	    }
	    else
		mnfree (mp);
	}

    if (grp > 0 && (hdr -> flags & HNGR)) {
	advise (NULLCP, "%s: field does not allow groups", name);
	badmsg++;
    }
    if (linepos) {
	if (fill_in && grp > 0)
	    (void) putc (';', out);
	(void) putc ('\n', out);
    }
}

/*  */

static  start_headers () {
    register char  *cp;
    char    myhost[BUFSIZ],
            sigbuf[BUFSIZ];
    register struct mailname   *mp;

    myuid = getuid ();
    mygid = getgid ();
    (void) time (&tclock);

    (void) strcpy (from, adrsprintf (NULLCP, NULLCP));

    (void) strcpy (myhost, LocalName ());
    for (cp = myhost; *cp; cp++)
	*cp = uptolow (*cp);

#ifdef	MHMTS
    if (deliver) {
	if (geteuid () == 0 && myuid != 0 && myuid != 1 && mygid != 1)
	    adios (NULLCP, "-deliver unknown");
	(void) strcpy (signature, from);
    }
#endif	/* MHMTS */

    if ((cp = getfullname ()) && *cp) {
	(void) strcpy (sigbuf, cp);
	(void) sprintf (signature, "%s <%s>", sigbuf, adrsprintf (NULLCP, NULLCP));
	if ((cp = getname (signature)) == NULL)
	    adios (NULLCP, "getname () failed -- you lose extraordinarily big");
	if ((mp = getm (cp, NULLCP, 0, AD_HOST, NULLCP)) == NULL)
	    adios (NULLCP, "bad signature '%s'", sigbuf);
	mnfree (mp);
	while (getname (""))
	    continue;
    }
    else
	(void) strcpy (signature, adrsprintf (NULLCP, NULLCP));
}

/*  */

static finish_headers (out)
register FILE *out;
{
    switch (msgstate) {
	case NORMAL: 
	    if (whomsw && !fill_up)
		break;

	    fprintf (out, "Date: %s\n", dtime (&tclock));
	    if (msgid)
		fprintf (out, "Message-ID: <%d.%ld@%s>\n",
			getpid (), tclock, LocalName ());
	    if (msgflags & MFRM)
		fprintf (out, "Sender: %s\n", from);
	    else
		fprintf (out, "From: %s\n", signature);
	    if (whomsw)
		break;

	    if (!(msgflags & MVIS))
		fprintf (out, "Bcc: Blind Distribution List: ;\n");
	    break;

	case RESENT: 
	    if (!(msgflags & MDAT)) {
		advise (NULLCP, "message has no Date: header");
		badmsg++;
	    }
	    if (!(msgflags & MFRM)) {
		advise (NULLCP, "message has no From: header");
		badmsg++;
	    }
	    if (whomsw && !fill_up)
		break;

#ifdef	MMDFI			/* sigh */
	    fprintf (out, "Sender: %s\n", from);
#endif	/* MMDFI */

	    fprintf (out, "Resent-Date: %s\n", dtime (&tclock));
	    if (msgid)
		fprintf (out, "Resent-Message-ID: <%d.%ld@%s>\n",
			getpid (), tclock, LocalName ());
	    if (msgflags & MRFM)
		fprintf (out, "Resent-Sender: %s\n", from);
	    else
		fprintf (out, "Resent-From: %s\n", signature);
	    if (whomsw)
		break;
	    if (!(msgflags & MVIS))
		fprintf (out, "Resent-Bcc: Blind Re-Distribution List: ;\n");
	    break;
    }

    if (badmsg)
	adios (NULLCP, "re-format message and try again");
    if (!recipients)
	adios (NULLCP, "no addressees");
}

/*  */

static int     get_header (header, table)
register char   *header;
register struct headers *table;
{
    register struct headers *h;

    for (h = table; h -> value; h++)
	if (uleq (header, h -> value))
	    return (h - table);

    return NOTOK;
}

/*  */

static int     putadr (name, aka, mp, out, flags)
register char   *name,
		*aka;
register struct mailname *mp;
register FILE *out;
unsigned int    flags;
{
    int     len;
    register char   *cp;
    char    buffer[BUFSIZ];

    if (mp -> m_mbox == NULL || ((flags & HTRY) && !insert (mp)))
	return 0;
    if (!fill_in && (flags & (HBCC | HDCC)) || mp -> m_ingrp)
	return 1;

    if (!nameoutput) {
	fprintf (out, "%s: ", name);
	linepos += (nameoutput = strlen (name) + 2);
    }

    if (*aka && mp -> m_type != UUCPHOST && !mp -> m_pers)
	mp -> m_pers = getcpy (aka);
    if (format) {
	if (mp -> m_gname && !fill_in)
	    (void) sprintf (cp = buffer, "%s;", mp -> m_gname);
	else
	    cp = adrformat (mp);
    }
    else
	cp = mp -> m_text;
    len = strlen (cp);

    if (linepos != nameoutput)
	if (len + linepos + 2 > outputlinelen)
	    fprintf (out, ",\n%*s", linepos = nameoutput, "");
	else {
	    fputs (", ", out);
	    linepos += 2;
	}

    fputs (cp, out);
    linepos += len;

    return (flags & HTRY);
}

/*  */

static putgrp (name, group, out, flags)
register char   *name,
		*group;
register FILE *out;
unsigned int    flags;
{
    int     len;
    char   *cp;

    if (!fill_in && (flags & HBCC))
	return;

    if (!nameoutput) {
	fprintf (out, "%s: ", name);
	linepos += (nameoutput = strlen (name) + 2);
	if (fill_in)
	    linepos -= strlen (group);
    }

    cp = fill_in ? group : concat (group, ";", NULLCP);
    len = strlen (cp);

    if (linepos > nameoutput)
	if (len + linepos + 2 > outputlinelen) {
	    fprintf (out, ",\n%*s", nameoutput, "");
	    linepos = nameoutput;
	}
	else {
	    fputs (", ", out);
	    linepos += 2;
	}

    fputs (cp, out);
    linepos += len;
}

/*  */

static int     insert (np)
register struct mailname   *np;
{
    register struct mailname   *mp;

    if (np -> m_mbox == NULL)
	return 0;

    for (mp = np -> m_type == LOCALHOST ? &localaddrs
	    : np -> m_type == UUCPHOST ? &uuaddrs
	    : &netaddrs;
	    mp -> m_next;
	    mp = mp -> m_next)
	if (uleq (np -> m_host, mp -> m_next -> m_host)
		&& uleq (np -> m_mbox, mp -> m_next -> m_mbox)
		&& np -> m_bcc == mp -> m_next -> m_bcc)
	    return 0;

    mp -> m_next = np;
    recipients++;
    return 1;
}


static  pl () {
    register int     i;
    register struct mailname *mp;

    printf ("-------\n\t-- Addresses --\nlocal:\t");
    for (mp = localaddrs.m_next; mp; mp = mp -> m_next)
	printf ("%s%s%s", mp -> m_mbox,
		mp -> m_bcc ? "[BCC]" : "",
		mp -> m_next ? ",\n\t" : "");

    printf ("\nnet:\t");
    for (mp = netaddrs.m_next; mp; mp = mp -> m_next)
	printf ("%s%s@%s%s%s", mp -> m_path ? mp -> m_path : "",
		mp -> m_mbox, mp -> m_host,
		mp -> m_bcc ? "[BCC]" : "",
		mp -> m_next ? ",\n\t" : "");

    printf ("\nuucp:\t");
    for (mp = uuaddrs.m_next; mp; mp = mp -> m_next)
	printf ("%s!%s%s", mp -> m_host, mp -> m_mbox,
		mp -> m_bcc ? "[BCC]" : "",
		mp -> m_next ? ",\n\t" : "");

    printf ("\n\t-- Folder Copies --\nfcc:\t");
    for (i = 0; i < fccind; i++)
	printf ("%s%s", fccfold[i], i + 1 < fccind ? ",\n\t" : "");
    printf ("\n");
}

/*  */

static  anno () {
    register struct mailname *mp;

    for (mp = localaddrs.m_next; mp; mp = mp -> m_next)
	if (annoaux (mp) == NOTOK)
	    goto oops;

    for (mp = netaddrs.m_next; mp; mp = mp -> m_next)
	if (annoaux (mp) == NOTOK)
	    goto oops;

    for (mp = uuaddrs.m_next; mp; mp = mp -> m_next)
	if (annoaux (mp) == NOTOK)
	    break;

oops: ;
    (void) close (pfd);
    pfd = NOTOK;
}


static int  annoaux (mp)
register struct mailname *mp;
{
    int     i;
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "%s\n", adrformat (mp));
    i = strlen (buffer);

    return (write (pfd, buffer, i) == i ? OK : NOTOK);
}

/*  */

static  insert_fcc (hdr, pp)
register struct	headers *hdr;
register char   *pp;
{
    register char   *cp;

    for (cp = pp; isspace (*cp); cp++)
	continue;
    for (pp += strlen (pp) - 1; pp > cp && isspace (*pp); pp--)
	continue;
    if (pp >= cp)
	*++pp = 0;
    if (*cp == 0)
	return;

    if (fccind >= FCCS)
	adios (NULLCP, "too many %ss", hdr -> value);
    fccfold[fccind++] = getcpy (cp);
}

/*    BCC GENERATION */

static  make_bcc_file () {
    int     fd,
	    i,
            child_id;
    char   *vec[6];
    register FILE   *out;

    (void) strcpy (bccfil, m_tmpfil ("bccs"));
    if ((out = fopen (bccfil, "w")) == NULL)
	adios (bccfil, "unable to create");
    (void) chmod (bccfil, 0600);

    fprintf (out, "Date: %s\n", dtime (&tclock));
    if (msgid)
	fprintf (out, "Message-ID: <%d.%ld@%s>\n",
		getpid (), tclock, LocalName ());
    fprintf (out, "From: %s\n", signature);
    if (subject)
	fprintf (out, "Subject: %s", subject);
    fprintf (out, "BCC:\n");
#ifdef MIME
    if (mime) {
	char   *cp;

	if ((cp = index (prefix, 'a')) == NULL)
	    adios (NULLCP, "lost prefix start");
	while (find_prefix () == NOTOK)
	    if (*cp < 'z')
		(*cp)++;
	    else
		if (*++cp == 0)
		    adios (NULLCP,
			   "giving up trying to find a unique delimiter string");
		else
		    (*cp)++;

	fprintf (out, "%s: %s\n%s: multipart/digest; boundary=\"",
		 VRSN_FIELD, VRSN_VALUE, TYPE_FIELD);
	fprintf (out, "%s\"\n\n--%s\n\n", prefix, prefix);
    }
    else
#endif /* MIME */
	fprintf (out, "\n------- Blind-Carbon-Copy\n\n");
    (void) fflush (out);

    if (filter == NULL) {
	if ((fd = open (tmpfil, 0)) == NOTOK)
	    adios (tmpfil, "unable to re-open");
#ifdef MIME
	if (mime)
	    cpydata (fd, fileno (out), tmpfil, bccfil);
	else
#endif /* MIME */
	    cpydgst (fd, fileno (out), tmpfil, bccfil);
	(void) close (fd);
    }
    else {
	vec[0] = r1bindex (mhlproc, '/');

	for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	    sleep (5);
	switch (child_id) {
	    case NOTOK: 
		adios ("fork", "unable to");

	    case OK: 
		(void) dup2 (fileno (out), 1);

		i = 1;
		vec[i++] = "-forward";
		vec[i++] = "-form";
		vec[i++] = filter;
		vec[i++] = tmpfil;
		vec[i] = NULL;

		execvp (mhlproc, vec);
		fprintf (stderr, "unable to exec ");
		perror (mhlproc);
		_exit (-1);

	    default: 
		(void) pidXwait (child_id, mhlproc);
		break;
	}
    }

    (void) fseek (out, 0L, 2);
#ifdef	MIME
    if (mime)
	fprintf (out, "\n--%s--\n", prefix);
    else
#endif /* MIME */
	fprintf (out, "\n------- End of Blind-Carbon-Copy\n");
    (void) fclose (out);
}

/*  */

#ifdef MIME
static int  find_prefix ()
{
    int	    len,
	    result;
    char    buffer[BUFSIZ];
    FILE   *in;

    if ((in = fopen (tmpfil, "r")) == NULL)
	adios (tmpfil, "unable to re-open");

    len = strlen (prefix);

    result = OK;
    while (fgets (buffer, sizeof buffer - 1, in))
	if (buffer[0] == '-' && buffer[1] == '-') {
	    register char *cp;

	    for (cp = buffer + strlen (buffer) - 1; cp >= buffer; cp--)
		if (!isspace (*cp))
		    break;
	    *++cp = NULL;
	    if (strcmp (buffer + 2, prefix) == 0) {
		result = NOTOK;
		break;
	    }
	}

    (void) fclose (in);
	
    return result;
}
#endif /* MIME */

/*    ADDRESS VERIFICATION */

static  verify_all_addresses (talk)
int     talk;
{
#ifndef	MHMTS
    int     retval;
#endif	/* not MHMTS */
#ifdef	MMDFMTS
#ifdef	RP_NS
    int	    len;
    struct rp_bufstruct reply;
#endif	/* RP_NS */
#endif	/* MMDFMTS */
    register struct mailname *lp;

#ifndef	MHMTS
    sigon ();
#endif	/* not MHMTS */

#ifdef	MMDFMTS
    if (!whomsw || checksw) {
	if (rp_isbad (retval = mm_init ())
		|| rp_isbad (retval = mm_sbinit ())
		|| rp_isbad (retval = mm_winit (NULLCP, submitopts, from)))
	    die (NULLCP, "problem initializing MMDF system [%s]",
		    rp_valstr (retval));
#ifdef	RP_NS
	if (rp_isbad (retval = mm_rrply (&reply, &len)))
	    die (NULLCP, "problem with sender address [%s]",
		    rp_valstr (retval));
#endif	/* RP_NS */
    }
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (!whomsw || checksw)
	if (rp_isbad (retval = sm_init (clientsw, serversw, 0, 0, snoop, 0, 0))
		|| rp_isbad (retval = sm_winit (smtpmode, from)))
	    die (NULLCP, "problem initializing server; %s",
		    rp_string (retval));
#endif	/* SENDMTS */

    if (talk && !whomsw)
	printf (" -- Address Verification --\n");
#ifndef	BERK
    if (talk && localaddrs.m_next)
	printf ("  -- Local Recipients --\n");
#endif	/* BERK */
    for (lp = localaddrs.m_next; lp; lp = lp -> m_next)
	do_an_address (lp, talk, encryptsw);

#ifndef	BERK
    if (talk && uuaddrs.m_next)
	printf ("  -- UUCP Recipients --\n");
#endif	/* BERK */
    for (lp = uuaddrs.m_next; lp; lp = lp -> m_next)
	do_an_address (lp, talk, encryptsw);

#ifndef	BERK
    if (talk && netaddrs.m_next)
	printf ("  -- Network Recipients --\n");
#endif	/* BERK */
    for (lp = netaddrs.m_next; lp; lp = lp -> m_next)
	do_an_address (lp, talk, encryptsw);

    chkadr ();
    if (talk && !whomsw)
	printf (" -- Address Verification Successful --\n");

#ifdef	MMDFMTS
    if (!whomsw || checksw)
	(void) mm_end (NOTOK);
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (!whomsw || checksw)
	(void) sm_end (DONE);
#endif	/* SENDMTS */
    (void) fflush (stdout);

#ifndef	MHMTS
    sigoff ();
#endif	/* not MHMTS */
}

/*  */

static  chkadr () {
#define	plural(x) (x == 1 ? "" : "s")

    if (badadr && unkadr)
	die (NULLCP, "%d address%s unparsable, %d addressee%s undeliverable",
		badadr, plural (badadr), unkadr, plural (badadr));
    if (badadr)
	die (NULLCP, "%d address%s unparsable", badadr, plural (badadr));
    if (unkadr)
	die (NULLCP, "%d addressee%s undeliverable", unkadr, plural (unkadr));
}

/*    MTS INTERACTION */

#ifdef	TMA
static postplain (file, bccque, talk)
#else	/* TMA */
static post (file, bccque, talk)
#endif	/* TMA */
register char   *file;
int     bccque,
        talk;
{
    int     fd,
    	    onex = !(msgflags & MINV) || bccque;
#ifndef	MHMTS
    int	    retval;
#ifdef	MMDFMTS
#ifdef	RP_NS
    int	    len;
    struct rp_bufstruct reply;
#endif	/* RP_NS */
#endif	/* MMDFMTS */
#else	/* MHMTS */
    int	    ud;
#endif	/* MHMTS */

    if (verbose)
	if (msgflags & MINV)
	    printf (" -- Posting for %s Recipients --\n",
		    bccque ? "Blind" : "Sighted");
	else
	    printf (" -- Posting for All Recipients --\n");

    sigon ();

#ifdef	MMDFMTS
    if (rp_isbad (retval = mm_init ())
	    || rp_isbad (retval = mm_sbinit ())
	    || rp_isbad (retval = mm_winit (NULLCP, submitopts, from)))
	die (NULLCP, "problem initializing MMDF system [%s]",
		rp_valstr (retval));
#ifdef	RP_NS
	if (rp_isbad (retval = mm_rrply (&reply, &len)))
	    die (NULLCP, "problem with sender address [%s]",
		    rp_valstr (retval));
#endif	/* RP_NS */
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (rp_isbad (retval = sm_init (clientsw, serversw, watch, verbose, snoop,
				    onex, queued))
	    || rp_isbad (retval = sm_winit (smtpmode, from)))
	die (NULLCP, "problem initializing server; %s", rp_string (retval));
#endif	/* SENDMTS */

#ifndef	MHMTS
    do_addresses (bccque, talk && verbose);
    if ((fd = open (file, 0)) == NOTOK)
	die (file, "unable to re-open");
    do_text (file, fd);
#else	/* MHMTS */
    if ((fd = open (file, 0)) == NULL)
	adios (file, "unable to re-open");
#ifdef	MF
    ud = UucpChan () && uuaddrs.m_next ? make_uucp_file (fd) : NOTOK;
#else	/* not MF */
    ud = NOTOK;
#endif	/* not MF */
    do_addresses (file, fd, ud, bccque, talk && verbose);
    if (ud != NOTOK)
	(void) close (ud);
#endif	/* MHMTS */
    (void) close (fd);
    (void) fflush (stdout);

#ifdef	MMDFMTS
    (void) mm_sbend ();
    (void) mm_end (OK);
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    (void) sm_end (onex ? OK : DONE);
#endif	/* SENDMTS */

    sigoff ();

    if (verbose)
	if (msgflags & MINV)
	    printf (" -- %s Recipient Copies Posted --\n",
		    bccque ? "Blind" : "Sighted");
	else
	    printf (" -- Recipient Copies Posted --\n");
    (void) fflush (stdout);
}

/*  */

#ifdef	TMA
static postcipher (file, bccque, talk)
register char   *file;
int     bccque,
        talk;
{
    int     fdP,
            state;
    char    reason[BUFSIZ];
    struct mailname *lp;

    if (verbose)
	if (msgflags & MINV)
	    printf (" -- Posting for %s Recipients --\n",
		    bccque ? "Blind" : "Sighted");
	else
	    printf (" -- Posting for All Recipients --\n");

    if ((fdP = open (file, 0)) == NOTOK)
	adios (file, "unable to re-open");
    if (ciphinit (fdP, reason) == NOTOK)
	adios (NULLCP, "%s", reason);
    (void) close (fdP);

    for (state = 0, lp = localaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
#ifndef	BERK
	    if (talk && !state)
		printf ("  -- Local Recipients --\n");
#endif	/* BERK */
	    do_a_cipher (lp, talk);
#ifndef	BERK
	    state++;
#endif	/* BERK */
	}

    for (state = 0, lp = uuaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
#ifndef	BERK
	    if (talk && !state)
		printf ("  -- UUCP Recipients --\n");
#endif	/* BERK */
	    do_a_cipher (lp, talk);
#ifndef	BERK
	    state++;
#endif	/* BERK */
	}

    for (state = 0, lp = netaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
#ifndef	BERK
	    if (talk && !state)
		printf ("  -- Network Recipients --\n");
#endif	/* BERK */
	    do_a_cipher (lp, talk);
#ifndef	BERK
	    state++;
#endif	/* BERK */
	}

    if (ciphdone (reason) == NOTOK)
	admonish (NULLCP, "%s", reason);
#ifdef	SENDMTS
    if (!(msgflags & MINV) || bccque)
	(void) sm_end (OK);
#endif	/* SENDMTS */

    if (verbose)
	if (msgflags & MINV)
	    printf (" -- %s Recipient Copies Posted --\n",
		    bccque ? "Blind" : "Sighted");
	else
	    printf (" -- Recipient Copies Posted --\n");
    (void) fflush (stdout);
}

/*  */

static do_a_cipher (lp, talk)
register struct mailname *lp;
int	talk;
{
    int     fd,
            retval;
    register char  *mbox,
                   *host;
    char    addr[BUFSIZ],
            reason[BUFSIZ];
#ifdef	MMDFMTS
#ifdef	RP_NS
    int	    len;
    struct rp_bufstruct reply;
#endif	/* RP_NS */
#endif	/* MMDFMTS */

    sigon ();

#ifdef	MMDFMTS
    if (rp_isbad (retval = mm_init ())
	    || rp_isbad (retval = mm_sbinit ())
	    || rp_isbad (retval = mm_winit (NULL, submitopts, from)))
	die (NULLCP, "problem initializing MMDF system [%s]",
		rp_valstr (retval));
#ifdef	RP_NS
	if (rp_isbad (retval = mm_rrply (&reply, &len)))
	    die (NULLCP, "problem with sender address [%s]",
		    rp_valstr (retval));
#endif	/* RP_NS */
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (rp_isbad (retval = sm_init (clientsw, serversw, watch, verbose, snoop,
				    0, 0))
	    || rp_isbad (retval = sm_winit (smtpmode, from)))
	die (NULLCP, "problem initializing server; %s", rp_string (retval));
#endif	/* SENDMTS */

    do_an_address (lp, talk, 0);

    switch (lp -> m_type) {
	case LOCALHOST: 
	    mbox = lp -> m_mbox;
	    host = LocalName ();
	    (void) strcpy (addr, mbox);
	    break;

	case UUCPHOST: 
#ifdef	MMDFMTS
	    mbox = concat (lp -> m_host, "!", lp -> m_mbox, NULLCP);
	    host = UucpChan ();
#endif	/* MMDFMTS */
#ifdef	SENDMTS
	    mbox = auxformat (lp, 0);
	    host = NULL;
#endif	/* SENDMTS */
	    (void) sprintf (addr, "%s!%s", lp -> m_host, lp -> m_mbox);
	    break;

	default: 
	    mbox = lp -> m_mbox;
	    host = lp -> m_host;
	    (void) sprintf (addr, "%s at %s", lp -> m_mbox, lp -> m_host);
	    break;
    }
    chkadr ();			/* XXX */

#ifdef	MMDFMTS
    if (rp_isbad (retval = mm_waend ()))
	die (NULLCP, "problem ending addresses [%s]\n",
		rp_valstr (retval));
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (rp_isbad (retval = sm_waend ()))
	die (NULLCP, "problem ending addresses; %s", rp_string (retval));
#endif	/* SENDMTS */

    if ((fd = encipher (mbox, host, reason)) == NOTOK)
	die (NULLCP, "%s: %s", addr, reason);
    do_text ("temporary file", fd);
    (void) close (fd);
    (void) fflush (stdout);

#ifdef	MMDFMTS
    (void) mm_sbend ();
    (void) mm_end (OK);
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    (void) sm_end (DONE);
#endif	/* SENDMTS */

    sigoff ();
}
#endif	/* TMA */

/*  */

#ifndef	MHMTS
static do_addresses (bccque, talk)
#else	/* MHMTS */
static do_addresses (file, fd, ud, bccque, talk)
register char   *file;
int	fd,
	ud;
#endif	/* MHMTS */
int	bccque,
	talk;
{
    int     retval;
#ifndef	BERK
    int	    state;
#endif	/* not BERK */
    register struct mailname *lp;

#ifndef	BERK
    state = 0;
#endif	/* not BERK */
    for (lp = localaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
#ifndef	BERK
	    if (talk && !state)
		printf ("  -- Local Recipients --\n");
#endif	/* not BERK */
#ifndef	MHMTS
	    do_an_address (lp, talk, 0);
#else	/* MHMTS */
	    localmail (lp, talk, fd);
#endif	/* MHMTS */
#ifndef	BERK
	    state++;
#endif	/* not BERK */
	}

#ifndef	BERK
    state = 0;
#endif	/* not BERK */
    for (lp = uuaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
#ifndef	BERK
	    if (talk && !state)
		printf ("  -- UUCP Recipients --\n");
#endif	/* not BERK */
#ifndef	MHMTS
	    do_an_address (lp, talk, 0);
#else	/* MHMTS */
	    uucpmail (lp, talk, ud != NOTOK ? ud : fd, ud == NOTOK);
#endif	/* MHMTS */
#ifndef	BERK
	    state++;
#endif	/* not BERK */
	}

#ifndef	BERK
    state = 0;
#endif	/* not BERK */
    for (lp = netaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
#ifndef	BERK
	    if (talk && !state)
		printf ("  -- Network Recipients --\n");
#endif	/* not BERK */
#ifndef	MHMTS
	    do_an_address (lp, talk, 0);
#else	/* MHMTS */
	    netmail (talk, fd, bccque);
#endif	/* MHMTS */
#ifndef	BERK
	    state++;
#endif	/* not BERK */
	}

/*  */

    chkadr ();

#ifdef	MMDFMTS
    if (rp_isbad (retval = mm_waend ()))
	die (NULLCP, "problem ending addresses [%s]\n",
		rp_valstr (retval));
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (rp_isbad (retval = sm_waend ()))
	die (NULLCP, "problem ending addresses; %s", rp_string (retval));
#endif	/* SENDMTS */
}

/*  */

#ifndef	MHMTS
static  do_text (file, fd)
register char   *file;
int     fd;
{
    int     retval,
            state;
    char    buf[BUFSIZ];
#ifdef	MMDFMTS
    struct rp_bufstruct reply;
#endif	/* MMDFMTS */

    (void) lseek (fd, (off_t)0, 0);
    while ((state = read (fd, buf, sizeof buf)) > 0)
#ifdef	MMDFMTS
	if (rp_isbad (mm_wtxt (buf, state)))
	    die (NULLCP, "problem writing text [%s]\n", rp_valstr (retval));
#endif	/* MMDFMTS */
#ifdef	SENDMTS
	if (rp_isbad (retval = sm_wtxt (buf, state)))
	    die (NULLCP, "problem writing text; %s\n", rp_string (retval));
#endif	/* SENDMTS */

    if (state == NOTOK)
	die (file, "problem reading from");

#ifdef	MMDFMTS
    if (rp_isbad (retval = mm_wtend ()))
	die (NULLCP, "problem ending text [%s]\n", rp_valstr (retval));

    if (rp_isbad (retval = mm_rrply (&reply, &state)))
	die (NULLCP, "problem getting submission status [%s]\n",
		rp_valstr (retval));

    switch (rp_gval (reply.rp_val)) {
	case RP_OK: 
	case RP_MOK: 
	    break;

	case RP_NO: 
	    die (NULLCP, "you lose; %s", reply.rp_line);

	case RP_NDEL: 
	    die (NULLCP, "no delivery occurred; %s", reply.rp_line);

	case RP_AGN: 
	    die (NULLCP, "try again later; %s", reply.rp_line);

	case RP_NOOP: 
	    die (NULLCP, "nothing done; %s", reply.rp_line);

	default: 
	    die (NULLCP, "unexpected response;\n\t[%s] -- %s",
		    rp_valstr (reply.rp_val), reply.rp_line);
    }
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    switch (retval = sm_wtend ()) {
	case RP_OK: 
	    break;

	case RP_NO: 
	case RP_NDEL: 
	    die (NULLCP, "posting failed; %s", rp_string (retval));

	default: 
	    die (NULLCP, "unexpected response; %s", rp_string (retval));
    }
#endif	/* SENDMTS */
}
#endif	/* not MHMTS */

/*    MTS-SPECIFIC INTERACTION */

#ifdef	MMDFMTS

#ifndef	TMA
/* ARGSUSED */
#endif	/* TMA */

static do_an_address (lp, talk, tma)
register struct mailname *lp;
int     talk,
	tma;
{
    int     len,
            retval;
    register char  *mbox,
                   *host,
                   *text,
                   *path;
    char    addr[BUFSIZ];
#ifdef	TMA
    char    reason[BUFSIZ];
#endif	/* TMA */
    struct rp_bufstruct reply;

    switch (lp -> m_type) {
	case LOCALHOST: 
	    mbox = lp -> m_mbox;
	    host = LocalName ();
	    (void) strcpy (addr, mbox);
	    break;

	case UUCPHOST: 
#ifdef	MF
	    mbox = concat (lp -> m_host, "!", lp -> m_mbox, NULLCP);
	    host = UucpChan ();
	    (void) strcpy (addr, mbox);
	    break;
#else	/* MF */
	    fprintf (talk ? stdout : stderr, "  %s!%s: %s\n",
		lp -> m_host, lp -> m_mbox, "not supported; UUCP address");
	    unkadr++;
	    (void) fflush (stdout);
	    return;
#endif	/* MF */

	default: 		/* let MMDF decide if the host is bad */
	    mbox = lp -> m_mbox;
	    host = lp -> m_host;
	    (void) sprintf (addr, "%s at %s", mbox, host);
	    break;
    }
#ifdef	TMA
    if ((!whomsw || checksw)
	    && tma
	    && seekaddr (mbox, host, reason) == NOTOK) {
	fprintf (talk ? stdout : stderr, "  %s%s: %s\n",
		addr, "[TMA]", reason);
	unkadr++;
    }
#endif	/* TMA */

    if (talk)
	printf ("  %s%s", addr, whomsw && lp -> m_bcc ? "[BCC]" : "");

    if (whomsw && !checksw) {
	(void) putchar ('\n');
	return;
    }
    if (talk)
	printf (": ");
    (void) fflush (stdout);

/*  */

#ifdef	MMDFII
    if (lp -> m_path)
	path = concat (lp -> m_path, mbox, "@", host, NULLCP);
    else
#endif	/* MMDFII */
	path = NULLCP;
    if (rp_isbad (retval = mm_wadr (path ? NULLCP : host, path ? path : mbox))
	    || rp_isbad (retval = mm_rrply (&reply, &len)))
	die (NULLCP, "problem submitting address [%s]", rp_valstr (retval));

    switch (rp_gval (reply.rp_val)) {
	case RP_AOK: 
	    if (talk)
		printf ("address ok\n");
	    (void) fflush (stdout);
	    return;

#ifdef	RP_DOK
	case RP_DOK: 
	    if (talk)
		printf ("nameserver timeout - queued for checking\n");
	    (void) fflush (stdout);
	    return;
#endif	/* RP_DOK */

	case RP_NO: 
	    text = "you lose";
	    break;

#ifdef	RP_NS
	case RP_NS: 
	    text = "temporary nameserver failure";
	    break;

#endif	/* RP_NS */

	case RP_USER: 
	case RP_NDEL: 
	    text = "not deliverable";
	    break;

	case RP_AGN: 
	    text = "try again later";
	    break;

	case RP_NOOP: 
	    text = "nothing done";
	    break;

	default: 
	    if (!talk)
		fprintf (stderr, "  %s: ", addr);
	    text = "unexpected response";
	    die (NULLCP, "%s;\n    [%s] -- %s", text,
		    rp_valstr (reply.rp_val), reply.rp_line);
    }

    if (!talk)
	fprintf (stderr, "  %s: ", addr);
    fprintf (talk ? stdout : stderr, "%s;\n    %s\n", text, reply.rp_line);
    unkadr++;

    (void) fflush (stdout);
}
#endif	/* MMDFMTS */

/*  */

#ifdef	MHMTS
/* ARGSUSED */

static do_an_address (lp, talk, tma)
register struct mailname *lp;
int     talk,
	tma;
{
    register char  *mbox;
    char    addr[BUFSIZ];

    switch (lp -> m_type) {
	case LOCALHOST: 
	    (void) strcpy (addr, lp -> m_mbox);
	    break;

	case UUCPHOST: 
	    (void) sprintf (addr, "%s!%s", lp -> m_host, lp -> m_mbox);
	    break;

	default: 
	    (void) sprintf (addr, "%s at %s", lp -> m_mbox, lp -> m_host);
	    break;
    }
    if (talk)
	printf ("  %s%s", addr, whomsw && lp -> m_bcc ? "[BCC]" : "");

    if (whomsw && !checksw) {
	(void) putchar ('\n');
	return;
    }
    if (talk)
	printf (": ");
    (void) fflush (stdout);

/*  */

    switch (lp -> m_type) {
	case LOCALHOST: 
	    mbox = lp -> m_mbox;
	    if (*mbox == '~')
		mbox++;
	    if (seek_home (mbox)) {
		lp -> m_mbox = mbox;
		if (talk)
		    printf ("address ok\n");
	    }
	    else {
		if (!talk)
		    fprintf (stderr, "  %s: ", addr);
		fprintf (talk ? stdout : stderr,
			"not deliverable; unknown user\n");
		unkadr++;
	    }
	    break;

	case UUCPHOST: 
	    if (uucpsite (lp -> m_host) == OK) {
		if (talk)
		    printf ("address ok\n");
	    }
	    else {
		if (!talk)
		    fprintf (stderr, "  %s: ", addr);
		fprintf (talk ? stdout : stderr,
			"not deliverable; unknown system\n");
		unkadr++;
	    }
	    break;

	case NETHOST: 
	    if (talk)
		printf ("address ok\n");
	    break;

	default: 
	    if (!talk)
		fprintf (stderr, "  %s: ", addr);
	    fprintf (talk ? stdout : stderr,
		    "not deliverable; unknown host\n");
	    unkadr++;
	    break;
    }

    (void) fflush (stdout);
}
#endif	/* MHMTS */

/*  */

#ifdef	SENDMTS

#ifndef	TMA
/* ARGSUSED */
#endif	/* TMA */

static do_an_address (lp, talk, tma)
register struct mailname *lp;
int     talk,
	tma;
{
    int     retval;
    register char  *mbox,
                   *host;
    char    addr[BUFSIZ];
#ifdef	TMA
    char    reason[BUFSIZ];
#endif	/* TMA */

    switch (lp -> m_type) {
	case LOCALHOST: 
	    mbox = lp -> m_mbox;
	    host = lp -> m_host;
	    (void) strcpy (addr, mbox);
	    break;

	case UUCPHOST: 
	    mbox = auxformat (lp, 0);
	    host = NULL;
	    (void) sprintf (addr, "%s!%s", lp -> m_host, lp -> m_mbox);
	    break;

	default: 		/* let SendMail decide if the host is bad  */
	    mbox = lp -> m_mbox;
	    host = lp -> m_host;
	    (void) sprintf (addr, "%s at %s", mbox, host);
	    break;
    }

#ifdef	TMA
    if ((!whomsw || checksw)
	    && tma
	    && seekaddr (mbox, host, reason) == NOTOK) {
	fprintf (talk ? stdout : stderr, "  %s%s: %s\n",
		addr, "[TMA]", reason);
	unkadr++;
    }
#endif	/* TMA */

    if (talk)
	printf ("  %s%s", addr, whomsw && lp -> m_bcc ? "[BCC]" : "");

    if (whomsw && !checksw) {
	(void) putchar ('\n');
	return;
    }
    if (talk)
	printf (": ");
    (void) fflush (stdout);

/*  */

    switch (retval = sm_wadr (mbox, host,
			 lp -> m_type != UUCPHOST ? lp -> m_path : NULLCP)) {
	case RP_OK: 
	    if (talk)
		printf ("address ok\n");
	    break;

	case RP_NO: 
	case RP_USER: 
	    if (!talk)
		fprintf (stderr, "  %s: ", addr);
	    fprintf (talk ? stdout : stderr, "loses; %s\n",
			rp_string (retval));
	    unkadr++;
	    break;

	default: 
	    if (!talk)
		fprintf (stderr, "  %s: ", addr);
	    die (NULLCP, "unexpected response; %s", rp_string (retval));
    }

    (void) fflush (stdout);
}
#endif	/* SENDMTS */

/*    SIGNAL HANDLING */

#ifndef	MHMTS

/* ARGSUSED */

static	TYPESIG sigser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (i, SIG_IGN);
#endif	/* not BSD42 */
    (void) unlink (tmpfil);
    if (msgflags & MINV)
	(void) unlink (bccfil);
#ifdef	MMDFMTS
    if (!whomsw || checksw)
	(void) mm_end (NOTOK);
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (!whomsw || checksw)
	(void) sm_end (NOTOK);
#endif	/* SENDMTS */
    done (1);
}
#endif	/* not MHMTS */


static  sigon () {
    if (debug)
	return;

#ifndef	MHMTS
    setsigx (hstat, SIGHUP, sigser);
    setsigx (istat, SIGINT, sigser);
    setsigx (qstat, SIGQUIT, sigser);
    setsigx (tstat, SIGTERM, sigser);
#else	/* MHMTS */
    setsigx (hstat, SIGHUP, SIG_IGN);
    setsigx (istat, SIGINT, SIG_IGN);
    setsigx (qstat, SIGQUIT, SIG_IGN);
    setsigx (tstat, SIGTERM, SIG_IGN);
#endif	/* MHMTS */
}


static sigoff () {
    if (debug)
	return;

    (void) signal (SIGHUP, hstat);
    (void) signal (SIGINT, istat);
    (void) signal (SIGQUIT, qstat);
    (void) signal (SIGTERM, tstat);
}

/*    FCC INTERACTION */

static  p_refile (file)
register char   *file;
{
    register int     i;

    if (fccind == 0)
	return;

#ifdef	MHMTS
    (void) setuid (myuid);
#endif	/* MHMTS */
    if (verbose)
	printf (" -- Filing Folder Copies --\n");
    for (i = 0; i < fccind; i++)
	fcc (file, fccfold[i]);
    if (verbose)
	printf (" -- Folder Copies Filed --\n");
}


static fcc (file, folder)
register char   *file,
		*folder;
{
    int     i,
            child_id,
	    status;
    char    fold[BUFSIZ];

    if (verbose)
	printf ("  %sFcc %s: ", msgstate == RESENT ? "Resent-" : "", folder);
    (void) fflush (stdout);

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 
	    if (!verbose)
		fprintf (stderr, "  %sFcc %s: ",
			msgstate == RESENT ? "Resent-" : "", folder);
	    fprintf (verbose ? stdout : stderr, "no forks, so not ok\n");
	    break;

	case OK: 
	    (void) sprintf (fold, "%s%s",
		    *folder == '+' || *folder == '@' ? "" : "+", folder);
	    execlp (fileproc, r1bindex (fileproc, '/'),
		    "-link", "-file", file, fold, NULLCP);
	    _exit (-1);

	default: 
	    if (status = pidwait (child_id, OK)) {
		if (!verbose)
		    fprintf (stderr, "  %sFcc %s: ",
			    msgstate == RESENT ? "Resent-" : "", folder);
		(void) pidstatus (status, verbose ? stdout : stderr, NULLCP);
	    }
	    else
		if (verbose)
		    printf ("folder ok\n");
    }

    (void) fflush (stdout);
}

/*    TERMINATION */

/* VARARGS2 */

static die (what, fmt, a, b, c, d)
char   *what,
       *fmt,
       *a,
       *b,
       *c,
       *d;
{
#ifndef	MHMTS
    (void) unlink (tmpfil);
    if (msgflags & MINV)
	(void) unlink (bccfil);
#endif	/* MHMTS */
#ifdef	MMDFMTS
    if (!whomsw || checksw)
	(void) mm_end (NOTOK);
#endif	/* MMDFMTS */
#ifdef	SENDMTS
    if (!whomsw || checksw)
	(void) sm_end (NOTOK);
#endif	/* SENDMTS */

    adios (what, fmt, a, b, c, d);
}


#ifdef	MMDFMTS
/* 
 *    err_abrt() is used by the mm_ routines
 *    		 do not, under *ANY* circumstances, remove it from post,
 *		 or you will lose *BIG*
 */

err_abrt (code, fmt, a, b, c)
int     code;
char   *fmt,
       *a,
       *b,
       *c;
{
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "[%s]", rp_valstr (code));

    adios (buffer, fmt, a, b, c);
}
#endif	/* MMDFMTS */

/*    STAND-ALONE DELIVERY */

#ifdef	MHMTS

/* BUG: MHMTS ignores 822-style route addresses... */

static  localmail (lp, talk, fd)
register struct mailname *lp;
int     talk,
        fd;
{
    int     md;
    char    mailbox[BUFSIZ],
	    ddate[BUFSIZ];
    register struct home *hp;

    if (talk)
	printf ("  %s: ", lp -> m_mbox);
    (void) fflush (stdout);

    if ((hp = seek_home (lp -> m_mbox)) == NULL) {
	if (!talk)
	    fprintf (stderr, "  %s: ", lp -> m_mbox);
	fprintf (talk ? stdout : stderr,
		"not deliverable; unknown address\n");
	unkadr++;
	return;
    }

    (void) sprintf (mailbox, "%s/%s",
	    mmdfldir[0] ? mmdfldir : hp -> h_home,
	    mmdflfil[0] ? mmdflfil : hp -> h_name);

/*  */

    switch (access (slocalproc, 01)) {
	default: 
	    if (talk)
		printf ("(invoking hook)\n\t");
	    (void) fflush (stdout);

	    if (usr_hook (lp, talk, fd, hp, mailbox) != NOTOK)
		return;
	    if (talk)
		printf ("  %s: ", lp -> m_mbox);
	    (void) fflush (stdout);

	case NOTOK: 
	    (void) lseek (fd, (off_t)0, 0);
	    if ((md = mbx_open (mailbox, hp -> h_uid, hp -> h_gid, m_gmprot ()))
		    == NOTOK) {
		if (!talk)
		    fprintf (stderr, "  %s: ", lp -> m_mbox);
		fprintf (talk ? stdout : stderr,
			"error in transmission; unable to open maildrop\n");
		unkadr++;
		return;
	    }
	    (void) sprintf (ddate, "Delivery-Date: %s\n", dtimenow ());
	    if (mbx_copy (mailbox, md, fd, 0, ddate, 0) == NOTOK) {
		if (!talk)
		    fprintf (stderr, "  %s: ", lp -> m_mbox);
		fprintf (talk ? stdout : stderr,
			"error in transmission; write to maildrop failed\n");
		unkadr++;
		(void) close (md);
		return;
	    }
	    mbx_close (mailbox, md);

	    if (talk)
		printf ("sent\n");
	    break;
    }

    (void) fflush (stdout);
}

/*  */

static int  usr_hook (lp, talk, fd, hp, mailbox)
register struct mailname *lp;
int     talk,
        fd;
register struct home *hp;
register char   *mailbox;
{
    int     i,
            child_id,
            status;
    char    tmpfil[BUFSIZ];

    if ((fd = copyfile (fd, tmpfil)) == NOTOK) {
	if (!talk)
	    fprintf (stderr, "  %s: ", lp -> m_mbox);
	fprintf (talk ? stdout : stderr,
		"unable to copy message; skipping hook\n");
	return NOTOK;
    }
    (void) chown (tmpfil, hp -> h_uid, hp -> h_gid);

    (void) fflush (stdout);

    for (i = 0; (child_id = fork ()) == NOTOK && i < 5; i++)
	sleep (5);
    switch (child_id) {
	case NOTOK: 
	    if (!talk)
		fprintf (stderr, "  %s: ", lp -> m_mbox);
	    fprintf (talk ? stdout : stderr,
		    "unable to invoke hook; fork() failed\n");
	    return NOTOK;

	case OK: 
	    if (fd != 0)
		(void) dup2 (fd, 0);
	    (void) freopen ("/dev/null", "w", stdout);
	    (void) freopen ("/dev/null", "w", stderr);
	    if (fd != 3)	/* backwards compatible... */
		(void) dup2 (fd, 3);
	    closefds (4);
#ifdef	TIOCNOTTY
	    if ((fd = open ("/dev/tty", 2)) != NOTOK) {
		(void) ioctl (fd, TIOCNOTTY, NULLCP);
		(void) close (fd);
	    }
#endif	/* TIOCNOTTY */
#ifdef	BSD42
	    (void) setpgrp (0, getpid ());
#endif	/* BSD42 */

	    *environ = NULL;
	    (void) m_putenv ("USER", hp -> h_name);
	    (void) m_putenv ("HOME", hp -> h_home);
	    (void) m_putenv ("SHELL", hp -> h_shell);
	    if (chdir (hp -> h_home) == NOTOK)
		(void) chdir ("/");
	    (void) umask (0077);
#ifdef	BSD41A
	    (void) inigrp (hp -> h_name, hp -> h_gid);
#endif	/* BSD41A */
	    (void) setgid (hp -> h_gid);
#ifdef	BSD42
	    (void) initgroups (hp -> h_name, hp -> h_gid);
#endif	/* BSD42 */
	    (void) setuid (hp -> h_uid);

	    execlp (slocalproc, r1bindex (slocalproc, '/'),
		    "-file", tmpfil, "-mailbox", mailbox,
		    "-home", hp -> h_home, "-addr", lp -> m_aka,
		    "-user", hp -> h_name, "-sender", from,
		    talk ? "-verbose" : NULLCP, NULLCP);
	    _exit (-1);

/*  */

	default: 
	    (void) close (fd);

	    status = pidwait (child_id, OK);

	    (void) unlink (tmpfil);
	    if (status == 0) {
		if (talk)
		    printf ("accepted\n");
		return OK;
	    }
	    if (!talk)
		fprintf (stderr, "  %s: ", lp -> m_mbox);
	    fprintf (talk ? stdout : stderr,
		    "%s error on hook; status=0%o\n",
		    status & 0x00ff ? "system" : "user",
		    status & 0x00ff ? status & 0xff
		    : (status & 0xff00) >> 8);
	    return NOTOK;
    }
}

/*  */

static int  copyfile (qd, tmpfil)
int     qd;
register char   *tmpfil;
{
    int     i,
            fd;
    char    buffer[BUFSIZ];

    (void) strcpy (tmpfil, m_tmpfil ("hook"));
    if ((fd = creat (tmpfil, 0600)) == NOTOK)
	return NOTOK;
    (void) close (fd);
    if ((fd = open (tmpfil, 2)) == NOTOK)
	return NOTOK;

    (void) lseek (qd, (off_t)0, 0);
    while ((i = read (qd, buffer, sizeof buffer)) > 0)
	if (write (fd, buffer, i) != i) {
	    (void) close (fd);
	    return NOTOK;
	}
    if (i == NOTOK) {
	(void) close (fd);
	return NOTOK;
    }

    (void) lseek (fd, (off_t)0, 0);

    return fd;
}

/*  */

static  uucpmail (lp, talk, fd, from)
register struct mailname *lp;
int     talk,
        fd,
	from;
{
    int     i;
    TYPESIG     (*pstat) ();
    char    addr[BUFSIZ],
            buffer[BUFSIZ];
    register FILE *fp;

    (void) sprintf (addr, "%s!%s", lp -> m_host, lp -> m_mbox);
    if (talk)
	printf ("  %s: ", addr);
    (void) fflush (stdout);

#ifndef	UCI
    (void) sprintf (buffer, "uux -r -p %s!rmail \\(%s\\)",
		lp -> m_host, lp -> m_mbox);
#else	/* UCI */
    (void) sprintf (buffer, "uux -p %s!rmail \\(%s\\)", lp -> m_host,
	    lp -> m_mbox);
#endif	/* UCI */
    if ((fp = popen (buffer, "w")) == NULL) {
	if (!talk)
	    fprintf (stderr, "  %s: ", addr);
	fprintf (talk ? stdout : stderr,
		"unable to start uux; popen() failed\n");
	unkadr++;
	return;
    }

    pstat = signal (SIGPIPE, SIG_IGN);
    if (from) {			/* no mail filtering, so... */
	(void) sprintf (buffer, "From %s %.24s remote from %s\n",
		getusr (), ctime (&tclock), SystemName ());
	i = strlen (buffer);
	if (fwrite (buffer, sizeof *buffer, i, fp) != i)
	    goto oops;
    }

    (void) lseek (fd, (off_t)0, 0);
    while ((i = read (fd, buffer, sizeof buffer)) > 0)
	if (fwrite (buffer, sizeof *buffer, i, fp) != i) {
    oops:   ;
	    if (!talk)
		fprintf (stderr, "  %s: ", addr);
	    fprintf (talk ? stdout : stderr,
		    "error in transmission; write to uux failed\n");
	    unkadr++;
	    (void) pclose (fp);
	    return;
	}
    if (pclose (fp))
	goto oops;
    (void) signal (SIGPIPE, pstat);

    if (i < 0) {
	if (!talk)
	    fprintf (stderr, "  %s: ", addr);
	fprintf (talk ? stdout : stderr,
		"error in transmission; read failed\n");
	unkadr++;
	return;
    }

    if (talk)
	printf ("queued (via uux)\n");
    (void) fflush (stdout);
}

/*  */

#ifdef	MF
static int  make_uucp_file (td)
int     td;
{
    int     i,
            qd,
            fd;
    char    tmpfil[BUFSIZ];

    (void) lseek (td, (off_t)0, 0);
    if ((qd = dup (td)) == NOTOK)
	adios ("fd", "unable to dup");

    (void) strcpy (tmpfil, m_tmpfil ("uumf"));
    if ((fd = creat (tmpfil, 0600)) == NOTOK)
	adios (tmpfil, "unable to create");
    (void) close (fd);
    if ((fd = open (tmpfil, 2)) == NOTOK)
	adios (tmpfil, "unable to re-open");

    switch (i = mmdf2uucp (qd, fd, 1)) {
	case OK: 
	    if (!debug)
		(void) unlink (tmpfil);
	    break;

	default: 
	    adios (NULLCP, "unable to filter mail(%d), examine %s", i, tmpfil);
    }
    (void) close (qd);

    return fd;
}
#endif	/* MF */

/*  */

static  netmail (talk, fd, bccque)
int     talk,
        fd,
        bccque;
{
    int     i,
            naddrs;
    char    buffer[BUFSIZ];
    register struct mailname *lp;

    naddrs = 0;
    if (nm_init (getusr (), &tclock) == NOTOK) {
	for (lp = netaddrs.m_next; lp; lp = lp -> m_next)
	    if (lp -> m_bcc ? bccque : !bccque)
		fprintf (stderr, "  %s at %s: unable to get queue file\n",
			lp -> m_mbox, lp -> m_host);
	return;
    }

    for (lp = netaddrs.m_next; lp; lp = lp -> m_next)
	if (lp -> m_bcc ? bccque : !bccque) {
	    (void) nm_wadr (lp -> m_mbox, lp -> m_host);
	    naddrs++;
	    if (talk)
		printf ("  %s at %s: queued\n", lp -> m_mbox, lp -> m_host);
	    (void) fflush (stdout);
	}
    nm_waend ();

    (void) lseek (fd, (off_t)0, 0);
    while ((i = read (fd, buffer, sizeof buffer)) > 0)
	if (nm_wtxt (buffer, i) == NOTOK) {
	    fprintf (stderr,
		    "error in transmission; write to temporary failed");
	    unkadr += naddrs;
	    return;
	}

    if (i < 0) {
	fprintf (stderr, "error in transmission; read failed\n");
	unkadr += naddrs;
	return;
    }

    if (nm_wtend () == NOTOK) {
	fprintf (stderr, "error in transmission; unable to queue message\n");
	unkadr += naddrs;
	return;
    }
}
#endif	/* MHMTS */
