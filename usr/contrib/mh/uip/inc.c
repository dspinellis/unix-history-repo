/* inc.c - incorporate messages from a maildrop into a folder */

#include "../h/mh.h"
#ifdef	POP
#include "../h/dropsbr.h"
#endif	POP
#include "../h/formatsbr.h"
#include "../h/scansbr.h"
#include "../zotnet/tws.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>

/*  */

static struct swit switches[] = {
#define	AUDSW	0
    "audit audit-file", 0,
#define	NAUDSW	1
    "noaudit", 0,

#define	CHGSW	2
    "changecur", 0,
#define	NCHGSW	3
    "nochangecur", 0,

#define	DECRSW	4
    "decrypt",
#ifndef	TMA
	-7,
#else	TMA
	0,
#endif	TMA
#define	NDECRSW	5
    "nodecrypt",
#ifndef	TMA
	-9,
#else	TMA
	0,
#endif	TMA

#define	MSW	6
    "file name", 0,

#define	FORMSW	7
    "form formatfile", 0,
#define	FMTSW	8
    "format string", 5,

#define	HOSTSW	9
    "host host",
#ifndef	POP
	-4,
#else	POP
	0,
#endif	POP
#define	USERSW	10
    "user user",
#ifndef	POP
	-4,
#else	POP
	0,
#endif	POP
#define	PACKSW	11
    "pack file",
#ifndef	POP
	-4,
#else	POP
	0,
#endif	POP
#define	NPACKSW	12
    "nopack",
#ifndef	POP
	-6,
#else	POP
	0,
#endif	POP
#define	RPOPSW	13
    "rpop",
#ifndef	RPOP
	-4,
#else	RPOP
	0,
#endif	RPOP
#define	NRPOPSW	14
    "norpop",
#ifndef	RPOP
	-6,
#else	RPOP
	0,
#endif	RPOP

#define	SILSW	15
    "silent", 0,
#define	NSILSW	16
    "nosilent", 0,

#define	TRNCSW	17
    "truncate", 0,
#define	NTRNCSW	18
    "notruncate", 0,

#define	UUCPSW	19
    "uucp",
#ifndef	MF
	-4,
#else	MF
	0,
#endif	MF
#define	NUUCPSW	20
    "nouucp",
#ifndef	MF
	-6,
#else	MF
	0,
#endif	MF

#define	WIDSW	21
    "width columns", 0,

#define	HELPSW	22
    "help", 4,

    NULL, NULL
};

/*  */

extern int  errno;

#ifdef	POP
int  snoop = 0;
extern char response[];

static  char *file = NULL;
static int  size;
static long pos;
static long start;
static long stop;

static  int   pd = NOTOK;
static	FILE *pf = NULL;

int	pop_action (), pop_pack ();
#endif	POP

/*  * /

/* ARGSUSED */

main (argc, argv)
int	argc;
char   *argv[];
{
    int     chgflag = 1,
	    trnflag = 1,
	    decflag = 1,
            noisy = 1,
	    width = 0,
	    uucp = 1,
	    locked = 0,
#ifdef	POP
	    nmsgs,
	    nbytes,
	    p,
#endif	POP
	    rpop = 1,
            i,
	    hghnum,
            msgnum;
    char   *cp,
           *maildir,
           *folder = NULL,
	   *form = NULL,
	   *format = NULL,
           *audfile = NULL,
           *from = NULL,
	   *host = NULL,
	   *user = NULL,
#ifdef	POP
	   *pass = NULL,
#endif	POP
           *newmail,
            buf[100],
          **ap,
          **argp,
           *nfs,
           *arguments[MAXARGS];
    struct msgs *mp;
    struct stat st,
                s1;
    FILE *in, *aud;
#ifdef	MHE
    FILE *mhe;
#endif	MHE

    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
#ifdef	POP
    if (pophost && *pophost)
	host = pophost;
    if ((cp = getenv ("MHPOPDEBUG")) && *cp)
	snoop++;
#endif	POP
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

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
		    (void) sprintf (buf, "%s [+folder] [switches]", invo_name);
		    help (buf, switches);
		    done (1);

		case AUDSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    audfile = getcpy (m_maildir (cp));
		    continue;
		case NAUDSW: 
		    audfile = NULL;
		    continue;

		case CHGSW: 
		    chgflag++;
		    continue;
		case NCHGSW: 
		    chgflag = 0;
		    continue;

		case TRNCSW: 
		    trnflag++;
		    continue;
		case MSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    from = path (cp, TFILE);/* fall */
		case NTRNCSW: 
		    trnflag = 0;
		    continue;

		case SILSW: 
		    noisy = 0;
		    continue;
		case NSILSW: 
		    noisy++;
		    continue;

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    format = NULL;
		    continue;
		case FMTSW: 
		    if (!(format = *argp++) || *format == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    form = NULL;
		    continue;

		case WIDSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    width = atoi (cp);
		    continue;

		case DECRSW:
		    decflag++;
		    continue;
		case NDECRSW:
		    decflag = 0;
		    continue;

		case UUCPSW: 
		    uucp++;
		    continue;
		case NUUCPSW: 
		    uucp = 0;
		    continue;

		case HOSTSW:
		    if (!(host = *argp++) || *host == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case USERSW:
		    if (!(user = *argp++) || *user == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case PACKSW:
#ifndef	POP
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
#else	POP
		    if (!(file = *argp++) || *file == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
#endif	POP
		    continue;
		case NPACKSW:
#ifdef	POP
		    file = NULLCP;
#endif	POP
		    continue;
		case RPOPSW:
		    rpop++;
		    continue;
		case NRPOPSW:
		    rpop = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    adios (NULLCP, "usage: %s [+folder] [switches]", invo_name);
    }

/*  */

#ifdef	POP
    if (host && !*host)
	host = NULL;
    if (from || !host || !rpop)
	(void) setuid (getuid ());
#endif	POP
    if (from) {
	newmail = from;
#ifdef	POP
	host = NULL;
#endif	POP
	if (stat (newmail, &s1) == NOTOK || s1.st_size == 0)
	    adios (NULLCP, "no mail to incorporate");
    }
#ifdef	POP
    else if (host) {
	if (rpop) {
	    if (user == NULL)
		user = getusr ();
	    pass = getusr ();
	}
	else
	    ruserpass (host, &user, &pass);

	if (pop_init (host, user, pass, snoop, rpop) == NOTOK
		|| pop_stat (&nmsgs, &nbytes) == NOTOK)
	    adios (NULLCP, "%s", response);
	if (rpop)
	    (void) setuid (getuid ());
	if (nmsgs == 0) {
	    (void) pop_quit ();
	    adios (NULLCP, "no mail to incorporate");
	}
    }
#endif	POP
    else {
	if (((newmail = getenv ("MAILDROP")) && *newmail)
		|| ((newmail = m_find ("maildrop")) && *newmail))
	    newmail = m_mailpath (newmail);
	else {
#ifdef	MF
	    if (uucp && umincproc && *umincproc)
		get_uucp_mail ();
#endif	MF
	    newmail = concat (MAILDIR, "/", MAILFIL, NULLCP);
	}
	if (stat (newmail, &s1) == NOTOK || s1.st_size == 0)
	    adios (NULLCP, "no mail to incorporate");
    }

#ifdef	POP
    if (host && file)
	goto go_to_it;
#endif	POP
    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!folder)
	folder = defalt;
    maildir = m_maildir (folder);

    if (stat (maildir, &st) == NOTOK) {
	if (errno != ENOENT)
	    adios (maildir, "error on folder");
	cp = concat ("Create folder \"", maildir, "\"? ", NULLCP);
	if (noisy && !getanswer (cp))
	    done (1);
	free (cp);
	if (!makedir (maildir))
	    adios (NULLCP, "unable to create folder %s", maildir);
    }

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);

/*  */

#ifdef	POP
go_to_it: ;
    if (host == NULL)
#endif	POP
    if (access (newmail, 02) == NOTOK) {
	trnflag = 0;
	if ((in = fopen (newmail, "r")) == NULL)
	    adios (newmail, "unable to read");
    }
    else {
	locked++;
	if (trnflag) {
	    (void) signal (SIGHUP, SIG_IGN);
	    (void) signal (SIGINT, SIG_IGN);
	    (void) signal (SIGQUIT, SIG_IGN);
	    (void) signal (SIGTERM, SIG_IGN);
	}
	if ((in = lkfopen (newmail, "r")) == NULL)
	    adios (NULLCP, "unable to lock and fopen %s", newmail);
	(void) fstat (fileno(in), &s1);
    }

    if (audfile) {
	if ((i = stat (audfile, &st)) == NOTOK)
	    advise (NULLCP, "Creating Receive-Audit: %s", audfile);
	if ((aud = fopen (audfile, "a")) == NULL)
	    adios (audfile, "unable to append to");
	else
	    if (i == NOTOK)
		(void) chmod (audfile, m_gmprot ());
#ifndef	POP
	fprintf (aud, from ? "<<inc>> %s  -ms %s\n" : "<<inc>> %s\n",
		dtimenow (), from);
#else	POP
	fprintf (aud, from ? "<<inc>> %s -ms %s\n"
			: host ? "<<inc>> %s -host %s -user %s%s\n"
			: "<<inc>> %s\n",
		dtimenow (), from ? from : host, user, rpop ? " -rpop" : "");
#endif	POP
    }

#ifdef	MHE
    if (m_find ("mhe")) {
	cp = concat (maildir, "/++", NULLCP);
	i = stat (cp, &st);
	if ((mhe = fopen (cp, "a")) == NULL)
	    admonish (cp, "unable to append to");
	else
	    if (i == NOTOK)
		(void) chmod (cp, m_gmprot ());
	free (cp);
    }
#endif	MHE

    nfs = new_fs (form, format, FORMAT);

    if (noisy) {
	printf ("Incorporating new mail into %s...\n\n", folder);
	(void) fflush (stdout);
    }

/*  */

#ifdef	POP
    if (host) {
	if (file) {
	    file = path (file, TFILE);
	    if (stat (file, &st) == NOTOK) {
		if (errno != ENOENT)
		    adios (file, "error on file");
		cp = concat ("Create file \"", file, "\"? ", NULLCP);
		if (noisy && !getanswer (cp))
		    done (1);
		free (cp);
	    }
	    msgnum = map_count ();
	    if ((pd = mbx_open (file, getuid (), getgid (), m_gmprot ()))
		    == NOTOK)
		adios (file, "unable to open");
	    if ((pf = fdopen (pd, "w+")) == NULL)
		adios (NULLCP, "unable to fdopen %s", file);
	}
	else {
	    hghnum = msgnum = mp -> hghmsg;
	    if ((mp = m_remsg (mp, 0, mp -> hghmsg + nmsgs)) == NULL)
		adios (NULLCP, "unable to allocate folder storage");
	}

	for (i = 1; i <= nmsgs; i++) {
	    msgnum++;
	    if (file) {
		(void) fseek (pf, 0L, 1);
		pos = ftell (pf);
		size = 0;
		(void) fwrite (mmdlm1, 1, strlen (mmdlm1), pf);
		start = ftell (pf);

		if (pop_retr (i, pop_pack) == NOTOK)
		    adios (NULLCP, "%s", response);

		(void) fseek (pf, 0L, 1);
		stop = ftell (pf);
		if (fflush (pf))
		    adios (file, "write error on");
		(void) fseek (pf, start, 0);
	    }
	    else {
		cp = getcpy (m_name (msgnum));
		if ((pf = fopen (cp, "w+")) == NULL)
		    adios (cp, "unable to write");
		(void) chmod (cp, m_gmprot ());
		start = stop = 0L;

		if (pop_retr (i, pop_action) == NOTOK)
		    adios (NULLCP, "%s", response);

		if (fflush (pf))
		    adios (cp, "write error on");
		(void) fseek (pf, 0L, 0);
	    }
	    switch (p = scan (pf, msgnum, 0, nfs, width,
			file ? 0 : msgnum == mp -> hghmsg + 1 && chgflag,
			0, stop - start, noisy)) {
		case SCNEOF: 
		    printf ("%*d  empty\n", DMAXFOLDER, msgnum);
		    break;

		case SCNERR: 
		case SCNNUM: 
		    break;

		case SCNMSG: 
		case SCNENC:
		default: 
		    if (aud)
			fputs (scanl, aud);
#ifdef	MHE
		    if (mhe)
			fputs (scanl, mhe);
#endif	MHE
		    if (noisy)
			(void) fflush (stdout);
		    if (!file) {
			mp -> msgstats[msgnum] = EXISTS;
#ifdef	TMA
			if (p == SCNENC) {
			    if (mp -> lowsel == 0 || msgnum < mp -> lowsel)
				mp -> lowsel = msgnum;
			    if (mp -> hghsel == 0 || msgnum > mp -> hghsel)
				mp -> hghsel = msgnum;
			    mp -> numsel++;
			    mp -> msgstats[msgnum] |= SELECTED;
			}
#endif	TMA
			mp -> msgstats[msgnum] |= UNSEEN;
			mp -> msgflags |= SEQMOD;
		    }
		    break;
		}
	    if (file) {
		(void) fseek (pf, stop, 0);
		(void) fwrite (mmdlm2, 1, strlen (mmdlm2), pf);
		if (fflush (pf))
		    adios (file, "write error on");
		(void) map_write (file, pd, 0, start, stop, pos, size, noisy);
	    }
	    else {
		(void) fclose (pf);
		free (cp);
	    }

	    if (trnflag && pop_dele (i) == NOTOK)
		adios (NULLCP, "%s", response);
	}
	if (pop_quit () == NOTOK)
	    adios (NULLCP, "%s", response);
	if (file) {
	    (void) mbx_close (file, pd);
	    pd = NOTOK;
	}
    }
    else {
#endif	POP

/*  */

    m_unknown (in);		/* the MAGIC invocation... */
    hghnum = msgnum = mp -> hghmsg;
    for (;;) {
	if (msgnum >= mp -> hghoff)
	    if ((mp = m_remsg (mp, 0, mp -> hghoff + MAXFOLDER)) == NULL)
		adios (NULLCP, "unable to allocate folder storage");

	switch (i = scan (in, msgnum + 1, msgnum + 1, nfs, width,
		    msgnum == hghnum && chgflag,
		    0,
		    0L,
		    noisy)) {
	    case SCNEOF: 
		break;

	    case SCNERR: 
		if (aud)
		    fputs ("inc aborted!\n", aud);
		adios (NULLCP, "aborted!");

	    case SCNNUM: 
		adios (NULLCP, "more than %d messages in folder %s, %s not zero'd",
			MAXFOLDER, folder, newmail);

	    default: 
		adios (NULLCP, "scan() botch (%d)", i);

	    case SCNMSG:
	    case SCNENC:
		if (aud)
		    fputs (scanl, aud);
#ifdef	MHE
		if (mhe)
		    fputs (scanl, mhe);
#endif	MHE
		if (noisy)
		    (void) fflush (stdout);

		msgnum++, mp -> hghmsg++;
		mp -> msgstats[msgnum] = EXISTS;
#ifdef	TMA
		if (i == SCNENC) {
		    if (mp -> lowsel == 0 || mp -> lowsel > msgnum)
			mp -> lowsel = msgnum;
		    if (mp -> hghsel == 0 || mp -> hghsel < msgnum)
			mp -> hghsel = msgnum;
		    mp -> numsel++;
		    mp -> msgstats[msgnum] |= SELECTED;
		}
#endif	TMA
		mp -> msgstats[msgnum] |= UNSEEN;
		mp -> msgflags |= SEQMOD;
		continue;
	}
	break;
    }
#ifdef	POP
    }
#endif	POP

    if (aud)
	(void) fclose (aud);
#ifdef	MHE
    if (mhe)
	(void) fclose (mhe);
#endif	MHE
    if (noisy)
	(void) fflush (stdout);
#ifdef	POP
    if (host && file)
	done (0);
#endif	POP

/*  */

#ifdef	POP
    if (host == NULL)
#endif	POP
    if (trnflag) {
	if (stat (newmail, &st) != NOTOK && s1.st_mtime != st.st_mtime)
	    advise (NULLCP, "new messages have arrived!\007");
	else {
	    if ((i = creat (newmail, 0600)) != NOTOK)
		(void) close (i);
	    else
		admonish (newmail, "error zero'ing");
	    (void) unlink (map_name (newmail));
	}
    }
    else
	if (noisy)
	    printf ("%s not zero'd\n", newmail);

    if (msgnum == hghnum)
	admonish (NULLCP, "no messages incorporated");
    else {
	m_replace (pfolder, folder);
	if (chgflag)
	    mp -> curmsg = hghnum + 1;
	mp -> hghmsg = msgnum;
	if (mp -> lowmsg == 0)
	    mp -> lowmsg = 1;
	if (chgflag)		/* sigh... */
	    m_setcur (mp, mp -> curmsg);
    }

#ifdef	POP
    if (host == NULL)
#endif	POP
    if (locked)
	(void) lkfclose (in, newmail);
    else
	(void) fclose (in);

    m_setvis (mp, 0);
    m_sync (mp);
    m_update ();

#ifdef	TMA
    if (decflag && mp -> numsel > 0) {
	if (noisy) {
	    printf ("\nIncorporating encrypted mail into %s...\n\n", folder);
	    (void) fflush (stdout);
	}

	tmastart ();
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED && decipher (msgnum) == OK) {
		if ((in = fopen (cp = m_name (msgnum), "r")) == NULL) {
		    admonish (cp, "unable to open message");
		    free (cp);
		    continue;
		}
		switch (scan (in, msgnum, 0, nfs, width,
			msgnum == mp -> curmsg,
			0,
			fstat (fileno (in), &st) != NOTOK ? (long) st.st_size
			    : 0L,
			noisy)) {
		    case SCNEOF: 
			printf ("%*d  empty\n", DMAXFOLDER, msgnum);
			break;

		    default: 
			break;
		}
		(void) fclose (in);
		free (cp);
	    }
	tmastop ();

	if (noisy)
	    (void) fflush (stdout);
    }
#endif	TMA

    done (0);
}

/*  */

#ifdef	POP
void	done (status)
int	status;
{
    if (file && pd != NOTOK)
	(void) mbx_close (file, pd);

    exit (status);
}
#endif	POP
/*  */

#ifdef MF
get_uucp_mail () {
    int     child_id;
    char    buffer[BUFSIZ];
    struct stat st;

    (void) sprintf (buffer, "%s/%s", UUCPDIR, UUCPFIL);
    if (stat (buffer, &st) == NOTOK || st.st_size == 0)
	return;

    switch (child_id = vfork ()) {
	case NOTOK: 
	    admonish ("fork", "unable to");
	    break;

	case OK: 
	    execlp (umincproc, r1bindex (umincproc, '/'), NULLCP);
	    fprintf (stderr, "unable to exec ");
	    perror (umincproc);
	    _exit (-1);

	default: 
	    (void) pidXwait (child_id, umincproc);
	    break;
    }
}
#endif	MF

/*  */

#ifdef	POP
static int  pop_action (s)
register char *s;
{
    fprintf (pf, "%s\n", s);
    stop += strlen (s) + 1;
}


static int  pop_pack (s)
register char *s;
{
    register int    j;
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "%s\n", s);
    for (j = 0; (j = stringdex (mmdlm1, buffer)) >= 0; buffer[j]++)
	continue;
    for (j = 0; (j = stringdex (mmdlm2, buffer)) >= 0; buffer[j]++)
	continue;
    fputs (buffer, pf);
    size += strlen (buffer) + 1;
}

static int  map_count () {
    int     md;
    char   *cp;
    struct drop d;
    struct stat st;

    if (stat (file, &st) == NOTOK)
	return 0;
    if ((md = open (cp = map_name (file), 0)) == NOTOK
	    || map_chk (cp, md, &d, (long) st.st_size, 1)) {
	if (md != NOTOK)
	    (void) close (md);
	return 0;
    }
    (void) close (md);
    return (d.d_id);
}
#endif	POP
