/* popi.c - POP initiator - for MPOP */
#ifndef	lint
static char ident[] = "@(#)$Id: popi.c,v 1.8 1992/10/28 18:52:45 jromine Exp $";
#endif	lint

#include "../h/mh.h"
#include "../h/formatsbr.h"
#include "../h/scansbr.h"
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#ifdef	SMTP
#include "../h/local.h"
#else	/* SMTP */
#include <sys/stat.h>
#endif	/* SMTP */
#include "../zotnet/mts.h"

/*  */

#ifndef	RPOP
#define	RPOPminc(a)	(a)
#else
#define	RPOPminc(a)	0
#endif

#ifndef	APOP
#define	APOPminc(a)	(a)
#else
#define	APOPminc(a)	0
#endif

#ifndef	BPOP
#define	BPOPminc(a)	(a)
#else
#define	BPOPminc(a)	0
#endif

#ifndef	SMTP
#define	BULKminc(a)	(a)
#else
#define	BULKminc(a)	0
#endif

static struct swit  switches[] = {
#define	APOPSW	0
    "apop", APOPminc (-4),
#define	NAPOPSW	1
    "noapop", APOPminc (-6),

#define	AUTOSW	2
    "auto", BPOPminc(-4),
#define	NAUTOSW	3
    "noauto", BPOPminc(-6),

#define	BULKSW	4
    "bulk directory", BULKminc(-4),

#define	FORMSW	5
    "form formatfile", 0,

#define	FMTSW	6
    "format string", 5,

#define	HOSTSW	7
    "host host", 0,

#define	PROGSW	8
    "mshproc program", 0,

#define	RPOPSW	9
    "rpop", RPOPminc (-4),
#define	NRPOPSW	10
    "norpop", RPOPminc (-6),

#define	USERSW	11
    "user user", 0,

#define	WIDSW	12
    "width columns", 0,

#define	HELPSW	13
    "help", 4,

    NULL, 0
};

/*  */

static	char   *bulksw = NULLCP;
static	int	snoop = 0;
static	int	width = 0;

static	char	mailname[BUFSIZ];

static	char   *nfs = NULL;

static	struct msgs *mp;

extern	int	errno;

extern	char	response[];

/*  */

/* ARGSUSED */

main (argc, argv)
int	argc;
char   *argv[];
{
    int	    autosw = 1,
	    noisy = 1,
	    rpop;
    char   *cp,
	   *maildir,
	   *folder = NULL,
	   *form = NULL,
	   *format = NULL,
	   *host = NULL,
	   *user = NULL,
	   *pass = NULL,
	    buf[100],
	  **ap,
	  **argp,
	   *arguments[MAXARGS];
    struct stat st;

    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
    if (pophost && *pophost)
	host = pophost;
    if ((cp = getenv ("MHPOPDEBUG")) && *cp)
	snoop++;
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

    rpop = getuid () && !geteuid ();

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

		case AUTOSW:
		    autosw = 1;
		    continue;
		case NAUTOSW:
		    autosw = 0;
		    continue;

		case BULKSW: 
		    if (!(bulksw = *argp++) || *bulksw == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
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

		case HOSTSW:
		    if (!(host = *argp++) || *host == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case USERSW:
		    if (!(user = *argp++) || *user == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case APOPSW:
		    rpop = -1;
		    continue;
		case RPOPSW:
		    rpop = 1;
		    continue;
		case NAPOPSW:
		case NRPOPSW:
		    rpop = 0;
		    continue;

		case PROGSW:
		    if (!(mshproc = *argp++) || *mshproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
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

    if (!host)
	adios (NULLCP, "usage: %s -host \"host\"", invo_name);

#ifdef	SMTP
    if (bulksw)
	do_bulk (host);
#endif

    if (user == NULL)
	user = getusr ();
    if (rpop > 0)
	pass = getusr ();
    else {
	(void) setuid (getuid ());
	ruserpass (host, &user, &pass);
    }
    (void) sprintf (mailname, "PO box for %s@%s", user, host);

    if (pop_init (host, user, pass, snoop, rpop) == NOTOK)
	adios (NULLCP, "%s", response);
    if (rpop > 0)
	(void) setuid (getuid ());

    nfs = new_fs (form, format, FORMAT);

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!folder && !(folder = m_find (inbox)))
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

#ifdef	BPOP
    if (autosw)
	msh ();
    else
#endif
	popi ();

    (void) pop_quit ();

    m_replace (pfolder, folder);
    m_setvis (mp, 0);
    m_sync (mp);
    m_update ();

    done (0);

    /* NOTREACHED */
}

/*  */

static struct swit popicmds[] = {
#define	DELECMD	 0
    "dele", 0,
#define	LASTCMD	 1
    "last", 0,
#define	LISTCMD	 2
    "list", 0,
#define	NOOPCMD	 3
    "noop", 0,
#define	QUITCMD	 4
    "quit", 0,
#define	RETRCMD	 5
    "retr", 0,
#define	RSETCMD	 6
    "rset", 0,
#define	SCANCMD	 7
    "scan", 0,
#define	STATCMD	 8
    "stat", 0,
#define	TOPCMD	 9
    "top", 0,
#ifdef	BPOP
#define	MSHCMD	10
    "msh", 0,
#endif

    NULL, 0
};

/*  */

static	popi ()
{
    int	    eof;

    eof = 0;
    for (;;) {
	int	i;
	register char *cp;
	char	buffer[BUFSIZ];

	if (eof)
	    return;

	printf ("(%s) ", invo_name);
	for (cp = buffer; (i = getchar ()) != '\n'; ) {
	    if (i == EOF) {
		(void) putchar ('\n');
		if (cp == buffer)
		    return;
		eof = 1;
		break;
	    }

	    if (cp < buffer + sizeof buffer - 2)
		*cp++ = i;
	}
	*cp = '\0';
	if (buffer[0] == '\0')
	    continue;
	if (buffer[0] == '?') {
	    printf ("commands:\n");
	    printsw (ALL, popicmds, "");
	    printf ("type CTRL-D or use \"quit\" to leave %s\n", invo_name);
	    continue;
	}

	if (cp = index (buffer, ' '))
	    *cp = '\0';
	switch (i = smatch (buffer, popicmds)) {
	    case AMBIGSW:
	        ambigsw (buffer, popicmds);
		continue;
	    case UNKWNSW:
		printf ("%s unknown -- type \"?\" for help\n", buffer);
		continue;
		
	    case QUITCMD:
		return;

	    case STATCMD:
	    case DELECMD:
	    case NOOPCMD:
	    case LASTCMD:
	    case RSETCMD:
	    case TOPCMD:
		if (cp)
		    *cp = ' ';
		(void) pop_command ("%s%s", popicmds[i].sw, cp ? cp : "");
		printf ("%s\n", response);
		break;		

	    case LISTCMD:
		if (cp)
		    *cp = ' ';
		if (pop_command ("%s%s", popicmds[i].sw, cp ? cp : "")
		        == OK) {
		    printf ("%s\n", response);
		    if (!cp)
			for (;;) {
			    switch (pop_multiline ()) {
				case DONE:
				    (void) strcpy (response, ".");
				    /* and fall... */
			        case NOTOK:
				    printf ("%s\n", response);
			            break;

				case OK:
				    printf ("%s\n", response);
				    continue;
			     }
			    break;
			}
		}
		break;

	    case RETRCMD:
		if (!cp) {
		    advise (NULLCP, "missing argument to %s", buffer);
		    break;
		}
		retr_action (NULLCP, OK);
		(void) pop_retr (atoi (++cp), retr_action);
		retr_action (NULLCP, DONE);
		printf ("%s\n", response);
		break;

	    case SCANCMD:
		{
		    char   *dp,
			   *ep,
			   *fp;

		    if (width == 0)
			width = sc_width ();

		    for (dp = nfs, i = 0; *dp; dp++, i++)
			if (*dp == '\\' || *dp == '"' || *dp == '\n')
			    i++;
		    i++;
		    if ((ep = malloc ((unsigned) i)) == NULL)
			adios (NULLCP, "out of memory");
		    for (dp = nfs, fp = ep; *dp; dp++) {
			if (*dp == '\n') {
			    *fp++ = '\\', *fp++ = 'n';
			    continue;
			}
			if (*dp == '"' || *dp == '\\')
			    *fp++ = '\\';
			*fp++ = *dp;
		    }
		    *fp = '\0';

		    (void) pop_command ("xtnd scan %d \"%s\"", width, ep);
		    printf ("%s\n", response);

		    free (ep);
		}
		break;

#ifdef	BPOP
	    case MSHCMD:
		msh ();
		break;
#endif
	}
    }
}

/*  */

static int  retr_action (rsp, flag)
char   *rsp;
int	flag;
{
    static FILE    *fp;

    if (rsp == NULL) {
	static int    msgnum;
	static char  *cp;

	if (flag == OK) {
	    if ((mp = m_remsg (mp, 0, msgnum = mp -> hghmsg + 1)) == NULL)
		adios (NULLCP, "unable to allocate folder storage");

	    cp = getcpy (m_name (mp -> hghmsg + 1));
	    if ((fp = fopen (cp, "w+")) == NULL)
		adios (cp, "unable to write");
	    (void) chmod (cp, m_gmprot ());
	}
	else {
	    struct stat st;

	    (void) fflush (fp);
	    if (fstat (fileno (fp), &st) != NOTOK && st.st_size > 0) {
		mp -> msgstats[msgnum] = EXISTS | UNSEEN;
		mp -> msgflags |= SEQMOD;

		if (ferror (fp))
		    advise (cp, "write error on");
		mp -> hghmsg = msgnum;
	    }
	    else
		(void) unlink (cp);

	    (void) fclose (fp), fp = NULL;
	    free (cp), cp = NULL;
	}

	return;
    }

    fprintf (fp, "%s\n", rsp);
}

/*  */

#ifdef	BPOP
static	msh ()
{
    int	    child_id,
	    vecp;
    char    buf1[BUFSIZ],
	    buf2[BUFSIZ],
	   *vec[9];

    if (pop_fd (buf1, buf2) == NOTOK)
	adios (NULLCP, "%s", response);

    vecp = 0;
    vec[vecp++] = r1bindex (mshproc, '/');
		    
    switch (child_id = fork ()) {
	case NOTOK:
	    adios ("fork", "unable to");

	case OK:
	    vec[vecp++] = "-popread";
	    vec[vecp++] = buf1;
	    vec[vecp++] = "-popwrite";
	    vec[vecp++] = buf2;
	    vec[vecp++] = "-idname";
	    vec[vecp++] = mailname;
	    vec[vecp++] = mailname;
	    vec[vecp] = NULL;
	    (void) execvp (mshproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (mshproc);
	    _exit (-1);

       default:
	    (void) pidXwait (child_id, mshproc);
	    break;
   }
}
#endif

/*  */

#ifdef	SMTP
#include "../zotnet/mts.h"
#include "../mts/sendmail/smail.h"


static int  dselect (d)
register struct direct *d;
{
    int	    i;

    if ((i = strlen (d -> d_name)) < sizeof "smtp"
	    || strncmp (d -> d_name, "smtp", sizeof "smtp" - 1))
	return 0;
    return ((i -= (sizeof ".bulk" - 1)) > 0
	        && !strcmp (d -> d_name + i, ".bulk"));
}


static int  dcompar (d1, d2)
struct direct **d1,
	      **d2;
{
    struct stat s1,
		s2;

    if (stat ((*d1) -> d_name, &s1) == NOTOK)
	return 1;
    if (stat ((*d2) -> d_name, &s2) == NOTOK)
	return -1;
    return ((int) (s1.st_mtime - s2.st_mtime));
}


static	do_bulk (host)
char   *host;
{
    register int    i;
    int	    n,
	    retval,
	    sm;
    struct direct **namelist;

    if (chdir (bulksw) == NOTOK)
	adios (bulksw, "unable to change directory to");

    if ((n = scandir (".", &namelist, dselect, dcompar)) == NOTOK)
	adios (bulksw, "unable to scan directory");

    sm = NOTOK;
    for (i = 0; i < n; i++) {
	register struct direct *d = namelist[i];

	if (sm == NOTOK) {
	    if (rp_isbad (retval = sm_init (NULLCP, host, 1, 1, snoop)))
		adios (NULLCP, "problem initializing server: %s",
		       rp_string (retval));
	    else
		sm = OK;
	}

	switch (retval = sm_bulk (d -> d_name)) {
	    default:
	        if (rp_isbad (retval))
		    adios (NULLCP, "problem delivering msg %s: %s",
			   d -> d_name, rp_string (retval));
		/* else fall... */
	    case RP_OK:
	    case RP_NO:
	    case RP_NDEL:
		advise (NULLCP, "msg %s: %s", d -> d_name, rp_string (retval));
		break;
	}
    }

    if (sm == OK) {
	register int j;
	int	l,
		m;
	struct direct **newlist;

	while ((l = scandir (".", &newlist, dselect, dcompar)) > OK) {
	    m = 0;

	    for (j = 0; j < l; j++) {
		register struct direct *d = newlist[j];

		for (i = 0; i < n; i++)
		    if (strcmp (d -> d_name, namelist[i] -> d_name) == 0)
			break;
		if (i >= n) {
		    switch (retval = sm_bulk (d -> d_name)) {
		        default:
			    if (rp_isbad (retval))
				adios (NULLCP, "problem delivering msg %s: %s",
				       d -> d_name, rp_string (retval));
			    /* else fall... */
			case RP_OK:
			case RP_NO:
			case RP_NDEL:
			    advise (NULLCP, "msg %s: %s", d -> d_name,
				    rp_string (retval));
			    break;
		    }

		    m = 1;
		}
	    }

	    for (i = 0; i < n; i++)
		free ((char *) namelist[i]);
	    free ((char *) namelist);
	    namelist = newlist, n = l;

	    if (!m)
		break;
	    newlist = NULL;
	}
    }

    if (sm == OK && rp_isbad (retval = sm_end (OK)))
	adios (NULLCP, "problem finalizing server: %s", rp_string (retval));

    for (i = 0; i < n; i++)
	free ((char *) namelist[i]);
    free ((char *) namelist);

    free ((char *) namelist);

    done (0);
}
#endif
