/* bbc.c - ZOTnet BBoard checker */
#ifndef	lint
static char ident[] = "@(#)$Id: bbc.c,v 2.13 1993/02/26 21:58:57 jromine Exp $";
#endif	lint

#include "../h/mh.h"
#include "../zotnet/bboards.h"
#include <stdio.h>
#ifdef	BPOP
#include "../zotnet/mts.h"
#endif	BPOP
#include <errno.h>
#include <signal.h>
#ifndef	sigmask
#define	sigmask(s)	(1 << ((s) - 1))
#endif	not sigmask
#ifdef	ridge
#undef	SIGTSTP
#endif	ridge
#include <sys/types.h>
#include <sys/stat.h>
#ifdef	SIGTSTP
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif	SIGTSTP
#ifdef LOCALE
#include	<locale.h>
#endif

#define	RCFILE	".bbrc"

#define	NBB	100

/*  */

static struct swit switches[] = {
#define	TOPICSW	0
    "topics", 6,
#define	CHECKSW	1
    "check", 5,
#define	READSW	2
    "read", 4,

#define	QUIETSW	3
    "quiet", 4,
#define	VERBOSW	4
    "verbose", 4,

#define ARCHSW	5
    "archive", 4,
#define NOARCH  6
    "noarchive", 3,

#define	PROTSW	7
    "protocol", 4,
#define	NPROTSW	8
    "noprotocol", 3,

#define	PROGSW	9
    "mshproc program", 4,

#define	RCSW	10
    "rcfile rcfile", 4,
#define	NRCSW	11
    "norcfile", 3,

#define	FILESW	12
    "file BBoardsfile", 4,
#define	USERSW	13
    "user BBoardsuser",
#ifndef	NNTP
	4,
#else	NNTP
	-4,
#endif	NNTP

#define	HOSTSW	14
    "host host",
#ifndef	BPOP
    -4,
#else	BPOP
    4,
#endif	BPOP
#define	RPOPSW	15
    "rpop",
#ifndef	RPOP
	-4,
#else	RPOP
	4,
#endif	RPOP
#define	NRPOPSW	16
    "norpop",
#ifndef	RPOP
	-6,
#else	RPOP
	6,
#endif	RPOP

#define	HELPSW	17
    "help", 4,

    NULL, NULL
};

struct bbcount {
    char   *key;
    int     count;
    struct bbcount *left;
    struct bbcount *right;
};

/*  */

extern int  errno;

static int  changed = 0;
static int  oops = 0;
static int  quitting = 0;

static int  archivesw = 0;
static int  checksw = 0;
static int  protsw = 1;
static int  quietsw = 0;
static int  readsw = 0;
static int  topicsw = 0;
static int  verbosw = 0;

static int  didpop = OK;
static int  rpop = 1;
static char *user = BBOARDS;
static char *host = NULL;
#ifdef	BPOP
extern char response[];

extern char   *getusr ();
static char  **getip ();
#endif	BPOP

TYPESIG	sigser (), hupser ();
int	action ();
#ifdef	SIGTSTP
int	tstpid;
static  TYPESIG	tstpser ();
#endif	SIGTSTP

static char *rcfile=NULL;


static struct bbcount  *bbc = NULL;
static struct bboard   *bbl = NULL;

struct bbcount *add_count (), *seek_count ();
struct bboard *getbbaux (), *getbbvis ();
static	void	bbreset();

#ifdef	UCL
extern	char	   *bbs[];
extern	int	   called_bbc;
static	int bbp;
#endif	UCL
/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
#ifndef	UCL
    int	    bbp = 0,
	    vecp = 1;
#else	UCL
    int    vecp = 1;
#endif	UCL
    char   *cp,
	   *rc=NULL,
          **ap,
          **argp,
            buffer[80],
           *arguments[MAXARGS],
#ifndef	UCL
	   *bbs[NBB + 1],
#endif	UCL
	   *vec[MAXARGS];
#ifdef	UCL
    called_bbc = 1;
    bbp = 0;
#endif	UCL

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
#ifdef	BPOP
    mts_init (invo_name);
    if (popbbhost && *popbbhost)
	host = popbbhost;
    if (popbbuser && *popbbuser)
	user = popbbuser, rpop = 0;
#endif	BPOP
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

    (void) setbbent (SB_STAY);

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		   vec[vecp++] = --cp;
		   continue;
		case HELPSW: 
		    (void) sprintf (buffer,
		    	"%s [bboards ...] [switches] [switches for mshproc]",
			invo_name);
	    	    help (buffer, switches);
		    done (1);

		case TOPICSW: 
		    topicsw++;
		    checksw = readsw = 0;
		    continue;
		case CHECKSW: 
		    checksw++;
		    readsw = topicsw = 0;
		    continue;
		case READSW: 
		    readsw++;
		    checksw = topicsw = 0;
		    continue;

		case ARCHSW: 
		    archivesw++;
		    continue;
		case NOARCH: 
		    archivesw = 0;
		    continue;

		case PROTSW: 
		    protsw++;
		    continue;
		case NPROTSW: 
		    protsw = 0;
		    continue;

		case QUIETSW: 
		    quietsw++;
		    verbosw = 0;
		    continue;
		case VERBOSW: 
		    verbosw++;
		    quietsw = 0;
		    continue;

		case PROGSW:
		    if (!(mshproc = *argp++) || *mshproc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case RCSW:
		    if (!(rc = *argp++) || *rc == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case NRCSW:
		    rc = NULL;
		    continue;

		case FILESW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    if (!setbbinfo (user, cp, 1))
			adios (NULLCP, "setbbinfo(%s, %s, 1) failed -- %s",
				user, cp, getbberr ());
		    continue;
		case USERSW:
		    if (!(user = *argp++) || *user == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case HOSTSW:
		    if (!(host = *argp++) || *host == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    didpop = NOTOK;
		    continue;
		case RPOPSW:
		    rpop++;
		    continue;
		case NRPOPSW:
		    rpop = 0;
		    continue;
	    }
	if (bbp < NBB)
	    bbs[bbp++] = cp;
	else
	    adios (NULLCP, "too many bboards, starting with %s", cp);
    }
    bbs[bbp] = NULL;

/*  */

#ifdef	BPOP
    if (host && !*host)
	host = NULL, didpop = OK;
    if (!host || !rpop)
	(void) setuid (getuid ());
#endif	BPOP

    if (!m_find ("path"))
	free (path ("./", TFOLDER));

    rcinit (rc);

    for (bbp = 0; cp = bbs[bbp]; bbp++)
	add_bb (cp, NOTOK);

#ifdef UCL
    if (topicsw) {
	called_bbc = 0;
	topics ();
	}
#else
    if (topicsw)
	topics ();
#endif
    else {
	default_bboards ();
	if (checksw)
	    check ();
	else
	    process (vecp, vec);
    }

#ifdef	BPOP
    if (didpop != OK && pop_quit () == NOTOK)
	adios (NULLCP, "%s", response);
#endif	BPOP

    done (0);
}

/*  */

topics () {
    register char  *cp,
                  **ap;
    register struct bboard *bb;

    printf ("%16s %s   %s\n", "BBoard", "Items",
	    verbosw ? "Interesting Facts" : "Last Update");
    printf ("%16s %s   %s\n", "------", "-----",
	    verbosw ? "-----------------" : "-----------");

    for (bb = bbl ? bbl : getbbvis ();
	    bb;
	    bb = bbl ? bb -> bb_link : getbbvis ()) {
	printf ("%16s %5d   %s\n",
		bb -> bb_name, bb -> bb_maxima,
		bb -> bb_date ? bb -> bb_date : "no deliveries");
	if (verbosw) {
	    if (*bb -> bb_aka) {
		cp = NULL;
		for (ap = bb -> bb_aka; *ap; ap++)
		    cp = add (*ap, cp ? add (", ", cp) : cp);
		printv ("AKA", cp);
		free (cp);
	    }

	    printv ("Leaders", *bb -> bb_leader);
	    for (ap = bb -> bb_leader + 1; *ap; ap++)
		printv (NULLCP, *ap);
	    printv ("File", bb -> bb_file);
	    printv ("Archive", bb -> bb_archive);
	    printv ("Info", bb -> bb_info);
	    printv ("Map", bb -> bb_map);
	    printv ("Password", bb -> bb_passwd);
	    if (strcmp (bb -> bb_name, bb -> bb_addr))
		printv ("Address", bb -> bb_addr);
	    if (strcmp (*bb -> bb_leader, bb -> bb_request))
		printv ("Request", bb -> bb_request);
	    if (*bb -> bb_relay)
		printv ("Relay", bb -> bb_relay);
	    if (*bb -> bb_dist) {
		changed = 0;
		(void) getbbdist (bb, action);
		if (!changed)
		    printv ("Dist", "");
		if (cp = getbberr ())
		    printv ("Error", cp);
	    }
	    printb (bb -> bb_flags & ~BB_SEEN);
	}
    }
}

/*  */

printv (key, value)
register char   *key,
	        *value;
{
    char    buffer[BUFSIZ];

    if (key)
	(void) sprintf (buffer, "%s: ", key);
    else
	buffer[0] = '\0';
    printf ("%*s%-*s", 25, "", 10, buffer);
    if (value && *value)
	printf ("%s", value);
    (void) putchar ('\n');
}


int     action (local, domain)
register char   *local,
		*domain;
{
    char    buffer[BUFSIZ];

    (void) sprintf (buffer, "%s@%s", local, domain);
    printv (changed++ ? NULL : "Dist", buffer);
    return 0;
}


printb (flags)
unsigned int flags;
{
    char buffer[BUFSIZ];

    printv ("Flags", sprintb (buffer, flags, BBITS));
}

/*  */

check () {
#define	grammar(a,b,c)	(a == 1 ? b : c)
#define	plural(d)	grammar(d, "", "s")

    int     diff;
    register struct bboard  *bb;

    for (bb = bbl; bb; bb = bb -> bb_link) {
	diff = bb -> bb_maxima - bb -> bb_count;
	if (quietsw) {
	    if (diff > 0)
		printf ("%s -- %d item%s unseen\n",
			bb -> bb_name, diff, plural (diff));
	}
	else
	    if (bb -> bb_maxima == 0)
		printf ("%s -- empty\n", bb -> bb_name);
	    else
		if (bb -> bb_count == 0)
		    printf ("%s -- %d item%sseen)\n",
			    bb -> bb_name, bb -> bb_maxima,
			    grammar (bb -> bb_maxima, " (un", "s (none "));
		else
		    if (diff <= 0)
			printf ("%s -- %d item%s (all seen)\n",
				bb -> bb_name, bb -> bb_maxima,
				plural (bb -> bb_maxima));
		    else
			printf ("%s -- %d item%s unseen\n",
				bb -> bb_name, diff, plural (diff));
    }
}

/*  */

process (vecp, vec)
int	vecp;
char   *vec[];
{
    int     diff;
#ifdef	SIGTSTP
    TYPESIG	    (*tstat) ();
#endif	SIGTSTP
    register struct bboard *bb;

    vec[0] = r1bindex (mshproc, '/');
#ifdef	SIGTSTP
    tstat = signal (SIGTSTP, tstpser);
#endif	SIGTSTP

    for (bb = bbl; bb && !quitting; bb = bb -> bb_link) {
	diff = bb -> bb_maxima - bb -> bb_count;
	if (bb -> bb_maxima == 0) {
	    if (!quietsw)
		printf ("%s -- empty\n", bb -> bb_name);
	    continue;
	}
	else {
	    if (diff < 0) {
		printf (
		"Oops! looks like someone reset %s -- assuming all unseen\n",
		    bb -> bb_name);
		diff = bb -> bb_maxima;
		bbreset (bb, 0);
	    }
	    if (verbosw || archivesw || diff > 0)
		bbread (bb, vecp, vec);
	    else
		if (!quietsw)
		    printf ("%s -- %d item%s (all seen)\n",
			    bb -> bb_name, bb -> bb_maxima,
			    plural (bb -> bb_maxima));
	}
    };

#ifdef	SIGTSTP
    (void) signal (SIGTSTP, tstat);
#endif	SIGTSTP
    rcend ();
}

/*  */

#ifdef	BPOP
/* ARGSUSED */

static int  xtnd1 (s)
char   *s;
{
    return OK;
}
#endif	BPOP

/*  */

bbread (bb, vecp, vec)
register struct bboard  *bb;
int	vecp;
char   *vec[];
{
    int     child_id,
            pd[2];
    char    buf1[BUFSIZ],
            buf2[BUFSIZ],
            buf3[BUFSIZ];
#ifdef	BPOP
    int	    nmsgs,
	    nbytes;
    char    buf4[BUFSIZ],
            buf5[BUFSIZ];
#endif	BPOP
    struct stat st;

#ifdef	BPOP
    if (bb -> bb_flags & BB_REMOTE) {
	if (pop_xtnd (xtnd1, "%s %s", archivesw ? "archive" : "bboards",
			bb -> bb_name) == NOTOK) {
	    advise (NULLCP, "%s", response);
	    return;
	}
	if (pop_stat (&nmsgs, &nbytes) == NOTOK)
	    adios (NULLCP, "%s", response);
	if (nmsgs == 0) {
	    if (verbosw)
		printf ("%s -- empty\n", bb -> bb_name);
	    return;
	}
	if (pop_fd (buf4, buf5) == NOTOK)
	    adios (NULLCP, "%s", response);
    }
    else
#endif	BPOP
    if (stat (archivesw ? bb -> bb_archive : bb -> bb_file, &st) != NOTOK
	    && st.st_size == 0)
	return;

    if (protsw) {
	if (pipe (pd) == NOTOK)
	    adios ("pipe", "unable to");
	(void) sprintf (buf3, "%d", getpid ());
    }

    switch (child_id = fork ()) {
	case NOTOK: 
	    adios ("fork", "unable to");

	case OK: 
	    if (protsw) {
		(void) close (pd[0]);
		(void) sprintf (buf1, "%d", bb -> bb_count + 1);
		(void) sprintf (buf2, "%d", pd[1]);
		vec[vecp++] = "-idname";
		vec[vecp++] = bb -> bb_name;
		vec[vecp++] = "-idstart";
		vec[vecp++] = buf1;
		vec[vecp++] = "-idstop";
		vec[vecp++] = buf2;
		vec[vecp++] = "-idquit";
		vec[vecp++] = buf3;
	    }
#ifdef	BPOP
	    if (bb -> bb_flags & BB_REMOTE) {
		vec[vecp++] = "-popread";
		vec[vecp++] = buf4;
		vec[vecp++] = "-popwrite";
		vec[vecp++] = buf5;
	    }
#endif	BPOP
	    vec[vecp++] = archivesw ? bb -> bb_archive : bb -> bb_file;
	    vec[vecp] = NULL;
	    execvp (mshproc, vec);
	    fprintf (stderr, "unable to exec ");
	    perror (mshproc);
	    _exit (-1);

	default: 
#ifdef	SIGTSTP
	    tstpid = child_id;
#endif	SIGTSTP
	    if (protsw) {
		(void) close (pd[1]);
		pgmread (pd[0], child_id, bb);
	    }
	    else
		(void) pidXwait (child_id, mshproc);
    }
}

/*  */

pgmread (pd, child_id, bb)
int     pd,
        child_id;
register struct bboard  *bb;
{
    int     i,
            j,
            n;
    TYPESIG (*estat) (), (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
    char    buffer[BUFSIZ];
    struct bbcount *selected;

    estat = signal (SIGEMT, sigser);
    hstat = signal (SIGHUP, hupser);
    istat = signal (SIGINT, SIG_IGN);
    qstat = signal (SIGQUIT, SIG_IGN);
    tstat = signal (SIGTERM, sigser);

    while ((n = read (pd, buffer, sizeof buffer)) == NOTOK && errno == EINTR)
	continue;
    (void) close (pd);
    (void) pidXwait (child_id, mshproc);

    (void) signal (SIGEMT, estat);
    (void) signal (SIGHUP, hstat);
    (void) signal (SIGINT, istat);
    (void) signal (SIGQUIT, qstat);
    (void) signal (SIGTERM, tstat);

    if (n <= 0)
	return;
    if (sscanf (buffer, "%d %d", &i, &j) != 2 || i <= 0 || j <= 0)
	return;

    if ((selected = seek_count (bbc, bb -> bb_name)) == NULL) {
	bbc = add_count (bbc, bb -> bb_name, i);
	changed++;
    }
    else
	if (archivesw) {
	    if (i > selected -> count) {
		selected -> count = i;
		changed++;
	    }
	}
	else {
	    if (bb -> bb_maxima > j && i >= j)/* bbl... */
		i = bb -> bb_maxima;
	    if (i != selected -> count) {
		selected -> count = i;
		changed++;
	    }
	}
}


/* ARGSUSED */

TYPESIG	sigser (i)
int     i;
{
#ifndef	BSD42
    (void) signal (i, sigser);
#endif	not BSD42
    quitting++;
}


/* ARGSUSED */

TYPESIG	hupser (i)
int	i;
{
    static int armed = 0;

#ifndef	BSD42
    (void) signal (i, hupser);
#endif

    if (!armed++)		/* tick tock... */
	alarm ((unsigned int) 30);
}

/*  */

rcinit (rc)
register char  *rc;
{
    int     state;
    register char  *cp;
    char    key[NAMESZ],
            value[BUFSIZ];
    register FILE  *bbrc;

    if ((cp = rc ? rc : getenv ("MHBBRC")) && *cp) {
	rcfile = path (cp, TFILE);
	if (*cp != '/')
	    (void) m_putenv ("MHBBRC", rcfile);
    }
    else
	rcfile = concat (mypath, "/", RCFILE, NULLCP);

    if ((bbrc = fopen (rcfile, "r")) == NULL)
	if (cp && *cp)
	    adios (rcfile, "unable to read");
	else
	    return;

    for (state = FLD;;) {
	switch (state = m_getfld (state, key, value, sizeof value, bbrc)) {
	    case FLD: 
	    case FLDEOF: 
		make_lower (key, key);
		bbc = add_count (bbc, key, atoi (value));
		if (state == FLDEOF)
		    break;
		continue;

	    default: 
		admonish (NULLCP, "bad format: %s", rcfile);
	    case FILEEOF:
		break;
	}

	break;
    }

    if (ferror (bbrc) && !feof (bbrc))
	admonish (rcfile, "error reading");
    (void) fclose (bbrc);
}

/*  */

rcend () {
    TYPESIG (*hstat) (), (*istat) (), (*qstat) (), (*tstat) ();
    register FILE *bbrc;

    if (!changed)
	return;

    hstat = signal (SIGHUP, SIG_IGN);
    istat = signal (SIGINT, SIG_IGN);
    qstat = signal (SIGQUIT, SIG_IGN);
    tstat = signal (SIGTERM, SIG_IGN);

    if ((bbrc = fopen (rcfile, "w")) == NULL)
	adios (rcfile, "unable to write");
    rcput (bbrc, bbc);

    if (ferror (bbrc))
	adios (rcfile, "error writing");
    (void) fclose (bbrc);

    (void) signal (SIGHUP, hstat);
    (void) signal (SIGINT, istat);
    (void) signal (SIGQUIT, qstat);
    (void) signal (SIGTERM, tstat);

    changed = 0;
}


rcput (bbrc, p)
register FILE *bbrc;
register struct bbcount *p;
{
    if (p == NULL)
	return;

    fprintf (bbrc, "%s: %d\n", p -> key, p -> count);
    rcput (bbrc, p -> left);
    rcput (bbrc, p -> right);
}

/*  */

#ifdef	SIGTSTP
static TYPESIG  tstpser (sig)
int	sig;
{
    int	    pid;
#if defined(BSD42) && !defined(WAITINT)
    union wait w;
#else
    int	w;
#endif

    rcend ();

#ifdef SVR4
    waitpid(tstpid, &w, WUNTRACED);
#else
    while ((pid = wait3 (&w, WUNTRACED, (struct rusage *) 0)) != NOTOK
	    && pid != tstpid)
	continue;
#endif

    (void) signal (SIGTSTP, SIG_DFL);
#ifdef	BSD42
    (void) sigsetmask (sigblock (0) & ~sigmask (SIGTSTP));
#endif	BSD42

    (void) kill (getpid (), sig);

#ifdef	BSD42
    (void) sigblock (sigmask (SIGTSTP));
#endif	BSD42
    (void) signal (SIGTSTP, tstpser);
}
#endif	SIGTSTP

/*  */
static	void	bbreset (bb, i)
register struct bboard  *bb;
int i;
{
    struct bbcount *selected;

    bb -> bb_count = i;
    if ((selected = seek_count (bbc, bb -> bb_name)) == NULL)
	bbc = add_count (bbc, bb -> bb_name, i);
    else
	selected -> count = i;

    changed++;
}

struct bbcount *add_count (p, w, i)
register struct bbcount *p;
register char   *w;
int     i;
{
    int     cond;

    if (p == NULL) {
	p = (struct bbcount *) malloc (sizeof *p);
	if (p == NULL)
	    adios (NULLCP,"insufficient memory");
	p -> key = getcpy (w);
	p -> count = i;
	p -> left = p -> right = NULL;
    }
    else
	if ((cond = strcmp (w, p -> key)) < 0)
	    p -> left = add_count (p -> left, w, i);
	else
	    if (cond > 0)
		p -> right = add_count (p -> right, w, i);

    return p;
}


struct bbcount *seek_count (p, w)
register struct bbcount *p;
register char   *w;
{
    int     cond;

    if (p == NULL || (cond = strcmp (w, p -> key)) == 0)
	return p;
    else
	return seek_count (cond < 0 ? p -> left : p -> right, w);
}

/*  */

default_bboards () {
    register char  *cp,
                  **ap;
#ifdef	UCL
    register int    i = bbp;
#endif	UCL

    if (bbl != NULL)
	return;

    if (!archivesw && ((cp = m_find ("bboards")) != NULL)) {
#ifndef	BPOP
	for (ap = brkstring (cp = getcpy (cp), " ", "\n"); *ap; ap++)
#else	BPOP
	for (ap = getip (cp); *ap; ap++)
#endif	BPOP
#ifndef	UCL
	    add_bb (*ap, OK);
#else	UCL
	    bbs[bbp++] = *ap;
	bbs[bbp] = NULL;
	while (i < bbp)
	    add_bb (bbs[i++], OK);
#endif	UCL
#ifndef	BPOP
	free (cp);
#endif	not BPOP
	if (oops)
	    advise (NULLCP, "please fix the %s: entry in your %s file",
		"bboards", mh_profile);
    }
    else {
#ifdef	UCL
	bbs[bbp++] = "system";
	bbs[bbp] = NULL;
#endif	UCL
#ifndef	NNTP
	add_bb ("system", NOTOK);
#else	NNTP
	add_bb ("general", NOTOK);
#ifdef	UCI
	add_bb ("ics.system", NOTOK);
	add_bb ("ics.general", NOTOK);
#endif	UCI
#endif	NNTP
    }
#ifdef	UCL
    bbs[bbp] = NULL;
#endif	UCL

    if (bbl == NULL)
	done (1);
}

/*  */

add_bb (s, hush)
register char   *s;
int     hush;
{
    register struct bboard  *bb;
    static struct bboard   *tail = NULL;

    make_lower (s, s);
    if ((bb = getbbaux (s)) == NULL)
	if (hush == OK)
	    return;
	else
	    adios (NULLCP, "no such bboard as '%s'", s);

    if (bb -> bb_flags & BB_SEEN) {
	if (hush == OK) {
	    admonish (NULLCP, "duplicate bboard '%s'", s);
	    oops++;
	}
	return;
    }
    bb -> bb_flags |= BB_SEEN;

    if (tail != NULL)
	tail -> bb_link = bb;
    if (bbl == NULL)
	bbl = bb;
    tail = bb;
}

/*  */

#ifdef	BPOP
static struct bboard   *Bhead = NULL;
static struct bboard   *Btail = NULL;

static int xtnd2 (s)
char   *s;
{
    int     maxima;
    char    name[BUFSIZ];
    register struct bboard *bb;

    if (sscanf (s, "%s %d", name, &maxima) != 2)
	adios (NULLCP, "XTND2 botch: %s", s);

    if ((bb = (struct bboard   *) calloc (1, sizeof *bb)) == NULL)
	adios (NULLCP, "insufficient memory");
    bb -> bb_name = getcpy (name);
#ifdef	NNTP
    if (index(name, '.')) {
	char *cp;
	bb -> bb_aka = getip (name);
	for (cp = *bb -> bb_aka; *cp; cp++)
	    if (*cp == '.')
		*cp = '-';
    } else {
#endif
    if ((bb -> bb_aka = (char **) calloc (1, sizeof *bb -> bb_aka)) == NULL)
	adios (NULLCP, "insufficient memory");
    *bb -> bb_aka = NULL;
#ifdef	NNTP
    }
#endif	NNTP
    bb -> bb_file = bb -> bb_archive = bb -> bb_info = bb -> bb_map = "";
    bb -> bb_passwd = "";
#ifndef	NNTP
    if ((bb -> bb_leader = (char **) calloc (1, sizeof *bb -> bb_leader))
	    == NULL)
	adios (NULLCP, "insufficient memory");
    *bb -> bb_leader = NULL;
#else	NNTP
    bb -> bb_leader = getip ("usenet");
#endif	NNTP
    bb -> bb_addr = bb -> bb_request = bb -> bb_relay = "";
    if ((bb -> bb_dist = (char **) calloc (1, sizeof *bb -> bb_dist)) == NULL)
	adios (NULLCP, "insufficient memory");
    *bb -> bb_dist = NULL;
    bb -> bb_flags = BB_REMOTE;
    bb -> bb_count = 0;
    bb -> bb_maxima = maxima;
    bb -> bb_date = "";
    bb -> bb_next = bb -> bb_link = bb -> bb_chain = NULL;

    if (Btail != NULL)
	Btail -> bb_chain = bb;
    if (Bhead == NULL)
	Bhead = bb;
    Btail = bb;

    return OK;
}

/*  */

static int xtnd3 (s)
char   *s;
{
    static int  bbs_int = 0;
    static struct bboard   *bb;

    switch (bbs_int++) {
	case 0: 
	    for (bb = Bhead; bb; bb = bb -> bb_chain)
		if (strcmp (bb -> bb_name, s) == 0)
		    break;
	    if (bb == NULL)
		adios (NULLCP, "XTND3 botch");

	    free (bb -> bb_name);
	    bb -> bb_name = getcpy (s);
	    break;
	case 1: 
	    if (bb -> bb_aka)
		free ((char *) bb -> bb_aka);
	    bb -> bb_aka = getip (s);
	    break;
	case 2: 
	    bb -> bb_file = getcpy (s);
	    break;
	case 3: 
	    bb -> bb_archive = getcpy (s);
	    break;
	case 4: 
	    bb -> bb_info = getcpy (s);
	    break;
	case 5: 
	    bb -> bb_map = getcpy (s);
	    break;
	case 6: 
	    bb -> bb_passwd = getcpy (s);
	    break;
	case 7: 
	    if (bb -> bb_leader)
		free ((char *) bb -> bb_leader);
	    bb -> bb_leader = getip (s);
	    break;
	case 8: 
	    bb -> bb_addr = getcpy (s);
	    break;
	case 9: 
	    bb -> bb_request = getcpy (s);
	    break;
	case 10: 
	    bb -> bb_relay = getcpy (s);
	    break;
	case 11: 
	    if (bb -> bb_dist)
		free ((char *) bb -> bb_dist);
	    bb -> bb_dist = getip (s);
	    break;
	case 12: 
	    bb -> bb_flags = bb -> bb_maxima = 0;
	    (void) sscanf (s, "%o %d", &bb -> bb_flags, &bb -> bb_maxima);
	    bb -> bb_flags |= BB_REMOTE;
	    break;
	case 13: 
	    bb -> bb_date = getcpy (s);
	    bbs_int = 0;
	    break;
    }

    return OK;
}


static char **getip (s)
char   *s;
{
    register char **ap,
                  **p,
                  **q;

    for (p = ap = brkstring (getcpy (s), " ", "\n"); *p; p++)
	continue;

    q = (char **) calloc ((unsigned) (p - ap + 1), sizeof *q);
    if (q == NULL)
	adios (NULLCP, "insufficient memory");

    for (p = ap, ap = q; *p; *q++ = *p++)
	continue;
    *q = NULL;

    return ap;
}

/*  */

static struct bboard   *rover = NULL;

struct bboard  *getbbpop () {
    int     snoop;
    char   *cp,
           *pass = NULL;
    register struct bboard *bb;

    if (didpop != NOTOK && ((bb = getbbent ()) || !host))
	return bb;

    if (Bhead == NULL) {
	snoop = (cp = getenv ("MHPOPDEBUG")) && *cp;
	if (rpop) {
	    if (user == NULL)
		user = getusr ();
	    pass = getusr ();
	}
	else
	    if (strcmp (user, popbbuser) == 0)
		pass = user;
	    else
		ruserpass (host, &user, &pass);
	if (didpop != NOTOK)
	    didpop = DONE;

	if (pop_init (host, user, pass, snoop, rpop) == NOTOK)
	    adios (NULLCP, "%s", response);
	if (rpop)
	    (void) setuid (getuid ());
	if (pop_xtnd (xtnd2, "bboards") == NOTOK)
	    adios (NULLCP, "%s", response);
	if (topicsw && verbosw)	/* could optimize here */
	    for (bb = Bhead; bb; bb = bb -> bb_chain)
		if (pop_xtnd (xtnd3, "x-bboards %s", bb -> bb_name) == NOTOK)
		    adios (NULLCP, "%s", response);
	rover = Bhead;
    }

    if (bb = rover)
	rover = rover -> bb_chain;
    return bb;
}

#define	getbbent	getbbpop
#endif	BPOP

/*  */

struct bboard *getbbaux (s)
register char	*s;
{
#ifdef	BPOP
    int	    nlatch = host ? 1 : 0;
#endif	BPOP
    register char  **ap;
    register struct bbcount *selected;
    register struct bboard *bb;
    static struct bboard   *head = NULL,
                           *tail = NULL;

    for (bb = head; bb; bb = bb -> bb_next) {
	if (strcmp (bb -> bb_name, s) == 0)
	    return bb;
	for (ap = bb -> bb_aka; *ap; ap++)
	    if (strcmp (*ap, s) == 0)
		return bb;
    }

#ifdef	BPOP
one_more_time: ;
#endif	BPOP
    while (bb = getbbent ()) {
	if ((selected = seek_count (bbc, bb -> bb_name)) != NULL)
	    bb -> bb_count = selected -> count;

#ifdef	BPOP
	if (!(bb -> bb_flags & BB_REMOTE))
#endif	BPOP
	if ((bb = getbbcpy (bb)) == NULL)
	    adios (NULLCP, "insufficient memory");
	if (tail != NULL)
	    tail -> bb_next = bb;
	if (head == NULL)
	    head = bb;
	tail = bb;

	if (strcmp (bb -> bb_name, s) == 0) {
found_it: ;
	    bb -> bb_flags &= ~BB_SEEN;
	    return bb;
	}
	for (ap = bb -> bb_aka; *ap; ap++)
	    if (strcmp (*ap, s) == 0)
		goto found_it;
    }

#ifdef	BPOP
    if (nlatch && pop_xtnd (xtnd2, "bboards %s", s) != NOTOK) {
	rover = Bhead;
	nlatch = 0;
	goto one_more_time;
    }
#endif	BPOP

    return NULL;
}


struct bboard *getbbvis () {
    register struct bboard *bb;

    while (bb = getbbent ())
	if (!(bb -> bb_flags & BB_INVIS)
		&& (access (bb -> bb_file, 04) != NOTOK || errno != EACCES))
	    break;

    return bb;
}
