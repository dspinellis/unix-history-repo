/* mts.c - definitions for the mail transport system */
#ifndef	lint
static char ident[] = "@(#)$Id: mts.c,v 2.20 1994/03/23 23:35:23 jromine Exp $";
#endif /* lint */

/* LINTLIBRARY */

#undef	NETWORK
#if	defined(BSD41A) || defined(BSD42) || defined(SOCKETS)
#define	NETWORK
#endif

#include "../h/strings.h"
#include <ctype.h>
#include <stdio.h>
#include "mts.h"
#ifdef	NETWORK
#if	defined(BSD42) || defined(SOCKETS)
#include <netdb.h>
#endif
#else	/* NETWORK */
#ifndef SYS5
#include <whoami.h>
#else
#include <sys/utsname.h>
#endif /* SYS5 */
#endif /* NETWORK */
#include <pwd.h>


#define	NOTOK	(-1)
#define	OK	0

#define	NULLCP	((char *) 0)

extern int  errno;

static char   *tailor_value ();


#if defined(SYS5) && !defined(_AIX)
#define	index	strchr
#define	rindex	strrchr
#endif /* SYS5 */

#if !defined(__STDC__) && !defined(_AIX) && !defined(_POSIX_SOURCE)
char   *index (), *malloc (), *mktemp (), *rindex (), *strcpy ();

#ifdef	SYS5
struct passwd  *getpwuid ();
#endif
#endif /* !__STDC__ */

/*  */

/*
   *mmdfldir and *uucpldir are the maildrop directories.  If maildrops
   are kept in the user's home directory, then these should be empty
   strings.  In this case, the appropriate ...lfil array should contain
   the name of the file in the user's home directory.  Usually, this is
   something like ".mail".
 */

static char *mtstailor = "/usr/contrib/mh-6.8/lib/mtstailor";

static char    *localname = "";
static char    *localdomain = "";
static char    *systemname = "";
#ifdef	MF
static char    *UUCPchan = "";
#endif /* MF */
char    *mmdfldir = "/var/mail";
char    *mmdflfil = "";
char    *uucpldir = "/usr/spool/mail";
char    *uucplfil = "";


char    *mmdlm1 = "\001\001\001\001\n";
char    *mmdlm2 = "\001\001\001\001\n";


static int  MMailids = 0;
static char *mmailid = "0";


#ifdef	MF
char   *umincproc = "/usr/contrib/mh-6.8/lib/uminc";
#else /* MF */
char   *umincproc = NULL;
#endif /* MF */


int	lockstyle = LOK_UNIX;
static char *lkstyle = "0";
char   *lockldir = "";

/*  */

/* MTS specific variables */

#ifdef	MHMTS
char   *Mailqdir = "/usr/spool/netmail";
char   *TMailqdir = "/usr/tmp";
int     Syscpy = 1;
static char *syscpy = "1";
char   *Overseer = "root";
char   *Mailer = "root";
char   *Fromtmp = "/tmp/rml.f.XXXXXX";
char   *Msgtmp = "/tmp/rml.m.XXXXXX";
char   *Errtmp = "/tmp/rml.e.XXXXXX";
int     Tmpmode = 0600;
static char *tmpmode = "0600";
char   *Okhosts = "/usr/contrib/mh-6.8/lib/Rmail.OkHosts";
char   *Okdests = "/usr/contrib/mh-6.8/lib/Rmail.OkDests";
#endif /* MHMTS */

#ifdef	MMDFMTS
#endif /* MMDFMTS */

#ifdef	SENDMTS
char   *hostable = "/usr/contrib/mh-6.8/lib/hosts";
char   *sendmail = "/usr/lib/sendmail";
#endif /* SENDMTS */


/* SMTP/POP stuff */

char   *clientname = (char *)0;
char   *servers = "localhost \01localnet";
char   *pophost = "";


/* BBoards-specific variables */

char   *bb_domain = "";


/* POP BBoards-specific variables */

#ifdef	BPOP
char    *popbbhost = "";
char    *popbbuser = "";
char    *popbblist = "/usr/contrib/mh-6.8/lib/hosts.popbb";
#endif /* BPOP */


/* MailDelivery */

char   *maildelivery = "/usr/contrib/mh-6.8/lib/maildelivery";


/* Aliasing Facility (doesn't belong here) */

int	Everyone = NOTOK;
static char *everyone = "-1";
char   *NoShell = "";

/*  */

/* customize the MTS settings for MH by reading /usr/contrib/mh-6.8/lib/mtstailor */

static  struct bind {
    char   *keyword;
    char  **value;
}       binds[] = {
    "localname", &localname,
    "localdomain", &localdomain,
    "systemname", &systemname,
#ifdef	MF
    "uucpchan", &UUCPchan,
#endif /* MF */
    "mmdfldir", &mmdfldir,
    "mmdflfil", &mmdflfil,
    "uucpldir", &uucpldir,
    "uucplfil", &uucplfil,
    "mmdelim1", &mmdlm1,
    "mmdelim2", &mmdlm2,
    "mmailid", &mmailid,
    "umincproc", &umincproc,
    "lockstyle", &lkstyle,
    "lockldir", &lockldir,

#ifdef	MHMTS
    "mailqdir", &Mailqdir,
    "tmailqdir", &TMailqdir,
    "syscpy", &syscpy,
    "overseer", &Overseer,
    "mailer", &Mailer,
    "fromtmp", &Fromtmp,
    "msgtmp", &Msgtmp,
    "errtmp", &Errtmp,
    "tmpmode", &tmpmode,
    "okhosts", &Okhosts,
    "okdests", &Okdests,
#endif /* MHMTS */

#ifdef	MMDFMTS
#endif /* MMDFMTS */

#ifdef	SENDMTS
    "hostable", &hostable,
    "sendmail", &sendmail,
#endif /* SENDMTS */

    "clientname",  &clientname,
    "servers", &servers,
    "pophost", &pophost,

    "bbdomain", &bb_domain,

#ifdef	BPOP
    "popbbhost", &popbbhost,
    "popbbuser", &popbbuser,
    "popbblist", &popbblist,
#endif /* BPOP */
#ifdef	NNTP
    "nntphost", &popbbhost,
#endif /* NNTP */

    "maildelivery", &maildelivery,

    "everyone", &everyone,
    "noshell", &NoShell,

    NULL
};

/*  */

/* I'd like to use m_getfld() here, but not all programs loading mts.o may be
   MH-style programs... */

/* ARGSUSED */

mts_init (name)
char    *name;
{
    register char  *bp,
                   *cp;
    char    buffer[BUFSIZ];
    register struct bind   *b;
    register    FILE *fp;
    static int  inited = 0;

    if (inited++ || (fp = fopen (mtstailor, "r")) == NULL)
	return;

    while (fgets (buffer, sizeof buffer, fp)) {
	if ((cp = index (buffer, '\n')) == NULL)
	    break;
	*cp = 0;
	if (*buffer == '#' || *buffer == '\0')
	    continue;
	if ((bp = index (buffer, ':')) == NULL)
	    break;
	*bp++ = 0;
	while (isspace (*bp))
	    *bp++ = 0;

	for (b = binds; b -> keyword; b++)
	    if (strcmp (buffer, b -> keyword) == 0)
		break;
	if (b -> keyword && (cp = tailor_value (bp)))
	    *b -> value = cp;
    }

    (void) fclose (fp);

    MMailids = atoi (mmailid);
    if ((lockstyle = atoi (lkstyle)) < LOK_UNIX || lockstyle > LOK_MMDF)
	lockstyle = LOK_UNIX;
#ifdef	MHMTS
    Syscpy = atoi (syscpy);
    (void) sscanf (tmpmode, "0%o", &Tmpmode);
#endif /* MHMTS */
    Everyone = atoi (everyone);
}

/*  */

#define	QUOTE	'\\'

static char *tailor_value (s)
register char   *s;
{
    register int    i,
                    r;
    register char  *bp;
    char    buffer[BUFSIZ];

    for (bp = buffer; *s; bp++, s++)
	if (*s != QUOTE)
	    *bp = *s;
	else
	    switch (*++s) {
#define	grot(y,z) case y: *bp = z; break;
		grot ('b', '\b');
		grot ('f', '\f');
		grot ('n', '\n');
		grot ('t', '\t');
#undef	grot

		case 0: s--;
		case QUOTE: 
		    *bp = QUOTE;
		    break;

		default: 
		    if (!isdigit (*s)) {
			*bp++ = QUOTE;
			*bp = *s;
		    }
		    r = *s != '0' ? 10 : 8;
		    for (i = 0; isdigit (*s); s++)
			i = i * r + *s - '0';
		    s--;
		    *bp = toascii (i);
		    break;
	    }
    *bp = 0;

    bp = malloc ((unsigned) (strlen (buffer) + 1));
    if (bp != NULL)
	(void) strcpy (bp, buffer);

    return bp;
}

/*  */

char   *LocalName () {
#ifdef	BSD41A
    char  *myname;
#endif /* BSD41A */
#if	defined(BSD42) || defined(SOCKETS)
    register struct hostent *hp;
#endif /* BSD42 or SOCKETS */
#if	defined(SYS5) && !defined(NETWORK)
    struct utsname name;
#endif /* SYS5 and not NETWORK */
    static char buffer[BUFSIZ] = "";

    if (buffer[0])
	return buffer;

    mts_init ("mts");
    if (*localname)
	strcpy (buffer, localname);
    else {

#ifdef	locname
    (void) strcpy (buffer, locname);
#else /* not locname */
#ifdef	NETWORK
#ifdef	BSD41A
    myname = "myname";
    if (rhost (&myname) == -1)
	(void) gethostname (buffer, sizeof buffer);
    else {
	(void) strcpy (buffer, myname);
	free (myname);
    }
#endif /* BSD41A */
#if	defined(BSD42) || defined(SOCKETS)
    (void) gethostname (buffer, sizeof buffer);
#ifndef	BIND
    sethostent (1);
#endif
    if (hp = gethostbyname (buffer))
	(void) strcpy (buffer, hp -> h_name);
#endif /* BSD42 or SOCKETS */
#else /* not NETWORK */
#ifndef	SYS5
    (void) strcpy (buffer, SystemName ());
#else /* SYS5 */
    (void) uname (&name);
    (void) strcpy (buffer, name.nodename);
#endif /* SYS5 */
#endif /* not NETWORK */
#endif /* not locname */
    }
    if (*localdomain) {
	strcat (buffer, ".");
	strcat (buffer, localdomain);
    }
    return buffer;
}

/*  */

char *SystemName () {
#if	defined(SYS5) && !defined(NETWORK)
    struct utsname name;
#endif /* SYS5 and not NETWORK */
    static char buffer[BUFSIZ] = "";

    if (buffer[0])
	return buffer;

    mts_init ("mts");
    if (*systemname)
	return strcpy (buffer, systemname);

#ifdef	sysname
    (void) strcpy (buffer, sysname);
#else /* sysname */
#if	!defined(SYS5) || defined(NETWORK)
    (void) gethostname (buffer, sizeof buffer);
#else /* SYS5 and not NETWORK */
#ifdef	SYS5
    (void) uname (&name);
    (void) strcpy (buffer, name.nodename);
#endif /* SYS5 */
#endif /* SYS5 and not NETWORK */
#endif /* sysname */

    return buffer;
}

/*  */

char   *UucpChan () {
#ifdef	MF
    static char buffer[BUFSIZ] = "";
#endif /* MF */

#ifndef	MF
    return NULL;
#else /* MF */
    if (buffer[0])
	return buffer;

    mts_init ("mts");
    if (*UUCPchan)
	return strcpy (buffer, UUCPchan);

#ifdef	uucpchan
    (void) strcpy (buffer, uucpchan);
#else /* uucpchan */
    (void) strcpy (buffer, "uucp");
#endif /* uucpchan */
    return buffer;
#endif /* MF */
}

/*  */

#ifdef	ALTOS
gethostname (name, len)
register char   *name;
register int     len;
{
    register char  *cp;
    register FILE  *fp;

    if (fp = fopen ("/etc/systemid", "r")) {
	if (fgets (name, len, fp)) {
	    if (cp = index (name, '\n'))
		*cp = 0;
	    (void) fclose (fp);
	    return OK;
	}
	(void) fclose (fp);
    }
    (void) strncpy (name, "altos", len);

    return OK;
}
#endif /* ALTOS */

/*  */

static char username[BUFSIZ];
static char fullname[BUFSIZ];


char   *getusr () {
    register char  *cp,
                   *np;
#ifdef KPOP
    int uid;
#endif /* KPOP */
    register struct passwd *pw;

    if (username[0])
	return username;

#ifndef KPOP
    if ((pw = getpwuid (getuid ())) == NULL
	    || pw -> pw_name == NULL
	    || *pw -> pw_name == NULL) {
#else /* KPOP */
    uid = getuid ();
    if (uid == geteuid () && (cp = getenv ("USER")) != NULL
	&& (pw = getpwnam (cp)) != NULL)
      strcpy (username, cp);
    else if ((pw = getpwuid (uid)) == NULL
	     || pw -> pw_name == NULL
	     || *pw -> pw_name == '\0') {
#endif /* KPOP */
	(void) strcpy (username, "unknown");
	(void) sprintf (fullname, "The Unknown User-ID (%d)", getuid ());
	return username;
    }

    np = pw -> pw_gecos;
#ifndef	GCOS_HACK
    for (cp = fullname; *np && *np != (MMailids ? '<' : ','); *cp++ = *np++)
	continue;
#else
    for (cp = fullname; *np && *np != (MMailids ? '<' : ','); )
	if (*np == '&')	{	/* blech! */
	    (void) strcpy(cp, pw -> pw_name);
	    *cp = toupper(*cp);
	    while (*cp)
		cp++;
	    np++;
	}
	else
	    *cp++ = *np++;
#endif
    *cp = '\0';
    if (MMailids) {
	if (*np)
	    np++;
	for (cp = username; *np && *np != '>'; *cp++ = *np++)
	    continue;
	*cp = '\0';
    }
    if (MMailids == 0 || *np == '\0')
	(void) strcpy (username, pw -> pw_name);

    if ((cp = getenv ("SIGNATURE")) && *cp)
	(void) strcpy (fullname, cp);
    if (index(fullname, '.')) {		/*  quote any .'s */
	  char tmp[BUFSIZ];
      sprintf (tmp, "\"%s\"", fullname);/* should quote "'s too */
      strcpy (fullname, tmp);
    }

    return username;
}


char   *getfullname () {
    if (username[0] == '\0')
	(void) getusr ();

    return fullname;
}

/*  */

#ifdef	SYS5
#ifndef	notdef			/* Supposedly this works, I prefer the
				   recursive solution... */

#include <fcntl.h>

int     dup2 (d1, d2)
register int    d1,
                d2;
{
    int     d;

    if (d1 == d2)
	return OK;

    (void) close (d2);
    if ((d = fcntl (d1, F_DUPFD, d2)) == NOTOK)
	return NOTOK;
    if (d == d2)
	return OK;

    errno = 0;
    return NOTOK;
}

#else /* notdef */
int     dup2 (d1, d2)
register int    d1,
                d2;
{
    if (d1 == d2)
	return OK;

    (void) close (d2);
    return dup2aux (d1, d2);
}


static int  dup2aux (d1, d2)
register int    d1,
                d2;
{
    int     d,
            i,
            eindex;

    if ((d = dup (d1)) == NOTOK)
	return NOTOK;
    if (d == d2)
	return OK;

    i = dup2aux (d1, d2);
    eindex = errno;
    (void) close (d);
    errno = eindex;
    return i;
}
#endif /* notdef */
#endif /* SYS5 */
