/* msgchk.c - check for mail */
#ifndef	lint
static char ident[] = "@(#)$Id: msgchk.c,v 1.13 1993/09/01 21:44:19 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include "../zotnet/tws.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#ifdef LOCALE
#include <locale.h>
#endif
#ifdef HESIOD
#include <hesiod.h>
#endif

/*  */

#ifndef	POP
#define	POPminc(a)	(a)
#else	/* POP */
#define	POPminc(a)	0
#endif	/* POP */

#ifndef	RPOP
#define	RPOPminc(a)	(a)
#else	/* RPOP */
#define	RPOPminc(a)	0
#endif	/* RPOP */

#ifndef	APOP
#define	APOPminc(a)	(a)
#else
#define	APOPminc(a)	0
#endif

static struct swit  switches[] = {
#define	DATESW	0
    "date", 0,
#define	NDATESW	1
    "nodate", 0,

#define	NOTESW	2
    "notify type", 0,
#define	NNOTESW	3
    "nonotify type", 0,

#define	HOSTSW	4
    "host host", POPminc (-4),
#define	USERSW	5
    "user user", POPminc (-4),

#define	APOPSW	6
    "apop", APOPminc (-4),
#define	NAPOPSW	7
    "noapop", APOPminc (-6),

#define	RPOPSW	8
    "rpop", RPOPminc (-4),
#define	NRPOPSW	9
    "norpop", RPOPminc (-6),

#define	HELPSW	10
    "help", 4,

    NULL, 0
};

/*  */

#define	NT_NONE	0x0
#define	NT_MAIL	0x1
#define	NT_NMAI	0x2
#define	NT_ALL	(NT_MAIL | NT_NMAI)


#define	NONEOK	0x0
#define	UUCPOLD	0x1
#define	UUCPNEW	0x2
#define	UUCPOK	(UUCPOLD | UUCPNEW)
#define	MMDFOLD	0x4
#define	MMDFNEW	0x8
#define	MMDFOK	(MMDFOLD | MMDFNEW)


#ifdef	SYS5
#ifndef __STDC__
struct passwd	*getpwuid(), *getpwnam();
#endif /* !__STDC__ */
#endif	/* SYS5 */

static int	donote(), checkmail(), remotemail();
/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     datesw = 1,
	    notifysw = NT_ALL,
	    rpop = 1,
	    status = 0,
	    snoop = 0,
	    vecp = 0;
    int uid = getuid ();
    char   *cp,
           *host = NULL,
            buf[80],
	  **ap,
          **argp,
	   *arguments[MAXARGS],
           *vec[50];
    char *user = getusr ();
    struct passwd  *pw;
#ifdef HESIOD
    struct hes_postoffice *po;
    char *tmphost;
#endif

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    mts_init (invo_name);
#ifdef	POP
    if ((cp = getenv ("MHPOPDEBUG")) && *cp)
	snoop++;
#endif

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
		    (void) sprintf (buf, "%s [switches] [users ...]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case DATESW:
		    datesw++;
		    continue;
		case NDATESW:
		    datesw = 0;
		    continue;

		case NOTESW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    notifysw |= donote (cp, 1);
		    continue;
		case NNOTESW:
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    notifysw &= ~donote (cp, 0);
		    continue;

		case HOSTSW: 
		    if (!(host = *argp++) || *host == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case USERSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    vec[vecp++] = cp;
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
	    }
	vec[vecp++] = cp;
    }

/*  */
#ifdef	POP
    if (!host || !*host) {	/* -host not specified by user */
#ifdef HESIOD
	/*
	 * Scheme is:
	 *        use MAILHOST environment variable if present,
	 *  else try Hesiod.
	 *  If that fails, use the default (if any)
	 *  provided by mtstailor in mts_init()
	 */
	if ((tmphost = getenv("MAILHOST")) != NULL)
	    pophost = tmphost;
	else if ((po = hes_getmailhost(vecp ? vec[0] : user)) != NULL &&
		strcmp(po->po_type, "POP") == 0)
	    pophost = po->po_host;
#endif /* HESIOD */
	if (pophost && *pophost)
	    host = pophost;
    }
    if (!host || !*host)
	host = NULL;
    if (!host || rpop <= 0)
	(void) setuid (uid);
#endif /* POP */
    if (vecp == 0) {
#ifdef	POP
	if (host)
	    status = remotemail (host, user, rpop, notifysw, 1, snoop);
	else
#endif
	  {
	    char *home = (uid = geteuid()) ? home = getenv ("HOME") : NULL;
	    if (home == NULL)
	      {
		pw = getpwnam (user);
		if (pw == NULL)
		  adios (NULLCP, "unable to get information about user");
		if (home == NULL)
		  home = pw->pw_dir;
	      }
	    status = checkmail (user, home, datesw, notifysw, 1);
	  }
    }
    else {
	vec[vecp] = NULL;

	for (vecp = 0; cp = vec[vecp]; vecp++)
#ifdef	POP
	    if (host)
		status += remotemail (host, cp, rpop, notifysw, 0, snoop);
	    else
#endif
		if (pw = getpwnam (cp))
		    status += checkmail (pw->pw_name, pw->pw_dir, datesw, notifysw, 0);
		else
		    advise (NULLCP, "no such user as %s", cp);
    }

    done (status);
}

/*  */

static struct swit ntswitches[] = {
#define	NALLSW	0
    "all", 0,
#define	NMAISW	1
    "mail", 0,
#define	NNMAISW	2
    "nomail", 0,

    NULL, 0
};


static int donote (cp, ntflag)
register char   *cp;
int	ntflag;
{
    switch (smatch (cp, ntswitches)) {
	case AMBIGSW: 
	    ambigsw (cp, ntswitches);
	    done (1);
	case UNKWNSW: 
	    adios (NULLCP, "-%snotify %s unknown", ntflag ? "" : "no", cp);

	case NALLSW: 
	    return NT_ALL;
	case NMAISW: 
	    return NT_MAIL;
	case NNMAISW: 
	    return NT_NMAI;
    }
}

/*  */

#ifdef	MF
/* ARGSUSED */
#endif	/* MF */

static int  checkmail (user, home, datesw, notifysw, personal)
register char *user, *home;
int	datesw,
	notifysw,
	personal;
{
    int     mf,
            status;
    char    buffer[BUFSIZ];
    struct stat st;

    (void) sprintf (buffer, "%s/%s",
	    mmdfldir[0] ? mmdfldir : home,
	    mmdflfil[0] ? mmdflfil : user);
#ifndef	MF
    if (datesw) {
	st.st_size = 0;
	st.st_atime = st.st_mtime = 0;
    }
#endif	/* MF */
    mf = (stat (buffer, &st) == NOTOK || st.st_size == 0) ? NONEOK
	: st.st_atime <= st.st_mtime ? MMDFNEW : MMDFOLD;

#ifdef	MF
    if (umincproc != NULL && *umincproc != NULL) {
	(void) sprintf (buffer, "%s/%s",
		uucpldir[0] ? uucpldir : home,
		uucplfil[0] ? uucplfil : user);
	mf |= (stat (buffer, &st) == NOTOK || st.st_size == 0) ? NONEOK
	    : st.st_atime <= st.st_mtime ? UUCPNEW : UUCPOLD;
    }
#endif	/* MF */

    if ((mf & UUCPOK) || (mf & MMDFOK)) {
	if (notifysw & NT_MAIL) {
	    printf (personal ? "You have " : "%s has ", user);
	    if (mf & UUCPOK)
		printf ("%s old-style bell", mf & UUCPOLD ? "old" : "new");
	    if ((mf & UUCPOK) && (mf & MMDFOK))
		printf (" and ");
	    if (mf & MMDFOK)
		printf ("%s%s", mf & MMDFOLD ? "old" : "new",
			mf & UUCPOK ? " Internet" : "");
	    printf (" mail waiting");
	}
	else
	    notifysw = 0;

	status = 0;
    }
    else {
	if (notifysw & NT_NMAI)
	    printf (personal ? "You don't %s%s" : "%s doesn't %s",
		    personal ? "" : user, "have any mail waiting");
	else
	    notifysw = 0;

	status = 1;
    }

#ifndef	MF
    if (notifysw)
	if (datesw && st.st_atime)
	    printf ("; last read on %s",
		    dasctime (dlocaltime ((long *) & st.st_atime), TW_NULL));
#endif	/* MF */
    if (notifysw)
	printf ("\n");

    return status;
}

/*  */

#ifdef	POP
extern	char response[];


static int  remotemail (host, user, rpop, notifysw, personal, snoop)
register char   *host;
char   *user;
int	rpop,
	notifysw,
	personal,
	snoop;
{
    int     nmsgs,
            nbytes,
            status;
    char   *pass;

    if (user == NULL)
	user = getusr ();
    if (rpop > 0)
	pass = getusr ();
    else
	ruserpass (host, &user, &pass);

    if (pop_init (host, user, pass, snoop, rpop) == NOTOK
	    || pop_stat (&nmsgs, &nbytes) == NOTOK
	    || pop_quit () == NOTOK) {
	advise (NULLCP, "%s", response);
	return 1;
    }

    if (nmsgs) {
	if (notifysw & NT_MAIL) {
	    printf (personal ? "You have " : "%s has ", user);
	    printf ("%d message%s (%d bytes)",
		    nmsgs, nmsgs != 1 ? "s" : "", nbytes);
	}
	else
	    notifysw = 0;

	status = 0;
    }
    else {
	if (notifysw & NT_NMAI)
	    printf (personal ? "You don't %s%s" : "%s doesn't %s",
		    personal ? "" : user, "have any mail waiting");
	else
	    notifysw = 0;
	status = 1;
    }
    if (notifysw)
	printf (" on %s\n", host);

    return status;
}
#endif	/* POP */
