/* msgchk.c - check for mail */

#include "../h/mh.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

/*  */

static struct swit switches[] = {
#define	HELPSW	0
    "help", 4,

#ifdef	POP
#define	HOSTSW	1
    "host host", 0,
#define	USERSW	2
    "user user", 0,

#define	RPOPSW	3
    "rpop", 0,
#define	NRPOPSW	4
    "norpop", 0,
#endif	POP


    NULL, NULL
};

/*  */

#define	NONEOK	0x0
#define	UUCPOLD	0x1
#define	UUCPNEW	0x2
#define	UUCPOK	(UUCPOLD | UUCPNEW)
#define	MMDFOLD	0x4
#define	MMDFNEW	0x8
#define	MMDFOK	(MMDFOLD | MMDFNEW)


#ifdef	POP
int	snoop = 0;
#endif	POP


#ifdef	SYS5
struct passwd	*getpwuid(), *getpwnam();
#endif	SYS5

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     vecp = 0;
#ifdef	POP
    int	    rpop = 1;
#endif	POP
    char   *cp,
#ifdef	POP
           *host = NULL,
#endif	POP
            buf[80],
	  **ap,
          **argp,
	   *arguments[MAXARGS],
           *vec[50];
    struct passwd  *pw;

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
		    (void) sprintf (buf, "%s [switches] [users ...]",
			    invo_name);
		    help (buf, switches);
		    done (1);

#ifdef	POP
		case HOSTSW: 
		    if (!(host = *argp++) || *host == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
		case USERSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    vec[vecp++] = cp;
		    continue;
		case RPOPSW: 
		    rpop++;
		    continue;
		case NRPOPSW: 
		    rpop = 0;
		    continue;
#endif	POP
	    }
	vec[vecp++] = cp;
    }

/*  */

#ifdef	POP
    if (!*host)
	host = NULL;
    if (!host || !rpop)
	(void) setuid (getuid ());
#endif	POP
    if (vecp == 0) {
#ifdef	POP
	if (host)
	    remotemail (host, NULLCP, rpop, 1);
	else
#endif	POP
	    if ((pw = getpwuid (getuid ())) == NULL)
		adios (NULLCP, "you lose");
	    else
		checkmail (pw, 1);
    }
    else {
	vec[vecp] = NULL;

	for (vecp = 0; cp = vec[vecp]; vecp++)
#ifdef	POP
	    if (host)
		remotemail (host, cp, rpop, 0);
	    else
#endif	POP
		if (pw = getpwnam (cp))
		    checkmail (pw, 0);
		else
		    advise (NULLCP, "no such user as %s", cp);
    }

    done (0);
}

/*  */

checkmail (pw, personal)
register struct passwd  *pw;
int	personal;
{
    int     mf;
    char    buffer[BUFSIZ];
    struct stat st;

    (void) sprintf (buffer, "%s/%s",
	    mmdfldir[0] ? mmdfldir : pw -> pw_dir,
	    mmdflfil[0] ? mmdflfil : pw -> pw_name);
    mf = (stat (buffer, &st) == NOTOK || st.st_size == 0) ? NONEOK
	: st.st_atime <= st.st_mtime ? MMDFNEW : MMDFOLD;

#ifdef	MF
    if (umincproc != NULL && *umincproc != NULL) {
	(void) sprintf (buffer, "%s/%s",
		uucpldir[0] ? uucpldir : pw -> pw_dir,
		uucplfil[0] ? uucplfil : pw -> pw_name);
	mf |= (stat (buffer, &st) == NOTOK || st.st_size == 0) ? NONEOK
	    : st.st_atime <= st.st_mtime ? UUCPNEW : UUCPOLD;
    }
#endif	MF

    if ((mf & UUCPOK) || (mf & MMDFOK)) {
	printf (personal ? "You have " : "%s has ", pw -> pw_name);
	if (mf & UUCPOK)
	    printf ("%s old-style bell",
		    mf & UUCPOLD ? "old" : "new");
	if ((mf & UUCPOK) && (mf & MMDFOK))
	    printf (" and ");
	if (mf & MMDFOK)
	    printf ("%s Internet",
		    mf & MMDFOLD ? "old" : "new");
	printf (" mail waiting\n");
    }
    else
	printf (personal ? "You don't %s%s" : "%s doesn't %s",
		personal ? "" : pw -> pw_name, "have any mail waiting\n");
}

/*  */

#ifdef	POP
extern	char response[];


remotemail (host, user, rpop, personal)
register char   *host;
char   *user;
int	rpop,
	personal;
{
    int     nmsgs,
            nbytes;
    char   *pass;

    if (rpop) {
	if (user == NULL)
	    user = getusr ();
	pass = getusr ();
    }
    else
	ruserpass (host, &user, &pass);

    if (pop_init (host, user, pass, snoop, rpop) == NOTOK
	    || pop_stat (&nmsgs, &nbytes) == NOTOK
	    || pop_quit () == NOTOK) {
	advise (NULLCP, "%s", response);
	return;
    }

    if (nmsgs) {
	printf (personal ? "You have " : "%s has ", user);
	printf ("%d message%s (%d bytes)",
		nmsgs, nmsgs != 1 ? "s" : "", nbytes);
    }
    else
	printf (personal ? "You don't %s%s" : "%s doesn't %s",
		personal ? "" : user, "have any mail waiting");
    printf (" on %s\n", host);
}
#endif	POP
