/*  $Revision: 1.9 $
**
**  Buffered file exploder for innd.
*/
#include "configdata.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <ctype.h>
#include <errno.h>
#include "paths.h"
#include "libinn.h"
#include "clibrary.h"
#include "qio.h"
#include "macros.h"

extern void	MAPread();
extern char	*MAPname();

/*
**  Hash functions for hashing sitenames.
*/
#define SITE_HASH(Name, p, j)    \
	for (p = Name, j = 0; *p; ) j = (j << 5) + j + *p++
#define SITE_SIZE	128
#define SITE_BUCKET(j)	&SITEtable[j & (SITE_SIZE - 1)]


/*
**  Entry for a single active site.
*/
typedef struct _SITE {
    BOOL	Dropped;
    STRING	Name;
    int		CloseLines;
    int		FlushLines;
    time_t	LastFlushed;
    time_t	LastClosed;
    int 	CloseSeconds;
    int		FlushSeconds;
    FILE	*F;
    STRING	Filename;
    char	*Buffer;
} SITE;


/*
**  Site hashtable bucket.
*/
typedef struct _SITEHASH {
    int		Size;
    int		Used;
    SITE	*Sites;
} SITEHASH;


/* Global variables. */
STATIC char	*Format;
STATIC STRING	Map;
STATIC int	BufferMode;
STATIC int	CloseEvery;
STATIC int	FlushEvery;
STATIC int	CloseSeconds;
STATIC int	FlushSeconds;
STATIC SIGVAR	GotInterrupt;
STATIC SITEHASH	SITEtable[SITE_SIZE];
STATIC TIMEINFO	Now;


#if	defined(DONT_HAVE_FCHMOD)
/*
**  A dummy fchmod.
*/
/* ARGSUSED */
int
fchmod(i)
    int		i;
{
}
#endif	/* defined(DONT_HAVE_FCHMOD) */


/*
**  Set up the site information.  Basically creating empty buckets.
*/
STATIC void
SITEsetup()
{
    register SITEHASH	*shp;

    for (shp = SITEtable; shp < ENDOF(SITEtable); shp++) {
	shp->Size = 3;
	shp->Sites = NEW(SITE, shp->Size);
	shp->Used = 0;
    }
}


/*
**  Close a site
*/
STATIC void
SITEclose(sp)
    register SITE       *sp;
{
    register FILE	*F;

    if ((F = sp->F) != NULL) {
	if (fflush(F) == EOF || ferror(F)
	 || fchmod((int)fileno(F), 0664) < 0
	 || fclose(F) == EOF)
	    (void)fprintf(stderr, "buffchan %s cant close %s, %s\n",
		    sp->Name, sp->Filename, strerror(errno));
	sp->F = NULL;
    }
}

/*
**  Close all open sites.
*/
STATIC void
SITEcloseall()
{
    register SITEHASH	*shp;
    register SITE	*sp;
    register int	i;

    for (shp = SITEtable; shp < ENDOF(SITEtable); shp++)
	for (sp = shp->Sites, i = shp->Used; --i >= 0; sp++)
	    SITEclose(sp);
}


/*
**  Open the file for a site.
*/
STATIC void
SITEopen(sp)
    register SITE	*sp;
{
    int			e;

    if ((sp->F = xfopena(sp->Filename)) == NULL
     && ((e = errno) != EACCES || chmod(sp->Filename, 0644) < 0
      || (sp->F = xfopena(sp->Filename)) == NULL)) {
	(void)fprintf(stderr, "buffchan %s cant fopen %s, %s\n",
		sp->Name, sp->Filename, strerror(e));
	if ((sp->F = fopen("/dev/null", "w")) == NULL) {
	    /* This really should not happen. */
	    (void)fprintf(stderr, "buffchan %s cant fopen %s, %s\n",
		    sp->Name, "/dev/null", strerror(errno));
	    exit(1);
	}
    }
    else if (fchmod((int)fileno(sp->F), 0444) < 0)
	(void)fprintf(stderr, "buffchan %s cant fchmod %s %s\n",
		sp->Name, sp->Filename, strerror(errno));
	
    if (BufferMode != '\0')
	setbuf(sp->F, sp->Buffer);

    /* Reset all counters. */
    sp->FlushLines = 0;
    sp->CloseLines = 0;
    sp->LastFlushed = Now.time;
    sp->LastClosed = Now.time;
    sp->Dropped = FALSE;
}


/*
**  Find a site, possibly create if not found.
*/
STATIC SITE *
SITEfind(Name, CanCreate)
    char		*Name;
    BOOL		CanCreate;
{
    register char	*p;
    register int	i;
    unsigned int	j;
    register SITE	*sp;
    SITEHASH		*shp;
    char		c;
    char		buff[BUFSIZ];

    /* Look for site in the hash table. */
    /* SUPPRESS 6 *//* Over/underflow from plus expression */
    SITE_HASH(Name, p, j);
    shp = SITE_BUCKET(j);
    for (c = *Name, sp = shp->Sites, i = shp->Used; --i >= 0; sp++)
	if (c == sp->Name[0] && caseEQ(Name, sp->Name))
	    return sp;
    if (!CanCreate)
	return NULL;

    /* Adding a new site -- grow hash bucket if we need to. */
    if (shp->Used == shp->Size - 1) {
	shp->Size *= 2;
	RENEW(shp->Sites, SITE, shp->Size);
    }
    sp = &shp->Sites[shp->Used++];

    /* Fill in the structure for the new site. */
    sp->Name = COPY(Name);
    (void)sprintf(buff, (STRING)Format, Map ? MAPname(Name) : sp->Name);
    sp->Filename = COPY(buff);
    if (BufferMode == 'u')
	sp->Buffer = NULL;
    else if (BufferMode == 'b')
	sp->Buffer = NEW(char, BUFSIZ);
    SITEopen(sp);

    return sp;
}


/*
**  Flush a site -- close and re-open the file.
*/
STATIC void
SITEflush(sp)
    register SITE	*sp;
{
    register FILE	*F;

    if ((F = sp->F) != NULL) {
	if (fflush(F) == EOF || ferror(F)
	 || fchmod((int)fileno(F), 0664) < 0
	 || fclose(F) == EOF)
	    (void)fprintf(stderr, "buffchan %s cant close %s, %s\n",
		    sp->Name, sp->Filename, strerror(errno));
	sp->F = NULL;
    }
    if (!sp->Dropped)
	SITEopen(sp);
}


/*
**  Flush all open sites.
*/
STATIC void
SITEflushall()
{
    register SITEHASH	*shp;
    register SITE	*sp;
    register int	i;

    for (shp = SITEtable; shp < ENDOF(SITEtable); shp++)
	for (sp = shp->Sites, i = shp->Used; --i >= 0; sp++)
	    SITEflush(sp);
}


/*
**  Write data to a site.
*/
STATIC void
SITEwrite(name, text, len)
    register char	*name;
    register char	*text;
    register int	len;
{
    register SITE	*sp;

    sp = SITEfind(name, TRUE);
    if (sp->F == NULL)
	SITEopen(sp);

    if (fwrite((POINTER)text, (SIZE_T)1, (SIZE_T)len, sp->F) != len)
	(void)fprintf(stderr, "buffchan %s cant write %s\n",
		sp->Name, strerror(errno));

    /* Bump line count; see if time to close or flush. */
    if (CloseEvery && ++(sp->CloseLines) >= CloseEvery) {
	SITEflush(sp);
	return;
    }
    if (CloseSeconds && sp->LastClosed + CloseSeconds < Now.time) {
	SITEflush(sp);
	return;
    }
    if (FlushEvery && ++(sp->FlushLines) >= FlushEvery) {
	if (fflush(sp->F) == EOF || ferror(sp->F))
	    (void)fprintf(stderr, "buffchan %s cant flush %s, %s\n",
		    sp->Name, sp->Filename, strerror(errno));
	sp->LastFlushed = Now.time;
	sp->FlushLines = 0;
    }
    else if (FlushSeconds && sp->LastFlushed + FlushSeconds < Now.time) {
	if (fflush(sp->F) == EOF || ferror(sp->F))
	    (void)fprintf(stderr, "buffchan %s cant flush %s, %s\n",
		    sp->Name, sp->Filename, strerror(errno));
	sp->LastFlushed = Now.time;
	sp->FlushLines = 0;
    }
}


/*
**  Handle a command message.
*/
STATIC void
Process(p)
    register char	*p;
{
    register SITE	*sp;

    if (*p == 'b' && EQn(p, "begin", 5))
	/* No-op. */
	return;

    if (*p == 'f' && EQn(p, "flush", 5)) {
	for (p += 5; ISWHITE(*p); p++)
	    continue;
	if (*p == '\0')
	    SITEflushall();
	else if ((sp = SITEfind(p, FALSE)) != NULL)
	    SITEflush(sp);
	else
	    /*(void)fprintf(stderr, "buffchan flush %s unknown site\n", p);*/
	    ;
	return;
    }

    if (*p == 'd' && EQn(p, "drop", 4)) {
	for (p += 4; ISWHITE(*p); p++)
	    continue;
	if (*p == '\0')
	    SITEcloseall();
	else if ((sp = SITEfind(p, FALSE)) == NULL)
	    (void)fprintf(stderr, "buffchan drop %s unknown site\n", p);
	else {
	    SITEclose(sp);
	    sp->Dropped = TRUE;
	}
	return;
    }

    if (*p == 'r' && EQn(p, "readmap", 7)) {
	MAPread(Map);
	return;
    }

    /* Other command messages -- ignored. */
    (void)fprintf(stderr, "buffchan unknown message %s\n", p);
}


/*
**  Print usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage error.\n");
    exit(1);
}


/*
**  Mark that we got a signal; let two signals kill us.
*/
STATIC SIGHANDLER
CATCHinterrupt(s)
    int		s;
{
    GotInterrupt = TRUE;
    (void)signal(s, SIG_DFL);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		BATCHDIR[] = _PATH_BATCHDIR;
    register QIOSTATE	*qp;
    register int	i;
    register int	Fields;
    register char	*p;
    register char	*next;
    register char	*line;
    char		*Directory;
    BOOL		Redirect;
    FILE		*F;

    /* Set defaults. */
    Directory = NULL;
    Fields = 1;
    Format = NULL;
    Redirect = TRUE;
    GotInterrupt = FALSE;
    (void)umask(NEWSUMASK);

    (void)signal(SIGHUP, CATCHinterrupt);
    (void)signal(SIGINT, CATCHinterrupt);
    (void)signal(SIGQUIT, CATCHinterrupt);
    (void)signal(SIGPIPE, CATCHinterrupt);
    (void)signal(SIGTERM, CATCHinterrupt);
    (void)signal(SIGALRM, CATCHinterrupt);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "bc:C:d:f:l:L:m:p:rs:u")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'b':
	case 'u':
	    BufferMode = i;
	    break;
	case 'c':
	    CloseEvery = atoi(optarg);
	    break;
	case 'C':
	    CloseSeconds = atoi(optarg);
	    break;
	case 'd':
	    Directory = optarg;
	    if (Format == NULL)
		Format ="%s";
	    break;
	case 'f':
	    Fields = atoi(optarg);
	    break;
	case 'l':
	    FlushEvery = atoi(optarg);
	    break;
	case 'L':
	    FlushSeconds = atoi(optarg);
	    break;
	case 'm':
	    Map = optarg;
	    MAPread(Map);
	    break;
	case 'p':
	    if ((F = fopen(optarg, "w")) == NULL) {
		(void)fprintf(stderr, "buffchan cant fopen %s %s\n",
			optarg, strerror(errno));
		exit(1);
	    }
	    (void)fprintf(F, "%ld\n", (long)getpid());
	    if (ferror(F) || fclose(F) == EOF) {
		(void)fprintf(stderr, "buffchan cant fclose %s %s\n",
			optarg, strerror(errno));
		exit(1);
	    }
	    break;
	case 'r':
	    Redirect = FALSE;
	    break;
	case 's':
	    Format = optarg;
	    break;
	}
    ac -= optind;
    av += optind;
    if (ac)
	Usage();

    /* Do some basic set-ups. */
    if (Redirect)
	(void)freopen(_PATH_ERRLOG, "a", stderr);
    if (Format == NULL) {
	Format = NEW(char, STRLEN(BATCHDIR) + 1 + 2 + 1);
	(void)sprintf(Format, "%s/%%s", BATCHDIR);
    }
    if (Directory && chdir(Directory) < 0) {
	(void)fprintf(stderr, "buffchan cant chdir %s %s\n",
	    Directory, strerror(errno));
	exit(1);
    }
    SITEsetup();

    /* Read input. */
    for (qp = QIOfdopen((int)fileno(stdin), 0); !GotInterrupt ; ) {
	if ((line = QIOread(qp)) == NULL) {
	    if (QIOerror(qp)) {
		(void)fprintf(stderr, "buffchan cant read %s\n",
			strerror(errno));
		break;
	    }
	    if (QIOtoolong(qp)) {
		(void)fprintf(stderr, "buffchan long_line");
		(void)QIOread(qp);
		continue;
	    }

	    /* Normal EOF. */
	    break;
	}

	/* Command? */
	if (*line == EXP_CONTROL && *++line != EXP_CONTROL) {
	    Process(line);
	    continue;
	}

	/* Skip the right number of leading fields. */
	for (i = Fields, p = line; *p; p++)
	    if (*p == ' ' && --i <= 0)
		break;
	if (*p == '\0')
	    /* Nothing to write.  Probably shouldn't happen. */
	    continue;

	/* Add a newline, get the length of all leading fields. */
	*p++ = '\n';
	i = p - line;

	if (GetTimeInfo(&Now) < 0) {
	    (void)fprintf(stderr, "buffchan cant gettime %s\n",
		    strerror(errno));
	    break;
	}

	/* Rest of the line is space-separated list of filenames. */
	for (; *p; p = next) {
	    /* Skip whitespace, get next word. */
	    while (*p == ' ')
		p++;
	    for (next = p; *next && *next != ' '; next++)
		continue;
	    if (*next)
		*next++ = '\0';

	    SITEwrite(p, line, i);
	}

    }

    SITEcloseall();
    exit(0);
    /* NOTREACHED */
}
