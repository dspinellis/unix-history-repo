/*  $Revision: 1.17 $
**
**  Read batchfiles on standard input and spew out batches.
*/
#include "configdata.h"
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#if	defined(DO_NEED_TIME)
#include <time.h>
#endif	/* defined(DO_NEED_TIME) */
#include <sys/time.h>
#include "paths.h"
#include "libinn.h"
#include "logging.h"
#include "clibrary.h"
#include "macros.h"


/*
**  Global variables.
*/
STATIC BOOL	BATCHopen;
STATIC BOOL	STATprint;
STATIC double	STATbegin;
STATIC double	STATend;
STATIC char	*Host;
STATIC char	*InitialString;
STATIC char	*Input;
STATIC char	*Processor;
STATIC int	ArtsInBatch;
STATIC int	ArtsWritten;
STATIC int	BATCHcount;
STATIC int	MaxBatches;
STATIC int	BATCHstatus;
STATIC long	BytesInBatch = 60 * 1024;
STATIC long	BytesWritten;
STATIC long	MaxArts;
STATIC long	MaxBytes;
STATIC SIGVAR	GotInterrupt;
STATIC STRING	Separator = "#! rnews %ld";


/*
**  Start a batch process.
*/
STATIC FILE *
BATCHstart()
{
    FILE	*F;
    char	buff[SMBUF];

    if (Processor && *Processor) {
	(void)sprintf(buff, Processor, Host);
	F = popen(buff, "w");
	if (F == NULL)
	    return NULL;
    }
    else
	F = stdout;
    BATCHopen = TRUE;
    BATCHcount++;
    return F;
}


/*
**  Close a batch, return exit status.
*/
STATIC int
BATCHclose(F)
    FILE	*F;
{
    BATCHopen = FALSE;
    if (F == stdout)
	return fflush(stdout) == EOF ? 1 : 0;
    return pclose(F);
}


/*
**  Update the batch file and exit.
*/
STATIC NORETURN
RequeueAndExit(Cookie, line, BytesInArt)
    OFFSET_T	Cookie;
    char	*line;
    long	BytesInArt;
{
    static char	LINE1[] = "batcher %s times user %.3f system %.3f elapsed %.3f";
    static char	LINE2[] ="batcher %s stats batches %d articles %d bytes %ld";
    static char	NOWRITE[] = "batcher %s cant write spool %s\n";
    static char	BATCHDIR[] = _PATH_BATCHDIR;
    char	temp[BUFSIZ];
    char	buff[BUFSIZ];
    int		i;
    FILE	*F;
    TIMEINFO	Now;
    double	usertime;
    double	systime;

    /* Do statistics. */
    (void)GetTimeInfo(&Now);
    STATend = TIMEINFOasDOUBLE(Now);
    if (GetResourceUsage(&usertime, &systime) < 0) {
	usertime = 0;
	systime = 0;
    }

    if (STATprint) {
	(void)printf(LINE1, Host, usertime, systime, STATend - STATbegin);
	(void)printf("\n");
	(void)printf(LINE2, Host, BATCHcount, ArtsWritten, BytesWritten);
	(void)printf("\n");
    }

    (void)openlog("batcher", L_OPENLOG_FLAGS | LOG_PID, LOG_INN_PROG);
    syslog(L_NOTICE, LINE1, Host, usertime, systime, STATend - STATbegin);
    syslog(L_NOTICE, LINE2, Host, BATCHcount, ArtsWritten, BytesWritten);

    /* Last batch exit okay? */
    if (BATCHstatus == 0) {
	if (feof(stdin)) {
	    /* Yes, and we're all done -- remove input and exit. */
	    (void)fclose(stdin);
	    if (Input)
		(void)unlink(Input);
	    exit(0);
	}
	/* Don't seek back -- batch was fine. */
	Cookie = -1;
    }

    /* Make an appropriate spool file. */
    if (Input == NULL)
	(void)sprintf(temp, "%s/%s", BATCHDIR, Host);
    else
	(void)sprintf(temp, "%s.bch", Input);
    if ((F = xfopena(temp)) == NULL) {
	(void)fprintf(stderr, "batcher %s cant open %s %s\n",
	    Host, temp, strerror(errno));
	exit(1);
    }

    /* If we can back up to where the batch started, do so. */
    i = 0;
    if (Cookie != -1 && fseek(stdin, Cookie, SEEK_SET) == -1) {
	(void)fprintf(stderr, "batcher %s cant seek %s\n",
		Host, strerror(errno));
	i = 1;
    }

    /* Write the line we had; if the fseek worked, this will be an
     * extra line, but that's okay. */
    if (line && fprintf(F, "%s %ld\n", line, BytesInArt) == EOF) {
	(void)fprintf(stderr, NOWRITE, Host, strerror(errno));
	i = 1;
    }

    /* Write rest of stdin to spool. */
    while (fgets(buff, sizeof buff, stdin) != NULL) 
	if (fputs(buff, F) == EOF) {
	    (void)fprintf(stderr, NOWRITE, Host, strerror(errno));
	    i = 1;
	    break;
	}
    if (fclose(F) == EOF) {
	(void)fprintf(stderr, "batcher %s cant close spool %s\n",
	    Host, strerror(errno));
	i = 1;
    }

    /* If we had a named input file, try to rename the spool. */
    if (Input != NULL && rename(temp, Input) < 0) {
	(void)fprintf(stderr, "batcher %s cant rename spool %s\n",
	    Host, strerror(errno));
	i = 1;
    }

    exit(i);
    /* NOTREACHED */
}


/*
**  Mark that we got interrupted.
*/
STATIC SIGHANDLER
CATCHinterrupt(s)
    int		s;
{
    GotInterrupt = TRUE;
    /* Let two interrupts kill us. */
    (void)signal(s, SIG_DFL);
}



/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "batcher usage_error.\n");
    exit(1);
}


int
main(ac, av)
    int		ac;
    char	*av[];
{
    static char	SKIPPING[] = "batcher %s skipping \"%.40s...\" %s\n";
    static char SPOOL[] = _PATH_SPOOL;
    static char	BATCHDIR[] = _PATH_BATCHDIR;
    BOOL	Redirect;
    FILE	*F;
    STRING	AltSpool;
    TIMEINFO	Now;
    char	*p;
    char	*data;
    char	line[BUFSIZ];
    char	buff[BUFSIZ];
    long	BytesInArt;
    long	BytesInCB;
    OFFSET_T	Cookie;
    SIZE_T	datasize;
    int		i;
    int		artfd;
    int		ArtsInCB;
    int		length;
    struct stat	Sb;

    /* Set defaults. */
    AltSpool = NULL;
    Redirect = TRUE;
    (void)umask(NEWSUMASK);

    /* Parse JCL. */
    while ((i = getopt(ac, av, "a:A:b:B:i:N:p:rs:S:v")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'a':
	    ArtsInBatch = atoi(optarg);
	    break;
	case 'A':
	    MaxArts = atol(optarg);
	    break;
	case 'b':
	    BytesInBatch = atol(optarg);
	    break;
	case 'B':
	    MaxBytes = atol(optarg);
	    break;
	case 'i':
	    InitialString = optarg;
	    break;
	case 'N':
	    MaxBatches = atoi(optarg);
	    break;
	case 'p':
	    Processor = optarg;
	    break;
	case 'r':
	    Redirect = FALSE;
	    break;
	case 's':
	    Separator = optarg;
	    break;
	case 'S':
	    AltSpool = optarg;
	    break;
	case 'v':
	    STATprint = TRUE;
	    break;
	}
    if (MaxArts && ArtsInBatch == 0)
	ArtsInBatch = MaxArts;
    if (MaxBytes && BytesInBatch == 0)
	BytesInBatch = MaxBytes;

    /* Parse arguments. */
    ac -= optind;
    av += optind;
    if (ac != 1 && ac != 2)
	Usage();
    Host = av[0];
    if ((Input = av[1]) != NULL) {
	if (Input[0] != '/') {
	    Input = NEW(char, STRLEN(BATCHDIR) + 1 + strlen(av[1]) + 1);
	    (void)sprintf(Input, "%s/%s", BATCHDIR, av[1]);
	}
	if (freopen(Input, "r", stdin) == NULL) {
	    (void)fprintf(stderr, "batcher %s cant open %s %s\n",
		    Host, Input, strerror(errno));
		exit(1);
	}
    }

    if (Redirect)
	(void)freopen(_PATH_ERRLOG, "a", stderr);

    /* Go to where the articles are. */
    if (chdir(SPOOL) < 0) {
	(void)fprintf(stderr, "batcher %s cant cd %s %s\n",
		Host, SPOOL, strerror(errno));
	exit(1);
    }

    /* Set initial counters, etc. */
    datasize = 8 * 1024;
    data = NEW(char, datasize);
    BytesInCB = 0;
    ArtsInCB = 0;
    Cookie = -1;
    GotInterrupt = FALSE;
    (void)signal(SIGHUP, CATCHinterrupt);
    (void)signal(SIGINT, CATCHinterrupt);
    (void)signal(SIGTERM, CATCHinterrupt);
    /* (void)signal(SIGPIPE, CATCHinterrupt); */
    (void)GetTimeInfo(&Now);
    STATbegin = TIMEINFOasDOUBLE(Now);

    F = NULL;
    while (fgets(line, sizeof line, stdin) != NULL) {
	/* Record line length in case we do an ftell. Not portable to
	 * systems with non-Unix file formats. */
	length = strlen(line);

	/* Get lines like "name size" */
	if ((p = strchr(line, '\n')) == NULL) {
	    (void)fprintf(stderr, SKIPPING, Host, line, "too long");
	    continue;
	}
	*p = '\0';
	if (line[0] == '\0' || line[0] == COMMENT_CHAR)
	    continue;
	if ((p = strchr(line, ' ')) != NULL) {
	    *p++ = '\0';
	    BytesInArt = atol(p);
	}
	else
	    BytesInArt = -1;

	/* Strip of leading spool pathname. */
	if (line[0] == '/'
	 && line[STRLEN(SPOOL)] == '/'
	 && EQn(line, SPOOL, STRLEN(SPOOL)))
	    p = line + STRLEN(SPOOL) + 1;
	else
	    p = line;

	/* Open the file. */
	if ((artfd = open(p, O_RDONLY)) < 0) {
	    if (errno != ENOENT)
		(void)fprintf(stderr, SKIPPING, Host, p, strerror(errno));
	    if (AltSpool == NULL)
		continue;
	    (void)sprintf(buff, "%s/%s", AltSpool, p);
	    if ((artfd = open(buff, O_RDONLY)) < 0) {
		if (errno != ENOENT)
		    (void)fprintf(stderr, SKIPPING,
			    Host, buff, strerror(errno));
		continue;
	    }
	}

	/* If we need to, get its size. */
	if (BytesInArt < 0) {
	    if (fstat(artfd, &Sb) < 0) {
		(void)fprintf(stderr, SKIPPING, Host, line, strerror(errno));
		(void)close(artfd);
		continue;
	    }
	    if (!S_ISREG(Sb.st_mode)) {
		(void)fprintf(stderr, SKIPPING, Host, line, "not a file");
		(void)close(artfd);
		continue;
	    }
	    BytesInArt = Sb.st_size;
	}

	/* Have an open article, do we need to open a batch?  This code
	 * is here (rather then up before the while loop) so that we
	 * can avoid sending an empty batch.  The goto makes the code
	 * a bit more clear. */
	if (F == NULL) {
	    if (GotInterrupt) {
		(void)close(artfd);
		RequeueAndExit(Cookie, (char *)NULL, 0L);
	    }
	    if ((F = BATCHstart()) == NULL) {
		(void)fprintf(stderr, "batcher %s cant startbatch %d %s\n",
			Host, BATCHcount, strerror(errno));
		(void)close(artfd);
		break;
	    }
	    if (InitialString && *InitialString) {
		(void)fprintf(F, "%s\n", InitialString);
		BytesInCB += strlen(InitialString) + 1;
		BytesWritten += strlen(InitialString) + 1;
	    }
	    Cookie = ftell(stdin) - length;
	    goto SendIt;
	}

	/* We're writing a batch, see if adding the current article
	 * would exceed the limits. */
	if ((ArtsInBatch > 0 && ArtsInCB + 1 >= ArtsInBatch)
	 || (BytesInBatch > 0 && BytesInCB + BytesInArt >= BytesInBatch)) {
	    if ((BATCHstatus = BATCHclose(F)) != 0) {
		if (BATCHstatus == -1)
		    (void)fprintf(stderr, "batcher %s cant closebatch %d %s\n",
			    Host, BATCHcount, strerror(errno));
		else
		    (void)fprintf(stderr, "batcher %s batch %d exit %d\n",
			    Host, BATCHcount, BATCHstatus);
		(void)close(artfd);
		break;
	    }
	    ArtsInCB = 0;
	    BytesInCB = 0;

	    /* See if we can start a new batch. */
	    if ((MaxBatches > 0 && BATCHcount >= MaxBatches)
	     || (MaxBytes > 0 && BytesWritten + BytesInArt >= MaxBytes)
	     || (MaxArts > 0 && ArtsWritten + 1 >= MaxArts)) {
		(void)close(artfd);
		break;
	    }

	    if (GotInterrupt) {
		(void)close(artfd);
		RequeueAndExit(Cookie, line, BytesInArt);
	    }

	    if ((F = BATCHstart()) == NULL) {
		(void)fprintf(stderr, "batcher %s cant startbatch %d %s\n",
			Host, BATCHcount, strerror(errno));
		(void)close(artfd);
		break;
	    }
	    Cookie = ftell(stdin) - length;
	}

    SendIt:
	/* Now we can start to send the article! */
	if (Separator && *Separator) {
	    (void)sprintf(buff, Separator, BytesInArt);
	    BytesInCB += strlen(buff) + 1;
	    BytesWritten += strlen(buff) + 1;
	    if (fprintf(F, "%s\n", buff) == EOF || ferror(F)) {
		(void)fprintf(stderr, "batcher %s cant write separator %s\n",
		    Host, strerror(errno));
		(void)close(artfd);
		break;
	    }
	}

	/* Write the article.  In case of interrupts, retry the read but
	 * not the fwrite because we can't check that reliably and
	 * portably. */
	while ((i = read(artfd, (POINTER)data, datasize)) > 0 || errno == EINTR)
	    if (fwrite((POINTER)data, (SIZE_T)1, (SIZE_T)i, F) != i)
	        break;
	if (ferror(F)) {
	    (void)fprintf(stderr, "batcher %s cant write article %s\n",
		    Host, strerror(errno));
	    (void)close(artfd);
	    break;
	}
	(void)close(artfd);

	/* Update the counts. */
	BytesInCB += BytesInArt;
	BytesWritten += BytesInArt;
	ArtsInCB++;
	ArtsWritten++;

	if (GotInterrupt) {
	    BATCHstatus = BATCHclose(F);
	    RequeueAndExit(Cookie, line, BytesInArt);
	}
    }

    if (BATCHopen)
	BATCHstatus = BATCHclose(F);
    RequeueAndExit(Cookie, (char *)NULL, 0L);
    /* NOTREACHED */
}
