/*  $Revision: 1.39 $
**
**  Variable definitions, miscellany, and main().
*/
#define DEFINE_DATA
#include "innd.h"
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/uio.h>
#if	NOFILE_LIMIT > 0
#include <sys/resource.h>
#endif	/* NOFILE_LIMIT > 0 */
#if	defined(DO_FAST_RESOLV)
#include <arpa/nameser.h>
#include <resolv.h>
#endif	/* defined(DO_FAST_RESOLV) */


#if	defined(HAVE_SETBUFFER)
#define SETBUFFER(F, buff, size)	setbuffer((F), (buff), (size))
STATIC int	LogBufferSize = 4096;
#else
#define SETBUFFER(F, buff, size)	setbuf((F), (buff))
STATIC int	LogBufferSize = BUFSIZ;
#endif	/* defined(HAVE_SETBUFFER) */


BOOL		AmRoot = TRUE;
BOOL		BufferedLogs = TRUE;
BOOL		NNRPTracing = FALSE;
BOOL		Tracing = FALSE;
char		LogName[] = "ME";
char		SPOOL[] = _PATH_SPOOL;
int		ErrorCount = IO_ERROR_COUNT;
int		MaxIncoming = DEFAULT_CONNECTIONS;
int		SPOOLlen = STRLEN(SPOOL);
long		LargestArticle = MAX_ART_SIZE;
OPERATINGMODE	Mode = OMrunning;
time_t		Cutoff = DEFAULT_CUTOFF * 24 * 60 * 60;

#if	defined(__CENTERLINE__)
BOOL		Debug = TRUE;
#else
BOOL		Debug = FALSE;
#endif	/* defined(__CENTERLINE__) */

#if	defined(lint) || defined(__CENTERLINE__)
int		KeepLintQuiet = 0;
#endif	/* defined(lint) || defined(__CENTERLINE__) */


STATIC char	*ErrlogBuffer;
STATIC char	*LogBuffer;
STATIC char	ERRLOG[] = _PATH_ERRLOG;
STATIC char	INNDDIR[] = _PATH_INNDDIR;
STATIC char	LOG[] = _PATH_LOGFILE;
STATIC char	PID[] = _PATH_SERVERPID;
STATIC UID_T	NewsUID;
STATIC GID_T	NewsGID;



/*
**  Sprintf a long into a buffer with enough leading zero's so that it
**  takes up width characters.  Don't add trailing NUL.  Return TRUE
**  if it fit.  Used for updating high-water marks in the active file
**  in-place.
*/
BOOL
FormatLong(p, value, width)
    register char	*p;
    register long	value;
    register int	width;
{
    for (p += width - 1; width-- > 0; ) {
	*p-- = (int)(value % 10) + '0';
	value /= 10;
    }
    return value == 0;
}


/*
**  Glue a string, a char, and a string together.  Useful for making
**  filenames.
*/
void
FileGlue(p, n1, c, n2)
    register char	*p;
    register char	*n1;
    char		c;
    register char	*n2;
{
    p += strlen(strcpy(p, n1));
    *p++ = c;
    (void)strcpy(p, n2);
}


/*
**  Turn any \r or \n in text into spaces.  Used to splice back multi-line
**  headers into a single line.
*/
STATIC char *
Join(text)
    register char	*text;
{
    register char	*p;

    for (p = text; *p; p++)
	if (*p == '\n' || *p == '\r')
	    *p = ' ';
    return text;
}


/*
**  Return a short name that won't overrun our bufer or syslog's buffer.
**  q should either be p, or point into p where the "interesting" part is.
*/
char *
MaxLength(p, q)
    char		*p;
    char		*q;
{
    static char		buff[80];
    register int	i;

    /* Already short enough? */
    i = strlen(p);
    if (i < sizeof buff - 1)
	return Join(p);

    /* Simple case of just want the begining? */
    if (q - p < sizeof buff - 4) {
	(void)strncpy(buff, p, sizeof buff - 4);
	(void)strcpy(&buff[sizeof buff - 4], "...");
    }
    /* Is getting last 10 characters good enough? */
    else if ((p + i) - q < 10) {
	(void)strncpy(buff, p, sizeof buff - 14);
	(void)strcpy(&buff[sizeof buff - 14], "...");
	(void)strcpy(&buff[sizeof buff - 11], &p[i - 10]);
    }
    else {
	/* Not in last 10 bytes, so use double elipses. */
	(void)strncpy(buff, p, sizeof buff - 17);
	(void)strcpy(&buff[sizeof buff - 17], "...");
	(void)strncpy(&buff[sizeof buff - 14], &q[-5], 10);
	(void)strcpy(&buff[sizeof buff - 4], "...");
    }
    return Join(buff);
}


/*
**  Split text into comma-separated fields.  Return an allocated
**  NULL-terminated array of the fields within the modified argument that
**  the caller is expected to save or free.  We don't use strchr() since
**  the text is expected to be either relatively short or "comma-dense."
*/
char **
CommaSplit(text)
    char		*text;
{
    register int	i;
    register char	*p;
    register char	**av;
    char		**save;

    /* How much space do we need? */
    for (i = 2, p = text; *p; p++)
	if (*p == ',')
	    i++;

    for (av = save = NEW(char*, i), *av++ = p = text; *p; )
	if (*p == ',') {
	    *p++ = '\0';
	    *av++ = p;
	}
	else
	    p++;
    *av = NULL;
    return save;
}


/*
**  Do we need a shell for the command?  If not, av is filled in with
**  the individual words of the command and the command is modified to
**  have NUL's inserted.
*/
BOOL
NeedShell(p, av, end)
    register char	*p;
    register char	**av;
    register char	**end;
{
    static char		Metachars[] = ";<>|*?[]{}()#$&=`'\"\\~\n";
    register char	*q;

    /* We don't use execvp(); works for users, fails out of /etc/rc. */
    if (*p != '/')
	return TRUE;
    for (q = p; *q; q++)
	if (strchr(Metachars, *q) != NULL)
	    return TRUE;

    for (end--; av < end; ) {
	/* Mark this word, check for shell meta-characters. */
	for (*av++ = p; *p && !ISWHITE(*p); p++)
	    continue;

	/* If end of list, we're done. */
	if (*p == '\0') {
	    *av = NULL;
	    return FALSE;
	}

	/* Skip whitespace, find next word. */
	for (*p++ = '\0'; ISWHITE(*p); p++)
	    continue;
	if (*p == '\0') {
	    *av = NULL;
	    return FALSE;
	}
    }

    /* Didn't fit. */
    return TRUE;
}


/*
**  Spawn a process, with I/O redirected as needed.  Return the PID or -1
**  (and a syslog'd message) on error.
*/
int
Spawn(fd0, fd1, fd2, av)
    int		fd0;
    int		fd1;
    int		fd2;
    char	*av[];
{
    static char	NOCLOSE[] = "%s cant close %d in %s %m";
    static char	NODUP2[] = "%s cant dup2 %d to %d in %s %m";
    int		i;

    /* Fork; on error, give up.  If not using the patched dbz, make
     * this call fork! */
    i = FORK();
    if (i == -1) {
	syslog(L_ERROR, "%s cant fork %s %m", LogName, av[0]);
	return -1;
    }

    /* If parent, do nothing. */
    if (i > 0)
	return i;

    /* Child -- do any I/O redirection. */
    if (fd0 != 0) {
	if (dup2(fd0, 0) < 0) {
	    syslog(L_FATAL, NODUP2, LogName, fd0, 0, av[0]);
	    _exit(1);
	}
	if (fd0 != fd1 && fd0 != fd2 && close(fd0) < 0)
	    syslog(L_ERROR, NOCLOSE, LogName, fd0, av[0]);
    }
    if (fd1 != 1) {
	if (dup2(fd1, 1) < 0) {
	    syslog(L_FATAL, NODUP2, LogName, fd1, 1, av[0]);
	    _exit(1);
	}
	if (fd1 != fd2 && close(fd1) < 0)
	    syslog(L_ERROR, NOCLOSE, LogName, fd1, av[0]);
    }
    if (fd2 != 2) {
	if (dup2(fd2, 2) < 0) {
	    syslog(L_FATAL, NODUP2, LogName, fd2, 2, av[0]);
	    _exit(1);
	}
	if (close(fd2) < 0)
	    syslog(L_ERROR, NOCLOSE, LogName, fd2, av[0]);
    }
    CloseOnExec(0, FALSE);
    CloseOnExec(1, FALSE);
    CloseOnExec(2, FALSE);

    /* Try to set our permissions. */
#if	defined(DO_INND_NICE_KIDS)
    (void)nice(INND_NICE_VALUE);
#endif	/* defined(DO_INND_NICE_KIDS) */
    if (setgid(NewsGID) == -1)
	syslog(L_ERROR, "%s cant setgid in %s %m", LogName, av[0]);
    if (setuid(NewsUID) == -1)
	syslog(L_ERROR, "%s cant setuid in %s %m", LogName, av[0]);

    /* Close the DBZ database without doing any writing. */
    /* Not needed with the patched DBZ; can't be used with vfork.
     * (void)dbzcancel();
     * (void)dbmclose();
     */

    /* Start the desired process (finally!). */
    (void)execv(av[0], av);
    syslog(L_FATAL, "%s cant exec in %s %m", LogName, av[0]);
    _exit(1);
    /* NOTREACHED */
}


/*
**  Stat our control directory and see who should own things.
*/
STATIC BOOL
GetNewsOwnerships()
{
    struct stat	Sb;

    /* Make sure item exists and is of the right type. */
    if (stat(INNDDIR, &Sb) < 0)
	return FALSE;
    if (!S_ISDIR(Sb.st_mode))
	return FALSE;
    NewsUID = Sb.st_uid;
    NewsGID = Sb.st_gid;
    return TRUE;
}


/*
**  Change the onwership of a file.
*/
void
xchown(p)
    char	*p;
{
    if (chown(p, NewsUID, NewsGID) < 0)
	syslog(L_ERROR, "%s cant chown %s %m", LogName, p);
}


/*
**  Try to make one directory.  Return FALSE on error.
*/
STATIC BOOL
MakeDir(Name)
    char		*Name;
{
    struct stat		Sb;

    if (mkdir(Name, GROUPDIR_MODE) >= 0) {
	if (AmRoot)
	    xchown(Name);
	return TRUE;
    }

    /* See if it failed because it already exists. */
    return stat(Name, &Sb) >= 0 && S_ISDIR(Sb.st_mode);
}


/*
**  Given a directory, comp/foo/bar, create that directory and all
**  intermediate directories needed.  Return 0 if ok, else -1.
*/
BOOL
MakeSpoolDirectory(Name)
    register char	*Name;
{
    register char	*p;
    BOOL		made;

    /* Optimize common case -- parent almost always exists. */
    if (MakeDir(Name))
	return TRUE;

    /* Try to make each of comp and comp/foo in turn. */
    for (p = Name; *p; p++)
	if (*p == '/') {
	    *p = '\0';
	    made = MakeDir(Name);
	    *p = '/';
	    if (!made)
		return FALSE;
	}

    return MakeDir(Name);
}


/*
**  Flush one log file, with pessimistic size of working filename buffer.
*/
void
ReopenLog(F)
    FILE	*F;
{
    char	buff[sizeof LOG + sizeof ERRLOG + 4 + 1];
    char	*Name;
    char	*Buffer;
    int		mask;
    
    if (Debug)
	return;
    if (F == Log) {
	Name = LOG;
	Buffer = LogBuffer;
    }
    else {
	Name = ERRLOG;
	Buffer = ErrlogBuffer;
    }

    FileGlue(buff, Name, '.', "old");
    if (rename(Name, buff) < 0)
	syslog(L_ERROR, "%s cant rename %s to %s %m", LogName, Name, buff);
    mask = umask(033);
    if (freopen(Name, "a", F) != F) {
	syslog(L_FATAL, "%s cant freopen %s %m", LogName, Name);
	exit(1);
    }
    (void)umask(mask);
    if (AmRoot)
	xchown(Name);
    if (BufferedLogs)
	SETBUFFER(F, Buffer, LogBufferSize);
}


/*
**  Function called when memory allocation fails.
*/
STATIC int
AllocationFailure(what, i)
    char		*what;
    unsigned int	i;
{
    /* Print i as %d so huge values are real obvious. */
    syslog(L_FATAL, "%s cant %s %d bytes %m", LogName, what, i);
    exit(1);
    /* NOTREACHED */
}


/*
**  We ran out of space or other I/O error, throttle ourselves.
*/
void
ThrottleIOError(when)
    char	*when;
{
    char	buff[SMBUF];
    STRING	p;
    int		oerrno;

    if (Mode == OMrunning) {
	oerrno = errno;
	if (Reservation) {
	    DISPOSE(Reservation);
	    Reservation = NULL;
	}
	(void)sprintf(buff, "%s writing %s file -- throttling",
	    strerror(oerrno), when);
	if ((p = CCblock(OMthrottled, buff)) != NULL)
	    syslog(L_ERROR, "%s cant throttle %s", LogName, p);
	syslog(L_FATAL, "%s throttle %s", LogName, buff);
	errno = oerrno;
    }
}


/*
**  Close down all parts of the system (e.g., before calling exit or exec).
*/
void
JustCleanup()
{
    SITEflushall(FALSE);
    /* PROCclose(FALSE); */
    CCclose();
    LCclose();
    NCclose();
    RCclose();
    ICDclose();
    HISclose();
    ARTclose();
    (void)sleep(1);
    /* PROCclose(TRUE); */
    if (unlink(PID) < 0 && errno != ENOENT)
	syslog(L_ERROR, "%s cant unlink %s %m", LogName, PID);
}


/*
**  The name is self-explanatory.
*/
NORETURN
CleanupAndExit(x, why)
    int		x;
    char	*why;
{
    JustCleanup();
    syslog(L_NOTICE, "%s shutdown %s", LogName, why);
    exit(x);
}


#if	NOFILE_LIMIT > 0
/*
**  Set the limit on the number of open files we can have.  I don't
**  like having to do this.
*/
STATIC void
SetDescriptorLimit(i)
    int			i;
{
    struct rlimit	rl;

    if (getrlimit(RLIMIT_NOFILE, &rl) < 0) {
	syslog(L_ERROR, "%s cant getrlimit(NOFILE) %m", LogName);
	return;
    }
    rl.rlim_cur = i;
    if (setrlimit(RLIMIT_NOFILE, &rl) < 0) {
	syslog(L_ERROR, "%s cant setrlimit(NOFILE) %d %m", LogName, i);
	return;
    }
}
#endif	/* NOFILE_LIMIT > 0 */


/*
**  Signal handler to catch SIGTERM and queue a clean shutdown.
*/
STATIC SIGHANDLER
CatchTerminate(s)
    int		s;
{
    GotTerminate = TRUE;
    (void)signal(s, CatchTerminate);
}


/*
**  Print a usage message and exit.
*/
STATIC NORETURN
Usage()
{
    (void)fprintf(stderr, "Usage error.\n");
    exit(1);
}


int
main(ac, av)
    int			ac;
    char		*av[];
{
    static char		WHEN[] = "PID file";
    int			i;
    int			port;
    int			logflags;
    char		buff[SMBUF];
    char		*master;
    STRING		path;
    STRING		p;
    FILE		*F;
    BOOL		ShouldFork;
    BOOL		ShouldRenumber;
    BOOL		ShouldSyntaxCheck;
    long		pid;
#if	defined(_DEBUG_MALLOC_INC)
    union malloptarg	m;
#endif	/* defined(_DEBUG_MALLOC_INC) */

    /* Set up the pathname, first thing. */
    path = av[0];
    if (path == NULL || *path == '\0')
	path = "innd";
    else if ((p = strrchr(path, '/')) != NULL)
	path = p + 1;
    ONALLLOCFAIL(AllocationFailure);
    Version = INNVersion();

    /* Handle malloc debugging. */
#if	defined(_DEBUG_MALLOC_INC)
    m.i = M_HANDLE_ABORT;
    dbmallopt(MALLOC_WARN, &m);
    dbmallopt(MALLOC_FATAL, &m);
    m.i = 3;
    dbmallopt(MALLOC_FILLAREA, &m);
    m.i = 0;
    dbmallopt(MALLOC_CKCHAIN, &m);
    dbmallopt(MALLOC_CKDATA, &m);
#endif	/* defined(_DEBUG_MALLOC_INC) */

    /* Set defaults. */
    TimeOut.tv_sec = DEFAULT_TIMEOUT;
    ShouldFork = TRUE;
    ShouldRenumber = FALSE;
    ShouldSyntaxCheck = FALSE;
    logflags = L_OPENLOG_FLAGS | LOG_NOWAIT;
    port = -1;
    master = NULL;
#if	defined(DONT_ALLOW_READERS)
    NNRPFollows = TRUE;
#endif	/* defined(DONT_ALLOW_READERS) */

#if	defined(DO_FAST_RESOLV)
    /* We only use FQDN's in the hosts.nntp file. */
    _res.options &= ~(RES_DEFNAMES | RES_DNSRCH);
#endif	/* defined(DO_FAST_RESOLV) */

    /* Parse JCL. */
    CCcopyargv(av);
    while ((i = getopt(ac, av, "ac:dfi:l:m:o:n:p:rsS:t:ux")) != EOF)
	switch (i) {
	default:
	    Usage();
	    /* NOTREACHED */
	case 'a':
	    AnyIncoming = TRUE;
	    break;
	case 'c':
	    Cutoff = atoi(optarg) * 24 * 60 * 60;
	    break;
	case 'd':
	    Debug = TRUE;
#if	defined(LOG_PERROR)
	    logflags = LOG_PERROR | (logflags & ~LOG_CONS);
#endif	/* defined(LOG_PERROR) */
	    break;
	case 'f':
	    ShouldFork = FALSE;
	    break;
	case 'i':
	    MaxIncoming = atoi(optarg);
	    break;
	case 'l':
	    LargestArticle = atol(optarg);
	    break;
	case 'm':
	    if (ModeReason)
		DISPOSE(ModeReason);
	    switch (*optarg) {
	    default:
		Usage();
		/* NOTREACHED */
	    case 'g':	Mode = OMrunning;	break;
	    case 'p':	Mode = OMpaused;	break;
	    case 't':	Mode = OMthrottled;	break;
	    }
	    if (Mode != OMrunning) {
		(void)sprintf(buff, "%sed from command line",
			Mode == OMpaused ? "Paus" : "Throttl");
		ModeReason = COPY(buff);
	    }
	    break;
	case 'n':
	    switch (*optarg) {
	    default:
		Usage();
		/* NOTREACHED */
	    case 'n':	NNRPFollows = TRUE;	break;
	    case 'y':	NNRPFollows = FALSE;	break;
	    }
	    break;
	case 'o':
	    MaxOutgoing = atoi(optarg);
	    break;
	case 'p':
	    /* Silently ignore multiple -p flags, in case ctlinnd xexec
	     * called inndstart. */
	    if (port == -1) {
		port = atoi(optarg);
		AmRoot = FALSE;
	    }
	    break;
	case 'r':
	    ShouldRenumber = TRUE;
	    break;
	case 's':
	    ShouldSyntaxCheck = TRUE;
	    break;
	case 'S':
	    master = optarg;
	    break;
	case 't':
	    TimeOut.tv_sec = atol(optarg);
	    break;
	case 'u':
	    BufferedLogs = FALSE;
	    break;
	case 'x':
	    AlwaysCrosspost = TRUE;
	    break;
	}
    ac -= optind;
    if (ac != 0)
	Usage();
    if (ModeReason && NNRPFollows)
	NNRPReason = COPY(ModeReason);

    openlog(path, logflags, LOG_INN_SERVER);

    if (ShouldSyntaxCheck) {
	if ((p = CCcheckfile((char **)NULL)) == NULL)
	    exit(0);
	(void)fprintf(stderr, "%s\n", p + 2);
    }

    /* Go to where the data is. */
    if (chdir(SPOOL) < 0) {
	syslog(L_FATAL, "%s cant chdir %s %m", LogName, SPOOL);
	exit(1);
    }

    /* Get the Path entry. */
    if ((path = GetConfigValue(_CONF_PATHHOST)) == NULL) {
	syslog(L_FATAL, "%s cant GetConfigValue %s %m",
	    LogName, _CONF_PATHHOST);
	exit(1);
    }
    Path.Used = strlen(path) + 1;
    Path.Data = NEW(char, Path.Used + 1);
    (void)sprintf(Path.Data, "%s!", path);

    /* Get the Xref prefix. */
    Xref.Size = SMBUF;
    Xref.Data = NEW(char, Xref.Size);
    (void)sprintf(Xref.Data, "Xref: %s", path);
    Xref.Used = strlen(Xref.Data);
    Xrefbase = Xref.Used;

#if	!defined(__CENTERLINE__)
    /* Set standard input to /dev/null. */
    if ((i = open("/dev/null", O_RDWR)) < 0) {
	syslog(L_FATAL, "%s cant open /dev/null %m", LogName);
	exit(1);
    }
    if (dup2(i, 0) != 0)
	syslog(L_NOTICE, "%s cant dup2 %d to 0 %m", LogName, i);
    (void)close(i);
#endif	/* !defined(__CENTERLINE__) */

    /* Set up our permissions. */
    (void)umask(NEWSUMASK);
    if (!GetNewsOwnerships()) {
	syslog(L_FATAL, "%s internal cant stat control directory %m", LogName);
	exit(1);
    }
    if (port != -1 && setgid(NewsGID) < 0)
	syslog(L_ERROR, "%s cant setgid running as %d not %d %m",
	    LogName, (int)getgid(), (int)NewsGID);

    if (Debug) {
	Log = stdout;
	Errlog = stderr;
	(void)signal(SIGINT, CatchTerminate);
    }
    else {
	if (ShouldFork) {
	    /* Become a server. */
	    i = fork();
	    if (i < 0) {
		syslog(L_FATAL, "%s cant fork %m", LogName);
		exit(1);
	    }
	    if (i > 0)
		_exit(0);

#if	defined(TIOCNOTTY)
	    /* Disassociate from terminal. */
	    if ((i = open("/dev/tty", O_RDWR)) >= 0) {
		if (ioctl(i, TIOCNOTTY, (char *)NULL) < 0)
		    syslog(L_ERROR, "%s cant ioctl(TIOCNOTTY) %m", LogName);
		if (close(i) < 0)
		    syslog(L_ERROR, "%s cant close /dev/tty %m", LogName);
	    }
#endif	/* defined(TIOCNOTTY) */
#if	defined(DO_HAVE_SETSID)
	    (void)setsid();
#endif	/* defined(DO_HAVE_SETSID) */
	}

	/* Open the Log. */
	(void)fclose(stdout);
	if ((Log = fopen(LOG, "a")) == NULL) {
	    syslog(L_FATAL, "%s cant fopen %s %m", LogName, LOG);
	    exit(1);
	}
	if (AmRoot)
	    xchown(LOG);
	if (BufferedLogs && (LogBuffer = NEW(char, LogBufferSize)) != NULL)
	    SETBUFFER(Log, LogBuffer, LogBufferSize);

	/* Open the Errlog. */
	(void)fclose(stderr);
	if ((Errlog = fopen(ERRLOG, "a")) == NULL) {
	    syslog(L_FATAL, "%s cant fopen %s %m", LogName, ERRLOG);
	    exit(1);
	}
	if (AmRoot)
	    xchown(ERRLOG);
	if (BufferedLogs && (ErrlogBuffer = NEW(char, LogBufferSize)) != NULL)
	    SETBUFFER(Errlog, ErrlogBuffer, LogBufferSize);
    }

    /* Set number of open channels. */
#if	NOFILE_LIMIT > 0
    if (AmRoot)
	SetDescriptorLimit(NOFILE_LIMIT);
#endif	/* NOFILE_LIMIT > 0 */
    /* Get number of open channels. */
    if ((i = getfdcount()) < 0) {
	syslog(L_FATAL, "%s cant getfdcount %m", LogName);
	exit(1);
    }
    syslog(L_NOTICE, "%s descriptors %d", LogName, i);
    if (MaxOutgoing == 0) {
	/* getfdcount() - (stdio + dbz + cc + lc + rc + art + fudge) */
	MaxOutgoing = i - (  3   +  3  +  2 +  1 +  1 +  1  +   2  );
	syslog(L_NOTICE, "%s outgoing %d", LogName, MaxOutgoing);
    }

    /* See if another instance is alive. */
    if ((F = fopen(PID, "r")) != NULL) {
	if (fgets(buff, sizeof buff, F) != NULL
	 && ((pid = atoi(buff)) > 0)
	 && (kill((PID_T)pid, 0) > 0 || errno != ESRCH)) {
	    (void)syslog(L_FATAL, "%s already_running pid %d", LogName, pid);
	    exit(1);
	}
	(void)fclose(F);
    }

    /* Set up the various parts of the system.  Channel feeds start
     * processes so call PROCsetup before ICDsetup.  NNTP needs to know
     * if it's a slave, so call RCsetup before NCsetup. */
    (void)signal(SIGTERM, CatchTerminate);
#if	defined(SIGDANGER)
    (void)signal(SIGDANGER, CatchTerminate);
#endif	/* defined(SIGDANGER) */
    CHANsetup(i);
    PROCsetup(i * 2);
    HISsetup();
    CCsetup();
    LCsetup();
    RCsetup(port, master);
    NCsetup(i);
    ARTsetup();
    ICDsetup(TRUE);
#if	defined(_DEBUG_MALLOC_INC)
    m.i = 1;
    dbmallopt(MALLOC_CKCHAIN, &m);
    dbmallopt(MALLOC_CKDATA, &m);
#endif	/* defined(_DEBUG_MALLOC_INC) */

    /* Record our PID. */
    pid = getpid();
    if ((F = fopen(PID, "w")) == NULL) {
	IOError(WHEN);
	syslog(L_ERROR, "%s cant fopen %s %m", LogName, PID);
    }
    else {
	if (fprintf(F, "%ld\n", (long)pid) == EOF || ferror(F)) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant fprintf %s %m", LogName, PID);
	}
	if (fclose(F) == EOF) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant fclose %s %m", LogName, PID);
	}
	if (chmod(PID, 0664) < 0) {
	    IOError(WHEN);
	    syslog(L_ERROR, "%s cant chmod %s %m", LogName, PID);
	}
    }

    /* And away we go... */
    if (ShouldRenumber) {
	syslog(L_NOTICE, "%s renumbering", LogName);
	ICDrenumberactive();
    }
    syslog(L_NOTICE, "%s starting", LogName);
    CHANreadloop();
    CleanupAndExit(1, "CHANreadloop returned");
    /* NOTREACHED */
}
