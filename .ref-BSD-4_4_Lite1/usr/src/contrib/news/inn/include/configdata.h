/*  $Revision: 1.29 $
**
**  Here be configuration data used by various InterNetNews programs.
**  The numbers refer to sections in the config.dist file.
*/


/*
**  1.  MAKE CONFIG PARAMETERS
*/
    /* =()<#define @<USE_CHAR_CONST>@_USE_CHAR_CONST>()= */
#define DUNNO_USE_CHAR_CONST
#if	defined(DO_USE_CHAR_CONST)
typedef char const	*STRING;
typedef char * const	CSTRING;
#endif	/* defined(DO_USE_CHAR_CONST) */
#if	defined(DONT_USE_CHAR_CONST)
typedef char		*STRING;
typedef char		*CSTRING;
#endif	/* defined(DONT_USE_CHAR_CONST) */
#if	defined(DUNNO_USE_CHAR_CONST)
#if	defined(__STDC__)
typedef char const	*STRING;
typedef char * const	CSTRING;
#else
typedef char		*STRING;
typedef char		*CSTRING;
#endif	/* defined(__STDC__) */
#endif	/* defined(DUNNO_USE_CHAR_CONST) */

/*
**  Declare a function that doesn't return.
*/
#if	defined(__dead)
    /* BSD4.4 */
#define NORETURN	__dead
#else
#if	defined(__GNUC__)
    /* GCC */
#define NORETURN	volatile void
#else
    /* Everyone else */
#define NORETURN	void
#endif	/* defined(__GNUC__) */
#endif	/* defined(__dead) */


/*
**  3.  OWNERSHIPS AND FILE MODES
*/
    /* =()<#define NEWSUSER		"@<NEWSUSER>@">()= */
#define NEWSUSER		"news"
    /* =()<#define NEWSGID		"@<NEWSGROUP>@">()= */
#define NEWSGID		"news"
    /* =()<#define NEWSMASTER		"@<NEWSMASTER>@">()= */
#define NEWSMASTER		"usenet"
    /* =()<#define PATHMASTER		"@<PATHMASTER>@">()= */
#define PATHMASTER		"not-for-mail"
    /* Umask to set. */
    /* =()<#define NEWSUMASK		@<NEWSUMASK>@>()= */
#define NEWSUMASK		02
    /* Mode that incoming articles are created under. */
    /* =()<#define ARTFILE_MODE	@<ARTFILE_MODE>@>()= */
#define ARTFILE_MODE	0664
    /* Mode that batch files are created under. */
    /* =()<#define BATCHFILE_MODE	@<BATCHFILE_MODE>@>()= */
#define BATCHFILE_MODE	0664
    /* Mode that directories are created under. */
    /* =()<#define GROUPDIR_MODE	@<GROUPDIR_MODE>@>()= */
#define GROUPDIR_MODE	0775


/*
**  4.  C LIBRARY DIFFERENCES
*/
    /* Use have stdargs, varargs, or neither? */
    /* =()<#define VAR_@<VAR_STYLE>@>()= */
#define VAR_STDARGS
    /* Use BSD4.2 or Posix directory names? */
    /* =()<#define DIR_@<DIR_STYLE>@>()= */
#define DIR_DIRENT
    /* Use flock, lockf, or nothing to lock files? */
    /* =()<#define LOCK_@<LOCK_STYLE>@>()= */
#define LOCK_FLOCK
    /* Do you have <unistd.h>? */
    /* =()<#define @<HAVE_UNISTD>@_HAVE_UNISTD>()= */
#define DO_HAVE_UNISTD
    /* Do you have setbuffer? */
    /* =()<#define @<HAVE_SETBUFFER>@_HAVE_SETBUFFER>()= */
#define DO_HAVE_SETBUFFER
    /* Do you have gettimeofday? */
    /* =()<#define @<HAVE_GETTIMEOFDAY>@_HAVE_GETTIMEOFDAY>()= */
#define DO_HAVE_GETTIMEOFDAY
    /* Do you have fchmod? */
    /* =()<#define @<HAVE_FCHMOD>@_HAVE_FCHMOD>()= */
#define DO_HAVE_FCHMOD
    /* Do you have setsid()? */
    /* =()<#define @<HAVE_SETSID>@_HAVE_SETSID>()= */
#define DONT_HAVE_SETSID
    /* Does your (struct tm) have a tm_gmtoff field? */
    /* =()<#define @<HAVE_TM_GMTOFF>@_HAVE_TM_GMTOFF>()= */
#define DONT_HAVE_TM_GMTOFF
    /* Does your (struct stat) have a st_blksize field? */
    /* =()<#define @<HAVE_ST_BLKSIZE>@_HAVE_ST_BLKSIZE>()= */
#define DO_HAVE_ST_BLKSIZE
    /* Use waitpid instead of wait3? */
    /* =()<#define @<HAVE_WAITPID>@_HAVE_WAITPID>()= */
#define DO_HAVE_WAITPID
    /* Use "union wait" instead of int? */
    /* =()<#define @<USE_UNION_WAIT>@_USE_UNION_WAIT>()= */
#define DONT_USE_UNION_WAIT
    /* How to fork? */
    /* =()<#define FORK()	@<FORK>@()>()= */
#define FORK()	vfork()
    /* Do you have symbolic links? */
    /* =()<#define @<HAVE_SYMLINK>@_HAVE_SYMLINK>()= */
#define DO_HAVE_SYMLINK
    /* Does your AF_UNIX bind use sizeof for the socket size? */
    /* =()<#define @<BIND_USE_SIZEOF>@_BIND_USE_SIZEOF>()= */
#define DONT_BIND_USE_SIZEOF
    /* Do you have Unix-domain sockets? */
    /* =()<#define @<HAVE_UNIX_DOMAIN>@_HAVE_UNIX_DOMAIN>()= */
#define DO_HAVE_UNIX_DOMAIN
    /* How should close-on-exec be done? */
    /* =()<#define CLX_@<CLX_STYLE>@>()= */
#define CLX_IOCTL
    /* How should non-blocking I/O be done? */
    /* =()<#define NBIO_@<NBIO_STYLE>@>()= */
#define NBIO_FCNTL
    /* How should resource-totalling be done? */
    /* =()<#define RES_@<RES_STYLE>@>()= */
#define RES_RUSAGE
    /* How to get number of available descriptors? */
    /* =()<#define FDCOUNT_@<FDCOUNT_STYLE>@>()= */
#define FDCOUNT_GETDTAB

    /* If greater than -1, then use [gs]etrlimit to set that many descriptors. */
    /* If -1, then no [gs]etrlimit calls are done. */
    /* =()<#define NOFILE_LIMIT		@<NOFILE_LIMIT>@>()= */
#define NOFILE_LIMIT		-1
    /* Do you need <time.h> as well as <sys/time.h>? */
    /* =()<#define @<NEED_TIME>@_NEED_TIME>()= */
#define DONT_NEED_TIME
    /* What predicate, if any, the <ctype.h> macros need. */
    /* =()<#define CTYPE(isXXXXX, c)	(@<CTYPE>@)>()= */
#define CTYPE(isXXXXX, c)	((isascii((c)) && isXXXXX((c))))


/*
**  6.  MISCELLANEOUS CONFIG DATA
*/
    /* Use mmap() to read the active file, or read it in? */
    /* =()<#define ACT_@<ACT_STYLE>@>()= */
#define ACT_READ
    /* Use our NNTP-server-open routine, or the one in NNTP? */
    /* INND is nicer, but you must install inn.conf files everywhere; NNTP */
    /* is better if you already have lots of /usr/lib/news/server files. */
    /* =()<#define REM_@<REM_STYLE>@>()= */
#define REM_INND
    /* Should rnews save articles that the server rejects? */
    /* =()<#define @<RNEWS_SAVE_BAD>@_RNEWS_SAVE_BAD>()= */
#define DONT_RNEWS_SAVE_BAD
    /* Should rnews syslog articles innd already has? */
    /* =()<#define @<RNEWS_LOG_DUPS>@_RNEWS_LOG_DUPS>()= */
#define DONT_RNEWS_LOG_DUPS
    /* Look in _PATH_RNEWSPROGS for rnews unpackers? */
    /* =()<#define @<RNEWSPROGS>@_RNEWSPROGS>()= */
#define DO_RNEWSPROGS
    /* Should rnews try the local host? */
    /* =()<#define @<RNEWSLOCALCONNECT>@_RNEWSLOCALCONNECT>()= */
#define DO_RNEWSLOCALCONNECT
    /* Disallow posts with more than 50% inclusion (">") lines? */
    /* (This is only for inews and nnrpd.) */
    /* =()<#define @<CHECK_INCLUDED_TEXT>@_CHECK_INCLUDED_TEXT>()= */
#define DO_CHECK_INCLUDED_TEXT
    /* Put hosts in the inews Path header? */
    /* =()<#define @<INEWS_PATH>@_INEWS_PATH>()= */
#define DO_INEWS_PATH
    /* Munge the gecos field of password entry? */
    /* =()<#define @<MUNGE_GECOS>@_MUNGE_GECOS>()= */
#define DO_MUNGE_GECOS
    /* How many times to try before giving up */
    /* =()<#define MAX_FORKS	@<MAX_FORKS>@>()= */
#define MAX_FORKS	10
    /* Largest acceptable article size; 0 allows any size */
    /* =()<#define MAX_ART_SIZE	@<MAX_ART_SIZE>@>()= */
#define MAX_ART_SIZE	1000000
    /* Value of dbzincore(FLAG) call in innd. */
    /* =()<#define INND_DBZINCORE	@<INND_DBZINCORE>@>()= */
#define INND_DBZINCORE	1
    /* Should sub-processes get a nice(2) value? */
    /* =()<#define @<INND_NICE_KIDS>@_INND_NICE_KIDS>()= */
#define DONT_INND_NICE_KIDS
    /* Value for nice(2) call in innd. */
    /* =()<#define INND_NICE_VALUE	@<INND_NICE_VALUE>@>()= */
#define INND_NICE_VALUE	10
    /* Null-terminated list of unknown commands to not log to syslog. */
    /* =()<#define INND_QUIET_BADLIST	@<INND_QUIET_BADLIST>@>()= */
#define INND_QUIET_BADLIST	NULL
    /* Null-terminated set of illegal distribution patterns. */
    /* =()<#define BAD_DISTRIBS	@<BAD_DISTRIBS>@>()= */
#define BAD_DISTRIBS	"*.*",NULL
    /* Check that poster is the person doing the cancel? */
    /* =()<#define @<VERIFY_CANCELS>@_VERIFY_CANCELS>()= */
#define DONT_VERIFY_CANCELS
    /* Log "ctlinnd cancel" commands to syslog? */
    /* =()<#define @<LOG_CANCEL_COMMANDS>@_LOG_CANCEL_COMMANDS>()= */
#define DONT_LOG_CANCEL_COMMANDS
    /* File unknown "to.*" groups into the "to" newsgroup? */
    /* =()<#define @<MERGE_TO_GROUPS>@_MERGE_TO_GROUPS>()= */
#define DONT_MERGE_TO_GROUPS
    /* File articles in unknown newsgroups into junk? */
    /* =()<#define @<WANT_TRASH>@_WANT_TRASH>()= */
#define DONT_WANT_TRASH
    /* Record rejected articles in history? */
    /* =()<#define @<REMEMBER_TRASH>@_REMEMBER_TRASH>()= */
#define DONT_REMEMBER_TRASH
    /* Check the linecount against the Lines header? */
    /* =()<#define @<CHECK_LINECOUNT>@_CHECK_LINECOUNT>()= */
#define DONT_CHECK_LINECOUNT
    /* If checking, the error must be within LINECOUNT_FUZZ lines. */
    /* =()<#define LINECOUNT_FUZZ	@<LINECOUNT_FUZZ>@>()= */
#define LINECOUNT_FUZZ	5
    /* Have innd throttle itself after this many I/O errors. */
    /* =()<#define IO_ERROR_COUNT	@<IO_ERROR_COUNT>@>()= */
#define IO_ERROR_COUNT	50
    /* Default value for ctlinnd -t flag. */
    /* =()<#define CTLINND_TIMEOUT	@<CTLINND_TIMEOUT>@>()= */
#define CTLINND_TIMEOUT	0
    /* Flush logs (and NNRP connections) if we go this long with no I/O. */
    /* =()<#define DEFAULT_TIMEOUT	@<DEFAULT_TIMEOUT>@>()= */
#define DEFAULT_TIMEOUT	300
    /* INND closes channel if inactive this long (seconds). */
    /* =()<#define PEER_TIMEOUT	@<PEER_TIMEOUT>@>()= */
#define PEER_TIMEOUT	(1 * 60 * 60)
    /* NNRP exits if inactive this long (seconds). */
    /* =()<#define CLIENT_TIMEOUT	@<CLIENT_TIMEOUT>@>()= */
#define CLIENT_TIMEOUT	(2 * 60 * 60)
    /* Allow nnrpd readers when paused or throttled? */
    /* =()<#define @<ALLOW_READERS>@_ALLOW_READERS>()= */
#define DO_ALLOW_READERS
    /* Refuse NNTP connections if load is higher then this; -1 disables. */
    /* =()<#define NNRP_LOADLIMIT	@<NNRP_LOADLIMIT>@>()= */
#define NNRP_LOADLIMIT	16
    /* Don't readdir() spool dir if same group within this many seconds. */
    /* =()<#define NNRP_RESCAN_DELAY	@<NNRP_RESCAN_DELAY>@>()= */
#define NNRP_RESCAN_DELAY	60
    /* Do gethostbyaddr on client addresses in nnrp? */
    /* =()<#define @<NNRP_GETHOSTBYADDR>@_NNRP_GETHOSTBYADDR>()= */
#define DO_NNRP_GETHOSTBYADDR
    /* Should nnrpd do a dbzincore? */
    /* =()<#define NNRP_DBZINCORE_DELAY	@<NNRP_DBZINCORE_DELAY>@>()= */
#define NNRP_DBZINCORE_DELAY	40
    /*  Strip Sender from posts that did authenticate? */
    /* =()<#define @<NNRP_AUTH_SENDER>@_NNRP_AUTH_SENDER>()= */
#define DONT_NNRP_AUTH_SENDER
    /* How many read/write failures until channel is put to sleep or closed? */
    /* =()<#define BAD_IO_COUNT	@<BAD_IO_COUNT>@>()= */
#define BAD_IO_COUNT	5
    /* Multiplier for sleep in EWOULDBLOCK writes (seconds). */
    /* =()<#define BLOCK_BACKOFF	@<BLOCK_BACKOFF>@>()= */
#define BLOCK_BACKOFF	(2 * 60)
    /* How many article-writes between active and history updates? */
    /* =()<#define ICD_SYNC_COUNT	@<ICD_SYNC_COUNT>@>()= */
#define ICD_SYNC_COUNT	10
    /* Tell resolver _res.options to be fast? */
    /* =()<#define @<FAST_RESOLV>@_FAST_RESOLV>()= */
#define DONT_FAST_RESOLV
    /* Drop articles that were posted this many days ago. */
    /* =()<#define DEFAULT_CUTOFF	@<DEFAULT_CUTOFF>@>()= */
#define DEFAULT_CUTOFF	14
    /* Maximum number of incoming NNTP connections. */
    /* =()<#define DEFAULT_CONNECTIONS	@<DEFAULT_CONNECTIONS>@>()= */
#define DEFAULT_CONNECTIONS	50
    /* Wait this many seconds before channel restarts. */
    /* =()<#define CHANNEL_RETRY_TIME	@<CHANNEL_RETRY_TIME>@>()= */
#define CHANNEL_RETRY_TIME	(5 * 60)
    /* Wait this many seconds before seeing if pause is ended. */
    /* =()<#define PAUSE_RETRY_TIME	@<PAUSE_RETRY_TIME>@>()= */
#define PAUSE_RETRY_TIME	(5 * 60)
    /* Wait this many seconds before seeing if pause is ended. */
    /* =()<#define CHANNEL_INACTIVE_TIME	@<CHANNEL_INACTIVE_TIME>@>()= */
#define CHANNEL_INACTIVE_TIME	(10 * 60)
    /* Put nntplink info (filename) into the log? */
    /* =()<#define @<NNTPLINK_LOG>@_NNTPLINK_LOG>()= */
#define DONT_NNTPLINK_LOG
    /* Log by host IP address, rather than from Path line? */
    /* =()<#define @<IPADDR_LOG>@_IPADDR_LOG>()= */
#define DONT_IPADDR_LOG
    /* Log NNTP activity after this many articles. */
    /* =()<#define NNTP_ACTIVITY_SYNC	@<NNTP_ACTIVITY_SYNC>@>()= */
#define NNTP_ACTIVITY_SYNC	200
    /* Free buffers bigger than this when we're done with them. */
    /* =()<#define BIG_BUFFER	@<BIG_BUFFER>@>()= */
#define BIG_BUFFER	(2 * START_BUFF_SIZE)
    /* A general small buffer. */
    /* =()<#define SMBUF	@<SMBUF>@>()= */
#define SMBUF	256
    /* Buffer for a single article name. */
    /* =()<#define MAXARTFNAME	@<MAXARTFNAME>@>()= */
#define MAXARTFNAME	10
    /* Buffer for a single pathname in the spool directory. */
    /* =()<#define SPOOLNAMEBUFF	@<SPOOLNAMEBUFF>@>()= */
#define SPOOLNAMEBUFF	512
    /* Maximum size of a single header. */
    /* =()<#define MAXHEADERSIZE	@<MAXHEADERSIZE>@>()= */
#define MAXHEADERSIZE	1024
    /* Byte limit on locally-posted articles; 0 to disable the check. */
    /* =()<#define LOCAL_MAX_ARTSIZE	@<LOCAL_MAX_ARTSIZE>@>()= */
#define LOCAL_MAX_ARTSIZE	0

    /* Function that returns no value, and a pointer to it. */
    /* =()<#define FUNCTYPE	@<FUNCTYPE>@>()= */
#define FUNCTYPE	void
typedef FUNCTYPE	(*FUNCPTR)();



    /* While reading input, if we have less than LOW_WATER bytes free, we
     * use the current buffersize as input to GROW_AMOUNT to determine how
     * much to realloc.  (Doubling seems to be the best thing right now.)
     * Growth must be at least NNTP_STRLEN bytes! */
#define START_BUFF_SIZE		(4 * 1024)
#define LOW_WATER		(1 * 1024)
#define GROW_AMOUNT(x)		((x))

    /* Some debuggers might need this set to an empty string. */
#define STATIC			static

    /* How to store article numbers; note that INN is not int/long clean. */
typedef unsigned long	ARTNUM;

    /* A general convenience; you shouldn't have to change this. */
typedef int		BOOL;

    /* General values that you should not have to change. */
#define MEMCPY_THRESHOLD	12
#define MAX_BUILTIN_ARGV	20
#define NNTP_PORT		119
#define TRUE			1
#define FALSE			0
#define MAXLISTEN		5
#define STDIN			0
#define STDOUT			1
#define PIPE_READ		0
#define PIPE_WRITE		1
#define DATE_FUZZ		(24L * 60L * 60L)
#define COMMENT_CHAR		'#'
#define ART_ACCEPT		'+'
#define ART_CANC		'c'
#define ART_JUNK		'j'
#define ART_REJECT		'-'
#define EXP_CONTROL		'!'
#define FEED_MAXFLAGS		20
#define FEED_BYTESIZE		'b'
#define FEED_FULLNAME		'f'
#define FEED_HDR_DISTRIB	'D'
#define FEED_HDR_NEWSGROUP	'N'
#define FEED_MESSAGEID		'm'
#define FEED_FNLNAMES		'*'
#define FEED_HEADERS		'H'
#define FEED_NAME		'n'
#define FEED_NEWSGROUP		'g'
#define FEED_OVERVIEW		'O'
#define FEED_REPLIC		'R'
#define FEED_SITE		's'
#define FEED_TIMERECEIVED	't'
#define HIS_BADCHAR		'_'
#define HIS_FIELDSEP		'\t'
#define HIS_NOEXP		"-"
#define HIS_SUBFIELDSEP		'~'
#define NF_FIELD_SEP		':'
#define NF_FLAG_ALIAS		'='
#define NF_FLAG_EXCLUDED	'j'
#define NF_FLAG_MODERATED	'm'
#define NF_FLAG_OK		'y'
#define NF_FLAG_NOLOCAL		'n'
#define NF_FLAG_IGNORE		'x'
#define NF_SUBFIELD_SEP		'/'
#define NG_SEPARATOR		","
#define NG_ISSEP(c)		((c) == ',')
#define RNEWS_MAGIC1		'#'
#define RNEWS_MAGIC2		'!'
#define SIG_MAXLINES		4
#define SIG_SEPARATOR		"-- \n"
#define SUB_DEFAULT		FALSE
#define SUB_NEGATE		'!'
#define LOOPBACK_HOST		"127.0.0.1"
