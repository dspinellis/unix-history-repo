#include "stdio.h"
	/*  some system names  */
#define MYNAME		"vax135"

#define THISDIR		"/usr/lib/uucp"
#define SQFILE		"/usr/lib/uucp/SQFILE"
#define SQTMP		"/usr/lib/uucp/SQTMP"
#define SYSFILE		"/usr/lib/uucp/L.sys"
#define SYSFILECR	"/usr/lib/uucp/L.sys.cr"
#define DEVFILE		"/usr/lib/uucp/L-devices"
#define DIALFILE	"/usr/lib/uucp/L-dialcodes"
#define USERFILE	"/usr/lib/uucp/USERFILE"

#define SPOOL		"/usr/spool/uucp"
#define LOGDIR		"/usr/spool/uucp"
#define SEQFILE		"/usr/spool/uucp/SEQF"
#define SQLOCK		"/usr/spool/uucp/LCK.SQ"
#define SYSLOG		"/usr/spool/uucp/SYSLOG"
#define XQTDIR		"/usr/spool/uucp/.XQTDIR"

#define SEQLOCK		"LCK.SEQL"
#define CMDPRE		'C'
#define DATAPRE		'D'
#define XQTPRE		'X'

#define LOGPREFIX	"LOG."
#define LOGLOCK	"/usr/spool/uucp/LCK.LOG"
#define LOGFILE	"/usr/spool/uucp/LOGFILE"

#define RMTDEBUG	"AUDIT"
#define SQTIME		60L

#define DEBUG(l, f, s) if (Debug >= l) fprintf(stderr, f, s)

#define ASSERT(e, f, v) if (!(e)) {\
fprintf(stderr, "AERROR - (%s) ", "e");\
fprintf(stderr, f, v);\
cleanup(FAIL);};

#define FMV(p, n) close(n); dup(p[n]); close(p[n]);

#define SAME 0
#define FAIL -1
#define SUCCESS 0
#define MASTER 1
#define SLAVE 0
#define MAXFULLNAME 100
#define MAXMSGTIME 45
#define MAXCHARTIME 15
#define NAMESIZE 15
#define ACULAST "-<"
#define EOTMSG "\004\n\004\n"
#define CALLBACK 1
#define LINKLEVEL 1
#define ANYREAD 04

	/*  commands  */
#define SHELL		"/bin/sh"
#define MAIL		"mail"
#define UUCICO		"/usr/lib/uucp/uucico"
#define UUXQT		"/usr/lib/uucp/uuxqt"
#define UUCP		"usr/lib/uucp/uucp"


	/*  call connect fail stuff  */
#define CF_SYSTEM	-1
#define CF_TIME		-2
#define CF_LOCK		-3
#define CF_DIAL		-5
#define CF_LOGIN	-6

	/*  system status stuff  */
#define SS_OK		0
#define SS_FAIL		4
#define SS_NODEVICE	1
#define SS_CALLBACK	2
#define SS_INPROGRESS	3
#define SS_BADSEQ	5

	/*  fail/retry parameters  */
#define RETRYTIME 3300L
#define INPROGTIME 7200L
#define MAXRECALLS 10

	/*  stuff for command execution  */
#define X_RQDFILE	'F'
#define X_STDIN		'I'
#define X_STDOUT	'O'
#define X_CMD		'C'
#define X_USER		'U'
#define X_SENDFILE	'S'
#define X_LOCK		"LCK.XQT"
#define X_LOCKTIME	3600L

int Ifn, Ofn;
char Rmtname[10];
char User[10];
char Loginuser[10];
char *Thisdir;
char *Spool;
char *Myname;
char *Sysfiles[];
char *Devfile;
char *Dialfile;
int Debug;
int Pkdebug;
int Pkdrvon;
int Bspeed;

	/* the define geteuid should be deleted if the */
	/* routine is available in the libc.a library. */
#define geteuid getuid

#define WKDSIZE	100	/*  size of work dir name  */
char Wrkdir[WKDSIZE];
