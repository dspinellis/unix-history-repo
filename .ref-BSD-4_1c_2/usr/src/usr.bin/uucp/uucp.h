/*	@(#)uucp.h	4.4	(Berkeley)	2/18/83	*/

#include "stdio.h"
/* define DATAKIT if datakit is available */

/* define DIALOUT if new dialout routine is to be used */

/* define the last characters for ACU */


#define ACULAST "-<"

	/*  some system names  */
/*  put in local uucp name of this machine */
/*
 * For UCB 4.1A and Later System, you will have the gethostname(2) call.
 * if this call exists, define GETHOST.
 *
 * For BTL 3.0 and Later USG products, the uname(2) call is used.
 * If your systemis one of these, use this call,
 *
 * Otherwise, you must set the string to your system name.
 */

#define GETHOST		1 /* We have a 4.1A System */
/* #define UNAME	1 */
/* #define MYNAME	"hostnameunknown" */

/*
 * If you have a Berkeley UNIX system, include <sysexits.h> to use exit
 * codes that will be understood by sendmail.
 *	Otherwise, define EX_NOHOST and EX_CANTCREAT.
 */

#include <sysexits.h>

/* #define EX_NOHOST	101 */
/* #define EX_CANTCREAT	1 */

/*
 * define LONGNAMES if you have login names >8 chars long.
 * This isn't handled too cleanly - they are arbitrarily truncated
 * at 8 for entries in the control files to avoid choking remote
 * sites which haven't allocated enough space for the username.
 * This unfortunately may cause return mail to be lost.  Sigh.
 */
/* #define	LONGNAMES	1 */

#define SLCKTIME 5400    /* system/device timeout (LCK.. files) */

#define THISDIR		"/usr/lib/uucp"
#define XQTDIR		"/usr/lib/uucp/.XQTDIR"
#define SQFILE		"/usr/lib/uucp/SQFILE"
#define SQTMP		"/usr/lib/uucp/SQTMP"
#define SEQFILE		"/usr/lib/uucp/SEQF"
#define SYSFILE		"/usr/lib/uucp/L.sys"
#define SYSFILECR	"/usr/lib/uucp/L.sys.cr"
#define DEVFILE		"/usr/lib/uucp/L-devices"
#define DIALFILE	"/usr/lib/uucp/L-dialcodes"
#define USERFILE	"/usr/lib/uucp/USERFILE"

#define SPOOL		"/usr/spool/uucp"
#define LOGDIR		"/usr/spool/uucp"
#define SQLOCK		"/usr/spool/uucp/LCK.SQ"
#define SYSLOG		"/usr/spool/uucp/SYSLOG"
#define PUBDIR		"/usr/spool/uucppublic"

#define SEQLOCK		"LCK.SEQL"
#define CMDPRE		'C'
#define DATAPRE		'D'
#define XQTPRE		'X'

#define LOGPREFIX	"LOG."
#define LOGLOCK	"/usr/spool/uucp/LCK.LOG"
#define LOGFILE	"/usr/spool/uucp/LOGFILE"

#define RMTDEBUG	"AUDIT"
#define SQTIME		60L
#define TRYCALLS	2	/* number of tries to dial call */

#define DEBUG(l, f, s) if (Debug >= l) fprintf(stderr, f, s)

#define ASSERT(e, f, v) if (!(e)) {\
fprintf(stderr, "AERROR - (%s) ", "e");\
fprintf(stderr, f, v);\
fprintf(stderr, "\n");\
cleanup(FAIL);};

#define FMV(p, n) close(n); dup(p[n]); close(p[n]);

#define SAME 0
#define ANYREAD 04
#define ANYWRITE 02
#define FAIL -1
#define SUCCESS 0
#define MASTER 1
#define SLAVE 0
#define MAXFULLNAME 256
#define MAXMSGTIME 100
#define MAXCHARTIME 8		/* Time to wait between sending "expect" field in conn.c */
#define NAMESIZE 256
#define EOTMSG "\004\n\004\n"
#define CALLBACK 1

	/*  commands  */
#define SHELL		"/bin/sh"
#define MAIL		"mail"
#define UUCICO		"/usr/lib/uucp/uucico"
#define UUXQT		"/usr/lib/uucp/uuxqt"
#define UUCP		"uucp"


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
#define X_NONOTI	'N'
#define X_NONZERO	'Z'
#define X_LOCK		"LCK.XQT"
#define X_LOCKTIME	3600L

int Ifn, Ofn;
char Rmtname[32];
char User[32];
char Loginuser[32];
char *Thisdir;
char *Spool;
char Myname[32];
char *Sysfiles[];
char *Devfile;
char *Dialfile;
int Debug;
int Pkdebug;
int Pkdrvon;
int Bspeed;


#define WKDSIZE	100	/*  size of work dir name  */
char Wrkdir[WKDSIZE];
