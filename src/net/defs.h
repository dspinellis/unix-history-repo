/* Copyright (c) 1979 Regents of the University of California */
# define THEKEY "hellotherekids"
/*
   get all the machine dependencies, standard I/O, and the
   configuration definitions (LOCAL machine, etc.)
*/

# include "mach.h"
# include <stdio.h>
# include "Paths.h"

# define min(a,b) (a > b ? b : a)
# define getremote(S) (remtable[(S)-'a'])

/* adjustable parameters, may differ per machine */

# define MAXBREAD 	3
# define ATIME 		20
# define DBV 		0
# define BLOCKSIZE 	500
# define SIZE 		100
# define INITFILE	"/usr/net/initfile"
# define NSEND 		20
# define SAMPL 		3600		/* 1 hour = 3600 */
# define BIGSAMPL 	64800L		/* 18 hours = 64800L */
# define LINKS		9

/* adjustable parameters, must be same on all machines */

# define MAXFILE 	100000L
# define VMAJOR		1
# define VMINOR 	0
# define TIMEBASE 	282098011L

/* non-adjustable constants */

# define FNS 40
# define BFS 260
# define NS 10
# define BROKENREAD -2
# define WRITEFAIL -3
# define INCR 040
# define MINSIZE 50

/* flags for packet type (pcode) */
# define RESET 01
# define REQUEST 02
# define ACK 04
# define PURGE 020

/* flags for mach type */
# define M_CORY 1
# define M_CC 2
# define M_VAX 3
# define M_INGRES 4
# define M_SRC 5

/* codes for cflag, powers of two */
# define F_NONOTIFY 04

/* tokens, returned by parser */
# define MACHINE 1
# define LOGIN 2
# define PASSWORD 3
# define NOTIFY 5
# define COMMAND 7
# define ID 8
# define YES 9
# define DEFAULT 10
# define WRITE 11
# define NO 12
# define FORCE 13
# define LOCALTOK 14
# define LINK 15
# define SPEED 16
# define VAXTOVAX 17
# define LENGTH 18
# define DEBUGTOK 19
# define ALTIME 20
# define ALCOUNT 21


int debugflg;
char remote, local;		/* must be global, remote is not initialized*/
char netcmd[],senddir[], resfile[], logfile[], writecmd[], mailcmd[], Bsh[];
char cmd[BFS*2], realcmd[BFS*2];
char device[20], machtype[], remtable[];
long ltime;
short masterseqno, lastseqno;
extern errno;
extern char *sys_errlist[];
int datasize;
char ttystr[20];
FILE *readtty,*writetty;
int readfd, writefd, pipesim;
int maxbread, atime;

/* used by parser which reads netrc and initfile */
struct tokstruct {
	char *tokstr;
	int tval;
	} toktab[];

/* used by parser to parse filenames, e.g. mach:file */
struct fd {
	char mach;
	char *fn;
	};

/* used to pass around info about user */
struct info {
	char login[NS];
	char mpasswd[20];
	int muid;		/* combines uid and gid for FUID */
	int mgid;		/* unused for FUID */
	char dir[FNS];
	char loginshell[FNS];
	char localname[NS];
	int jobno;
	char defcmd[FNS];
	char force;		/* if true, always prompt for login and pass */
	char nonotify;		/* if true, don't send anything back */
	char nowrite;		/* if true, mail rather than write to user */
	} status;
struct stat statbuf;
struct direct dirbuf;
struct packet {
	short seqno;
	char pcode;
	char len;
	char chksum;
	char data[1];
	};
	
/* the chksum is only on a per-perpacket level,
   which is not enough.
   There should be a checksum on the entire file as well.
   */
struct dumpstruc {
	long shorttime, longtime, elaptot, waittime, waittot;	/* in secs */
	long outime, ostime;				/* in 60ths sec */
	long nbytesent,nbytercv, bytetot;		/* in bytes */
	int nretrans, nabnormal, nloop;
	int ncksum,npacksent,npackrcv;
	int nnetcp,nnetlpr,nsmail,nnetmail,nresp,nnet;
	int npass, nsend, nsendfail;
	} dump;
struct bstruct {
	char *bname;
	char bmach;
	} btable[];

/* functions */

char *calloc(), *crypt(), *ctime(), *getenv(), *longname();
char *getun(), *comptime(), *getpass(), *handlesp();
FILE *mopen(), *fdopen();
struct passwd *getpwnam(), *getpwuid();
struct packet *getpacket();
long fixuplong(),atol();
