# define THEKEY "hellotherekids"
/*
   get all the machine dependencies, standard I/O, and the
   configuration definitions (LOCAL machine, etc.)
*/

# include <stdio.h>

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
# define SUPERUSER 	0

/* adjustable parameters, must be same on all machines */

/* MAXFILE is the file size limit.  If changed on one machine 
   but not the others, files bigger than the common minimum will
   be flushed with no guarantee of err msgs. Thus if one link
   is to be of a different limit than the others, make sure the users
   know this.
*/
# define MAXFILE 	100000L
# define MAXFILELARGE	500000L
/* the version of the protocol the network speaks */
# define VMAJOR		1
# define VMINOR 	0
/* the time constant added to all time stamps sent around the net */
# define TIMEBASE 	282098011L
/* the number of mail forwarding hops allowed before looping is detected */
# define MAXHOPS	30

/* non-adjustable constants */

/* PARMLIST = max size of variable length parm list used in protocol */
# define PARMLIST 2000
/* FNS = max length of file name string */
# define FNS 50
/* NS = length of UNIX user name*/
# define NS 10
/* returned by prot.c */
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
# define M_OTHER 6

/* codes for cflag, powers of two, max (8 bits - 'a'), others may be added */

/* F_QUIET means send back only error messages and output of programs,
   don't send back confimation with no data */
/* F_NONOTIFY means don't send back anything, ever, 
   even if there are errors (used for responses, etc.) */

# define F_QUIET 	02
# define F_NONOTIFY 	04


/* 
   at this point bring in the locally-dependent definitions.
   this way the above parms may be altered.
*/
# include "mach.h"
# include "Paths.h"

/* structure declarations */
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
	};

struct bstruct {
	char *bname;
	char bmach;
	};
/* functions */

char *calloc(), *crypt(), *ctime(), *getenv(), *longname();
char *SnFromUid(), *comptime(), *getpass(), *handlesp(), *SnCurrent();
FILE *mopen(), *fdopen();
struct passwd *getpwnam(), *getpwuid(), *PwdCurrent(), *getpwent();
struct packet *getpacket();
long fixuplong(),atol();

/* variables */
extern errno;
extern char *sys_errlist[];
int debugflg;
char remote, local;		/* must be global, remote is not initialized*/
char netcmd[],senddir[], resfile[], Bsh[];
char machtype[], remtable[];

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
	char quiet;		/* if true, only send a response back if rc !=0
				   or if there is stdout or stderr */
	char sTtyname[20];	/* tty user is on,full name e.g. /dev/tty0 */
	char sCmdAct[BUFSIZ];	/* the actual command the net will exec */
	char sCmdVirt[BUFSIZ];	/* the command the user thinks he is getting */
	long lTtytime;		/* the time recorded for tty login in utmp */
	} status;
