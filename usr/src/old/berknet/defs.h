/*	@(#)defs.h	4.1	(Berkeley)	9/12/82	*/

/* sccs id variable */
static char *defs_h_sid = "@(#)defs.h	1.5";
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
/* the size of routing tables, etc. */
# define MAXINX 	26

/* adjustable parameters, must be same on all machines */

/* MAXFILELARGE is the file size limit.  If changed on one machine 
   but not the others, files bigger than the common minimum will
   be flushed with no guarantee of err msgs. Thus if one link
   is to be of a different limit than the others, make sure the users
   know this.
   MAXDAYFILE is the largest file that will be transmitted across the
   network during the day on CC machines.
*/
# define MAXFILELARGE	500000L
# define MAXDAYFILE	200000L
/* the version of the protocol the network speaks */
# define VMAJOR		1
# define VMINOR 	0
/* the time constant added to all time stamps sent around the net */
# define TIMEBASE 	282098011L
/* the number of mail forwarding hops allowed before looping is detected */
# define MAXHOPS	30
/* the buffer size used in prot.c */
# define MAXNBUF 1024

/* non-adjustable constants */

/* PARMLIST = max size of variable length parm list used in protocol */
# define PARMLIST 2000
/* FNS = max length of file name string */
# define FNS 80
/* NS = length of UNIX user name*/
# define NS 10
/* returned by prot.c */
# define BROKENREAD -2
# define WRITEFAIL -3
# define INCR 040
# define MINSIZE 50
# define TRUE 1
# define FALSE 0

/* flags for packet type (pcode) */
# define REQUEST 02
# define ACK 04
# define PURGE 020

/* flags for mach type */
# define M_CC 2
# define M_INGRES 4
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
/* strings for the netsend and netrecieve logins */
# define RESP	"hello hello this is a network"
# define RESPSIZE 29
# define QUERY	"This login for network use only"
# define QSIZE 31
# define WHOAREYOU "who are you?"
# define WHOSIZE 12

# include "mach.h"
# include "Paths.h"
/* bring in the exit codes */
# include <sysexits.h>

/* structure declarations */
struct packet {
	short seqno;
	char pcode;
	short len;
	char chksum;
	char data[1];
	};
	
struct packet *packptr;	/* just used to get the sizeof to work */
# define ACKLENGTH (sizeof *packptr - 1)
/* these are the lengths to be read and writ if using high-speed block dev. */
/* must be bigger than ACKLENGTH */
# define SENDLEN 256

/* the chksum is only on a per-perpacket level,
   which is not enough.
   There should be a checksum on the entire file as well.
   */
struct dumpstruc {
	long longtime, elaptot;				/* in secs */
	long nbytesent,nbytercv, bytetot;		/* in bytes */
	long lastndays;					/* in days */
	long braw, brawtot;				/* raw bytes*/
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
char *comptime(), *getpass(), *handlesp();
FILE *fopen(), *fdopen(), *popen(), *mailopen();
struct passwd *getpwnam(), *getpwuid(), *PwdCurrent(), *getpwent();
struct packet *getpacket();
long atol();

/* constant variables */
extern char *sys_errlist[];
char netcmd[],senddir[], resfile[], Bsh[];
char machtype[], remtable[];
char local;

/* variables which are modified */
extern errno;
int debugflg;
char remote;		/* must be global, remote is not initialized*/

/* various structure types */

/* used to pass around info about user */
struct userinfo {
	char 	login[NS];
	char 	mpasswd[20];
	int 	muid;		/* combines uid and gid for FUID */
	int 	mgid;		/* unused for FUID */
	char	jobno[10];	/* CC crn, length = 0 if null and not */
				/*      CC machine (else MAGICCRN ) */
	char 	dir[FNS];	/* login directory */
	char 	loginshell[FNS];/* login shell */
	char 	localname[NS];
	char 	defcmd[FNS];
	char 	force;		/* if true, always prompt for login and pass */
	char 	nonotify;	/* if true, don't send anything back */
	char 	nowrite;	/* if true, mail rather than write to user */
	char 	quiet;		/* if true, only send a response back if rc !=0
				   or if there is stdout or stderr */
	} ;

/* unique message - id sent with requests */
struct messageid {
	char 	msg_mch;	/* machine it is on */
	int 	msg_pid;	/* process id */
	long	msg_ltime;	/* current time */
};

/* header which describes information transferred across the link */
struct header {
	char	hd_mchto;		/* 1-letter code for dest. machine */
	char	hd_mchfrom;		/* 1-letter code for source machine */
	char	hd_snto[NS];		/* login name on mchto mach */
	char	hd_snfrom[NS];		/* login name on mchfrom mach */
	char    hd_spasswd[20];		/* password for snto */
	char	hd_code;		/* request code in protocol */
	char	hd_fnonotify;		/* if true, don't send anything back */
	char	hd_fquiet;		/* if true, only send back if error */
	char	hd_vmajor;		/* major version number */
	char	hd_vminor;		/* minor version number */
	char 	hd_sttyname[20];	/* tty user is on,e.g. /dev/tty0 */
	char 	hd_scmdact[BUFSIZ];	/* the actual cmd the net will exec */
	char 	hd_scmdvirt[BUFSIZ];	/* the cmd the user thinks he is exec */
	long 	hd_lttytime;		/* the time for tty login in utmp */
	long 	hd_ltimesent;		/* the time the request was sent */
	char	hd_srespfile[FNS];	/* response file name, if sepecified */
	char	hd_sinfile[FNS];	/* remote input file, if sepecified */
	char	hd_soutfile[FNS];	/* remote output file, if sepecified */
	/* sent but not computed (always MAGICCRN or 0 length) across the net */
	char	hd_ijobno[10];		/* CC job number, if applicable */
	/* computed, not transferred across the net */
	char	hd_addrto[FNS];		/* address of dest. acct */
	char	hd_addrfrom[FNS];	/* address of source acct */
	/* not now being sent over, will be someday, don't use now */
	char	hd_sencpasswd[20];	/* encrypted passwd with nbs 2way enc */
	int 	hd_ifilemode;		/* file mode for netcp */
	char 	hd_sfndefault[FNS];	/* default filename ext, for netcp */
	int	hd_uidfrom;		/* userid on the from machine */
	int 	hd_gidfrom;		/* groupid on the from machine */
	struct messageid hd_mesgid;	/* message id unique to this request */
	char	hd_fcompressed;		/* if true, data is compressed */
	char	hd_facctpair;		/* if true, is an accnt pair w/o pwds */
	char	hd_addrreplyto[FNS];	/* reply to this address */
};

/* 
	this structure defines the various parameters the daemon and testing
   	programs use -- most of the info comes from netrc.c
	NOTE-- this structure is initialized in netrc.c
	don't add members without changing that structure
*/
struct daemonparms {
	int 	dp_inspeed;	/* for stty, 7=300, 9=1200, 13=9600 baud */
	int 	dp_outspeed;	/* for stty, 7=300, 9=1200, 13=9600 baud */
	int 	dp_maxbread;	/* number of read time outs allowed */
	int 	dp_atime;	/* time to set alarm for timeout */
	int 	dp_oatime;	/* default time alarm for timeout */
	char 	dp_device[20];	/* name of the network file, e.g. /dev/net-A*/
	int 	dp_datasize;	/* length of data part of packet */
	int	dp_trynetl;	/* try to use netl, -l disables */
	int 	dp_onlyuid;	/* if non-zero, only send this uid's things */
	int	dp_linefd;	/* daemon should read and write from this */
	char	dp_usehispeed;	/* if true, use high-speed link */
	char	dp_hispeedlink[20];/* device name of high speed link */
	short	dp_sndorcv;	/* if <0, only send, if > 0, only recieve */
	int	dp_linedis;	/* line disc we use, normal is 0 */ 
	int 	dp_pipesim;	/* simulate with pipes */
	FILE	*dp_rdfile;	/* if pipesim then should read from this */
	int	dp_pwritefd;	/* if pipesim then should write from this */
	int	dp_use8bit;	/* use 8 bit protocol */
	short	dp_timeout;	/* timeout deamon (dialup) if > 0 */
};

/* macro implementation of harg due to bugs in Onyx C compiler */
# define harg(str) {\
	if (argv[0][2]) \
		strcpy(str, argv[0]+2); \
	else \
		{ strcpy(str, argv[1]); argc--; argv++; } \
	}
