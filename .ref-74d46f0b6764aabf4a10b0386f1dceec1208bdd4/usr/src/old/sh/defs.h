/*	defs.h	4.5	89/05/08	*/

#
/*
 *	UNIX shell
 */

/* error exits from various parts of shell */
#define ERROR	1
#define SYNBAD	2
#define SIGFAIL 3
#define SIGFLG	0200

/* command tree */
#define FPRS	020
#define FINT	040
#define FAMP	0100
#define FPIN	0400
#define FPOU	01000
#define FPCL	02000
#define FCMD	04000
#define COMMSK	017

#define TCOM	0
#define TPAR	1
#define TFIL	2
#define TLST	3
#define TIF	4
#define TWH	5
#define TUN	6
#define TSW	7
#define TAND	8
#define TORF	9
#define TFORK	10
#define TFOR	11

/* execute table */
#define SYSSET	1
#define SYSCD	2
#define SYSEXEC	3
#define SYSLOGIN 4
#define SYSTRAP	5
#define SYSEXIT	6
#define SYSSHFT 7
#define SYSWAIT	8
#define SYSCONT 9
#define SYSBREAK 10
#define SYSEVAL 11
#define SYSDOT	12
#define SYSRDONLY 13
#define SYSTIMES 14
#define SYSXPORT 15
#define SYSNULL 16
#define SYSREAD 17
#define SYSTST	18
#define	SYSUMASK	19

/* used for input and output of shell */
#define INIO 10
#define OTIO 11

/*io nodes*/
#define USERIO	10
#define IOUFD	15
#define IODOC	16
#define IOPUT	32
#define IOAPP	64
#define IOMOV	128
#define IORDW	256
#define INPIPE	0
#define OTPIPE	1

/* arg list terminator */
#define ENDARGS	0

#include	"mac.h"
#include	"mode.h"
#include	"name.h"


/* result type declarations */
#define alloc malloc
ADDRESS		alloc();
VOID		addblok();
STRING		make();
STRING		movstr();
TREPTR		cmd();
TREPTR		makefork();
NAMPTR		lookup();
VOID		setname();
VOID		setargs();
DOLPTR		useargs();
REAL		expr();
STRING		catpath();
STRING		getpath();
STRING		*scan();
STRING		mactrim();
STRING		macro();
STRING		execs();
VOID		await();
VOID		post();
STRING		copyto();
VOID		exname();
STRING		staknam();
VOID		printnam();
VOID		printflg();
VOID		prs();
VOID		prc();
VOID		setupenv();
STRING		*setenv();

#define attrib(n,f)	(n->namflg |= f)
#define round(a,b)	(((int)((ADR(a)+b)-1))&~((b)-1))
#define closepipe(x)	(close(x[INPIPE]), close(x[OTPIPE]))
#define eq(a,b)		(cf(a,b)==0)
#define max(a,b)	((a)>(b)?(a):(b))
#define assert(x)	;

/* temp files and io */
UFD		output;
INT		ioset;
IOPTR		iotemp;		/* files to be deleted sometime */
IOPTR		iopend;		/* documents waiting to be read at NL */

/* substitution */
INT		dolc;
STRING		*dolv;
DOLPTR		argfor;
ARGPTR		gchain;

/* stack */
#define		BLK(x)	((BLKPTR)(x))
#define		BYT(x)	((BYTPTR)(x))
#define		STK(x)	((STKPTR)(x))
#define		ADR(x)	((char*)(x))

/* stak stuff */
#include	"stak.h"

/* string constants */
MSG		atline;
MSG		readmsg;
MSG		colon;
MSG		minus;
MSG		nullstr;
MSG		sptbnl;
MSG		unexpected;
MSG		endoffile;
MSG		synmsg;

/* name tree and words */
SYSTAB		reserved;
INT		wdval;
INT		wdnum;
ARGPTR		wdarg;
INT		wdset;
BOOL		reserv;

/* prompting */
MSG		stdprompt;
MSG		supprompt;
MSG		profile;

/* built in names */
NAMNOD		fngnod;
NAMNOD		ifsnod;
NAMNOD		homenod;
NAMNOD		mailnod;
NAMNOD		pathnod;
NAMNOD		ps1nod;
NAMNOD		ps2nod;

/* special names */
MSG		flagadr;
STRING		cmdadr;
STRING		exitadr;
STRING		dolladr;
STRING		pcsadr;
STRING		pidadr;

/* names always present */
MSG		mailname;
MSG		homename;
MSG		pathname;
MSG		fngname;
MSG		ifsname;
MSG		ps1name;
MSG		ps2name;

/* transput */
CHAR		tmpout[];
STRING		tmpnam;
INT		serial;
#define		TMPNAM 7
FILE		standin;
#define input	(standin->fdes)
#define eof	(standin->feof)
INT		peekc;
STRING		comdiv;
MSG		devnull;

/* flags */
#define		noexec	01
#define		intflg	02
#define		prompt	04
#define		setflg	010
#define		errflg	020
#define		ttyflg	040
#define		forked	0100
#define		oneflg	0200
#define		rshflg	0400
#define		waiting	01000
#define		stdflg	02000
#define		execpr	04000
#define		readpr	010000
#define		keyflg	020000
#define		batchflg	040000
INT		flags;

/* error exits from various parts of shell */
#include	<setjmp.h>
jmp_buf		subshell;
jmp_buf		errshell;
jmp_buf		INTbuf;

/* fault handling */
#include	"brkincr.h"
POS		brkincr;

#define MINTRAP	0
#define MAXTRAP	32

#define INTR	2
#define QUIT	3
#define MEMF	11
#define ALARM	14
#define KILL	15
#define TRAPSET	2
#define SIGSET	4
#define SIGMOD	8

VOID		fault();
BOOL		trapnote;
STRING		trapcom[];
BOOL		trapflg[];
BOOL		trapjmp[];

/* name tree and words */
STRING		*environ;
CHAR		numbuf[];
MSG		export;
MSG		readonly;

/* execflgs */
INT		exitval;
BOOL		execbrk;
INT		loopcnt;
INT		breakcnt;

/* messages */
MSG		mailmsg;
MSG		coredump;
MSG		badopt;
MSG		badparam;
MSG		badsub;
MSG		nospace;
MSG		notfound;
MSG		badtrap;
MSG		baddir;
MSG		badshift;
MSG		illegal;
MSG		restricted;
MSG		execpmsg;
MSG		notid;
MSG		wtfailed;
MSG		badcreate;
MSG		piperr;
MSG		badopen;
MSG		badnum;
MSG		arglist;
MSG		txtbsy;
MSG		toobig;
MSG		badexec;
MSG		notfound;
MSG		badfile;

address	end[];

#include	"ctype.h"

