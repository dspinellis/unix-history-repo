/* Copyright (c) 1979 Regents of the University of California */
#include "sh.local.h"
/*
 * C shell
 *
 * Bill Joy, UC Berkeley
 * October, 1978
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/dir.h>

#define	isdir(d)	((d.st_mode & S_IFMT) == S_IFDIR)

#include <errno.h>
#include <setjmp.h>
#include <signal.h>

typedef	char	bool;

#define	eq(a, b)	(strcmp(a, b) == 0)

/*
 * Global flags
 */
bool	didcch;			/* Have closed unused fd's for child */
bool	didfds;			/* Have setup i/o fd's for child */
bool	doneinp;		/* EOF indicator after reset from readc */
bool	exiterr;		/* Exit if error or non-zero exit status */
bool	child;			/* Child shell ... errors cause exit */
bool	haderr;			/* Reset was because of an error */
bool	intty;			/* Input is a tty */
bool	intact;			/* We are interactive... therefore prompt */
bool	justpr;			/* Just print because of :p hist mod */
bool	loginsh;		/* We are a loginsh -> .login/.logout */
bool	noexec;			/* Don't execute, just syntax check */
bool	setintr;		/* Set interrupts on/off -> Wait intr... */
bool	timflg;			/* Time the next waited for command */

/*
 * Global i/o info
 */
char	*arginp;		/* Argument input for sh -c and internal `xx` */
int	onelflg;		/* 2 -> need line for -t, 1 -> exit on read */
char	*file;			/* Name of shell file for $0 */

char	*err;			/* Error message from scanner/parser */
int	errno;			/* Error from C library routines */
char	*shtemp;		/* Temp name for << shell files in /tmp */
time_t	time0;			/* Time at which the shell started */

/*
 * Miscellany
 */
char	*doldol;		/* Character pid for $$ */
int	uid;			/* Invokers uid */
time_t	chktim;			/* Time mail last checked */

/*
 * These are declared here because they want to be
 * initialized in sh.init.c (to allow them to be made readonly)
 */
char	*mesg[];

struct	biltins {
	char	*bname;
	int	(*bfunct)();
	short	minargs, maxargs;
} bfunc[];

struct srch {
	char	*s_name;
	short	s_value;
} srchn[];

/*
 * To be able to redirect i/o for builtins easily, the shell moves the i/o
 * descriptors it uses away from 0,1,2.
 * Ideally these should be in units which are closed across exec's
 * (this saves work) but for version 6, this is not usually possible.
 * The desired initial values for these descriptors are defined in
 * sh.local.h.
 */
short	SHIN;			/* Current shell input (script) */
short	SHOUT;			/* Shell output */
short	SHDIAG;			/* Diagnostic output... shell errs go here */
short	OLDSTD;			/* Old standard input (def for cmds) */

/*
 * Error control
 *
 * Errors in scanning and parsing set up an error message to be printed
 * at the end and complete.  Other errors always cause a reset.
 * Because of source commands and .cshrc we need nested error catches.
 */

jmp_buf	reslab;

#define	setexit()	setjmp(reslab)
#define	reset()		longjmp(reslab)
	/* Should use structure assignment here */
#define	getexit(a)	copy(a, reslab, sizeof reslab)
#define	resexit(a)	copy(reslab, a, sizeof reslab)

char	*gointr;		/* Label for an onintr transfer */
int	(*parintr)();		/* Parents interrupt catch */
int	(*parterm)();		/* Parents terminate catch */

/*
 * Lexical definitions.
 *
 * All lexical space is allocated dynamically.
 * The eighth bit of characters is used to prevent recognition,
 * and eventually stripped.
 */
#define QUOTE 	0200		/* Eighth char bit used internally for 'ing */
#define	TRIM	0177		/* Mask to strip quote bit */

/*
 * Each level of input has a buffered input structure.
 * There are one or more blocks of buffered input for each level,
 * exactly one if the input is seekable and tell is available.
 * In other cases, the shell buffers enough blocks to keep all loops
 * in the buffer.
 */
struct	Bin {
	off_t	Bfseekp;		/* Seek pointer */
	off_t	Bfbobp;			/* Seekp of beginning of buffers */
	off_t	Bfeobp;			/* Seekp of end of buffers */
	short	Bfblocks;		/* Number of buffer blocks */
	char	**Bfbuf;		/* The array of buffer blocks */
} B;

#define	fseekp	B.Bfseekp
#define	fbobp	B.Bfbobp
#define	feobp	B.Bfeobp
#define	fblocks	B.Bfblocks
#define	fbuf	B.Bfbuf

off_t	btell();

/*
 * The shell finds commands in loops by reseeking the input
 * For whiles, in particular, it reseeks to the beginning of the
 * line the while was on; hence the while placement restrictions.
 */
off_t	lineloc;

#ifdef	TELL
off_t	tell();
bool	cantell;			/* Is current source tellable ? */
#endif

/*
 * Input lines are parsed into doubly linked circular
 * lists of words of the following form.
 */
struct	wordent {
	char	*word;
	struct	wordent *prev;
	struct	wordent *next;
};

/*
 * During word building, both in the initial lexical phase and
 * when expanding $ variable substitutions, expansion by `!' and `$'
 * must be inhibited when reading ahead in routines which are themselves
 * processing `!' and `$' expansion or after characters such as `\' or in
 * quotations.  The following flags are passed to the getC routines
 * telling them which of these substitutions are appropriate for the
 * next character to be returned.
 */
#define	DODOL	1
#define	DOEXCL	2
#define	DOALL	DODOL|DOEXCL

/*
 * Labuf implements a general buffer for lookahead during lexical operations.
 * Text which is to be placed in the input stream can be stuck here.
 * We stick parsed ahead $ constructs during initial input,
 * process id's from `$$', and modified variable values (from qualifiers
 * during expansion in sh.dol.c) here.
 */
char	labuf[256];
char	*lap;

/*
 * Parser structure
 *
 * Each command is parsed to a tree of command structures and
 * flags are set bottom up during this process, to be propagated down
 * as needed during the semantics/exeuction pass (sh.sem.c).
 */
struct	command {
	short	t_dtyp;				/* Type of node */
	short	t_dflg;				/* Flags, e.g. FAND|... */
	union {
		char	*T_dlef;		/* Input redirect word */
		struct	command *T_dcar;	/* Left part of list/pipe */
	} L;
	union {
		char	*T_drit;		/* Output redirect word */
		struct	command *T_dcdr;	/* Right part of list/pipe */
	} R;
#define	t_dlef	L.T_dlef
#define	t_dcar	L.T_dcar
#define	t_drit	R.T_drit
#define	t_dcdr	R.T_dcdr
	char	**t_dcom;			/* Command/argument vector */
	struct	command *t_dspr;		/* Pointer to ()'d subtree */
};

#define	TCOM	1		/* t_dcom <t_dlef >t_drit	*/
#define	TPAR	2		/* ( t_dspr ) <t_dlef >t_drit	*/
#define	TFIL	3		/* t_dlef | t_drit		*/
#define	TLST	4		/* t_dlef ; t_drit		*/
#define	TOR	5		/* t_dlef || t_drit		*/
#define	TAND	6		/* t_dlef && t_drit		*/

#define FAND	(1<<0)		/* executes in background	*/
#define FCAT	(1<<1)		/* output is redirected >>	*/
#define FPIN	(1<<2)		/* input is a pipe		*/
#define FPOU	(1<<3)		/* output is a pipe		*/
#define FPAR	(1<<4)		/* don't fork, last ()ized cmd	*/
#define FINT	(1<<5)		/* don't make interruptible	*/
#define FPRS	(1<<6)		/* print number when forked	*/
#define FDIAG	(1<<7)		/* redirect unit 2 with unit 1	*/
#define	FANY	(1<<8)		/* output was !			*/
#define	FHERE	(1<<9)		/* input redirection is <<	*/
#define	FREDO	(1<<10)		/* reexec aft if, repeat,...	*/

/*
 * The keywords for the parser
 */
#define	ZBREAK		0
#define	ZBRKSW		1
#define	ZCASE		2
#define	ZDEFAULT 	3
#define	ZELSE		4
#define	ZEND		5
#define	ZENDIF		6
#define	ZENDSW		7
#define	ZEXIT		8
#define	ZFOREACH	9
#define	ZGOTO		10
#define	ZIF		11
#define	ZLABEL		12
#define	ZLET		13
#define	ZSET		14
#define	ZSWITCH		15
#define	ZTEST		16
#define	ZTHEN		17
#define	ZWHILE		18

/*
 * Structure defining the existing while/foreach loops at this
 * source level.  Loops are implemented by seeking back in the
 * input.  For foreach (fe), the word list is attached here.
 */
struct	whyle {
	off_t	w_start;		/* Point to restart loop */
	off_t	w_end;			/* End of loop (0 if unknown) */
	char	**w_fe, **w_fe0;	/* Current/initial wordlist for fe */
	char	*w_fename;		/* Name for fe */
	struct	whyle *w_next;		/* Next (more outer) loop */
} *whyles;

/*
 * Variable structure
 *
 * Lists of aliases and variables are sorted alphabetically by name
 */
struct	varent {
	char	**vec;		/* Array of words which is the value */
	char	*name;		/* Name of variable/alias */
	struct	varent *link;
} shvhed, aliases;

/*
 * The following are for interfacing redo substitution in
 * aliases to the lexical routines.
 */
struct	wordent *alhistp;		/* Argument list (first) */
struct	wordent *alhistt;		/* Node after last in arg list */
char	**alvec;			/* The (remnants of) alias vector */

/*
 * Filename/command name expansion variables
 */
short	gflag;				/* After tglob -> is globbing needed? */

/*
 * A reasonable limit on number of arguments would seem to be
 * the maximum number of characters in an arg list / 6.
 */
#define	GAVSIZ	NCARGS / 6

/*
 * Variables for filename expansion
 */
char	**gargv;			/* Pointer to the (stack) arglist */
short	gargc;				/* Number args in gargv */
short	gnleft;

/*
 * Variables for command expansion.
 */
char	**pargv;			/* Pointer to the argv list space */
char	*pargs;				/* Pointer to start current word */
short	pargc;				/* Count of arguments in pargv */
short	pnleft;				/* Number of chars left in pargs */
char	*pargcp;			/* Current index into pargs */

/*
 * History list
 *
 * Each history list entry contains an embedded wordlist
 * from the scanner, a number for the event, and a reference count
 * to aid in discarding old entries.
 *
 * Essentially "invisible" entries are put on the history list
 * when history substitution includes modifiers, and thrown away
 * at the next discarding since their event numbers are very negative.
 */
struct	Hist {
	struct	wordent Hlex;
	int	Hnum;
	int	Href;
	struct	Hist *Hnext;
} Histlist;

int	eventno;			/* Next events number */
int	lastev;				/* Last event reference (default) */

char	*Dfix1();
struct	varent *adrof(), *adrof1();
char	**blkcat();
char	**blkcpy();
char	**blkend();
char	**blkspl();
char	*calloc();
char	*cname();
char	**copyblk();
char	**dobackp();
char	*domod();
struct	wordent *dosub();
char	*exp3();
char	*exp3a();
char	*exp4();
char	*exp5();
char	*exp6();
struct	Hist *enthist();
struct	Hist *findev();
struct	wordent *freenod();
char	*getenv();
char	*getinx();
struct	varent *getvx();
struct	passwd *getpwnam();
struct	wordent *gethent();
struct	wordent *getsub();
char	*globone();
struct	biltins *isbfunc();
char	**glob();
char	*operate();
int	pintr();
char	*putn();
char	**saveblk();
char	*savestr();
char	*strcat();
char	*strcpy();
char	*strend();
char	*strings();
char	*strip();
char	*strspl();
char	*subword();
struct	command *syntax();
struct	command *syn0();
struct	command *syn1();
struct	command *syn1a();
struct	command *syn1b();
struct	command *syn2();
struct	command *syn3();
int	tglob();
int	trim();
char	*value(), *value1();
char	*xhome();
char	*xname();
char	*xset();

#define	NOSTR	((char *) 0)

/*
 * setname is a macro to save space (see sh.err.c)
 */
char	*bname;
#define	setname(a)	bname = (a);
