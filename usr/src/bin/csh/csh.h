/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)csh.h	5.10 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/signal.h>
#include <errno.h>
#include <setjmp.h>
#include "sh.local.h"
#include "sh.char.h"

/*
 * C shell
 *
 * Bill Joy, UC Berkeley
 * October, 1978; May 1980
 *
 * Jim Kulp, IIASA, Laxenburg Austria
 * April, 1980
 */

typedef	char	bool;

#define	eq(a, b)	(strcmp(a, b) == 0)

/*
 * Global flags
 */
bool	chkstop;		/* Warned of stopped jobs... allow exit */
bool	didfds;			/* Have setup i/o fd's for child */
bool	doneinp;		/* EOF indicator after reset from readc */
bool	exiterr;		/* Exit if error or non-zero exit status */
bool	child;			/* Child shell ... errors cause exit */
bool	haderr;			/* Reset was because of an error */
bool	intty;			/* Input is a tty */
bool	intact;			/* We are interactive... therefore prompt */
bool	justpr;			/* Just print because of :p hist mod */
bool	loginsh;		/* We are a loginsh -> .login/.logout */
bool	neednote;		/* Need to pnotify() */
bool	noexec;			/* Don't execute, just syntax check */
bool	pjobs;			/* want to print jobs if interrupted */
bool	setintr;		/* Set interrupts on/off -> Wait intr... */
bool	timflg;			/* Time the next waited for command */
bool	havhash;		/* path hashing is available */
#ifdef FILEC
bool	filec;			/* doing filename expansion */
#endif

/*
 * Global i/o info
 */
char	*arginp;		/* Argument input for sh -c and internal `xx` */
int	onelflg;		/* 2 -> need line for -t, 1 -> exit on read */
char	*file;			/* Name of shell file for $0 */

char	*err;			/* Error message from scanner/parser */
int	errno;			/* Error from C library routines */
char	*shtemp;		/* Temp name for << shell files in /tmp */
struct	timeval time0;		/* Time at which the shell started */
struct	rusage ru0;

/*
 * Miscellany
 */
char	*doldol;		/* Character pid for $$ */
int	uid;			/* Invokers uid */
time_t	chktim;			/* Time mail last checked */
int	shpgrp;			/* Pgrp of shell */
int	tpgrp;			/* Terminal process group */
/* If tpgrp is -1, leave tty alone! */
int	opgrp;			/* Initial pgrp and tty pgrp */

/*
 * These are declared here because they want to be
 * initialized in sh.init.c (to allow them to be made readonly)
 */

struct biltins {
	char	*bname;
	int	(*bfunct)();
	short	minargs, maxargs;
} bfunc[];
extern int nbfunc;

struct srch {
	char	*s_name;
	short	s_value;
} srchn[];
extern int nsrchn;

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

/* Should use structure assignment here. */
#define	getexit(a)	bcopy((void *)reslab, (void *)(a), sizeof(reslab))
#define	resexit(a)	bcopy(((void *)(a)), (void *)reslab, sizeof(reslab))

char	*gointr;		/* Label for an onintr transfer */
sig_t	parintr;		/* Parents interrupt catch */
sig_t	parterm;		/* Parents terminate catch */

/*
 * Lexical definitions.
 *
 * All lexical space is allocated dynamically.
 * The eighth bit of characters is used to prevent recognition,
 * and eventually stripped.
 */
#define	QUOTE 	0200		/* Eighth char bit used internally for 'ing */
#define	TRIM	0177		/* Mask to strip quote bit */

/*
 * Each level of input has a buffered input structure.
 * There are one or more blocks of buffered input for each level,
 * exactly one if the input is seekable and tell is available.
 * In other cases, the shell buffers enough blocks to keep all loops
 * in the buffer.
 */
struct Bin {
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

#define btell()	fseekp

#ifndef btell
off_t	btell();
#endif

/*
 * The shell finds commands in loops by reseeking the input
 * For whiles, in particular, it reseeks to the beginning of the
 * line the while was on; hence the while placement restrictions.
 */
off_t	lineloc;

#ifdef	TELL
bool	cantell;			/* Is current source tellable ? */
#endif

/*
 * Input lines are parsed into doubly linked circular
 * lists of words of the following form.
 */
struct wordent {
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
 * Text which is to be placed in the input stream can be stuck here.  We stick
 * parsed ahead $ constructs during initial input, process id's from `$$',
 * and modified variable values (from qualifiers during expansion in sh.dol.c)
 * here.
 */
char	labuf[BUFSIZ];

char	*lap;

/*
 * Parser structure
 *
 * Each command is parsed to a tree of command structures and flags are set
 * bottom up during this process, to be propagated down as needed during the
 * semantics/exeuction pass (sh.sem.c).
 */
struct	command {
#define	NODE_COMMAND	1		/* t_dcom <t_dlef >t_drit	*/
#define	NODE_PAREN	2		/* ( t_dspr ) <t_dlef >t_drit	*/
#define	NODE_PIPE	3		/* t_dlef | t_drit		*/
#define	NODE_LIST	4		/* t_dlef ; t_drit		*/
#define	NODE_OR		5		/* t_dlef || t_drit		*/
#define	NODE_AND	6		/* t_dlef && t_drit		*/
	short t_dtyp;			/* Node type */

#define	F_SAVE	(F_NICE|F_TIME|F_NOHUP)	/* save these when re-doing */

#define	F_AMPERSAND	0x0001		/* executes in background	*/
#define	F_APPEND	0x0002		/* output is redirected >>	*/
#define	F_NICE		0x0004		/* t_nice is meaningful */
#define	F_NOFORK	0x0008		/* don't fork, last ()ized cmd	*/
#define	F_NOHUP		0x0010		/* nohup this command */
#define	F_NOINTERRUPT	0x0020		/* should be immune from intr's */
#define	F_OVERWRITE	0x0040		/* output was !			*/
#define	F_PIPEIN	0x0080		/* input is a pipe		*/
#define	F_PIPEOUT	0x0100		/* output is a pipe		*/
#define	F_READ		0x0200		/* input redirection is <<	*/
#define	F_REPEAT	0x0400		/* reexec aft if, repeat,...	*/
#define	F_STDERR	0x0800		/* redirect unit 2 with unit 1	*/
#define	F_TIME		0x1000		/* time this command */
	short t_dflg;			/* flags */

	union {
		char *T_dlef;		/* Input redirect word */
		struct command *T_dcar;	/* Left part of list/pipe */
	} L;
	union {
		char *T_drit;		/* Output redirect word */
		struct command *T_dcdr;	/* Right part of list/pipe */
	} R;
#define	t_dlef	L.T_dlef
#define	t_dcar	L.T_dcar
#define	t_drit	R.T_drit
#define	t_dcdr	R.T_dcdr
	char **t_dcom;			/* Command/argument vector */
	struct command *t_dspr;		/* Pointer to ()'d subtree */
	short t_nice;
};

/* Parser tokens. */
#define	T_BREAK		0
#define	T_BRKSW		1
#define	T_CASE		2
#define	T_DEFAULT 	3
#define	T_ELSE		4
#define	T_END		5
#define	T_ENDIF		6
#define	T_ENDSW		7
#define	T_EXIT		8
#define	T_FOREACH	9
#define	T_GOTO		10
#define	T_IF		11
#define	T_LABEL		12
#define	T_LET		13
#define	T_SET		14
#define	T_SWITCH	15
#define	T_TEST		16
#define	T_THEN		17
#define	T_WHILE		18

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
 * Aliases and variables are stored in AVL balanced binary trees.
 */
struct	varent {
	char	**vec;		/* Array of words which is the value */
	char	*v_name;	/* Name of variable/alias */
	struct	varent *v_link[3];	/* The links, see below */
	int	v_bal;		/* Balance factor */
} shvhed, aliases;
#define v_left		v_link[0]
#define v_right		v_link[1]
#define v_parent	v_link[2]

struct varent *adrof1();
#define adrof(v)	adrof1(v, &shvhed)
#define value(v)	value1(v, &shvhed)

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

struct	wordent	paraml;			/* Current lexical word list */
int	eventno;			/* Next events number */
int	lastev;				/* Last event reference (default) */

char	HIST;				/* history invocation character */
char	HISTSUB;			/* auto-substitute character */

/*
 * In lines for frequently called functions
 */
#define XFREE(cp) { \
	extern char end[]; \
	char stack; \
	if ((cp) >= end && (cp) < &stack) \
		free(cp); \
}
char	*alloctmp;
#define xalloc(i) \
	((alloctmp = malloc(i)) ? alloctmp : (char *)nomem(i))
#define xrealloc(p, i) \
	((alloctmp = realloc(p, i)) ? alloctmp : (char *)nomem(i))

char	*Dfix1();
char	**blkcat();
char	**blkcpy();
char	**blkend();
char	**blkspl();
char	*calloc();
char	*malloc();
char	*realloc();
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
char	*getwd();
char	**globall();
char	*globone();
char	*index();
struct	biltins *isbfunc();
off_t	lseek();
char	*operate();
int	phup();
void	pintr();
void	pchild();
char	*putn();
char	*rindex();
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
char	*value1();
char	*xhome();
char	*xname();
char	*xset();

#define	NOSTR	((char *) 0)

/*
 * setname is a macro to save space (see sh.err.c)
 */
char	*bname;
#define	setname(a)	(bname = (a))

#ifdef VFORK
char	*Vsav;
char	**Vav;
char	*Vdp;
#endif

char	**evalvec;
char	*evalp;

struct	mesg {
	char	*iname;		/* name from /usr/include */
	char	*pname;		/* print name */
} mesg[];
