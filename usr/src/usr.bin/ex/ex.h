/* Copyright (c) 1979 Regents of the University of California */
#ifdef V6
#include <retrofit.h>
#endif

/*
 * Ex version 3 (see exact version in ex_cmds.c, search for /Version/)
 *
 * Mark Horton, UC Berkeley
 * Bill Joy, UC Berkeley
 * November 1979
 *
 * This file contains most of the declarations common to a large number
 * of routines.  The file ex_vis.h contains declarations
 * which are used only inside the screen editor.
 * The file ex_tune.h contains parameters which can be diddled per installation.
 *
 * The declarations relating to the argument list, regular expressions,
 * the temporary file data structure used by the editor
 * and the data describing terminals are each fairly substantial and
 * are kept in the files ex_{argv,re,temp,tty}.h which
 * we #include separately.
 *
 * If you are going to dig into ex, you should look at the outline of the
 * distribution of the code into files at the beginning of ex.c and ex_v.c.
 * Code which is similar to that of ed is lightly or undocumented in spots
 * (e.g. the regular expression code).  Newer code (e.g. open and visual)
 * is much more carefully documented, and still rough in spots.
 *
 * Please forward bug reports to
 *
 *	Mark Horton
 *	Computer Science Division, EECS
 *	EVANS HALL
 *	U.C. Berkeley 94704
 *	(415) 642-4948
 *	(415) 642-1024 (dept. office)
 *
 * or to csvax.mark@berkeley on the ARPA-net.  I would particularly like to hear
 * of additional terminal descriptions you add to the termcap data base.
 */

#include <sys/types.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/stat.h>

/*
 *	The following little dance copes with the new USG tty handling.
 *	This stuff has the advantage of considerable flexibility, and
 *	the disadvantage of being incompatible with anything else.
 *	The presence of the symbol USG3TTY will indicate the new code:
 *	in this case, we define CBREAK (because we can simulate it exactly),
 *	but we won't actually use it, so we set it to a value that will
 *	probably blow the compilation if we goof up.
 */
#ifdef USG3TTY
#include <termio.h>
#define CBREAK xxxxx
#else
#include <sgtty.h>
#endif

extern	int errno;

#ifndef VMUNIX
typedef	short	line;
#else
typedef	int	line;
#endif
typedef	short	bool;

#include "ex_tune.h"
#include "ex_vars.h"
/*
 * Options in the editor are referred to usually by "value(name)" where
 * name is all uppercase, i.e. "value(PROMPT)".  This is actually a macro
 * which expands to a fixed field in a static structure and so generates
 * very little code.  The offsets for the option names in the structure
 * are generated automagically from the structure initializing them in
 * ex_data.c... see the shell script "makeoptions".
 */
struct	option {
	char	*oname;
	char	*oabbrev;
	short	otype;		/* Types -- see below */
	short	odefault;	/* Default value */
	short	ovalue;		/* Current value */
	char	*osvalue;
};

#define	ONOFF	0
#define	NUMERIC	1
#define	STRING	2		/* SHELL or DIRECTORY */
#define	OTERM	3

#define	value(a)	options[a].ovalue
#define	svalue(a)	options[a].osvalue

struct	option options[NOPTS + 1];


/*
 * The editor does not normally use the standard i/o library.  Because
 * we expect the editor to be a heavily used program and because it
 * does a substantial amount of input/output processing it is appropriate
 * for it to call low level read/write primitives directly.  In fact,
 * when debugging the editor we use the standard i/o library.  In any
 * case the editor needs a printf which prints through "putchar" ala the
 * old version 6 printf.  Thus we normally steal a copy of the "printf.c"
 * and "strout" code from the standard i/o library and mung it for our
 * purposes to avoid dragging in the stdio library headers, etc if we
 * are not debugging.  Such a modified printf exists in "printf.c" here.
 */
#ifdef TRACE
#	include <stdio.h>
	FILE	*trace;
	bool	trubble;
	bool	techoin;
	char	tracbuf[BUFSIZ];
#	undef	putchar
#	undef	getchar
#else
# ifdef	VMUNIX
#	define	BUFSIZ	1024
# else
#	define	BUFSIZ	512
# endif
#	define	NULL	0
#	define	EOF	-1
#endif

/*
 * Character constants and bits
 *
 * The editor uses the QUOTE bit as a flag to pass on with characters
 * e.g. to the putchar routine.  The editor never uses a simple char variable.
 * Only arrays of and pointers to characters are used and parameters and
 * registers are never declared character.
 */
#define	QUOTE	0200
#define	TRIM	0177
#define	CTRL(c)	('c' & 037)
#define	NL	CTRL(j)
#define	CR	CTRL(m)
#define	DELETE	0177		/* See also ATTN, QUIT in ex_tune.h */
#define	ESCAPE	033

/*
 * Miscellaneous random variables used in more than one place
 */
bool	aiflag;			/* Append/change/insert with autoindent */
bool	anymarks;		/* We have used '[a-z] */
int	chng;			/* Warn "No write" */
char	*Command;
short	defwind;		/* -w# change default window size */
int	dirtcnt;		/* When >= MAXDIRT, should sync temporary */
#ifdef TIOCLGET
bool	dosusp;			/* Do SIGTSTP in visual when ^Z typed */
#endif
bool	edited;			/* Current file is [Edited] */
line	*endcore;		/* Last available core location */
bool	endline;		/* Last cmd mode command ended with \n */
#ifndef VMUNIX
short	erfile;			/* Error message file unit */
#endif
line	*fendcore;		/* First address in line pointer space */
char	file[FNSIZE];		/* Working file name */
char	genbuf[LBSIZE];		/* Working buffer when manipulating linebuf */
bool	hush;			/* Command line option - was given, hush up! */
char	*globp;			/* (Untyped) input string to command mode */
bool	holdcm;			/* Don't cursor address */
bool	inappend;		/* in ex command append mode */
bool	inglobal;		/* Inside g//... or v//... */
char	*initev;		/* Initial : escape for visual */
bool	inopen;			/* Inside open or visual */
char	*input;			/* Current position in cmd line input buffer */
bool	intty;			/* Input is a tty */
short	io;			/* General i/o unit (auto-closed on error!) */
short	lastc;			/* Last character ret'd from cmd input */
bool	laste;			/* Last command was an "e" (or "rec") */
char	lastmac;		/* Last macro called for ** */
char	lasttag[TAGSIZE];	/* Last argument to a tag command */
char	*linebp;		/* Used in substituting in \n */
char	linebuf[LBSIZE];	/* The primary line buffer */
bool	listf;			/* Command should run in list mode */
char	*loc1;			/* Where re began to match (in linebuf) */
char	*loc2;			/* First char after re match (") */
line	names['z'-'a'+2];	/* Mark registers a-z,' */
int	notecnt;		/* Count for notify (to visual from cmd) */
bool	numberf;		/* Command should run in number mode */
char	obuf[BUFSIZ];		/* Buffer for tty output */
short	oprompt;		/* Saved during source */
short	ospeed;			/* Output speed (from gtty) */
int	otchng;			/* Backup tchng to find changes in macros */
short	peekc;			/* Peek ahead character (cmd mode input) */
char	*pkill[2];		/* Trim for put with ragged (LISP) delete */
bool	pfast;			/* Have stty -nl'ed to go faster */
int	pid;			/* Process id of child */
int	ppid;			/* Process id of parent (e.g. main ex proc) */
jmp_buf	resetlab;		/* For error throws to top level (cmd mode) */
int	rpid;			/* Pid returned from wait() */
bool	ruptible;		/* Interruptible is normal state */
bool	shudclob;		/* Have a prompt to clobber (e.g. on ^D) */
int	status;			/* Status returned from wait() */
int	tchng;			/* If nonzero, then [Modified] */
short	tfile;			/* Temporary file unit */
bool	vcatch;			/* Want to catch an error (open/visual) */
jmp_buf	vreslab;		/* For error throws to a visual catch */
int	xchng;			/* Suppresses multiple "No writes" in !cmd */

/*
 * Macros
 */
#define	CP(a, b)	(ignore(strcpy(a, b)))
			/*
			 * FIXUNDO: do we want to mung undo vars?
			 * Usually yes unless in a macro or global.
			 */
#define FIXUNDO		(inopen >= 0 && (inopen || !inglobal))
#define ckaw()		{if (chng && value(AUTOWRITE)) wop(0);}
#define	copy(a,b,c)	Copy((char *) a, (char *) b, c)
#define	eq(a, b)	((a) && (b) && strcmp(a, b) == 0)
#define	getexit(a)	copy(a, resetlab, sizeof (jmp_buf))
#define	lastchar()	lastc
#define	outchar(c)	(*Outchar)(c)
#define	pastwh()	(ignore(skipwh()))
#define	pline(no)	(*Pline)(no)
#define	reset()		longjmp(resetlab,1)
#define	resexit(a)	copy(resetlab, a, sizeof (jmp_buf))
#define	setexit()	setjmp(resetlab)
#define	setlastchar(c)	lastc = c
#define	ungetchar(c)	peekc = c

#define	CATCH		vcatch = 1; if (setjmp(vreslab) == 0) {
#define	ONERR		} else { vcatch = 0;
#define	ENDCATCH	} vcatch = 0;

/*
 * Environment like memory
 */
char	altfile[FNSIZE];	/* Alternate file name */
char	direct[ONMSZ];		/* Temp file goes here */
char	shell[ONMSZ];		/* Copied to be settable */
char	ttytype[ONMSZ];		/* A long and pretty name */
char	uxb[UXBSIZE + 2];	/* Last !command for !! */

/*
 * The editor data structure for accessing the current file consists
 * of an incore array of pointers into the temporary file tfile.
 * Each pointer is 15 bits (the low bit is used by global) and is
 * padded with zeroes to make an index into the temp file where the
 * actual text of the line is stored.
 *
 * To effect undo, copies of affected lines are saved after the last
 * line considered to be in the buffer, between dol and unddol.
 * During an open or visual, which uses the command mode undo between
 * dol and unddol, a copy of the entire, pre-command buffer state
 * is saved between unddol and truedol.
 */
line	*addr1;			/* First addressed line in a command */
line	*addr2;			/* Second addressed line */
line	*dol;			/* Last line in buffer */
line	*dot;			/* Current line */
line	*one;			/* First line */
line	*truedol;		/* End of all lines, including saves */
line	*unddol;		/* End of undo saved lines */
line	*zero;			/* Points to empty slot before one */

/*
 * Undo information
 *
 * For most commands we save lines changed by salting them away between
 * dol and unddol before they are changed (i.e. we save the descriptors
 * into the temp file tfile which is never garbage collected).  The
 * lines put here go back after unddel, and to complete the undo
 * we delete the lines [undap1,undap2).
 *
 * Undoing a move is much easier and we treat this as a special case.
 * Similarly undoing a "put" is a special case for although there
 * are lines saved between dol and unddol we don't stick these back
 * into the buffer.
 */
short	undkind;

line	*unddel;		/* Saved deleted lines go after here */
line	*undap1;		/* Beginning of new lines */
line	*undap2;		/* New lines end before undap2 */
line	*undadot;		/* If we saved all lines, dot reverts here */

#define	UNDCHANGE	0
#define	UNDMOVE		1
#define	UNDALL		2
#define	UNDNONE		3
#define	UNDPUT		4

/*
 * Various miscellaneous flags and buffers needed by the encryption routines.
 */
#define	KSIZE   9       /* key size for encryption */
#define	KEYPROMPT       "Key: "
int	xflag;		/* True if we are in encryption mode */
int	xtflag;		/* True if the temp file is being encrypted */
int	kflag;		/* True if the key has been accepted */
char	perm[768];
char	tperm[768];
char	*key;
char	crbuf[CRSIZE];
char	*getpass();

/*
 * Function type definitions
 */
#define	NOSTR	(char *) 0
#define	NOLINE	(line *) 0

int	(*Outchar)();
int	(*Pline)();
int	(*Putchar)();
int	(*oldhup)();
int	(*setlist())();
int	(*setnorm())();
int	(*setnorm())();
int	(*setnumb())();
line	*address();
char	*cgoto();
char	*genindent();
char	*getblock();
char	*getenv();
line	*getmark();
char	*longname();
char	*mesg();
char	*place();
char	*plural();
line	*scanfor();
line	*setin();
char	*strcat();
char	*strcpy();
char	*strend();
char	*tailpath();
char	*tgetstr();
char	*tgoto();
char	*ttyname();
line	*vback();
char	*vfindcol();
char	*vgetline();
char	*vinit();
char	*vpastwh();
char	*vskipwh();
int	put();
int	putreg();
int	YANKreg();
int	delete();
int	execl();
int	filter();
int	getfile();
int	getsub();
int	gettty();
int	join();
int	listchar();
off_t	lseek();
int	normchar();
int	normline();
int	numbline();
int	(*oldquit)();
int	onhup();
int	onintr();
int	onsusp();
int	putch();
int	shift();
int	termchar();
int	vfilter();
#ifdef CBREAK
int	vintr();
#endif
int	vputch();
int	vshftop();
int	yank();

/*
 * C doesn't have a (void) cast, so we have to fake it for lint's sake.
 */
#ifdef lint
#	define	ignore(a)	Ignore((char *) (a))
#	define	ignorf(a)	Ignorf((int (*) ()) (a))
#else
#	define	ignore(a)	a
#	define	ignorf(a)	a
#endif
