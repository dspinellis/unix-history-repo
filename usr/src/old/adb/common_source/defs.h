/*	@(#)defs.h	5.1 (Berkeley) %G%	*/

/*
 * adb: common definitions
 */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>

#include <a.out.h>

/* machine dependent types and definitions */
#include "machdep.h"

/*
 * Signals.  Adb catches SIGINT and SIGQUIT; the variables sigint and
 * sigquit hold the original state for adb's children.
 */
typedef	int (*sig_t)();		/* for signal syscall */
sig_t	sigint;			/* original SIGINT state */
sig_t	sigquit;		/* original SIGQUIT state */
sig_t	intcatch;		/* interrupt catch routine, or SIG_IGN */

/*
 * Address spaces.  We distinguish only between instruction & data.
 * The spaces NONE, INSTR, and DATA (without the STAR flag) are also used
 * to tag symbols.
 */
#define	SP_NONE		0	/* not in any space, just a number */
#define	SP_INSTR	1	/* instruction space */
#define	SP_DATA		2	/* data space */
#define	SP_STAR		4	/* or'ed in; see below */

/*
 * The symbol and core files.
 */
struct {
	char	*name;		/* file name */
	int	fd;		/* and descriptor */
} symfile, corefile;

/*
 * File address maps.
 */
struct m1 {
	addr_t	b;		/* begins at b */
	addr_t	e;		/* ends at e */
	addr_t	f;		/* with offset f */
};
struct map {
	struct	m1 m1;		/* regular map */
	struct	m1 m2;		/* `star' (alternate) map */
	int	ufd;		/* unix file descriptor */
};

struct	map txtmap;		/* the `?' or text file */
struct	map datmap;		/* the `/' or data/core file */

/*
 * Program and file I/O.
 */
enum rwmode { RWMODE_READ, RWMODE_WRITE };
#define	adbread(space, rmtaddr, localaddr, nbytes) \
	adbio(RWMODE_READ, space, rmtaddr, (caddr_t)(localaddr), nbytes)
#define	adbwrite(space, rmtaddr, localaddr, nbytes) \
	adbio(RWMODE_WRITE, space, rmtaddr, (caddr_t)(localaddr), nbytes)

addr_t	vtophys();		/* -k: kernel virtual addr to physical */

/*
 * Errors.  errflag should be set to point to the error message when
 * an error occurs.  error(s) sets errflag to s and jumps back
 * to the main loop via longjmp().  In some places this is unsafe
 * or undesirable, and instead errflag is set directly; checkerr()
 * can be used to test (and jump on) for such errors later.
 *
 * mkfault creates a `generic' error after a keyboard interrupt.
 *
 * Various error strings are defined in message.c and referenced
 * through names below.
 */
char	*errflag;		/* the error, or NULL */
int	mkfault;		/* interrupted; pretend an error */

#define	iserr()	(errflag || mkfault)
#define	checkerr() \
	if (!iserr()) \
		/* void */; \
	else \
		error(errflag)
/* if checkerr() above is undefined, a function version is used instead */

/*
 * Locations.
 *
 * HSZ and FSZ are defined here as the sizes of `half' and `full' words
 * respectively.  While these are not `locations', they are commonly used
 * to set the value of dotinc.
 */
int	gavedot;		/* true iff this command set dot */
addr_t	dot;			/* current location; but see also edot */
addr_t	ditto;			/* previous dot */
int	dotinc;			/* size of last object examined */

extern	char ADDRWRAP[];	/* "address wrap around" */

/* compute dot+offset, checking for overflow */
#define	inkdot(o) (ADDRESS_WRAP(dot, dot+(o)) ? error(ADDRWRAP), 0 : dot+(o))
/* if inkdot() above is undefined, a function version is used instead */

/*
 * Expressions.
 *
 * oexpr() evaluates an optional expression; rexpr() does a required
 * expression, and returns expv as a convenience.
 *
 * N.B.: edot is valid only if gavedot.
 */
int	oexpr();		/* returns 1 if found expr, else 0 */
expr_t	rexpr();		/* aborts if no expression found */

expr_t	edot;			/* dot as an expression (possibly more bits) */
int	gavecount;		/* true iff this command gave a count */
expr_t	ecount;			/* repeat count from addr,count format */
expr_t	expv;			/* value from last expression */
expr_t	var[36];		/* adb's 36 variables (0..9 then a..z) */

/*
 * Input.
 *
 * The global lp points to the current input line.  The routine
 * readchar() picks up the next character, incrementing lp, but
 * can read more if appropriate.  lastc retains the most recently
 * read character.  unreadc() in effect `puts back' lastc.  rdc()
 * is like readchar() but skips white space.
 */
char	*lp;			/* pointer into current line */
int	lastc;			/* character most recently read */
int	readchar();		/* get the next char */
int	rdc();			/* get the next nonblank char */
#ifndef lint
#define	unreadc()	(lastc ? lp-- : 0)
#else
#define	unreadc()	(lp--)
#endif
#ifndef lint
#define	readchar()	(((lastc = *lp) == 0 ? 0 : lp++), lastc)
#endif
/* if readchar() above is undefined, a function version is used instead */
#define	eol(c)		((c) == '\n' || (c) == ';')

/*
 * Miscellaneous globals, functions, and macros.
 */
int	kernel;			/* debugging kernel (-k flag) */
int	kcore;			/* have a post-mortem dump (-k + core) */
int	wtflag;			/* adb => 0, adb -w => 2 */
int	radix;			/* current radix (input and %r/%R formats) */
int	pid;			/* process id being debugged, or 0 */
int	signo;			/* signal that stopped process pid */
int	sigcode;		/* extension info (machine dependent) */

addr_t	maxoff;			/* max offset for symbol match ($s) */
#define	MAXOFF	255		/* default value */

int	maxcol;			/* max output column ($w) */
#define	MAXCOL	80		/* default value */

#define	LINELEN	1024		/* max input line length */
#define	SYMLEN	1024		/* max symbol length */

int	errno;			/* our old friend */

/*
 * checkfloat() returns an error string if a float or double is
 * some sort of reserved bit pattern, such that trying to print it
 * would cause a fault.  It is called with the address of the
 * float or double, and a 0 or 1 to indicate float and double
 * respectively.  checkfloat() returns NULL if the number is printable.
 */
char	*checkfloat();		/* check a float or double for correctness */

struct reglist *reglookup();	/* find a register by name */

struct nlist *lookup();		/* look up a symbol */
struct nlist *findsym();	/* like lookup, but allows an offset */
struct nlist *nextlocal();	/* given a sym, return the next local sym */

struct nlist *symtab;		/* symbol table */
struct nlist *esymtab;		/* end of symtab */

expr_t	getreg();		/* returns the value in a register */

addr_t	eval_localsym();	/* compute the address of a local symbol */

/*
 * eqstr(a, b) is true iff the given strings compare equal.
 * eqsym(a, b, c) is true if symbols a and b match, but allowing
 * the `a' symbol to begin with the character `c'.
 */
#define	eqstr(a, b)	(*(a) == *(b) && strcmp(a, b) == 0)
#define	eqsym(a, b, c)	(eqstr(a, b) || *(a) == (c) && eqstr((a) + 1, b))

/*
 * The user structure.
 */
union {
	struct	user user;		/* the actual user struct */
	char	upages[ctob(UPAGES)];	/* u. + kernel stack */
} uu;
#define	u uu.user
