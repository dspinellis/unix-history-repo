/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* jove.h header file to be included by EVERYONE */

#include <setjmp.h>
#ifndef	TUNED
# include "tune.h"
#endif

#ifndef	MAC
# include <sys/types.h>
# include <string.h>
#else
# include <types.h>
#endif

/* proto: macro to allow us to prototype any function declaration
 * without upsetting old compilers.
 */

#ifdef	REALSTDC
# define    USE_PROTOTYPES  1
#endif

#ifdef	USE_PROTOTYPES
# define proto(x)        x
# ifdef	NO_PTRPROTO
   /* on these systems, a prototype cannot be used for a pointer to function */
#  define ptrproto(x)		()
# else
#  define ptrproto(x)		x
# endif
#else
# define proto(x)		()
# define ptrproto(x)		()
#endif

/* There are two ways to handle functions with a variable number of args.
 * The old portable way uses varargs.h.  The way sanctioned by ANSI X3J11
 * uses stdarg.h.
 */
#ifdef	REALSTDC
#define	STDARGS	1
# define	va_init(ap, parmN)	{ va_start((ap), (parmN)); }
#else
# define	va_init(ap, parmN)	{ va_start((ap)); }
#endif

/* ANSI Goodies and their substitutes
 *
 * const: readonly type qualifier
 *
 * volatile: type qualifier indicating one of two kinds of magic.
 * 1. This object may be modified by an event unknown to the implementation
 *    (eg. asynchronous signal or memory-mapped I/O device).
 * 2. This automatic variable might be modified between a setjmp()
 *    and a longjmp(), and we wish it to have the correct value after
 *    the longjmp().  This second meaning is an X3J11 abomination.
 * So far, only the second meaning is used.
 *
 * UnivPtr: universal pointer type
 *
 * UnivConstPtr: universal pointer to const
 */

#ifdef	REALSTDC

  typedef void	*UnivPtr;
  typedef const void	*UnivConstPtr;

#else	/* !REALSTDC */

# ifndef const
#  define	const	/* Only in ANSI C.  Pity */
# endif
# ifndef volatile
#  define	volatile
# endif
  typedef char	*UnivPtr;
  typedef const char	*UnivConstPtr;

#endif	/* !REALSTDC */

/* According to the ANSI standard for C, any library routine may
 * be defined as a macro with parameters.  In order to prevent
 * the expansion of this macro in a declaration of the routine,
 * ANSI suggests parenthesizing the identifier.  This is a reasonable
 * and legal approach, even for K&R C.
 *
 * A bug in the MIPS compiler used on MIPS, IRIS, and probably other
 * MIPS R[23]000 based systems, causes the compiler to reject
 * these declarations (at least at the current time, 1989 August).
 * To avoid this bug, we conditionally define and use UNMACRO.
 */
#ifdef	mips
# define UNMACRO(proc)	proc
#else
# define UNMACRO(proc)	(proc)
#endif

/* Since we don't use stdio.h, we may have to define NULL and EOF */

#ifndef	NULL
# define NULL	0
#endif

#ifndef	EOF
#define EOF	(-1)
#endif

#define private		static

typedef int	bool;
#define NO		0
#define YES		1
#define FALSE		0
#define TRUE		1
#define OFF		0
#define ON		1

/* typedef structure definitions */
#ifdef	IPROCS
typedef struct process	Process;
#endif
typedef struct window	Window;
typedef struct position	Bufpos;
typedef struct mark	Mark;
typedef struct buffer	Buffer;
typedef struct line	Line;
typedef struct iobuf	IOBUF;

#include "buf.h"
#include "wind.h"
#include "io.h"
#include "dataobj.h"
#include "keymaps.h"
#include "argcount.h"
#include "util.h"
#include "vars.h"
#include "screen.h"

/* return codes for command completion (all < 0 because >= 0 are
   legitimate offsets into array of strings */

#define AMBIGUOUS	(-2)	/* matches more than one at this point */
#define UNIQUE		(-3)	/* matches only one string */
#define ORIGINAL	(-4)	/* matches no strings at all! */
#define NULLSTRING	(-5)	/* just hit return without typing anything */

/* values for the `flags' argument to complete */
#define NOTHING		0	/* opposite of RET_STATE */
#define RET_STATE	1	/* return state when we hit return */
#define RCOMMAND	2	/* we are reading a joverc file */
#define CASEIND		4	/* map all to lower case */

#define FORWARD		1
#define BACKWARD	(-1)

#define ARG_CMD		1
#define LINECMD		2
#define KILLCMD		3	/* so we can merge kills */
#define YANKCMD		4	/* so we can do ESC Y (yank-pop) */

extern jmp_buf	mainjmp;

/* setjmp/longjmp args for DoKeys() mainjmp */
#define FIRSTCALL	0
#define ERROR		1
#define COMPLAIN	2	/* do the error without a getDOT */
#define QUIT		3	/* leave this level of recursion */

#define INT_OKAY	0
#define INT_BAD		(-1)

extern char	NullStr[];
extern char	*ProcFmt;

extern int
	LastKeyStruck,

	RecDepth,	/* recursion depth */
	InJoverc;	/* depth in sourcing */

extern bool
	InMacDefine,	/* are we defining a macro right now? */

	TOabort,	/* flag set by Typeout() */

	errormsg,	/* last message was an error message
			   so don't erase the error before it
			   has been read */
	InputPending,	/* nonzero if there is input waiting to
			   be processed */
	Interactive,
	inIOread,	/* so we know whether we can do a redisplay. */

	Asking,		/* are we on read a string from the terminal? */
	InRealAsk;	/* are we currently executing real_ask()? */

extern int
	AskingWidth;	/* width of question being asked */

extern char
	*Inputp,
	Minibuf[LBSIZE],
	ShcomBuf[LBSIZE],
	*version;

#define MESG_SIZE 128
extern char	mesgbuf[MESG_SIZE];

#include "externs.h"

#ifndef	W_OK
# define W_OK	2
# define X_OK	1
# define F_OK	0
#endif
