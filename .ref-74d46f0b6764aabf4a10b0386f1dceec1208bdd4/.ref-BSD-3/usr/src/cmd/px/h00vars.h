#
/*
 * px - Berkeley Pascal interpreter
 *
 * Version 2.0, January 1979
 *
 * Original version by Ken Thompson
 *
 * Substantial revisions by Bill Joy and Chuck Haley
 * November-December 1976
 *
 * Rewritten for VAX 11/780 by Kirk McKusick
 * Fall 1978
 *
 * Px is described in detail in the "PX 1.0 Implementation Notes"
 * The source code for px is in several major pieces:
 *
 *	int.c		C main program which reads in interpreter code
 *	00case.s	Driver including main interpreter loop
 *	dd*.s		Where dd are digits, interpreter instructions
 *			grouped by their positions in the interpreter table.
 *	p*.c		Various C language routines supporting the system.
 *
 * In addition there are several headers defining mappings for error
 * messages names into codes, and a definition of the interpreter transfer
 * table. These are made by the script Emake in this directory and the scripts
 * in the directory '../opcodes'.
 */

long	argc;
char	**argv;

/*
 * Pascal runtime errors transfer to the routine
 * 'error' in the file perror.c to decode them.
 */
int	perrno;		/* number of error which occurred */

/*
 * Definitions for memory allocation
 * Memory allocation is done by palloc in utilities.c
 */

/*
 * The file i/o routines maintain a notion of a "current file".
 * The printing name of this file is kept in the variable
 * "file" for use in error messages.
 */
char	*file;		/* ptr to active file name */
long	fchain;		/* head of active file chain */
int	bufopt;		/* controls flushing of std output as follows:
			 * 0 => flush on every write
			 * 1 => flush before std read or at end of line
			 * 2 => normal buffering
			 */
/*
 * THE RUNTIME DISPLAY
 *
 * The entries in the display point to the active static block marks.
 * The first entry in the display is for the global variables,
 * then the procedure or function at level one, etc.
 * Each display entry points to a stack frame as shown:
 *
 *		base of stack frame
 *		  ---------------
 *		  |		|
 *		  | block mark  |
 *		  |		|
 *		  ---------------  <-- display entry points here
 *		  |             |
 *		  |   local	|
 *		  |  variables  |
 *		  |		|
 *		  ---------------
 *		  |		|
 *		  |  expression |
 *		  |  temporary  |
 *		  |  storage	|
 *		  |		|
 *		  - - - - - - - -
 *
 * The information in the block mark is thus at positive offsets from
 * the display pointer entries while the local variables are at negative
 * offsets. The block mark actually consists of two parts. The first
 * part is created at CALL and the second at entry, i.e. BEGIN. Thus:
 *
 *		-------------------------
 *		|			|
 *		|  Saved lino		|
 *		|  Saved lc		|
 *		|  Saved dp		|
 *		|			|
 *		-------------------------
 *		|			|
 *		|  Saved (dp)		|
 *		|			|
 *		|  Current section name	|
 *		|   and entry line ptr	|
 *		|			|
 *		|  Saved file name and	|
 *		|   file buffer ptr	|
 *		|			|
 *		|  Empty tos value	|
 *		|			|
 *		-------------------------
 */

/*
 * Structure for accessing things in the block mark
 */
struct stack {
	long	*tos;		/* pointer to top of stack frame */
	char	*file;		/* pointer to active file name */
	long	buf;		/* pointer to active file record */
	struct	{
		char	name[8];/* name of active procedure */
		short	offset;	/* offset of procedure in source file */
		} *entry;
	struct  stack *disp;	/* previous display value for this level */
	struct	stack **dp;	/* pointer to active display entry */
	long	lc;		/* previous location counter */
	long	lino;		/* previous line number */
	} *display[40];

long	addrsze;		/* size of display addresses */


/*
 * Program option variables
 */
long	stcnt;		/* number of statements executed */
long	stlim;		/* max number of statements to execute */
long	llimit;		/* max number of lines per text file */
short	nodump;		/* 1 => no post mortum dump */
short	mode;		/* mode of input to interpreter */
#define	PX	0	/* normal run of px */
#define	PIX	1	/* load and go */
#define	PIPE	2	/* bootstrap via a pipe */

/*
 * Pxp variables
 */
char	*pxpbuf;	/* pointer to pxp buffer */
long	pxpsize;	/* size of pxp buffer */

#ifdef profile
/*
 * Px execution profile data
 */
#define	numops 256
struct cntrec {
	double	counts[numops];	/* instruction counts */
	long	runs;		/* number of interpreter runs */
	long	startdate;	/* date profile started */
	long	usrtime;	/* total user time consumed */
	long	systime;	/* total system time consumed */
	double	stmts;		/* number of pascal statements executed */
	} profdata;
long	profcnts[numops];
#define	proffile	"/usr/ucb/pascal/px/pcnt.out"
FILE	*datafile;		/* input datafiles */
#else
int	profcnts;	/* dummy just to keep the linker happy */
#endif
