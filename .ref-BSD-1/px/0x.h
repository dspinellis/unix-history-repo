/*
 * px - UNIX Pascal interpreter
 *
 * Version 1.0, July 1977
 *
 * Written by Bill Joy and Chuck Haley
 * November-December 1976
 *
 * Based on an earlier version by Ken Thompson
 *
 * Px is described in detail in the "PX 1.0 Implementation Notes"
 * The source code for px is in several major pieces:
 *
 *	int.c		C main program which reads in interpreter code
 *	00int.s		Driver including main interpreter loop
 *	dd*.s		Where dd are digits, interpreter instructions
 *			grouped by their positions in the interpreter table.
 *	p*.c		Various C language routines supporting the system.
 *
 * In addition there are several headers defining mappings for error
 * messages names into codes, and a definition of the interpreter transfer
 * table. These are made by the script Emake in this directory and the scripts
 * in the directory '../opcodes'.
 */

int	argc;
char	**argv;

/*
 * Pascal runtime errors cause emulator traps
 * to the routine onemt which transfers to the
 * routine 'error' in the file perror.c to decode them.
 * This method saves at least one word per static error.
 */
int	onemt();

/*
 * Definitions for memory allocation
 * Memory allocation routines are in 'palloc.c'
 */
char	*bottmem, *memptr, *high, *maxstk;

/*
 * The file i/o routines maintain a notion of a "current file".
 * The printing name of this file is kept in the variable
 * "file" for use in error messages.
 */
char	*file;

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
int	display[20], *dp;

int	lino;
int	nodump;

/*
 * Random number generator constants
 */
long	seed;
double	randa;
double	randc;
double	randm;
double	randim;

/*
 * Structures to access things on the stack
 */
struct {
	char	pchar;
};
struct {
	int	pint;
	int	p2int;
};
struct {
	long	plong;
};
struct {
	double	pdouble;
};

char	discard;
