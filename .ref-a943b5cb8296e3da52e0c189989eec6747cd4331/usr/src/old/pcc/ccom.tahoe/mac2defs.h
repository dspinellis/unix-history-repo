/*	mac2defs.h	1.3	86/07/27	*/

/*
 * Tahoe Registers
 */

/*
 * Scratch registers
 */
#define R0	0
#define R1	1
#define R2	2
#define R3	3
#define R4	4
#define R5	5

/*
 * Register variables
 */
#define R6	6
#define R7	7
#define R8	8
#define R9	9
#define R10	10
#define R11	11
#define R12	12

/*
 * Special purpose registers
 */
#define FP	13		/* frame pointer */
#define SP	14		/* stack pointer */
#define PC	15		/* program counter */

/*
 * Floating registers
 */
#define	ACC	16		/* accumulator */

#define	TMPREG	FP		/* reg off which temporaries are referenced */
#define	REGSZ	16		/* size of register set */

#define R2REGS	1		/* permit double indexing */

extern	int fregs;
extern	int maxargs;

#define BYTEOFF(x)	((x)&03)
#define wdal(k)		(BYTEOFF(k)==0)		/* word align */
#define BITOOR(x)	((x)>>3)		/* bit offset to oreg offset */

/*
 * Some macros used in store():
 *	just evaluate the arguments, and be done with it...
 */
#define STOARG(p)
#define STOFARG(p)
#define STOSTARG(p)

/*
 * Some short routines that get called an awful lot are actually macros.
 */
#define	szty(t)	((t) == DOUBLE ? 2 : 1)
#define	shltype(o, p) \
	((o) == REG || (o) == NAME || (o) == ICON || \
	 (o) == OREG || ((o) == UNARY MUL && shumul((p)->in.left)))
#define	ncopy(q, p)	((q)->in = (p)->in)

#define MYREADER(p)	myreader(p)
int	optim2();
#define	SIREG		(SPECIAL|6)	/* indexed OREG */
int	special();			/* additional special shapes */
#define callchk(p)	if ((p)->in.op != FORTCALL) allchk()
