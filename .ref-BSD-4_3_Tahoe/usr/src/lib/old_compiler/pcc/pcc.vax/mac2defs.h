/*	mac2defs.h	4.2	85/08/23	*/

/*
 * VAX-11/780 Registers
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

/*
 * Special purpose registers
 */
#define AP	12		/* argument pointer */
#define FP	13		/* frame pointer */
#define SP	14		/* stack pointer */
#define PC	15		/* program counter */

#define REGSZ	16
#define TMPREG	FP

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
#define genfcall(a,b)	gencall(a,b)

/*
 * Some short routines that get called an awful lot are actually macros.
 */
#if defined(FORT) || defined(SPRECC)
#define	szty(t)	((t) == DOUBLE ? 2 : 1)
#else
#define	szty(t)	(((t) == DOUBLE || (t) == FLOAT) ? 2 : 1)
#endif
#define	shltype(o, p) \
	((o) == REG || (o) == NAME || (o) == ICON || \
	 (o) == OREG || ((o) == UNARY MUL && shumul((p)->in.left)))
#define	ncopy(q, p)	((q)->in = (p)->in)

#define MYREADER(p) myreader(p)
int	optim2();

/* This indicates there are no additional special shapes, see match.c */
#define special(a, b)	0
