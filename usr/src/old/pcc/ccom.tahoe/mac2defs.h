/*	mac2defs.h	1.1	86/01/11	*/

/*	Tahoe Registers */

	/* scratch registers */
# define R0 0
# define R1 1
# define R2 2
# define R3 3
# define R4 4
# define R5 5

	/* register variables */
# define R6 6
# define R7 7
# define R8 8
# define R9 9
# define R10 10
# define R11 11
# define R12 12

	/* special purpose */
# define FP 13	/* frame pointer */
# define SP 14	/* stack pointer */
# define PC 15	/* program counter */

	/* floating registers */
# define ACC 16	/* accumulator */

extern int fregs;
extern int maxargs;

# define BYTEOFF(x) ((x)&03)
# define wdal(k) (BYTEOFF(k)==0)
# define BITOOR(x) ((x)>>3)  /* bit offset to oreg offset */

# define REGSZ 16

# define TMPREG FP

# define R2REGS   /* permit double indexing */

# define STOARG(p)     /* just evaluate the arguments, and be done with it... */
# define STOFARG(p)
# define STOSTARG(p)

# define NESTCALLS

# define MYREADER(p) myreader(p)
int optim2();
# define SIREG (SPECIAL|6)	/* indexed OREG */
int special();
#define callchk(p) if(p->in.op!=FORTCALL)allchk()
