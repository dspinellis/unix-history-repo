/*	@(#)macdefs.h	1.5	(Berkeley)	1/8/86	*/

#if defined(pdp11) || defined(vax)
#define makecc(val,i)  lastcon |= val<<(8*i);  /* pdp-11 womp next char  */
#else
#define makecc(val,i)	lastcon = i ? (val<<8)|lastcon : val
#endif

# define  ARGINIT 288 /* initial offset for arguments */
# define  AUTOINIT 0   /* initial automatic offset */
extern int  SZCHAR;
extern int  SZINT;
extern int  SZFLOAT;
extern int  SZDOUBLE;
extern int  SZLONG;
extern int  SZSHORT;
extern int SZPOINT;
extern int ALCHAR;
extern int ALINT;
extern int ALFLOAT;
extern int ALDOUBLE;
extern int ALLONG;
extern int ALSHORT;
extern int ALPOINT;
extern int ALSTRUCT;
# define SAVEADJUST 0 /* bits of adjustment required for stackframe */


/* type (INT OR LONG) big enough to hold pointers */


/*	size in which constants are converted */
/*	should be long if feasable */

# define CONSZ long
# define CONFMT "%Ld"
# define CONOFMT "%Lo"
# define LABFMT	"L%d"

/*	size in which offsets are kept
/*	should be large enough to cover address space in bits
*/

# define OFFSZ long

/* 	character set macro */

# define  CCTRANS(x) x

/*	register cookie for stack pointer */

# define STKREG 9

/*	maximum and minimum register variable values */

# define MAXRVAR 1000
# define MINRVAR 1

/* macros carried over from the PCC */
# define ncopy(q, p)	((q)->in = (p)->in)

/* many macro definitions for functions irrelevant to lint */

# define locctr(n) 0
# define getlab() 10
# define genswitch( x,y)
# define bccode()
# define cendarg()
# define incode(a,s) (inoff += (s))
# define fincode(a,s) (inoff += (s) )
# define vfdzero(n) (inoff += (n))
# define aobeg()
# define aoend()
# define econvert(p)

# ifndef unix
# define NOFORTRAN  {extern int pflag; if(pflag) werror( "fortran keyword nonportable" );}
# else
# define NOFORTRAN { werror( "fortran keyword nonportable" ); }
# endif

# define LINT
