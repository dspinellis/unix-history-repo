/*	macdefs.h	1.1	86/01/11	*/

# define makecc(val,i)  lastcon = (lastcon<<8)|((val<<24)>>24);  

# define  ARGINIT SZINT
# define  AUTOINIT (13*SZINT)
# define  SZCHAR 8
# define  SZINT 32
# define  SZFLOAT 32
# define  SZDOUBLE 64
# define  SZLONG 32
# define  SZSHORT 16
# define SZPOINT 32
# define ALCHAR 8
# define ALINT 32
# define ALFLOAT 32
# define ALDOUBLE 32
# define ALLONG 32
# define ALSHORT 16
# define ALPOINT 32
# define ALSTRUCT 8
# define  ALSTACK 32 

/*	size in which constants are converted */
/*	should be long if feasable */

# define CONSZ long
# define CONFMT "%ld"

/*	size in which offsets are kept
/*	should be large enough to cover address space in bits
*/

# define OFFSZ long

/* 	character set macro */

# define  CCTRANS(x) x

/* register cookie for stack pointer */

# define  STKREG 13

/*	maximum and minimum register variables */

# define MAXRVAR 12
# define MINRVAR 6

	/* various standard pieces of code are used */
# define STDPRTREE
# define LABFMT "L%d"

/* show stack grows negatively */
#define BACKAUTO
#define BACKTEMP

/* no field hardware support on Tahoe
#define FIELDOPS */

/* bytes are numbered from left to right
#define RTOLBYTES */

/* we want prtree included */
# define STDPRTREE
# ifndef FORT
# define ONEPASS
#endif

# define ENUMSIZE(high,low) INT


/* register char and short are not allowed
#define REG_CHAR */
/* addr of reg+offset is computed */
# define ADDROREG
# define FIXDEF(p) outstab(p)
# define FIXARG(p) fixarg(p)
# define FIXSTRUCT(a,b) outstruct(a,b)
# define PRTDCON	/* use local version of prtdcon */
# define ASSTRINGS	/* assembler recognizes strings */
# define STABDOT	/* output .stabdot entries */
# define LCOMM		/* .lcomm for non-global commons */
# define BUFSTDERR	/* buffer error messages */
# define FLEXNAMES	/* unlimited name length */
