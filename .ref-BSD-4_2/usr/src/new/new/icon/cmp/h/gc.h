#include "rt.h"

#define MARK        0100000       /* high-order bit for gc marking */
#define SQLINC		128	  /* increment for s. q. list space */

/*
 * external definitions
 */

extern unsigned heapneed;	  /* stated need for heap space */
extern unsigned strneed;	  /* stated need for string space */
extern int *boundary;		  /* Icon/C stack boundary address */
#ifdef INT
extern struct descrip *globals;   /* beginning of global variables */
extern struct descrip *eglobals;  /* end of global variables */
extern struct descrip *gnames;    /* beginning of global variable names */
extern struct descrip *egnames;   /* end of global variable names */
extern struct descrip *statics;   /* beginning of static variables */
extern struct descrip *estatics;  /* end of static variables */
#endif INT
#ifdef CMP
extern struct descrip globals[];   /* beginning of global variables */
extern struct descrip eglobals[];  /* end of global variables */
extern struct descrip gnames[];    /* beginning of global variable names */
extern struct descrip egnames[];   /* end of global variable names */
extern struct descrip statics[];   /* beginning of static variables */
extern struct descrip estatics[];  /* end of static variables */
#endif CMP
extern struct descrip tended[];	  /* tended descriptors, used by lib routines */
extern struct descrip etended[];  /* end of tended descriptors */
extern struct descrip **sqlist;	  /* beginning of string qualifier list */
extern struct descrip **sqfree;	  /* string qualifier list free pointer */
extern struct descrip **esqlist;  /* end of string qualifier list */

/*
 * common macros
 */

#define MAX(a,b)     ((a)>(b)?(a):(b))
#define isptr(x)     ((!QUAL(*x)) && (VAR(*x) || POINTER(*x)))
#define blktype(x)   (*(int *)x)
/*
 * getsize(x) takes the block pointed to by x and if the size of
 *  the block as indicated by bsizes[] is non-zero it returns the
 *  indicated size, otherwise it returns the second word in the
 *  block which should be the size. --whm
 */
#define getsize(x)   (bsizes[*(int *)x & ~MARK] ? \
                       bsizes[*(int *)x & ~MARK] : *((int *)x + 1))
