/*
 * Definitions and declarations used for storage management.
 */

#define MARK		0100000		/* high-order bit for gc marking */
#define SQLINC		    128		/* increment for s. q. list space */

/*
 * External definitions.
 */

extern unsigned heapneed;		/* stated need for heap space */
extern unsigned strneed;		/* stated need for string space */
extern int *boundary;			/* Icon/C stack boundary address */
extern struct descrip *globals;		/* start of global variables */
extern struct descrip *eglobals;	/* end of global variables */
extern struct descrip *gnames;		/* start of global variable names */
extern struct descrip *egnames;		/* end of global variable names */
extern struct descrip *statics;		/* start of static variables */
extern struct descrip *estatics;	/* end of static variables */
extern struct descrip tended[];		/* tended descriptors, used by lib routines */
extern struct descrip etended[];	/* end of tended descriptors */
extern struct descrip **sqlist;		/* beginning of string qualifier list */
extern struct descrip **sqfree;		/* string qualifier list free pointer */
extern struct descrip **esqlist;  	/* end of string qualifier list */

/*
 * Test if *x is a pointer.
 */
#define isptr(x)     ((!QUAL(*x)) && (VAR(*x) || POINTER(*x))) 
/*
 * Get type of block pointed at by x.
 */
#define blktype(x)   (*(int *)x) 
/*
 * getsize(x) takes the block pointed to by x and if the size of
 *  the block as indicated by bsizes[] is non-zero it returns the
 *  indicated size, otherwise it returns the second word in the
 *  block which should be the size.
 */
#define getsize(x) (bsizes[*(int *)x & ~MARK] ? \
		     bsizes[*(int *)x & ~MARK] : *((int *)x + 1))
