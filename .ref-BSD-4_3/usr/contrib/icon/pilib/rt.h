/*
 * Definitions and declarations used throughout the run-time system.
 */

#include <stdio.h>
#include "../h/config.h"
#ifdef VAX
/*
 * Memory sizes.
 */
#define MAXHEAPSIZE		51200	/* size of the heap in bytes */
#define MAXSTRSPACE		51200	/* size of the string space in bytes */
#define STACKSIZE		 2000	/* size of co-expression stack in words */
#define MAXSTACKS		    4	/* number of coexpression stacks */
#define NUMBUF			   10	/* number of i/o buffers available */
#define NBUCKETS		   37   /* number of hash buckets */
/*
 * Implementation specific constants.
 */
#define INTSIZE			   32	/* bits per integer */
#define LOGINTSIZE		    5	/* log of INTSIZE */
#define MINSHORT	      0100000	/* smallest short integer */
#define MAXSHORT	       077777	/* largest short integer */
#define MINLONG		020000000000L	/* smallest long integer */
#define MAXLONG		017777777777L	/* largest long integer */
#define LGHUGE			   39	/* maximum base-10 exp+1 of real */
#define FRAMELIMIT		    2	/* maxmum negative offset in proc frame */
#define WORDSIZE	  sizeof(int *)	/* size in bytes of a pointer */
#define STKBASE		     0x7fffffff	/* highest possible address for sp */
#define GRANSIZE		 1024	/* storage allocation granule size */
/*
 * Some macros for source code tailoring.
 */ 
#define SetBound	setbound() 
#define ClearBound	clrbound() 
#define DclSave		register int saveefp,savegfp;
#define EntryPoint(x)	(char *)x + 2
#define Global(x)	.globl x
#define DummyFcn(x)	.globl x; x: halt
#define DummyData(x)	.globl x; x: .long 0
#define DummyRef(x)	.long x
#define gfp		r10
#define efp		r11
#define ipc		r9
/*
 * Cset initialization macros.
 */
#define twd(w0, w1)     ((w0)&0xffff | (w1)<<16) 
#define cset_display(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf) \
        {twd(w0,w1),twd(w2,w3),twd(w4,w5),twd(w6,w7), \
         twd(w8,w9),twd(wa,wb),twd(wc,wd),twd(we,wf)}
#endif VAX
#ifdef PORT
/*
 * Memory sizes.
 */
#define MAXHEAPSIZE	    x	/* size of the heap in bytes */
#define MAXSTRSPACE	    x	/* size of the string space in bytes */
#define STACKSIZE	    x	/* size of coexpression stack in WORDSIZE
				    words */
#define MAXSTACKS	    x	/* number of coexpression stacks */
#define NUMBUF		    x	/* number of i/o buffers available */
#define NBUCKETS	    x   /* number of hash buckets */
/*
 * Implementation specific constants.
 */
#define INTSIZE		     x	/* bits per integer */
#define LOGINTSIZE	     x	/* log of INTSIZE */
#define MINSHORT	     x	/* smallest short integer */
#define MAXSHORT	     x	/* largest short integer */
#define MINLONG		     x	/* smallest long integer */
#define MAXLONG		     x	/* largest long integer */
#define LGHUGE		     x	/* maximum base-10 exp + 1 of float number */
#define FRAMELIMIT	     x	/* maximum negative offset in proc frame (int) */
#define WORDSIZE sizeof(int *)	/* size in bytes of a pointer */
#define STKBASE		     x	/* highest possible address for sp */
#define GRANSIZE	     x	/* storage allocation granule size */
/*
 * Some macros to allow customization.
 */ 
/*#define SetBound*/
/*#define ClearBound*/
/*#define DclSave*/
/*#define EntryPoint(name) (char *)name + x*/
/*#define Global(x)*/
/*#define DummyFcn(x)*/
/*#define DummyData(x)*/
/*#define DummyRef(x)*/
/*#define gfp x*/
/*#define efp x*/
/*#define ipc x*/
/*
 * Cset initialization macros.
 */
/*
 For ints with 32 bits use these:
    
    #define twd(w0, w1)     ((w0)&0xffff | (w1)<<16)
    #define cset_display(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf) \
        {twd(w0,w1),twd(w2,w3),twd(w4,w5),twd(w6,w7), \
         twd(w8,w9),twd(wa,wb),twd(wc,wd),twd(we,wf)}

 For ints with 16 bits use these:

    #define cset_display(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf) \
        {w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf}
*/
#endif PORT

#ifdef PDP11
#define MAXHEAPSIZE	10240	/* size of the heap in bytes */
#define MAXSTRSPACE	10240	/* size of the string space in bytes */
#define STACKSIZE	 1000	/* size a coexpression stack in WORDSIZE
				    words */
#define MAXSTACKS	    2	/* number of coexpression stacks */
#define NUMBUF              5	/* number of i/o buffers available */
#define NBUCKETS	   13   /* number of hash buckets */
#define MAXLISTSIZE	 4090	/* Defined if lists are limited in size 
				    due to addressing limitations of a
				    particular architecture.  Specified
				    value is the largest list element
				    block that can be made. */
/*
 * Implementation specific constants.
 */
#define INTSIZE            16	/* bits per integer */
#define LOGINTSIZE          4	/* log of INTSIZE */
#define LONGS			/* longs are not same as ints */
#define MINLONG 020000000000L	/* smallest long integer */
#define MAXLONG 017777777777L	/* largest long integer */
#define MINSHORT      0100000	/* smallest short integer */
#define MAXSHORT       077777	/* largest short integer */
#define LGHUGE	 	   39	/* maximum base-10 exp +1 of float number */
#define FRAMELIMIT          5	/* maxmum negative offset in proc frame (int) */
#define WORDSIZE sizeof(int *)	/* size in bytes of a pointer */
#define STKBASE	      0177776	/* highest possible address for sp */
#define GRANSIZE           64	/* storage allocation granule size */
/*
 * Some macros to allow customization.
 */ 
#define SetBound
#define ClearBound
#define DclSave
#define EntryPoint(x)	(char *)x + 4
#define Global(x)	.globl x
#define DummyFcn(x)	.globl x; x: 0
#define DummyData(x)	.globl x; x: 0
#define DummyRef(x)	.globl x; x
/*
 * Cset initialization macros.
 */
#define cset_display(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf) \
        {w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf}
#endif PDP11

/*
 * Constants that are not likely to vary between implementations.
 */
#define BITOFFMASK (INTSIZE-1)
#define CSETSIZE (256/INTSIZE)	/* number of ints to hold 256 cset
				     bits.  Use (256/INTSIZE)+1 if
				     256 % INTSIZE != 0 */
#define LISTBLKSIZE	     8	/* number of elements in an expansion
				 *  list element block  */
#define MAXSTRING	   257	/* largest string in conversions */
#define MAXREADSTRING	  2049	/* largest string to read() in one piece */
#define RANDA	    1103515245	/* random seed multiplier */
#define RANDC	     453816694	/* random seed additive constant */
#define RSCALE	 4.65661286e-10	/* random scale factor = 1/(2^31-1)) */

/*
 * Offset in word of cset bit.
 */
#define CSOFF(b)	((b) & BITOFFMASK) 
/*
 * Address of word of cset bit.
 */
#define CSPTR(b,c)	((c) + (((b)&0377) >> LOGINTSIZE)) 
/*
 * Set bit b in cset c.
 */
#define setb(b,c)	(*CSPTR(b,c) |= (01 << CSOFF(b))) 
/*
 * Test bit b in cset c.
 */
#define tstb(b,c)	((*CSPTR(b,c) >> CSOFF(b)) & 01) 

/*
 * Runtime data structures.
 */

union numeric {			/* long integers or real numbers */
   long integer;
   double real;
   };

struct descrip {		/* descriptor */
   int  type;			/* type field */
   union {
      int integr;		/* integer value */
      char *cptr;		/* pointer to character string */
      union block *bptr;	/* pointer to a block */
      struct descrip *dptr;	/* pointer to a descriptor */
      } value;
   };

struct sdescrip {
   int length;			/* length of string */
   char *string;		/* pointer to string */
   };

struct b_int {			/* long integer block */
   int type;			/*   T_LONGINT */
   long intval;			/*   value */
   };

struct b_real {			/* real block */
   int type;			/*   T_REAL */
   double realval;		/*   value */
   };

struct b_cset {			/* cset block */
   int type;			/*   T_CSET */
   int bits[CSETSIZE];		/*   array of bits, one per ascii character */
   };

struct b_file {			/* file block */
   int type;			/*   T_FILE */
   FILE *fd;			/*   Unix file descriptor */
   int status;			/*   file status */
   struct descrip fname;	/*   file name (string qualifier) */
   };

struct b_proc {			/* procedure block */
   int type;			/*   T_PROC */
   int size;			/*   size of block */
   char *entryp;		/*   entry point (code) */
   int nparam;			/*   number of parameters */
   int ndynam;			/*   number of dynamic locals */
   int nstatic;			/*   number of static locals */
   int fstatic;			/*   index (in global table) of first static */
   struct descrip pname;	/*   procedure name (string qualifier) */
   struct descrip lnames[1];	/*   list of local names (qualifiers) */
   };

/*
 * b_iproc blocks are used to statically initialize information about
 *  functions.  They are identical to b_proc blocks except for
 *  the pname field which is a sdecrip (simple/string descriptor) instead
 *  of a descrip.  This is done because unions can't be initialized.
 */
	
struct b_iproc {		/* procedure block */
   int ip_type;			/*   T_PROC */
   int ip_size;			/*   size  of block */
   char *ip_entryp;		/*   entry point (code) */
   int ip_nparam;		/*   number of parameters */
   int ip_ndynam;		/*   number of dynamic locals */
   int ip_nstatic;		/*   number of static locals */
   int ip_fstatic;		/*   index (in global table) of first static */
   struct sdescrip ip_pname;	/*   procedure name (string qualifier) */
   struct descrip ip_lnames[1];	/*   list of local names (qualifiers) */
   };

struct b_list {			/* list header block */
   int type;			/*   T_LIST */
   int cursize;			/*   current list size */
   struct descrip listhead;	/*   pointer to first list element block */
   struct descrip listtail;	/*   pointer to last list element block */
   };

struct b_lelem {		/* list element block */
   int type;			/*   T_LELEM */
   int size;			/*   size of block */
   int nelem;			/*   total number of elements */
   int first;			/*   index of first element */
   int nused;			/*   number of used elements */
   struct descrip listprev;	/*   pointer to previous list element block */
   struct descrip listnext;	/*   pointer to next list element block */
   struct descrip lslots[1];	/*   array of elements */
   };

struct b_table {		/* table header block */
   int type;			/*   T_TABLE */
   int cursize;			/*   current table size */
   struct descrip defvalue;	/*   default table element value */
   struct descrip buckets[NBUCKETS]; /* hash buckets */
   };

struct b_telem {		/* table element block */
   int type;			/*   T_TELEM */
   int hashnum;			/*   for ordering chain */
   struct descrip blink;	/*   hash bucket link */
   struct descrip tref;		/*   reference field */
   struct descrip tval;		/*   value field */
   };

#ifdef SETS
struct b_set {			/* set header block */
   int type;			/*   T_SET */
   int setsize;			/*   cardinality of the set */
   struct descrip sbucks[NBUCKETS];  /* hash buckets */
   };

struct b_selem {		/* set element block */
   int type;			/*   T_SELEM */
   int hnum;			/*   hash number */
   struct descrip setmem;	/*   the element */
   struct descrip sblink;	/*   hash chain link */
   };
#endif SETS

struct b_record {		/* record block */
   int type;			/*   T_RECORD */
   int size;			/*   size of block */
   struct b_proc *recptr;	/*   pointer to record constructor proc */
   struct descrip fields[1];	/*   fields */
   };

struct b_tvsubs {		/* substring trapped variable block */
   int type;			/*   T_TVSUBS */
   int sslen;			/*   length of substring */
   int sspos;			/*   position of substring */
   struct descrip ssvar;	/*   variable that substring is from */
   };

struct b_tvtbl {		/* table element trapped variable block */
   int type;			/*   T_TVTBL */
   int hashnum;			/* for ordering */
   struct descrip tvtable;	/* pointer to table header block */
   struct descrip tvtref;	/*   reference field */
   struct descrip tvtval;	/* used when block is converted to telem */
   };

struct b_estack {		/* co-expression stack block */
   int type;			/*   T_ESTACK */
   struct descrip activator;	/*   most recent activator */
   int *sbase;			/*   stack base */
   int *sp;			/*   stack pointer */
   int *ap;			/*   address pointer, only on Vax */
   int *boundary;		/*   Icon/C boundary */
   int nresults;		/*   number of results produced */
   struct descrip freshblk;	/*   refresh block pointer */
   };

struct b_eblock {		/* co-expression heap block */
   int type;			/*   T_EBLOCK */
   int size;			/*   size of block */
   int *ep;			/*   entry point */
   int numargs;			/*   number of arguments */
   int numlocals;		/*   number of locals */
   struct descrip elems[1];	/*   arguments and locals, including arg0 */
   };

union block {			/* general heap block */
   struct b_int longint;
   struct b_real realblk;
   struct b_cset cset;
   struct b_file file;
   struct b_proc proc;
   struct b_list list;
   struct b_lelem lelem;
   struct b_table table;
   struct b_telem telem;
#ifdef SETS
   struct b_set set;
   struct b_selem selem;
#endif SETS
   struct b_record record;
   struct b_tvsubs tvsubs;
   struct b_tvtbl tvtbl;
   struct b_estack estack;
   struct b_eblock eblock;
   };

/*
 * External declarations.
 */

extern char (*bufs)[BUFSIZ];	/* i/o buffers */
extern FILE **bufused;		/* i/o buffer use markers */

extern int *stacks;		/* start of expression stack space */
extern int *estacks;		/* end of expression stack space */
extern int *esfree;		/* expression stack space free list header */

extern int ssize;		/* size of string space (bytes) */
extern char *strings;		/* start of string space */
extern char *estrings;		/* end of string space */
extern char *sfree;		/* string space free pointer */

extern int hpsize;		/* size of heap (words) */
extern char *hpbase;		/* base of heap */
extern char *maxheap;		/* maximum address in heap */
extern char *hpfree;		/* first free location in heap */

extern int bsizes[];		/* sizes of heap blocks */
extern int firstd[];		/* offset (words) of first descrip. */
extern char *blkname[];		/* print names for block types. */

extern int numbufs;		/* number of buffers */

extern int stksize;		/* size of coexpression stacks in words */

extern struct b_cset k_ascii;	/* value of &ascii */
extern struct b_cset k_cset;	/* value of &cset */
extern struct b_file k_errout;	/* value of &errout */
extern struct b_file k_input;	/* value of &input */
extern struct b_cset k_lcase;	/* value of &lcase */
extern int k_level;		/* value of &level */
extern int k_pos;		/* value of &pos */
extern struct descrip k_main;	/* value of &main */
extern struct b_file k_output;	/* value of &output */
extern long k_random;		/* value of &random */
extern struct descrip k_subject;  /* value of &subject */
extern int k_trace;		/* value of &trace */
extern struct b_cset k_ucase;	/* value of &ucase */
extern long starttime;		/* start time in milliseconds */
extern struct descrip nulldesc;	/* universal &null */
extern struct descrip zerodesc;	/* universal 0 */
extern struct descrip onedesc;	/* universal 1 */
extern struct descrip nullstr;	/* universal null string */
extern struct descrip blank;	/* universal blank */
extern struct descrip letr;	/* universal letter 'r' */
extern struct descrip maps2;	/* save space for 2nd arg to map() */
extern struct descrip maps3;	/* save space for 3rd arg to map() */
extern struct descrip current;	/* current expression stack pointer */
extern struct descrip input;	/* universal input file */
extern struct descrip errout;	/* universal error file */
extern struct descrip lcase;	/* universal lower case string */
extern struct descrip ucase;	/* universal upper case string */

extern int line;		/* current source program line number */
extern char *file;		/* current source program file name */

/*
 * Descriptor flags.
 */
#ifdef VAX
#define F_NQUAL	0x80000000	/* set if NOT string qualifier */
#define F_VAR	0x40000000	/* set if variable */
#define F_TVAR	0x20000000	/* set if trapped variable */
#define F_PTR	0x10000000	/* set if value field is pointer */
#endif VAX

#ifdef PORT
#define F_NQUAL		    x	/* set if NOT string qualifier */
#define F_VAR		    x	/* set if variable */
#define F_TVAR		    x	/* set if trapped variable */
#define F_PTR		    x	/* set if value field is pointer */
#endif PORT

#ifdef PDP11
#define F_NQUAL    0100000	/* set if NOT string qualifier */
#define F_VAR      0040000	/* set if variable */
#define F_TVAR     0020000	/* set if trapped variable */
#define F_PTR      0010000	/* set if value field is pointer */
#endif PDP11

#define TYPEMASK	63	/* type mask */
#define OFFSETMASK	(~(F_NQUAL|F_VAR|F_TVAR)) /* offset mask for variables */

/*
 * Type codes (descriptors and blocks).
 */

#define T_INTEGER	 1	/* short integer */
/*
 * For 32 bit machines, e.g. the Vax, we make LONGINT's and INTEGER's
 *  the same.  It would be better to have a generic integer type, and
 *  also have LONGINT's and SHORTINT's, but at the current time,
 *  LONGINT is used both to refer to integers not representable by
 *  a short, and as a generic integer type.
 */
#ifdef LONGS
#define T_LONGINT	 2	/* long integer */
#else LONGS
#define T_LONGINT	 1	/* long integer */
#endif LONGS
#define T_REAL		 3	/* real number */
#define T_CSET		 4	/* cset */
#define T_FILE		 5	/* file block */
#define T_PROC		 6	/* procedure block */
#define T_LIST		 7	/* list header block */
#define T_TABLE		 8	/* table header */
#define T_RECORD	 9	/* record block */
#define T_TELEM		10	/* table element block */
#define T_LELEM		11	/* list element block */
#define T_TVSUBS	12	/* substring trapped variable */
#define JUNK_13		13	/* (no longer used) */
#define T_TVTBL		14	/* table element trapped variable block */
#define T_TVPOS		15	/* &pos trapped variable */
#define T_TVRAND	16	/* &random trapped variable */
#define T_TVTRACE	17	/* &trace trapped variable */
#define T_ESTACK	18	/* expression stack block */
#define T_EBLOCK	19	/* expression heap block */
#ifdef SETS
#define T_SET		20	/* set header block */
#define T_SELEM		21	/* set element block */

#define MAXTYPE		21	/* maximum type number */
#else SETS
#define MAXTYPE		19	/* maximum type number w/o sets */
#endif SETS

/*
 * Descriptor types and flags.
 */

#define D_VAR		(F_VAR | F_NQUAL)
#define D_TVAR		(F_VAR | F_TVAR | F_NQUAL)

#define D_NULL		0
#define D_INTEGER	(T_INTEGER | F_NQUAL)
#define D_LONGINT	(T_LONGINT | F_PTR | F_NQUAL)
#define D_REAL		(T_REAL | F_PTR | F_NQUAL)
#define D_CSET		(T_CSET | F_PTR | F_NQUAL)
#define D_FILE		(T_FILE | F_PTR | F_NQUAL)
#define D_PROC		(T_PROC | F_PTR | F_NQUAL)
#define D_LIST		(T_LIST | F_PTR | F_NQUAL)
#define D_TABLE		(T_TABLE | F_PTR | F_NQUAL)
#ifdef SETS
#define D_SET		(T_SET | F_PTR | F_NQUAL)
#define D_SELEM		(T_SELEM | F_PTR | F_NQUAL)
#endif SETS
#define D_RECORD	(T_RECORD | F_PTR | F_NQUAL)
#define D_TELEM		(T_TELEM | F_PTR | F_NQUAL)
#define D_LELEM		(T_LELEM | F_PTR | F_NQUAL)
#define D_TVSUBS	(T_TVSUBS | D_TVAR)
#define D_TVTBL		(T_TVTBL | D_TVAR)
#define D_TVPOS		(T_TVPOS | D_TVAR)
#define D_TVRAND	(T_TVRAND | D_TVAR)
#define D_TVTRACE	(T_TVTRACE | D_TVAR)
#define D_ESTACK	(T_ESTACK | F_PTR | F_NQUAL)
#define D_EBLOCK	(T_EBLOCK | F_PTR | F_NQUAL)

/*
 * File status flags in status field of file blocks.
 */

#define FS_READ		 01	/* read access */
#define FS_WRITE	 02	/* write access */
#define FS_CREATE	 04	/* file created on open */
#define FS_APPEND	010	/* append mode */
#define FS_PIPE		020	/* reading/writing on a pipe */

/*
 * Macros for testing descriptors.  d must be of type struct descrip.
 */

#define NULLDESC(d)	(((d).type|(d).value.integr)==0) /* check for &null */

/*
 * String qualifiers.
 */
#define QUAL(d)		(!((d).type & F_NQUAL))	/* check for qualifier */
#define STRLEN(d)	((d).type)		/* get length of string */
#define STRLOC(d)	((d).value.cptr)	/* get address of string */

/*
 * Values, d must not be qualifier or variable.
 */
#define TYPE(d)		((d).type & TYPEMASK)	/* get type code */
#define POINTER(d)	((d).type & F_PTR)	/* check for pointer */
#define INTVAL(d)	((d).value.integr)	/* get short integer value */
#define BLKLOC(d)	((d).value.bptr)	/* get pointer to block */

/*
 * Variables, d must not be qualifier.
 */
#define VAR(d)		((d).type & F_VAR)	/* check for variable */
#define TVAR(d)		((d).type & F_TVAR)	/* check for trapped var */
#define OFFSET(d)	((d).type & OFFSETMASK)	/* get offset field */
#define VARLOC(d)	((d).value.dptr)	/* get pointer to descriptor */
#define TVARLOC(d)	((d).value.bptr)	/* get ptr to t.v. block */

/*
 * Macros to define procedure blocks.
 */
#define IDENT(x) x
#define CAT(x,y) IDENT(x)y
#define Procblock(f,nargs)\
	struct b_iproc CAT(B,f) = {\
	T_PROC,\
	vsizeof(struct b_proc),\
	EntryPoint(CAT(X,f)),\
	nargs,\
	-1,\
	0, 0,\
	{sizeof("f")-1,"f"}};

#define Opblock(a,b,c) Opblock1(a,b,c,0)
#define Opblockx(a,b,c,d) Opblock1(a,b,c,-d)
#define Opblock1(f,nargs,sname,realargs)\
	struct b_iproc CAT(B,f) = {\
	T_PROC,\
	vsizeof(struct b_proc),\
	EntryPoint(f),\
	nargs,\
	-1,\
	realargs,\
	0,\
	{sizeof(sname)-1,sname}};

/*
 * Macros to access Icon arguments from C-language library procedures.
 *  Library procedures must have exactly one argument, named nargs.
 */

/*
 * n-th argument.
 */
#define ARG(n)	 	(*((struct descrip *)(&nargs+1)+(nargs-n))) 
/*
 * Type field of n-th argument.
 */
#define ARGTYPE(n)	(*(&nargs+1+2*(nargs-n))) 
/*
 * Value field of n-th argument.
 */
#define ARGVAL(n)	(*(&nargs+2+2*(nargs-n))) 

/*
 * Minimum of x and y.
 */
#define MIN(x,y)	((x)<(y)?(x):(y)) 

/*
 * Maximum of x and y.
 */
#define MAX(x,y)	((x)>(y)?(x):(y))

/*
 * Derefence d.
 */
#define DeRef(d)	if(!QUAL(d)&&VAR(d))deref(&d);

/*
 *  vsizeof is for use with variable-sized (i.e., indefinite)
 *  structures containing an array declared of size 1
 *  to avoid compiler warnings associated with 0-sized arrays.
 */

#define vsizeof(s)	(sizeof(s) - sizeof(struct descrip))
