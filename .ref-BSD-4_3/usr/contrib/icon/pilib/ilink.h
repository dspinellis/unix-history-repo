/*
 * External declarations for the linker.
 */

#include <stdio.h>
#include "../h/config.h"

/*
 * Miscellaneous external declarations.
 */

extern FILE *infile;		/* current input file */
extern FILE *outfile;		/* interpreter output file */
extern FILE *dbgfile;		/* debug file */
extern char inname[];		/* input file name */
extern char outname[];		/* output file name */
extern char *pname;		/* this program name (from command line) */
extern int line;		/* source program line number (from ucode) */
extern char *file;		/* source program file name (from ucode) */
extern int statics;		/* total number of statics */
extern int dynoff;		/* stack offset counter for locals */
extern int argoff;		/* stack offset counter for arguments */
extern int static1;		/* first static in procedure */
extern int nlocal; 		/* number of locals in local table */
extern int nconst;		/* number of constants in constant table */
extern int nrecords;		/* number of records in program */
extern int trace;		/* initial setting of &trace */
extern int Dflag;		/* debug flag */
extern char ixhdr[];		/* header line for direct execution */
extern char *iconx;		/* location of iconx to put in #! line */
extern int hdrloc;		/* location to place hdr block at */
extern struct lfile *lfiles;	/* list of files to link */
extern struct lfile *getlfile();

/*
 * Interpreter code file header - this is written at the start of
 *  an icode file after the start-up program (if any) and the #! line.
 */
struct header {
   int size;			/* size of interpreter code */
   int trace;			/* initial value of &trace */
   int records;			/* location of record blocks */
   int ftab;			/* location of record/field table */
   int globals;			/* location of global variables */
   int gnames;			/* location of names of globals */
   int statics;			/* location of static variables */
   int ident;			/* location of identifier table */
   } hdr;

/*
 * Structures for symbol table entries.
 */

struct lentry {			/* local table entry */
   char *l_name;		/*   name of variable */
   int l_flag;			/*   variable flags */
   union {			/*   value field */
      int staticid;		/*     unique id for static variables */
      int offset;		/*     stack offset for args and locals */
      struct gentry *global;	/*     global table entry */
      } l_val;
   };

struct gentry {			/* global table entry */
   struct gentry *g_blink;	/*   link for bucket chain */
   char *g_name;		/*   name of variable */
   int g_flag;			/*   variable flags */
   int g_nargs; 		/*   number of args or fields */
   int g_procid;		/*   procedure or record id */
   int g_pc;			/*   position in icode of object */
   };

struct centry {			/* constant table entry */
   int c_flag;			/*   type of literal flag */
   union {			/*   value field */
      long ival;		/*     integer */
      double rval;		/*     real */
      char *sval;		/*     string */
      } c_val;
   int c_length;		/*   length of literal string */
   int c_pc;			/*   position in icode of object */
   };

struct ientry {			/* identifier table entry */
   struct ientry *i_blink;	/*   link for bucket chain */
   char *i_name;		/*   pointer to string */
   int i_length;		/*   length of string */
   };

struct fentry {			/* field table header entry */
   struct fentry *f_blink;	/*   link for bucket chain */
   char *f_name;		/*   name of field */
   int f_fid;			/*   field id */
   struct rentry *f_rlist;	/*   head of list of records */
   };

struct rentry {			/* field table record list entry */
   struct rentry *r_link;	/*   link for list of records */
   int r_recid;			/*   record id */
   int r_fnum;			/*   offset of field within record */
   };

/*
 * Structure for linked list of file names to link.
 */
struct lfile {
   struct lfile *lf_link;	/* next file in list */
   char *lf_name;		/* name of file */
   };

/*
 * Flag values in symbol tables.
 */

#define F_GLOBAL	    01	/* variable declared global externally */
#define F_PROC		    05	/* procedure (includes GLOBAL) */
#define F_RECORD	   011	/* record (includes GLOBAL) */
#define F_DYNAMIC	   020	/* variable declared local dynamic */
#define F_STATIC	   040	/* variable declared local static */
#define F_BUILTIN	  0101	/* identifier refers to built-in procedure */
#define F_IMPERROR  	  0400	/* procedure has default error */
#define F_ARGUMENT 	 01000	/* variable is a formal parameter */
#define F_INTLIT   	 02000	/* literal is an integer */
#define F_REALLIT  	 04000	/* literal is a real */
#define F_STRLIT  	010000	/* literal is a string */
#define F_CSETLIT 	020000	/* literal is a cset */
#define F_LONGLIT	040000	/* literal is a long integer */

/*
 * Symbol table region pointers.
 */

extern struct gentry **ghash;	/* hash area for global table */
extern struct ientry **ihash;	/* hash area for identifier table */
extern struct fentry **fhash;	/* hash area for field table */

extern struct lentry *ltable;	/* local table */
extern struct gentry *gtable;	/* global table */
extern struct centry *ctable;	/* constant table */
extern struct ientry *itable;	/* identifier table */
extern struct fentry *ftable;	/* field table headers */
extern struct rentry *rtable;	/* field table record lists */
extern char *strings;		/* string space */
extern int *labels;		/* label table */
extern char *code;		/* generated code space */

extern struct gentry *gfree;	/* free pointer for global table */
extern struct ientry *ifree;	/* free pointer for identifier table */
extern struct fentry *ffree; 	/* free pointer for field table headers */
extern struct rentry *rfree; 	/* free pointer for field table record lists */
extern char *sfree;		/* free pointer for string space */
extern char *codep;		/* free pointer for code space */

extern int lsize;		/* size of local table */
extern int gsize;		/* size of global table */
extern int csize;		/* size of constant table */
extern int isize;		/* size of identifier table */
extern int fsize;		/* size of field table headers */
extern int rsize;		/* size of field table record lists */
extern int ssize;		/* size of string space */
extern int ihsize;		/* size of identifier table hash area */
extern int ghsize;		/* size of global table hash area */
extern int fhsize;		/* size of field table hash area */
extern int maxlabels;		/* maximum number of labels per procedure */
extern int maxcode;		/* maximum amount of code per procedure */

extern int gmask;		/* mask for global table hash */
extern int imask;		/* mask for identifier table hash */
extern int fmask;		/* mask for field table hash */

/*
 * Symbol table parameters.
 */

#define LSIZE		 100	/* default size of local table */
#define GSIZE		 200	/* default size of global table */
#define CSIZE		 100	/* default size of constant table */
#define ISIZE		 500	/* default size of identifier table */
#define FSIZE		 100	/* default size of field table headers */
#define RSIZE		 100	/* default size of field table record lists */
#define SSIZE		5000	/* default size of string space */
#define GHSIZE		  64	/* default size of global table hash area */
#define IHSIZE		 128	/* default size of identifier table hash area */
#define FHSIZE		  32	/* default size of field table hash area */
#define MAXLABELS	 500	/* default maximum number of labels/proc */

/*
 * Hash computation macros.
 */

#define ghasher(x)	(((int)x)&gmask)	/* for global table */
#define fhasher(x)	(((int)x)&fmask)	/* for field table */

/*
 * Machine-dependent constants.
 */
#ifdef VAX
#define INTSIZE		   32	/* # of bits in an int */
#define LOGINTSIZE	    5	/* log of INTSIZE */
#define MAXCODE		10000	/* default maximum amount of code/proc */
#define OPSIZE		    1	/* # of bytes for opcode */
#define OPNDSIZE	    4	/* # of bytes in interpreter operands */
#define WORDSIZE  sizeof(int *)	/* # of bytes in a pointer (sizeof(int *)) */
#endif VAX

#ifdef PORT
#define INTSIZE		 x	/* # of bits in an int */
#define LOGINTSIZE	 x	/* log of INTSIZE */
/*#define LONGS			/* longs are different from ints */
/*#define MINSHORT       x	/* smallest short integer */
/*#define MAXSHORT       x	/* largest short integer */
#define MAXCODE	     	 x	/* default maximum amount of code/proc */
#define OPSIZE		 1	/* # of bytes for opcode */
#define OPNDSIZE         4	/* # of bytes in interpreter operands */
#define WORDSIZE  	 4	/* # of bytes in a pointer (sizeof(int *)) */
#endif PORT

#ifdef PDP11
#define INTSIZE		16	/* # of bits in an int */
#define LOGINTSIZE	 4	/* log of INTSIZE */
#define LONGS			/* longs are different from ints */
#define MINSHORT   0100000	/* smallest short integer */
#define MAXSHORT    077777	/* largest short integer */
#define MAXCODE	      2000	/* default maximum amount of code/proc */
#define OPSIZE		 1	/* # of bytes for opcode */
#define OPNDSIZE	 2	/* # of bytes in interpreter operands */
#define WORDSIZE sizeof(int *)	/* # of bytes in a pointer (sizeof(int *)) */
#endif PDP11

#define RKBLKSIZE 9*WORDSIZE	/* size of record constructor block */
#define BITOFFMASK (INTSIZE-1)
#define CSETSIZE (256/INTSIZE)	/* number of ints to hold 256 cset
				     bits.  Use (256/INTSIZE)+1 if
				     256 % INTSIZE != 0 */

#define MAXHDR	    1024	/* size of autoloading header */
#define HDRFILE	IconxHdr

/*
 * Cset accessing macros.  The definition of setb(b,c) is the total
 *  result of the following definitions.  setb is only used in
 *  code.c/emitcon.
 */
/*
 * Offset in word of cs bit.
 */
#define CSOFF(b)	((b) & BITOFFMASK) 
/*
 * Address of word of cs bit.
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
