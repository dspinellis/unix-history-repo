#include <stdio.h>
#include "../h/config.h"             /* should define VAX or PDP11 */
#ifdef VAX
#define SetBound	setbound()
#define ClearBound	clrbound()
#define DclSave		register int r11,r10;
#define EntryPoint(x)	(char *)x + 2
#define BIT32
/*
 * memory sizes (memory set up is in mem.c)
 */
#define MAXHEAPSIZE	51200
#define MAXSTRSPACE	51200
#define STACKSIZE	 2000
#define MAXSTACKS	    4
#define NUMBUF             10  /* number of i/o buffers available */
/*
 * implementation-dependent parameters
 */
#define INTSIZE         32	/* bits per integer */
#define LOGINTSIZE       5	/* log of INTSIZE */
#define BITOFFMASK     037	/* mask for bit offset into word */
#define FRAMELIMIT       2	/* max neg offset in proc frame (int) */
#define CSETSIZE         8	/* # of words to contain cset bits */
#define ADDRSIZE	 4	/* size in bytes of a pointer, could
				    be sizeof(*int) as well */
/*
 * Some macros to help define csets
 */
#define twd(w0, w1)     ((w0)&0xffff | (w1)<<16)
#define cset_display(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf) \
        {twd(w0,w1),twd(w2,w3),twd(w4,w5),twd(w6,w7), \
         twd(w8,w9),twd(wa,wb),twd(wc,wd),twd(we,wf)}
#endif VAX

#ifdef PDP11
#define SetBound
#define ClearBound
#define DclSave
#define EntryPoint(x)	(char *)x + 4
#define BIT16
#define MAXHEAPSIZE	10240
#define MAXSTRSPACE	10240
#define STACKSIZE	 1000
#define MAXSTACKS	    2
#define NUMBUF              5  /* number of i/o buffers available */
#define MAXLISTSIZE	 4090  /* Defined if lists are limited in size
				   due to addressing limitations of a
				   particular architecture.  Specified
				   value is the largest list block that
				   can be made. */
/*
 * implementation-dependent parameters
 */
#define INTSIZE         16	/* bits per integer */
#define LOGINTSIZE       4	/* log of INTSIZE */
#define BITOFFMASK     017	/* mask for bit offset into word */
#define FRAMELIMIT       5	/* max neg offset in proc frame (int) */
#define CSETSIZE        16	/* # of words to contain cset bits */
#define ADDRSIZE	 2	/* size in bytes of a pointer, could
				    be sizeof(*int) as well */
/*
 * Some macros to help define csets
 */
#define cset_display(w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf) \
                    {w0,w1,w2,w3,w4,w5,w6,w7,w8,w9,wa,wb,wc,wd,we,wf}
#endif

#define LISTBLKSIZE          8  /* # of elements in an expansion list block */
#define NBUCKETS            14  /* # of hash buckets for tables */
#define MAXSTRING          257  /* largest string in conversions */
#define MAXREADSTRING	  2049	/* largest string to read() in one piece */
#define MINLONG  020000000000L  /* smallest long integer */
#define MAXLONG  017777777777L  /* largest long integer */
#define MINSHORT       0100000  /* smallest short integer */
#define MAXSHORT        077777  /* largest short integer */
#define RANDA       1103515245  /* random seed multiplier */
#define RANDC        453816694  /* random seed additive constant */
#define RSCALE  4.65661286e-10  /* random scale factor = 1/(2^31-1)) */

/*
 * runtime data structures
 */

union numeric {                 /* long integers or real numbers */
   long i;
   double r;
   };

struct descrip {                /* descriptor */
   int  type;
   union {
      int integer;
      char *cptr;
      union block *bptr;
      struct descrip *dptr;
      } value;
   };

struct sdescrip {
	int length;		/* length of string */
	char *string;		/* pointer to string */
	};

struct b_int {                  /* long integer block */
   int type;                    /*   T_LONGINT */
   long intval;                 /*   value */
   };

struct b_real {                 /* real block */
   int type;                    /*   T_REAL */
   double realval;              /*   value */
   };

struct b_cset {                 /* cset block */
   int type;                    /*   T_CSET */
   int bits[CSETSIZE];          /*   array of bits, one per ascii character */
   };

struct b_file {                 /* file block */
   int type;                    /*   T_FILE */
   FILE *fd;                    /*   Unix file descriptor */
   int status;                  /*   file status */
   struct descrip fname;       /*   file name (string qualifier) */
   };

struct b_proc {                 /* procedure block */
   int type;                    /*   T_PROC */
   int size;                    /*   size of block */
   char *entryp;                 /*   entry point (code) */
   int nparam;                  /*   number of parameters */
   int ndynam;                  /*   number of dynamic locals */
   int nstatic;                 /*   number of static locals */
   int fstatic;                 /*   index (in global table) of first static */
   struct descrip pname;        /*   procedure name (string qualifier) */
   struct descrip lnames[0];    /*   list of local names (qualifiers) */
   };
/*
 * We use b_iproc blocks to initialize information about functions
 *  at runtime.  They are identical to b_proc blocks except for
 *  the pname field which is a sdecrip (simple/string descriptor) instead
 *  of a descrip.  We have to do this because we can't initialize
 *  the union in a descrip.
 */
	
struct b_iproc {                /* procedure block */
   int ip_type;                    /*   T_PROC */
   int ip_size;                    /*   size of block */
   char *ip_entryp;                /*   entry point (code) */
   int ip_nparam;                  /*   number of parameters */
   int ip_ndynam;                  /*   number of dynamic locals */
   int ip_nstatic;                 /*   number of static locals */
   int ip_fstatic;                 /*   index (in global table) of first static */
   struct sdescrip ip_pname;        /*   procedure name (string qualifier) */
   struct descrip ip_lnames[0];    /*   list of local names (qualifiers) */
   };

struct b_list {                 /* list header block */
   int type;                    /*   T_LIST */
   int cursize;                 /*   current list size */
   struct descrip listhead;     /*   pointer to first list block */
   struct descrip listtail;     /*   pointer to last list block */
   };

struct b_listb {                /* list block */
   int type;                    /*   T_LISTB */
   int size;                    /*   size of block */
   int nelem;                   /*   total number of elements */
   int first;                   /*   index of first element */
   int nused;                   /*   number of used elements */
   struct descrip listprev;     /*   pointer to previous list block */
   struct descrip listnext;     /*   pointer to next list block */
   struct descrip lelem[0];     /*   array of elements */
   };
   struct descrip stacknext;    /*   pointer to next stack block */

struct b_table {                /* table header block */
   int type;                    /*   T_TABLE */
   int cursize;                 /*   current table size */
   struct descrip defvalue;     /*   default table element value */
   struct descrip buckets[NBUCKETS];
                                /*   hash buckets */
   };

struct b_telem {                /* table element block */
   int type;                    /*   T_TELEM */
   struct descrip blink;        /*   hash bucket link */
   struct descrip tref;         /*   reference field */
   struct descrip tval;         /*   value field */
   };

struct b_record {               /* record block */
   int type;                    /*   T_RECORD */
   int size;                    /*   size of block */
   struct b_proc *recptr;       /*   pointer to record constructor proc */
   struct descrip fields[0];    /*   fields */
   };

struct b_tvsubs {               /* substring trapped variable block */
   int type;                    /*   T_TVSUBS */
   int sslen;                   /*   length of substring */
   int sspos;                   /*   position of substring */
   struct descrip ssvar;        /*   variable that substring is from */
   };

struct b_tvtbl {                /* table element trapped variable block */
   int type;                    /*   T_TVTBL */
   struct descrip tvtable;      /*      pointer to table */
   struct descrip tvtref;       /*   reference field */
   };

struct b_estack {               /* co-expression stack block */
   int type;                    /*   T_ESTACK */
   struct descrip activator;    /*   most recent activator */
   int *sbase;                  /*   stack base */
   int *sp;                     /*   stack pointer */
   int *ap;			/*   address pointer, only on Vax */
   int *boundary;               /*   Icon/C boundary */
   int nresults;                /*   number of results produced */
   struct descrip freshblk;     /*   refresh block pointer */
   };

struct b_eblock {               /* co-expression heap block */
   int type;                    /*   T_EBLOCK */
   int size;                    /*   size of block */
   int *ep;                     /*   entry point */
   int numargs;                 /*   number of arguments */
   int numlocals;               /*   number of locals */
   struct descrip elems[0];     /*   arguments and locals */
   };

union block {                   /* general heap block */
   struct b_int    longint;
   struct b_real   real;
   struct b_cset   cset;
   struct b_file   file;
   struct b_proc   proc;
   struct b_list   list;
   struct b_listb  listb;
   struct b_table  table;
   struct b_telem  telem;
   struct b_record record;
   struct b_tvsubs tvsubs;
   struct b_tvtbl  tvtbl;
   struct b_estack estack;
   struct b_eblock eblock;
   };

/*
 * external declarations
 */

extern char (*bufs)[BUFSIZ];    /* i/o buffers */
extern FILE **bufused;          /* i/o buffer use markers */

extern int *stacks;             /* start of expression stack space */
extern int *estacks;            /* end of expression stack space */
extern int *esfree;             /* expression stack space free list header */

extern int ssize;               /* size of string space (bytes) */
extern char *strings;           /* start of string space */
extern char *estrings;          /* end of string space */
extern char *sfree;             /* string space free pointer */

extern int hpsize;              /* size of heap (words) */
extern char *hpbase;            /* base of heap */
extern char *maxheap;           /* maximum address in heap */
extern char *hpfree;            /* first free location in heap */

extern int bsizes[];            /* sizes of heap blocks */
extern int firstd[];            /* offset (words) of first descrip. */
extern char *blkname[];         /* print names for block types. */

extern int numbufs;		/* number of buffers */

extern struct b_cset  k_ascii;  /* value of &ascii */
extern struct b_cset  k_cset;   /* value of &cset */
extern struct b_file  k_errout; /* value of &errout */
extern struct b_file  k_input;  /* value of &input */
extern struct b_cset  k_lcase;  /* value of &lcase */
extern int            k_level;  /* value of &level */
extern int            k_pos;    /* value of &pos */
extern struct descrip k_main;   /* value of &main */
extern struct b_file  k_output; /* value of &output */
extern long           k_random; /* value of &random */
extern struct descrip k_subject;/* value of &subject */
extern int            k_trace;  /* value of &trace */
extern struct b_cset  k_ucase;  /* value of &ucase */
extern long starttime;          /* start time in milliseconds */
extern struct descrip nulldesc; /* universal &null     */
extern struct descrip zerodesc; /* universal 0         */
extern struct descrip onedesc;  /* universal 1         */
extern struct descrip nullstr;	/* universal null string */
extern struct descrip blank;	/* universal blank     */
extern struct descrip letr; 	/* universal letter 'r' */
extern struct descrip maps2;    /* save space for 2nd arg to map() */
extern struct descrip maps3;    /* save space for 3rd arg to map() */
extern struct descrip current;  /* current expression stack pointer */
extern struct descrip input;    /* universal input file */
extern struct descrip errout;   /* universal error file */
extern struct descrip lcase;    /* universal lower case string */
extern struct descrip ucase;    /* universal upper case string */

extern int line;                /* current source program line number */
extern char *file;              /* current source program file name */

/*
 * descriptor flags
 */
#ifdef VAX
#define F_NQUAL    0x80000000              /* set if NOT string qualifier */
#define F_VAR      0x40000000              /* set if variable */
#define F_TVAR     0x20000000              /* set if trapped variable */
#define F_PTR      0x10000000              /* set if value field is pointer */
#define F_NUM      0x08000000              /* set if numeric value */
#define F_INT      0x04000000              /* set if integer value */
#define F_AGGR     0x02000000              /* set if aggregrate */
#define TYPEMASK   0x0000003f              /* type mask */
#define OFFSETMASK 0x1fffffff              /* offset mask for variables */
#endif VAX
#ifdef PDP11
#define F_NQUAL    0100000              /* set if NOT string qualifier */
#define F_VAR      0040000              /* set if variable */
#define F_TVAR     0020000              /* set if trapped variable */
#define F_PTR      0010000              /* set if value field is pointer */
#define F_NUM      0004000              /* set if numeric value */
#define F_INT      0002000              /* set if integer value */
#define F_AGGR     0001000              /* set if aggregrate */
#define TYPEMASK   0000077              /* type mask */
#define OFFSETMASK 0017777              /* offset mask for variables */
#endif PDP11


/*
 * type codes (descriptors and blocks)
 */

#define T_INTEGER        1              /* short integer (not put in heap) */
/*
 * For 32 bit machines, e.g. the Vax, we make LONGINT's and INTEGER's
 *  the same.  It would be better to have a generic integer type, and
 *  also have LONGINT's and SHORTINT's, but at the current time,
 *  LONGINT is used both to refer to integers not representable by
 *  a short, and as a generic integer type. -- whm
 */
#ifdef BIT32
#define T_LONGINT        1              /* long integer type */
#else
#define T_LONGINT        2              /* long integer type */
#endif
#define T_REAL           3              /* real number */
#define T_CSET           4              /* cset */
#define T_FILE           5              /* file block */
#define T_PROC           6              /* procedure block */
#define T_LIST           7              /* list header */
#define T_TABLE          8              /* table header */
#define T_RECORD         9              /* record block */
#define T_TELEM         10              /* table element */
#define T_LISTB         11              /* list element block */
#define T_TVSUBS        12              /* substring trapped variable */
#define JUNK_13         13              /* (no longer used) */
#define T_TVTBL         14              /* table element trapped variable */
#define T_TVPOS         15              /* &pos trapped variable */
#define T_TVRAND        16              /* &random trapped variable */
#define T_TVTRACE       17              /* &trace trapped variable */
#define T_ESTACK        18              /* expression stack block */
#define T_EBLOCK        19              /* expression heap block */

#define MAXTYPE         19              /* maximum type number */

/*
 * descriptor types and flags
 */

#define D_VAR           (F_VAR | F_NQUAL)
#define D_TVAR          (F_VAR | F_TVAR | F_NQUAL)

#define D_NULL          0
#define D_INTEGER       (T_INTEGER | F_NUM | F_INT | F_NQUAL)
#define D_LONGINT       (T_LONGINT | F_NUM | F_INT | F_PTR | F_NQUAL)
#define D_REAL          (T_REAL | F_NUM | F_PTR | F_NQUAL)
#define D_CSET          (T_CSET | F_PTR | F_NQUAL)
#define D_FILE          (T_FILE | F_PTR | F_NQUAL)
#define D_PROC          (T_PROC | F_PTR | F_NQUAL)
#define D_LIST          (T_LIST | F_AGGR | F_PTR | F_NQUAL)
#define D_TABLE         (T_TABLE | F_AGGR | F_PTR | F_NQUAL)
#define D_RECORD        (T_RECORD | F_AGGR | F_PTR | F_NQUAL)
#define D_TELEM         (T_TELEM | F_AGGR | F_PTR | F_NQUAL)
#define D_LISTB         (T_LISTB | F_AGGR | F_PTR | F_NQUAL)
#define D_TVSUBS        (T_TVSUBS | D_TVAR)
#define D_TVTBL         (T_TVTBL | D_TVAR)
#define D_TVPOS         (T_TVPOS | D_TVAR)
#define D_TVRAND        (T_TVRAND | D_TVAR)
#define D_TVTRACE       (T_TVTRACE | D_TVAR)
#define D_ESTACK        (T_ESTACK | F_PTR | F_NQUAL)
#define D_EBLOCK        (T_EBLOCK | F_PTR | F_NQUAL)

/*
 * File status flags in status field of file blocks
 */

#define FS_READ     01          /* read access */
#define FS_WRITE    02          /* write access */
#define FS_CREATE   04          /* file created on open */
#define FS_APPEND  010          /* append mode */
#define FS_PIPE    020          /* reading/writing on a pipe */

/*
 * macros for testing descriptors
 * d must be of type struct descrip
 */

#define NULLDESC(d)  (((d).type|(d).value.integer)==0) /* check for &null */

/* string qualifiers */
#define QUAL(d)      (!((d).type & F_NQUAL))    /* check for qualifier */
#define STRLEN(d)    ((d).type)                 /* get length of string */
#define STRLOC(d)    ((d).value.cptr)           /* get address of string */

/* values, d must not be qualifier or variable */
#define TYPE(d)      ((d).type & TYPEMASK)      /* get type code */
#define NUMBER(d)    ((d).type & F_NUM)         /* check for numeric */
#define INTEGER(d)   ((d).type & F_INT)         /* check for integer */
#define POINTER(d)   ((d).type & F_PTR)         /* check for pointer */
#define AGGREGATE(d) ((d).type & F_AGGR)        /* check for aggregate */
#define INTVAL(d)    ((d).value.integer)        /* get short integer value */
#define BLKLOC(d)    ((d).value.bptr)           /* get pointer to block */

/* variables, d must not be qualifier */
#define VAR(d)       ((d).type & F_VAR)         /* check for variable */
#define TVAR(d)      ((d).type & F_TVAR)        /* check for trapped var */
#define OFFSET(d)    ((d).type & OFFSETMASK)    /* get offset field */
#define VARLOC(d)    ((d).value.dptr)           /* get pointer to descriptor */
#define TVARLOC(d)   ((d).value.bptr)           /* get ptr to t.v. block */

/*
 * macros to access Icon arguments from C-language library procedures
 * library procedures must have exactly one argument, named nargs
 */

#define ARG(n)       (*((struct descrip *)(&nargs+1)+(nargs-n))) /* n-th arg */
#define ARGTYPE(n)   (*(&nargs+1+2*(nargs-n)))  /* type field of n-th arg */
#define ARGVAL(n)    (*(&nargs+2+2*(nargs-n)))  /* value field of n-th arg */

/*
 * miscellaneous macros
 */

#define MIN(x,y)     ((x)<(y)?(x):(y))          /* minimum of x and y */
#define CSOFF(b)     ((b) & BITOFFMASK)         /* offset in word of cs bit */
#define CSPTR(b,c)   ((c) + (((b)&0377) >> LOGINTSIZE))
                                                /* address of word of cs bit */
#define setb(b,c)    (*CSPTR(b,c) |= (01 << CSOFF(b)))
                                                /* set bit b in cset c */
#define tstb(b,c)    ((*CSPTR(b,c) >> CSOFF(b)) & 01)
                                                /* test bit b in cset c */

