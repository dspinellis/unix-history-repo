/*					-[Sun Jun 19 14:42:59 1983 by jkf]-
 * 	global.h			$Locker:  $
 * main include file 
 *
 * $Header: global.h,v 1.11 85/03/24 11:06:11 sklower Exp $
 *
 * (c) copyright 1982, Regents of the University of California
 */


#include <stdio.h>
#include "config.h"
#include "ltypes.h"
#ifdef UNIXTS
#include "tsfix.h"
#endif

#define AD 0

#define	peekc(p)	(p->_cnt>0? *(p)->_ptr&0377:_filbuf(p)==-1?-1:((p)->_cnt++,*--(p)->_ptr&0377))

#define FALSE	0
#define	TRUE	1
#define EVER	;;
#define STRBLEN 512
#define LBPG	512


#define	NULL_CHAR	0
#define	LF	'\n'
#define	WILDCHR	'\0177'


/* the numbers per page of the different data objects *******************/

#define NUMSPACES (VECTORI+1)

#define ATOMSPP 25
#define STRSPP 1
#define INTSPP 128
#define DTPRSPP 64
#define DOUBSPP 64
#define ARRAYSPP 25
#define SDOTSPP 64
#define VALSPP 128
#define BCDSPP 64


#define HUNK2SPP 64		 /* hunk page sizes */
#define HUNK4SPP 32
#define HUNK8SPP 16
#define HUNK16SPP 8
#define HUNK32SPP 4
#define HUNK64SPP 2
#define HUNK128SPP 1
#define VECTORSPP 512

/* offset of size info from beginning of vector,  in longwords */
/* these values are not valid when a vector is stored in the free */
/* list, in which case the chaining is done through the propery field */
#define VSizeOff -2
#define VPropOff -1

/* VecTotSize: the total number of longwords for the data segment of
 * the vector. Takes a byte count and rounds up to nearest long.
 */

#define VecTotSize(x)  (((x)+3) >> 2)
#define VecTotToByte(x) ((x) * sizeof(long))

/* these vector size macros determine the number of complete objects
   in the vector
 */
#define VecSize(x) 	((x) >> 2)
#define VecWordSize(x)	((x) >> 1)
#define VecByteSize(x)	(x)

/* maximum and minimum fixnums */
#define MaxINT 0x3fffffff
#define MinINT (- 0x4000000)
/* 
 * macros for saving state and restoring state
 *
 * Savestack and Restorestack are required at the beginning and end of
 * functions which modify the stack pointers np and lbot.
 * The Savestack(n) should appear at the end of the variable declarations
 * The n refers to the number of register variables declared in this routine.
 * The information is required for the Vax version only.
 */
#ifdef PORTABLE
extern struct atom nilatom, eofatom;
#define nil	((lispval) &nilatom)
#define eofa	((lispval) &eofatom)
#define Savestack(n) struct argent *OLDlbot = lbot, *OLDnp = np
#define Restorestack() (lbot = OLDlbot), np = OLDnp
#else
#define nil	((lispval) 0)
#define eofa	((lispval) 20)
#define Savestack(n) snpand(n)
#define Restorestack() 
#endif

#ifdef SIXONLY
#define errorh1 errh1
#define errorh2 errh2
#endif

#define	CNIL	((lispval) (OFFSET-4))
#define NOTNIL(a)	(nil!=a)
#define ISNIL(a)	(nil==a)

#ifdef SPISFP
extern long *xsp, xstack[];
#define sp() xsp
#define stack(z) (xsp > xstack ? (*--xsp = z): xserr())
#define unstack() (*xsp++)
#define Keepxs() long *oxsp = xsp;
#define Freexs() xsp = oxsp;
#else
extern long *sp(), stack(), unstack();
#define Keepxs() /* */
#define Freexs() /* */
#endif

extern char typetable[];  /*  the table with types for each page  */
#define ATOX(a1)	((((int)(a1)) - OFFSET) >> 9)
#define	TYPE(a1)	((typetable+1)[ATOX(a1)])
#define	TYPL(a1)	((typetable+1)[ATOX(a1)])
#define SETTYPE(a1,b,c)   {if((itemp = ATOX(a1)) >= fakettsize) \
			 { if(fakettsize >= TTSIZE) \
			   {\
			      printf(" all space exausted, goodbye\n");\
			      exit(1);\
			   }\
			   fakettsize++;  badmem(c);\
			 }\
			(typetable + 1)[itemp] = (b); }

#define	HUNKP(a1)	((TYPE(a1) >= 11) & (TYPE(a1) <= 17))
#define	HUNKSIZE(a1)	((TYPE(a1)+5) & 15)

#define UPTR(x)	((unsigned)(((long)(x))-(long)CNIL))
#define VALID(a)	(UPTR(a) <= UPTR(datalim))

#define Popframe() (errp->olderrp)


/* some types ***********************************************************/
#define lispint long
#define MAX10LNG 200000000		/* max long divided by 10	*/


typedef union lispobj *lispval ;

struct dtpr {
	lispval	cdr, car;
};

struct sdot {
	int 	I;
	lispval	CDR;
};


struct	atom	{
	lispval		clb;		/* current level binding*/
	lispval 	plist;		/* pointer to prop list	*/
#ifndef WILD
	lispval		fnbnd;		/* function binding	*/
#endif
	struct	atom	*hshlnk;	/* hash link to next	*/
	char		*pname;		/* print name	*/
	};
#ifdef WILD
#define fnbnd clb
#endif

struct array {
	lispval accfun,		/*  access function--may be anything  */
		aux;		/*  slot for dimensions or auxilliary data  */
	char *data;		/*  pointer to first byte of array    */
	lispval length, delta;	/* length in items and length of one item */
};

struct bfun {
	lispval (*start)();	/*  entry point to routine  */
	lispval	discipline,	/*  argument-passing discipline  */
		language,	/*  language coded in	*/
		params,		/*  parameter list if relevant  */
		loctab;		/*  local table  */
};

struct Hunk {
	lispval hunk[1];
};

struct Vector {
        lispval vector[1];
};

/* the vectori types */
struct Vectorb {
    	char vectorb[1];
};

struct Vectorw {
       short  vectorw[1];
};

struct Vectorl {
    long vectorl[1];
};

union lispobj {
	struct atom a;
	FILE *p;
	struct dtpr d;
	long int i;
	long int *j;
	double r;
	lispval (*f)();
	struct array ar;
	struct sdot s;
	char c;
	lispval l;
	struct bfun bcd;
	struct Hunk h;
	struct Vector v;
	struct Vectorb vb;
	struct Vectorw vw;
	struct Vectorl vl;
};

#ifdef lint
extern lispval Inewint();
#define inewint(p) Inewint((long)(p))
#else
extern lispval inewint();
#endif


#include "sigtab.h"   /* table of all pointers to lisp data */

/* Port definitions *****************************************************/
extern FILE	*piport,		/* standard input port		*/
	*poport,		/* standard output port		*/
	*errport,		/* port for error messages	*/
	*rdrport;		/* temporary port for readr	*/

#ifndef RTPORTS
extern FILE *xports[];		/* page of file *'s for lisp	*/
#define P(p)		((lispval) (xports +((p)-_iob)))
#define PN(p)		((int) ((p)-_iob))
#else
extern lispval P();
extern FILE **xports;
#define PN(p) (((FILE **)P(p))-xports)
#endif

extern int lineleng ;		/* line length desired		*/
extern char rbktf;		/* logical flag: ] mode		*/
extern unsigned char *ctable;		/* Character table in current use */
#define Xdqc ctable[131]
#define Xesc ctable[130]
#define Xsdc ctable[129]

/* name stack ***********************************************************/

#define NAMESIZE 3072

/* the name stack limit is raised by NAMINC every namestack overflow to allow
   a user function to handle the error
*/
#define NAMINC 25

extern struct nament {
	lispval	val,
		atm;
}	*bnp,			/* first free bind entry*/
	*bnplim;		/* limit of bindstack   */

struct argent {
	lispval	val;
};
extern struct argent *lbot, *np, *namptr;
extern struct nament	*bnp;			/* first free bind entry*/
extern struct argent *nplim;	 	/* don't have this = np	*/
extern struct argent *orgnp;	/* used by top level to reset to start  */
extern struct nament *orgbnp;	/* used by top level to reset to start  */
extern struct nament *bnplim;		/* limit of bindstack   */
extern struct argent	*np,			/* top entry on stack	*/
		*lbot,			/* bottom of cur frame	*/
		*namptr;		/* temporary pointer	*/
extern lispval sigacts[16];
extern lispval hunk_pages[7], hunk_items[7], hunk_name[7];

extern lispval Vprintsym;

#define TNP	if(np >= nplim) namerr();

#define TNP	if(np >= nplim) namerr();
#define INRNP	if (np++ >= nplim) namerr();
#define protect(p) (np++->val = (p))
#define chkarg(p,x); if((p)!=np-lbot) argerr(x);


/** status codes **********************************************/
/*							      */
/* these define how status and sstatus should service probes  */
/* into the lisp data base				      */

/* common status codes */
#define ST_NO 0

/* status codes */
#define ST_READ 1
#define ST_FEATR 2
#define ST_SYNT 3
#define ST_RINTB 4
#define ST_NFETR 5
#define ST_DMPR  6
#define ST_CTIM  7
#define ST_LOCT  8
#define ST_ISTTY 9
#define ST_UNDEF 10

/* sstatus codes */
#define ST_SET 1
#define ST_FEATW 2
#define ST_TOLC 3
#define ST_CORE 4
#define ST_INTB 5
#define ST_NFETW 6
#define ST_DMPW  7
#define ST_AUTR 8
#define ST_TRAN 9
#define ST_BCDTR 10
#define ST_GCSTR 11


/* number of counters for fasl to use in a profiling lisp  */
#define NMCOUNT 5000

/* hashing things *******************************************************/
#define	HASHTOP	1024	/*  we handle 8-bit characters by dropping top bit  */
extern struct	atom	*hasht[HASHTOP];
extern int	hash;		/* set by ratom		*/
extern int	atmlen;		/* length of atom including final null	*/


/** exception handling ***********************************************/
extern int exception;	/* if TRUE then an exception is pending, one of */
			/* the below 				        */
extern int sigintcnt;   /* if > 0 then there is a SIGINT pending	*/

/* big string buffer for whomever needs it ******************************/
extern char	*strbuf;
extern char	*endstrb;

/* break and error declarations *****************************************/
#define	SAVSIZE	44		/* number of bytes saved by setexit	*/
#define	BRRETB	1
#define BRCONT	2
#define	BRGOTO	3
#define	BRRETN	4
#define INTERRUPT 5
#define THROW	6
extern int	depth;		/* depth of nested breaks		*/
extern lispval	contval;	/* the value being returned up		*/
extern int	retval;		/* used by each error/prog call		*/
extern lispval  lispretval;	/* used by non-local go			*/
extern int	rsetsw;		/* used by *rset mode			*/
extern int	evalhcallsw;	/* used by evalhook			*/
extern int	funhcallsw;	/* used by evalhook			*/


/* other stuff **********************************************************/
extern lispval	ftemp,vtemp,argptr,ttemp;	/* temporaries: use briefly  */
extern int itemp;
					/* for pointer type conversion  */
#include	"dfuncs.h"

#define	NUMBERP	2
#define	BCDP	5
#define	PORTP	6
#define ARRAYP	7

#define	ABSVAL	0
#define	MINUS	1
#define	ADD1	2
#define	SUB1	3
#define	NOT	4
#define	LNILL	5
#define	ZEROP	6
#define	ONEP	7
#define	PLUS	8
#define	TIMES	9
#define	DIFFERENCE	10
#define	QUOTIENT	11
#define	MOD	12
#define	LESSP	13
#define	GREATERP	14
#define	SUM	15
#define	PRODUCT	16
#define	AND	17
#define	OR	18
#define	XOR	19

interpt();
handler();  extern sigdelay, sigstruck;

/* limit of valid data area **************************************/

extern lispval datalim;

/** macros to push and pop the value of an atom on the stack ******/

#define PUSHDOWN(atom,value)\
	{bnp->atm=(atom);bnp++->val=(atom)->a.clb;(atom)->a.clb=value;\
	if(bnp>bnplim) binderr();}

#define POP\
	{--bnp;bnp->atm->a.clb=bnp->val;}

/* PUSHVAL  is used to store a specific atom and value on the
 * bindstack.   Currently only used by closure code
 */  
#define PUSHVAL(atom,value)\
	{bnp->atm=(atom);bnp++->val=value;\
	if(bnp>bnplim) binderr();}

/** macro for evaluating atoms in eval and interpreter  ***********/

#define EVALATOM(x)	vtemp = x->a.clb;\
			if( vtemp == CNIL ) {\
				printf("%s: ",(x)->a.pname);\
				vtemp = error("UNBOUND VARIABLE",TRUE);}

/*  having to do with small integers					*/
extern long Fixzero[];
#define SMALL(i)	((lispval)(Fixzero + i))
#define okport(arg,default) (vtemp = arg,((TYPE((vtemp))!=PORT)?default:(vtemp)->p))

extern lispval ioname[];	/* names of open files */
/*  interpreter globals   */

extern int lctrace;

/* register lisp macros for registers */

#define saveonly(n)	asm("#save	n")
#define snpand(n)	asm("#protect	n")
