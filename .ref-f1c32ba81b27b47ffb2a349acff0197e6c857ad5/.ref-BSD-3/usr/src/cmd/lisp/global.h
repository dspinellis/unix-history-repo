#include <stdio.h>

#define AD 0

#define	peekc(p)	(p->_cnt>0? *(p)->_ptr&0377:_filbuf(p)==-1?-1:((p)->_cnt++,*--(p)->_ptr&0377))

/**********************************************************************/
/*                                                                    */
/*   file: global.h						      */
/*   contents:							      */
/* GLOBAL STUFF *******************************************************/


#define FALSE	0
#define	TRUE	1
#define EVER	;;
#include <pagsiz.h>
#define	CNIL	((lispval) -4)
#define nil	((lispval) 0)
#define eofa	((lispval) 20)
#define NOTNIL(a)	((int)a)
#define ISNIL(a)	(!(int)a)
#define STRBLEN 256


#define	NULL_CHAR	0
#define	LF	'\n'
#define	WILDCHR	'\0177'


/* type flags and the macros to get them ********************************/

#define	UNBO	-1
#define	STRNG	0
#define	ATOM	1
#define	INT	2
#define	DTPR	3
#define DOUB	4
#define	BCD	5
#define	PORT	6
#define	ARRAY	7
#define SDOT	9
#define VALUE	10

/* the numbers per page of the different data objects *******************/
#define ATOMSPP 25
#define STRSPP NBPG
#define INTSPP 128
#define DTPRSPP 64
#define DOUBSPP 64
#define ARRAYSPP 25
#define SDOTSPP 64
#define VALSPP 128
#define BCDSPP 64

extern char typetab[];  /*  the table with types for each page  */

#define TTSIZE 4096
			/*  Absolute limit, in pages of the
			 size to which the lisp system may grow.
			 If you change this, you must recompile
			 alloc.c and data.c */

#define TYPL(a1)	((typetab+1)[(int)(a1) >> 9])
#define SETTYPE(a1,b)   {if((itemp = ((int)a1) >> 9) > TTSIZE) badmem();\
			(typetab + 1)[itemp] = (b); }

#define	TYPE(a1)	((typetab+1)[(int)(a1) >> 9])

#define VALID(a)	(a >= CNIL && a < datalim)

/* some types ***********************************************************/
#define lispint long
#define MAX10LNG 200000000		/* max long divided by 10	*/


typedef union lispobj *lispval ;
struct dtpr {lispval	cdr,
			car;};
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
	lispval (*entry)();	/*  entry point to routine  */
	lispval	discipline,	/*  argument-passing discipline  */
		language,	/*  language coded in	*/
		params,		/*  parameter list if relevant  */
		loctab;		/*  local table  */
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
};


#include "sigtab.h"   /* table of all pointers to lisp data */

/* Port definitions *****************************************************/
extern FILE	*piport,		/* standard input port		*/
	*poport,		/* standard output port		*/
	*errport,		/* port for error messages	*/
	*rdrport;		/* temporary port for readr	*/
extern FILE *xports[];		/* page of file *'s for lisp	*/
extern int lineleng ;		/* line length desired		*/
extern char rbktf;		/* logical flag: ] mode		*/
extern char *ctable;		/* Character table in current use */
#define Xdqc ctable[131]
#define Xesc ctable[130]
#define Xsdc ctable[129]

/* name stack ***********************************************************/

#ifdef ROWAN
#define NAMESIZE 4096
#else
#define NAMESIZE 1024
#endif

extern struct	nament	{
	lispval	val,
		atm;
}	*bnp,			/* first free bind entry*/
	*bnplim;		/* limit of bindstack   */

extern struct argent {
	lispval	val;
}	*np,			/* top entry on stack	*/
	*lbot,			/* bottom of cur frame	*/
	*namptr,		/* temporary pointer	*/
	*nplim;		 	/* don't have this = np	*/

#define TNP	if(np >= nplim) namerr();
#define INRNP	if (np++ >= nplim) namerr();

#define INCNP(x,y)	{np[1].atm = (lispval)(x); np[1].val = (lispval)(y);\
			if(np++ >=  nplim) namerr();}


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

/* sstatus codes */
#define ST_SET 1
#define ST_FEATW 2
#define ST_TOLC 3
#define ST_CORE 4
#define ST_INTB 5
#define ST_NFETW 6
#define ST_DMPW  7



/* hashing things *******************************************************/
#define	HASHTOP	128	/*  we handle 8-bit characters by dropping top bit  */
extern struct	atom	*hasht[HASHTOP];
extern int	hash;		/* set by ratom		*/
extern int	atmlen;		/* length of atom including final null	*/


/* big string buffer for whomever needs it ******************************/
extern char	strbuf[STRBLEN];
extern char	*endstrb;

/* break and error declarations *****************************************/
#define	SAVSIZE	40		/* number of bytes saved by setexit	*/
#define	BRRETB	1
#define BRCONT	2
#define	BRGOTO	3
#define	BRRETN	4
#define INTERRUPT 5
#define THROW	6
extern int	depth;		/* depth of nested breaks		*/
extern lispval	contval;	/* the value being returned up		*/
extern int	retval;		/* used by each error/prog call		*/
extern struct argent *orgnp;	/* used by top level to reset to start  */
extern struct nament *orgbnp;	/* used by top level to reset to start  */


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
handler(); extern lispval sigacts[16]; extern sigdelay, sigstruck;

/* limit of valid data area **************************************/

extern lispval datalim;

/** macros to push and pop the value of an atom on the stack ******/

#define PUSHDOWN(atom,value)\
	{bnp->atm=atom;bnp++->val=atom->clb;atom->clb=value;if(bnp>bnplim) binderr();}

#define POP\
	{--bnp;bnp->atm->clb=bnp->val;}

/** macro for evaluating atoms in eval and interpreter  ***********/

#define EVALATOM(x)	vtemp = x->clb;\
			if( vtemp == CNIL ) {\
				printf("%s: ",(x)->pname);\
				vtemp = error("UNBOUND VARIABLE",TRUE);}

/*  having to do with small integers					*/

#define SMALL(i)	((lispval)(1024 + (i<<2)))
#define P(p)		((lispval) (xports +((p)-_iob)))

/*  interpreter globals   */

extern int lctrace;

/* register lisp macros for registers */
#define saveonly(n)	asm("#save	n");
#define snpand(n)	asm("#protect	n");
