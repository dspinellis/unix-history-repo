/*-
 * %sccs.include.proprietary.c%
 *
 *	@(#)1.incl.h	4.3 (Berkeley) %G%
 */

#define maxlsw		10	/* max number of switches and labels per statement */
#define implicit 0L		/* "label" of following line so all flow can be treated as jump to label */
struct lablist	{long labelt;  struct lablist *nxtlab; };
extern struct lablist *endlab, *errlab, *reflab, *linelabs, *newlab;
extern long label();

extern int routbeg;			/* line number of first line of routine */
extern int nameline;			/* line number of function/subroutine st., if any */
extern int stflag;		/* determines whether at beginning or middle of block of straight line code */



extern char buffer[];
extern int endbuf;

extern int   nlabs, lswnum, swptr, flag,
	 counter, p1, p3, begline, endline, r1,r2, endcom;
extern long begchar, endchar, comchar;


/* statement types not associated with actual node types */
#define contst		-1
#define ungo		-2
#define arithif		-3
#define readst		-8
#define writest		-9
#define entry	-10


extern char *pred, *inc, *prerw, *postrw, *exp, *stcode;

#define maxdo	20	/* max nesting of do loops */
extern long dostack[maxdo];		/* labels of do nodes */
extern int doloc[maxdo];		/* loc of do node */
extern int doptr;


extern struct list *FMTLST;		/* list of FMTVX's generated */
extern struct list *ENTLST;		/* list of STLNVX nodes corresponding to entry statements */
