/*	ideal.h	(CWI)	1.1	85/03/01	*/
#include <stdio.h>
#include "stdas.h"
#include <math.h>

extern double modf();

#define TRUE	1
#define FALSE	0
typedef int	boolean;

#define EPSILON 0.0001
#define	PI	3.1415926535
#define	INFINITY	1e30
#define INTERSIZE	20
#define	POSSINTER	2

#define known(x) (!(((DEPPTR)x->left)->var || ((DEPPTR)x->right)->var))
#define fabs(z)	((z>0)?z:-(z))
#define	iabs(z)	((z>0)?z:-(z))
#define max(x,y) (((x)>(y))?(x):(y))
#define min(x,y) (((x)<(y))?(x):(y))
#define Re(z)	((DEPPTR)z->left)->coeff
#define Im(z)	((DEPPTR)z->right)->coeff
#define	ISREAL(z)	(z->re_name > 0)
#define	THENAME(z)	(iabs(z->re_name))
#define	arecollinear(a,b,c,d,e,f)	(fabs((f-b)*(c-a)-(e-a)*(d-b))<EPSILON)
#define	between(ax,ay,bx,by,cx,cy)	((ax-bx)*(bx-cx) > 0 || (ay-by)*(by-cy) > 0)

extern int when_bug;
extern boolean dbg;
#define bug_on	dbg = TRUE
#define bug_off	dbg = FALSE
#define dprintf	if (dbg) fprintf(stderr,

extern char *filename;
extern int lineno;
#define	LIBFIL	1
#define	SILENT	2
#define	CHATTY	3

extern boolean radflag;
extern boolean wantout;
#define dtor(x)	x *= PI/180
#define rtod(x)	x *= 180/PI

/* these are codes for classification of intersection points */
#define	UNUSED		0
#define	SIMPLE		1
#define	AT0		2
#define	AT1		3
#define	COLLINEAR	4
#define	ON0		5
#define	ON1		6
#define	TANGENT		7

/* these are codes for setting up the list of intersections */
#define	INHERIT		0
/*
#define	SIMPLE		1
*/
#define	EXTREMUM	2
#define	INFLECTION	3
#define	EXT0		4
#define	EXT1		5
#define	INFL0		6
#define	INFL1		7
#define	IGNORE		8

#define	INBEGIN		0
#define	OUTBEGIN	1
#define	ONBEGIN		2

/* structure definitions for data structures */
typedef char *EXPR;
#define	tryfree(doomed)	free((char *)doomed)

typedef struct stmtnode {	/* hooks together stmts in bodies */
	struct stmtnode *next;
	EXPR stmt;
	int kind;
	} STMTNODE, *STMTPTR;

typedef struct boxnode {	/* hooks together box definitions */
	struct boxnode *next;
	int name;
	STMTPTR stmtlist;
	} BOXNODE, *BOXPTR;

typedef struct namenode {	/* holds var lists and path names */
	struct namenode *next;
	int name;
	} NAMENODE, *NAMEPTR;

typedef struct exprnode {	/* points to equations and bdlists */
	struct exprnode *next;
	EXPR expr;
	} EXPRNODE, *EXPRPTR;

typedef struct putnode {	/* put statements */
	int name;
	BOXPTR parm;
	int p_or_c;
	} PUTNODE, *PUTPTR;

typedef struct pen_node {	/* conn ... using statements */
	EXPR from,
		to,
		copies,
		start,
		end;
	BOXPTR pen;
	} PEN_NODE, *PENPTR;

typedef struct miscnode {	/* opaque, draw handling */
	int info;
	} MISCNODE, *MISCPTR;

typedef struct strnode {	/* strings */
	int command;
	char *string;
	EXPR at;
	} STRNODE, *STRPTR;

typedef struct exprintl {	/* internal node of expr tree */
	boolean leaf;	/* always FALSE */
	int oper;
	EXPR left;
	EXPR right;
	} EXPRINTL, *INTLPTR;

typedef struct exprextl {	/* external node of expr tree */
	boolean leaf;	/* always TRUE */
	union u {
		struct namenode *path;
		float const;
	} info;
	int kind;	/* should be one of PATH, CONST */
	} EXPREXTL, *EXTLPTR;

typedef struct noad {	/* linked structures in which variables reside */
	PUTPTR defnode;
	struct varnode *edgevarlist;
	struct varnode *boxvarlist;
	struct linenode *linelist;
	struct noad *father;
	struct noad *brother;
	struct noad *son;
	} NOAD, *NOADPTR;

typedef struct varnode {	/* where ONE variable--the real or imag part of a var--lives */
	struct varnode *next;
	int re_name;	/* positive for real part, negative for imag part */
	struct depnode *deplist;
	} VARNODE, *VARPTR;

typedef struct depnode {	/* a term in the dependency list representation of a variable */
	struct depnode *next;
	VARPTR var;
	float coeff;
	} DEPNODE, *DEPPTR;

typedef struct linenode {	/* a line segment on linelist */
	struct linenode *next;
	int kind;	/* always LINE */
	float x0,
		y0,
		x1,
		y1;
	} LINENODE, *LINEPTR;

typedef struct edgenode {	/* an edge of an opaque polygon */
	struct edgenode *next,
		*prev;
	struct arcnode *fax;
	boolean flipped;
	float sx, sy,		/* start point */
		ex, ey,		/* end point */
		stx, sty,	/* coords of endpt of a tan vector at start */
		etx, ety;	/* coords of endpt of a tan vector at end */
	int code[POSSINTER];
	float alpha[POSSINTER];
	} EDGENODE, *EDGEPTR;

typedef struct circnode {	/* a circle on linelist */
	struct linenode *next;
	int kind;	/* always CIRCLE */
	float x0,
		y0,
		r;
	} CIRCNODE, *CIRCPTR;

typedef struct arcnode {	/* an arc on linelist */
	struct linenode *next;
	int kind;	/* always ARC */
	float x0,
		y0,
		x1,
		y1,
		x2,
		y2,
		theta1,
		theta2,
		radius;	/* TROFF figures out the center depending on sign of radius */
	} ARCNODE, *ARCPTR;

typedef struct textnode {	/* a string on linelist */
	struct linenode *next;
	int kind,	/* always STRING */
		command;
	char *string;
	float x0,
		y0;
	} TEXTNODE, *TEXTPTR;

typedef struct splnode {	/* a spline on linelist */
	struct linenode *next;
	int kind;	/* always SPLINE */
	EXPRPTR knotlist;
	} SPLNODE, *SPLPTR;

typedef struct eqnnode {	/* a non-linear equation residing on list */
	struct eqnnode *next;
	EXPR eqn;
	NOADPTR noad;
	} EQNNODE, *EQNPTR;

typedef struct opqnode {	/* an alpha or theta of intersection */
	struct opqnode *next;
	int code;
	float alpha;
	} OPQNODE, *OPQPTR;

/* routines in memut.c */
extern STMTPTR stmtgen ();
extern BOXPTR boxgen ();
extern NAMEPTR namegen ();
extern EXPRPTR exprgen ();
extern PUTPTR putgen ();
extern PENPTR pengen ();
extern MISCPTR miscgen ();
extern INTLPTR intlgen ();
extern INTLPTR commagen ();
extern EXTLPTR extlgen ();
extern EXTLPTR fextlgen ();
extern NOADPTR noadgen ();
extern VARPTR vargen ();
extern DEPPTR depgen ();
extern LINEPTR linegen ();
extern EDGEPTR edgeline ();
extern LINEPTR circgen ();
extern LINEPTR arcgen ();
extern LINEPTR angularc ();
extern LINEPTR pointarc ();
extern EDGEPTR edgearc ();
extern LINEPTR textgen ();
extern LINEPTR splgen ();
extern STRPTR strgen ();
extern EQNPTR eqngen ();
extern OPQPTR opqgen ();
extern void nextfree ();
extern void depfree ();
extern void namefree ();
extern void exprlsfree ();
extern void linefree ();
extern void intlfree ();
extern void noadfree ();
extern void varfree ();
extern void exprfree ();
extern void boxfree ();
extern void emergency ();

/* routines in util.c */
extern int lookup();
extern char* idprint();
extern BOXPTR findbox();
extern INTLPTR varfind();
extern INTLPTR pathfind();
extern BOXPTR tail ();
extern void forget ();
extern void exprprint ();
extern STMTPTR nextstmt ();
extern EXPR bracket ();
extern void depprint ();
extern void dexch();
extern void fexch();
extern float rprin();
extern float dprin();
extern void angorder();
extern STMTPTR reverse ();
extern void impossible ();

/* routines in bldds.c */
extern NOADPTR buildnoadtree();
extern VARPTR buildvarlist();
extern NOADPTR walkputlist();

/* routines in simul.c */
extern DEPPTR depadd();
extern DEPPTR depsubst();

/* routines in exprn.c */
extern INTLPTR expreval();
extern void eqndo();
extern void depvarclean();
extern void depvarkill();
extern void eqneval();
extern void nl_eval();

/* routines in action.c */
extern LINEPTR build();
extern LINEPTR connact();
extern LINEPTR penact();
extern LINEPTR drawact();
extern LINEPTR stract();
extern LINEPTR circact();
extern LINEPTR arcact();
extern LINEPTR splact();

/* routines in piece.c */
extern void linecall ();
extern void circcall ();
extern void arccall ();
extern void textcall ();
extern void splcall ();
extern void boundscall ();

/* routines in opaque.c */
extern LINEPTR opqact();
extern void opqinsert();
extern LINEPTR lineclean();
extern void halfplane();
extern void triangle();

/* routines in inter.c */
extern boolean llinter();
extern boolean lcinter();
extern boolean ccinter();

/* routines in opqpoly.c */
extern void opqpoly();
extern void polyline();
extern void polyarc();
extern void linetest();
extern void arctest();

/* routines in opqcirc.c */
extern void opqcirc();
extern void circline();
extern void circarc();

/* routines in opqsect.c */
extern void opqsect();

/* routines in opqseg.c */
extern void opqseg();

/* lexical analyzer routines */
extern void filepush ();
extern void filepop ();
