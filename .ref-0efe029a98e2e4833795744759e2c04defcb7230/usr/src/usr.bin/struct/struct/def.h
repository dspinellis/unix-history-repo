/*	def.h	4.2	83/08/11	*/

#define ASSERT(P,R)	{if (!(P)) {fprintf(stderr,"failed assertion in routine R: P\n"); abort();}}

extern int routnum, routerr;
extern long rtnbeg;		/* number of chars up to beginnine of curernt routing */
extern int **graph, nodenum;
extern int stopflg;		/* turns off generation of stop statements */

#define TRUE 1
#define FALSE 0
#define LOGICAL int
#define VERT int
#define DEFINED(v)	(v >= 0)
#define UNDEFINED	-1

/* node types */
#define STLNVX		0
#define IFVX		1
#define DOVX		2
#define IOVX		3
#define FMTVX		4
#define COMPVX		5
#define ASVX		6
#define ASGOVX		7
#define LOOPVX		8
#define WHIVX		9
#define UNTVX		10
#define ITERVX		11
#define THENVX		12
#define STOPVX		13
#define RETVX		14
#define DUMVX		15
#define GOVX		16
#define BRKVX		17
#define NXTVX		18
#define SWCHVX		19
#define ACASVX		20
#define ICASVX		21

#define TYPENUM	22


extern int hascom[TYPENUM];		/* FALSE for types with no comments, 2 otherwise */
extern int nonarcs[TYPENUM];		/* number of wds per node other than arcs */
extern VERT *arc(), *lchild();
extern int *vxpart(), *negpart(), *predic(), *expres(), *level(), *stlfmt();
/* node parts */
#define FIXED 4		/* number of wds needed in every node */
#define NTYPE(v)	graph[v][0]
#define BEGCOM(v)	graph[v][1]
#define RSIB(v)	graph[v][2]
#define REACH(v)	graph[v][3]
#define LCHILD(v,i)	*lchild(v,i)
#define CHILDNUM(v)	childper[NTYPE(v)]
#define ARC(v,i)	*arc(v,i)
#define ARCNUM(v)	*((arcsper[NTYPE(v)] >= 0) ? &arcsper[NTYPE(v)]: &graph[v][-arcsper[NTYPE(v)]])

/* STLNVX, FMTVX parts */
#define BEGCODE(v)	*stlfmt(v,0)		/* 1st char of line on disk or address of string */
#define ONDISK(v)	*stlfmt(v,1)		/* FALSE if in core,# of lines on disk otherwise */
#define CODELINES(v)		*vxpart(v,STLNVX,2)		/* # of statements stored in node */

/* IOVX parts */
#define FMTREF(v)	*vxpart(v,IOVX,0)	/* FMTVX associated with i/o statememt */
#define PRERW(v)	*vxpart(v,IOVX,1)	/* string occurring in i/o statement before parts with labels */
#define POSTRW(v)	*vxpart(v,IOVX,2)	/* string occurring in i/o statement after parts wih labels */
#define ENDEQ	1		/* arc number associated with endeq */
#define ERREQ	2		/* arc number associated wth erreq */

/* ITERVX parts */
#define NXT(v)	*vxpart(v,ITERVX,0)		/* THENVX containing condition for iteration for WHILE or UNTIL */
#define FATH(v) *vxpart(v,ITERVX,1)		/* father of v */
#define LPRED(v) *vxpart(v,ITERVX,2)		/* loop predicate for WHILE, UNTIL */

/*DOVX parts */
#define INC(v)	*vxpart(v,DOVX,0)		/* string for iteration condition of DO */

/* IFVX,THENVX parts */
#define PRED(v)		*predic(v)	/* string containing predicate */
#define NEG(v)			*negpart(v)		/* TRUE if predicate negated */
#define THEN	0		/* arc number of true branch */
#define ELSE 1		/* arc number of false branch */

/* miscellaneous parts */
#define EXP(v)	*expres(v)		/* expression - ASVX, COMPVX, ASGOVX, SWCHVX, ICASVX */
#define LABREF(v)	*vxpart(v,ASVX,1)		/* node referred to by label in ASSIGN statement */


/* BRKVX, NXTVX parts */
#define LEVEL(v)	*level(v)

/* also COMPVX, ASGOVX, SWCHVX, and DUMVX contain wd for number of arcs */
/* location of this wd specified by negative entry in arcsper */
extern int arcsper[TYPENUM];

/* also nodes contain wds for children as specified by childper */
extern childper[TYPENUM];


/* switches */
extern int intcase, arbcase, whiloop, invelse, exitsize, maxnode,
	maxhash, progress, labinit, labinc, inputform, debug,levbrk,levnxt,mkunt;

/* arrays */
extern int *after;
extern char *typename[];

struct list {
	VERT elt;
	struct list *nxtlist;
	};
struct list *append(), *consl();
extern VERT retvert, stopvert;	/* specifies unique return and stop vertices */
extern VERT START;
extern int progtype;		/* type of program - main or sub or blockdata */
#define sub	1
#define blockdata	2

extern FILE *infd, *debfd, *outfd;
