#define	dprintf	if(dbg)printf

#define	String	01
#define	Macro	02
#define	File	04
#define	Char	010
#define	Thru	020
#define	Free	040

#define	MARGIN	0.07	/* default margin around data */
#define	SLOP	1.001	/* slop for limits of for loops */
#define	FRAMEWID 3	/* default width for boxes and ellipses */
#define	FRAMEHT	2	/* default height and line length */
#define	TICKLEN	0.1

#define	MAXNUM	200

#define	XFLAG	01
#define	YFLAG	02

#define	INTICK	01
#define	OUTICK	02

#define	BOT	01
#define	TOP	02
#define	RIGHT	04
#define	LEFT	010

#define	RJUST	01
#define	LJUST	02
#define	ABOVE	04
#define	BELOW	010

typedef struct infile {
	FILE	*fin;
	char	*fname;
	int	lineno;
} Infile;

typedef struct {	/* input source */
	int	type;	/* Macro, String, File */
	char	*sp;	/* if String or Macro */
} Src;

extern	Src	src[], *srcp;	/* input source stack */

#define	MAXARGS	20
typedef struct {	/* argument stack */
	char	*argstk[MAXARGS];	/* pointers to args */
	char	*argval;	/* points to space containing args */
} Arg;

extern	Infile	infile[10];
extern	Infile	*curfile;

typedef struct {
	struct obj *obj;
	double	x, y;
} Point;

typedef struct attr {	/* e.g., DASH 1.1 or "..." rjust size *.5 */
	short	type;
	double	fval;
	char	*sval;
	short	just;	/* justification, for STRING type */
	short	op;	/* optional operator, ditto */
	struct attr *next;
} Attr;

typedef struct obj {	/* a name and its properties */
	char	*name;
	char	*val;	/* body of define, etc. */
	double	fval;	/* if a numeric variable */
	Point	pt;	/* usually for max and min */
	Point	pt1;
	short	type;	/* NAME, DEFNAME, ... */
	short	first;	/* 1 after 1st item seen */
	short	coord;	/* 1 if coord system specified for this name */
	short	log;	/* x, y, or z (= x+y) */
	Attr	*attr;	/* DASH, etc., for now */
	struct obj *next;
} Obj;

typedef union {		/* the yacc stack type */
	int	i;
	char	*p;
	double	f;
	Point	pt;
	Obj	*op;
	Attr	*ap;
} YYSTYPE;

extern	YYSTYPE	yylval, yyval;

extern	int	dbg;

extern	int	ntext;
extern	double	num[MAXNUM];
extern	int	nnum;
extern	int	ntick, tside;

extern	char	*calloc(), *malloc(), *realloc(), *tostring(), *grow();
extern	char	*desc_str(), *ifstat(), *delimstr();
extern	char	*xyname(), *slprint();
extern	Obj	*lookup();
extern	Obj	*copythru();
extern	Obj	*objlist;
extern	Attr	*makeattr(), *makefattr(), *makesattr(), *addattr();
extern	Point	makepoint();
extern	double	setvar(), getvar();

extern	int	lineno;
extern	int	synerr;
extern	int	codegen;
extern	char	*tempfile;
extern	FILE	*tfd;

extern	Point	ptmin, ptmax;

extern	char	*dflt_coord;
extern	char	*curr_coord;
extern	int	ncoord;
extern	int	auto_x;
extern	double	margin;
extern	int	autoticks;
extern	int	pointsize, ps_set;

extern	char	*sizeit(), *juststr();
extern	double	log10(), log(), exp(), sin(), cos(), sqrt(), atof(), errcheck();
extern	double	fabs(), floor(), ceil(), moddouble(), modceil();

#define	logit(x) (x) = log10(x)
#define	Log10(x) errcheck(log10(x), "log")
#define	Exp(x)	errcheck(exp(x), "exp")
#define	Sqrt(x)	errcheck(sqrt(x), "sqrt")

#define	min(x,y)	(((x) <= (y)) ? (x) : (y))
#define	max(x,y)	(((x) >= (y)) ? (x) : (y))
