/* pic.h	(Berkeley)	1.2	83/08/09	*/
#define	dprintf	if(dbg)printf

#define	HEAD1	1
#define	HEAD2	2
#define	HEAD12	(HEAD1+HEAD2)
#define	INVIS	4
#define	CW_ARC	8	/* clockwise arc */
#define	PI	3.141592654
#define	PI2	PI/2
#define	SCALE	1.0	/* default scale: units/inch */
#define	WID	0.75	/* default width for boxes and ellipses */
#define	WID2	0.375
#define	HT	0.5	/* default height and line length */
#define	HT2	0.25	/* because no floating init exprs! */
#define	HT5	0.1
#define	HT10	0.05

#define	MAXOBJ	1200
#define	MAXTEXT	1200
#define	SYMTAB	200

/* these have to be like so, so that we can write */
/* things like R & V, etc. */
#define	H	0
#define	V	1
#define	R_DIR	0
#define	U_DIR	1
#define	L_DIR	2
#define	D_DIR	3
#define	ishor(n)	(((n) & V) == 0)
#define	isvert(n)	(((n) & V) != 0)
#define	isright(n)	((n) == R_DIR)
#define	isleft(n)	((n) == L_DIR)
#define	isdown(n)	((n) == D_DIR)
#define	isup(n)		((n) == U_DIR)

typedef union {		/* the yacc stack type */
	int	i;
	char	*p;
	struct obj *o;
	float	f;
} YYSTYPE;

extern	YYSTYPE	yylval, yyval;

struct attr {	/* attribute of an object */
	int	a_type;
	YYSTYPE	a_val;
};

struct obj {	/* stores various things in variable length */
	int	o_type;
	int	o_count;	/* number of things */
	int	o_nobj;		/* index in objlist */
	int	o_mode;		/* hor or vert */
	float	o_x;	/* coord of "center" */
	float	o_y;
	int	o_nt1;	/* 1st index in text[] for this object */
	int	o_nt2;	/* 2nd; difference is #text strings */
	int	o_attr;	/* various attributes of interest */
	int	o_dotdash;	/* kludge in a dot/dash mode */
	float	o_ddval;	/* value of dot/dash expression */
	float	o_val[1];	/* actually this will be > 1 in general */
				/* type is not always FLOAT!!!! */
};

struct symtab {
	char	*s_name;
	int	s_type;
	YYSTYPE	s_val;
	struct symtab *s_next;
};

struct text {
	int	t_type;
	char	*t_val;
};

extern	int	dbg;
extern	struct	obj	*objlist[];
extern	int	nobj;
extern	struct	attr	attr[];
extern	int	nattr;
extern	struct	text	text[];
extern	int	ntext;
extern	int	ntext1;
extern	float	curx, cury;
extern	int	hvmode;
extern	int	codegen;
extern	char	*malloc(), *tostring();
extern	float	getfval(), getcomp();
extern	YYSTYPE	getvar();
extern	struct symtab *lookup(), *makevar();

extern	float	deltx, delty;
extern	int	lineno;
extern	int	synerr;
extern	int	crop;
extern	int	res, DX, DY;

extern	float	sxmin, sxmax, symin, symax;
extern	float	xmin, ymin, xmax, ymax;
extern	struct obj *leftthing(), *boxgen(), *circgen(), *arcgen();
extern	struct obj *linegen(), *splinegen(), *movegen(), *textgen();
extern	struct obj *troffgen(), *rightthing(), *blockgen();
extern	struct	obj *makenode(), *makepos(), *fixpos(), *makebetween();
extern	struct	obj *getpos(), *gethere(), *getfirst(), *getlast(), *getblock();

struct pushstack {
	float	p_x;
	float	p_y;
	int	p_hvmode;
	float	p_xmin;
	float	p_ymin;
	float	p_xmax;
	float	p_ymax;
	struct symtab *p_symtab;
};
extern	struct pushstack stack[];
extern	int	nstack;
extern int cw;

extern float atof();
