#include <stdio.h>

#define	dprintf		if (dbg) printf
#define	max(x,y)	(((x) >= (y)) ? (x) : (y))

#define	FATAL	1
#define	ROM	'1'
#define	ITAL	'2'
#define	BLD	'3'

#define	DEFGAP	-999	/* default gap in piles */

extern int	dbg;
extern int	ct;
extern int	lp[];
extern int	used[];		/* available registers */
extern int	ps;		/* dflt init pt size */
extern int	deltaps;	/* default change in ps */
extern int	dps_set;	/* 1 => -p option used */
extern int	gsize;		/* global size */
extern int	ft;		/* default font */
extern int	display;	/* 1 => inline, 0 => .EQ/.EN */
extern int	synerr;		/* 1 if syntax error in this eqn */

extern char	*typesetter;	/* typesetter name for -T... */
extern int	minsize;	/* min size it can print */
extern int	ttype;		/* actual type of typesetter: */

#define	DEVCAT	1
#define	DEV202	2
#define	DEVAPS	3
#define DEVHAR	4
#define DEVVER	5
#define DEVPSC	6

extern float	eht[];
extern float	ebase[];
extern int	eps[];
extern int	lfont[];
extern int	rfont[];
extern int	yyval;
extern int	yylval;
extern int	eqnreg;
extern float	eqnht;
extern int	lefteq, righteq;
extern int	markline;	/* 1 if this EQ/EN contains mark or lineup */

typedef struct s_tbl {
	char	*name;
	char	*defn;
	struct s_tbl *next;
} tbl;

extern	char	*spaceval;	/* use in place of normal \x (for pic) */

#define	String	01
#define	Macro	02
#define	File	04
#define	Char	010
#define	Free	040

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

typedef struct {	/* font number and name */
	int	ft;
	char	name[10];
} Font;

extern	Font	ftstack[];
extern	Font	*ftp;

extern	int	szstack[];
extern	int	nszstack;

extern	Infile	infile[10];
extern	Infile	*curfile;
extern	char	*grow(), *malloc(), *realloc(), *strsave();
extern	char	*DPS(), *ABSPS();
extern	double	EM(), REL();
extern	tbl	*lookup(), *keytbl[], *deftbl[], *restbl[];
