#include <stdio.h>


#define	FATAL	1
#define	ROM	'1'
#define	ITAL	'2'
#define	BLD	'3'

#define	VERT(n)	((((n)+(minvert>>1))/minvert)*minvert)
#define	EFFPS(p)	((p) >= minsize ? (p) : minsize)
#define	POINT	72
#define	EM(m, ps)	(int)((((float)(m)*(ps) * res) / POINT))

extern int	dbg;
extern int	ct;
extern int	lp[];
extern int	used[];	/* available registers */
extern int	ps;	/* dflt init pt size */
extern int	deltaps;	/* default change in ps */
extern int	gsize;	/* global size */
extern int	gfont;	/* global font */
extern int	ft;	/* dflt font */

extern char	*device;	/* name of output device */
extern int	res;	/* resolution of output device */
extern int	minsize;	/* min size it can print */
extern int	minvert;	/* min size it can move vertically */

extern FILE	*curfile;	/* current input file */
extern int	ifile;	/* input file number */
extern int	linect;	/* line number in current file */
extern int	eqline;	/* line where eqn started */
extern int	svargc;
extern char	**svargv;
extern int	eht[];
extern int	ebase[];
extern int	lfont[];
extern int	rfont[];
extern int	yyval;
extern int	*yypv;
extern int	yylval;
extern int	eqnreg, eqnht;
extern int	lefteq, righteq;
extern int	lastchar;	/* last character read by lex */
extern int	markline;	/* 1 if this EQ/EN contains mark or lineup */

typedef struct s_tbl {
	char	*name;
	char	*defn;
	struct s_tbl *next;
} tbl;

extern	char	*spaceval;	/* use in place of normal \x (for pic) */
