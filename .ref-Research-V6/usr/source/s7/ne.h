#
int	dbg;
int	ct;
int	lp[40];
char	used[100];	/* available registers */
int	ps;	/* dflt init pt size */
int	ft;	/* dflt font */
int	first;
extern	int	fout, fin;
int	ifile;
int	linect;	/* line number in file */
int	eqline;	/* line where eqn started */
int	svargc;
char	**svargv;
int	eht[100];
int	ebase[100];
int	ewid[100];
struct { char c1; char c2; };
int	yyval;
int	*yypv;
int	yylval;
int	tht[30];
int	tbase[30];
int	ptr;
char	*nptr[50];
char *sptr[50];
int	eqnreg, eqnht, eqnbase;
int	lefteq, righteq;
int	lastchar;	/* last character read by lex */
#define	FATAL	1
int	ESC;
int	HREV;
int	HFWD;
int	SI;
int	SO;
