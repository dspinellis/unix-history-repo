struct	tempent {
		char	*beg;
		int	ct;
		char	term;
	} temp[30];

char	lbuf[5];
int	cflag;
int	t1;

int	level;
int	hlevel;
int	dlevel;
int	xtrn;

int	tp[5];

struct	{
		int	cl[NUMC];
	} tab[NUMS];

int	coll();
int	save();
int	out();
int	asym();
int	asw();
int	csym();
int	csw();
int	incl();
int	decl();
int	sk2();
int	sk();
int	tabs();
int	semi();

char	line[132];
int	l;
int	lno;
char	c;
int	cursl;
char	curs[9];
int	curfl;
char	curf[10];

int	usw;
int	xsw;
int	only;
int	cross;
int	file;
int	cs;
int	(*flag[8])();
int	fl;
char	gch[8];

struct	htab	{
		int	hsiz;
		int	ssiz;
		int	nsym;
		int	curb;
		int	*hptr;
		char	*symt;
		};

struct	htab	itab;
struct	htab	xtab;

int	ipsp[PTRI];
char	issp[CHARI];
int	xpsp[PTRX];
char	xssp[CHARX];

int	ib1[259];
int	ib2[259];

char	*ibuf;
char	*ibuf1;
char	*ibuf2;

char	mone;
int	order;
int	ssw;
int	type;
char	*utmp;
