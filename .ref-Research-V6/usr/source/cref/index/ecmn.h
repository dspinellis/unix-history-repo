int	count;
int	page;
struct	{
		char	*beg;
		int	ct;
		char	term;
	} temp[30];

int	pn;
int	word;
int	hsw;
int	t1;

int	level;
int	hlevel;
int	dlevel;
int	xtrn;

int	tp[2];

struct	{
		int	cl[NUMC];
	} tab[NUMS];

int	coll();
int	save();
int	out();
int	hyphen();
int	hyp1();
int	hyp2();
int	pno();
int	error();

char	line[300];
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

int	ipsp[PTRI];
char	issp[CHARI];

int	ib1[259];

char	*ibuf;
char	*ibuf1;

char	mone;
int	order;
int	ssw;
int	type;
char	*utmp;
