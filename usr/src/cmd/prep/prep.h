# define NUMC 128
# define NUMS 3
# define	NUMA	8
# define PTRI 1600
# define CHARI 16000
# define OUT	3

# define SKIP	0
# define COLLECT	1
# define SKIP2	2
# define WIDTH 6
struct	{
		char	*beg;
		int	ct;
		long	wdno;
	} temp[30];

int	lflag;
int	puncfl;
int	hsw;
int	san;
int	t1;



char	tab[NUMS][NUMC];

int	coll();
int	save();
int	hyphen();
int	hyp1();
int	hyp2();
int	error();
int	ctout();
int	bsp();
int	bsp1();
int	gobble2();
int	bslash();
int	punc();

char	line[300];
int	l;
int	lno;
int	c;

int	only;
int	cs;
int	(*flag[8])();
int	fl;
int	wdflg;
long	wdnum;
char	num[WIDTH + 1];
int	igflg;

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



FILE	*fi;
