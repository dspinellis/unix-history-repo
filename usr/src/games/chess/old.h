#define	uleft	04040
#define	uright	04004
#define	dleft	00440
#define	dright	00404
#define	left	00040
#define	right	00004
#define	up	04000
#define	down	00400
#define	u2r1	06004
#define	u1r2	04006
#define	d1r2	00406
#define	d2r1	00604
#define	d2l1	00640
#define	d1l2	00460
#define	u1l2	04060
#define	u2l1	06040
#define	rank2	00200
#define	rank7	02000

int	attacv[64];
int	center[64];
int	wheur[];
int	bheur[];
int	control[64];
int	clktim[2];
int	testf;
int	qdepth;
int	mdepth;
int	bookf;
int	bookp;
int	manflg;
int	matflg;
int	intrp;
int	moveno;
int	gval;
int	game;
int	abmove;
int	*lmp;
int	*amp;
char	*sbufp;
int	lastmov;
int	mantom;
int	ply;
int	value;
int	ivalue;
int	mfmt;
int	depth;
int	flag;
int	eppos;
int	bkpos;
int	wkpos;
int	column;
int	edge[8];
int	pval[13];
int	ipval[13];
int	dir[64];
int	board[64];
int	lmbuf[1000];
int	ambuf[1200];
char	sbuf[100];
