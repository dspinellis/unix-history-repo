#define	M	3
#define	C	100

/*
 * character Q structure
 */
struct	clist
{
	int	c_cc;
	int	c_cf;
	int	c_cl;
};

struct	chan
{
	char	cflag;
	char	m, c;
	struct	chan	*dest;
	struct	clist	ioq;
};

/* c flags */
#define	BLOCK	01

struct	line
{
	char	xbuf[24+2];
	char	rbuf[100+2];
	char	rseq;
	char	xseq;
	char	ackf;
	char	xflag;
	char	state;
	char	time;
	int	sum;
	char	*ip;
	char	*ep;
};
#define	WWAIT	02
#define	CRUN	04
#define	RWAIT	010
#define	ALLOC	020
#define	DIS	040
#define	DLY	0100

/*
 * machine structure
 */
struct	mach
{
	char	mflag;
	char	rchan;
	char	rcount;
	char	xchan;
	char	xcount;
	struct	clist	ackq;
	struct	clist	datq;
	struct	clist	disq;
	struct	chan	*chanp[128];
};

/* m flags */
#define	RNEXT	01
#define	MRUN	04
#define	XNEXT	010

/*
 * trace buffer
 */
#define	TBSIZ	100
