/*
 * Dump core to magtape
 * Assumes memory mapping has been disabled
 * and IPL has be set high ( > 0x15 )
 */


#define PHYSPAGES 1024
#define UBA 0x20006000
#define mba0 0x20010000
#define mba1 0x20012000

struct mba_regs {
	int mba_csr,
	    mba_cr,
	    mba_sr,
	    mba_var,
	    mba_bcr;
};

struct	device
{
	int	htcs1;
	int	htds;
	int	hter;
	int	htmr;
	int	htas;
	int	htfc;
	int	htdt;
	int	htck;
	int	htsn;
	int	httc;
};

#define	HTADDR	((struct device *)(mba1 + 0x400))
#define HTMAP ((int *) (mba1 + 0x800))

#define	GO	01
#define	WCOM	060
#define	RCOM	070
#define	NOP	0
#define	WEOF	026
#define	SFORW	030
#define	SREV	032
#define	ERASE	024
#define	REW	06
#define	DCLR	010
#define P800	01300		/* 800 + pdp11 mode */
#define	P1600	02300		/* 1600 + pdp11 mode */
#define	IENABLE	0100
#define	RDY	0200
#define	TM	04
#define	DRY	0200
#define EOT	02000
#define CS	02000
#define COR	0100000
#define PES	040
#define WRL	04000
#define MOL	010000
#define ERR	040000
#define FCE	01000
#define	TRE	040000
#define HARD	064023	/* UNS|OPI|NEF|FMT|RMR|ILR|ILF */

#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define SRETRY	4
#define SCOM	5
#define SOK	6

dump()
{

	HTADDR->httc = P800;	/* set 800 bpi mode */

	twall((char *)0, PHYSPAGES);	/* write out memory */

	teof();
	teof();
	rewind();
}

twall(start, num)
	char *start;
	int num;
{
	HTADDR->htcs1 = DCLR | GO;
	while (num--) {
		twrite(start);
		start += 512;
		}
}

twrite(buf)
char *buf;
{

	twait();
	HTADDR->htfc = -512;
	*HTMAP = (((int)buf)>>9) | 0x80000000;	/* map entry */
	((struct mba_regs *)mba1)->mba_sr = -1;
	((struct mba_regs *)mba1)->mba_bcr = -512;
	((struct mba_regs *)mba1)->mba_var = 0;
	HTADDR->htcs1 = WCOM | GO;
	return;
}

twait()
{
	register s;

	do
		s = HTADDR->htds;
	while ((s & RDY) == 0);
}

rewind()
{
	twait();
	HTADDR->htcs1 = REW | GO;
}

teof()
{
	twait();
	HTADDR->htcs1 = WEOF | GO;
}
