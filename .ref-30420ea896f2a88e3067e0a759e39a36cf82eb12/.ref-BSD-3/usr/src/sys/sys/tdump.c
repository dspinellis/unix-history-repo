/*	tdump.c	2.1	1/5/80	*/

#include "../h/param.h"
#include "../h/vm.h"
#include "../h/pte.h"
#include "../h/systm.h"
#include "../h/cmap.h"

/*
 * Dump core to magtape.
 * Assumes memory mapping has been disabled
 * and IPL has been set high ( > 0x15 )
 */

#define	UBA	0x20006000
#define	mba0	0x20010000
#define	mba1	0x20012000

struct mba_regs {
	int	mba_csr;
	int	mba_cr;
	int	mba_sr;
	int	mba_var;
	int	mba_bcr;
};

struct device
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
#define	HTMAP ((int *) (mba1 + 0x800))

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
#define	P800	01300		/* 800 + pdp11 mode */
#define	P1600	02300		/* 1600 + pdp11 mode */
#define	IENABLE	0100
#define	RDY	0200
#define	TM	04
#define	DRY	0200
#define	EOT	02000
#define	CS	02000
#define	COR	0100000
#define	PES	040
#define	WRL	04000
#define	MOL	010000
#define	ERR	040000
#define	FCE	01000
#define	TRE	040000
#define	HARD	064023	/* UNS|OPI|NEF|FMT|RMR|ILR|ILF */

#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define	SRETRY	4
#define	SCOM	5
#define	SOK	6

dump()
{

	HTADDR->httc = P800;	/* set 800 bpi mode */

	twall((char *)0, maxfree);	/* write out memory */

	teof();
	teof();
	rewind();
	twait();
}

twall(start, num)
	char *start;
	int num;
{
	HTADDR->htcs1 = DCLR | GO;
	while (num > 0) {
		twrite(start);
		start += BSIZE;
		num -= (BSIZE / NBPG);
	}
}

twrite(buf)
char *buf;
{

	twait();
	HTADDR->htfc = -BSIZE;
	*HTMAP = btop(buf) | PG_V;
	*(HTMAP+1) = (btop(buf)+1) | PG_V;
	((struct mba_regs *)mba1)->mba_sr = -1;
	((struct mba_regs *)mba1)->mba_bcr = -BSIZE;
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
