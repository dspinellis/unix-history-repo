/*	tdump.c  3.4	10/5/80 */

#include "../ch/param.h"
#include "../ch/vm.h"
#include "../ch/pte.h"
#include "../ch/map.h"
#include "../ch/uba.h"
#include "../ch/systm.h"
#include "../ch/cmap.h"
#include "../ch/mba.h"

#ifdef	HTDUMP
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
#define	HTMAP ((struct pte *) (mba1 + 0x800))

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
#define	IENABLE 0100
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

#define	DBSIZE	20

dump()
{

	HTADDR->httc = P800;	/* set 800 bpi mode */

	htwall((char *)0, maxfree);	/* write out memory */

	hteof();
	hteof();
	htrewind();
	htwait();
}

htwall(start, num)
	char *start;
	int num;
{
	int blk;

	HTADDR->htcs1 = DCLR | GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		htwrite(start, blk);
		start += blk*NBPG;
		num -= blk;
	}
}

htwrite(buf, num)
char *buf;
{
	register struct pte *hpte = HTMAP;
	register int i;

	htwait();
	HTADDR->htfc = -(num*NBPG);
	for (i = 0; i < num; i++)
		*(int *)hpte++ = (btop(buf)+i) | PG_V;
	((struct mba_regs *)mba1)->mba_sr = -1;
	((struct mba_regs *)mba1)->mba_bcr = -(num*NBPG);
	((struct mba_regs *)mba1)->mba_var = 0;
	HTADDR->htcs1 = WCOM | GO;
}

htwait()
{
	register s;

	do
		s = HTADDR->htds;
	while ((s & RDY) == 0);
}

htrewind()
{

	htwait();
	HTADDR->htcs1 = REW | GO;
}

hteof()
{

	htwait();
	HTADDR->htcs1 = WEOF | GO;
}

#endif
#ifdef	TMDUMP

/*
 * Dump core to magtape.
 * Assumes memory mapping has been disabled
 * and IPL has been set high ( > 0x15 )
 */

#define	UBA	0x20006000
#define	UBA_DEV (UBA+0x130000-0160000)
struct device
{
	short	tmer;
	short	tmcs;
	short	tmbc;
	unsigned short tmba;
	short	tmdb;
	short	tmrd;
};

#define	TMADDR	((struct device*)(0x2013F550))

#define	GO	01
#define	RCOM	02
#define	WCOM	04
#define	WEOF	06
#define	NOP	0100
#define	SFORW	010
#define	SREV	012
#define	WIRG	014
#define	REW	016
#define	DCLR	010000
#define	D800	060000
#define D1600   0117777	
#define	IENABLE	0100

#define GAPSD	010
#define	CRDY	0200
#define	TUR	1
#define	HARD	0102200	
#define RLE	0100
#define	EOF	0040000	
#define	WL	04
#define RWS	02

#define	SSEEK	1
#define	SIO	2
#define	SCOM	3

#define	DBSIZE	20

dump()
{

	tubainit();
	tmwall(0, maxfree);		/* write out memory */

	tmeof();
	tmeof();
	tmrewind();
	tmwait();
}

tmwall(start, num)
	int start, num;
{
	int blk;

	TMADDR->tmcs = DCLR | GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		tmdwrite(start, blk);
		start += blk;
		num -= blk;
	}
}

tmdwrite(buf, num)
register buf, num;
{
	register int *io, npf;
	tmwait();
	/* Flush buffered data path 0 */
	((struct uba_regs *)UBA)->uba_dpr[1] = 0;
	((struct uba_regs *)UBA)->uba_dpr[1] = BNE;
	/* Map unibus address 0 to section of interest */
	io = (int *)((struct uba_regs *)UBA)->uba_map;
	npf = num+1;

	while(--npf != 0)
		 *io++ = (int)(buf++ | (1<<21) | MRV);
	*io = 0;

	TMADDR->tmbc = -(num*NBPG);
	TMADDR->tmba = 0;
	TMADDR->tmcs = WCOM | GO | D800;
}

tmwait()
{
	register short s;

	do
		s = TMADDR->tmcs;
	while ((s & CRDY) == 0);
}

tmrewind()
{

	tmwait();
	TMADDR->tmcs = REW | GO;
}

tmeof()
{

	tmwait();
	TMADDR->tmcs = WEOF | GO | D800;
}

tubainit()
{
	register struct uba_regs *up = (struct uba_regs *)UBA;

	up->uba_cr = ADINIT;
	up->uba_cr = IFS|BRIE|USEFIE|SUEFIE;
	while ((up->uba_cnfgr & UBIC) == 0) ;
}
#endif
