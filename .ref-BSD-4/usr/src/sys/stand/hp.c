/*	hp.c	4.1	11/9/80	*/

/*
 * RP06/RM03/RM05 disk driver
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/mba.h"
#include "saio.h"

struct	device
{
	int	hpcs1;		/* control and Status register 1 */
	int	hpds;		/* Drive Status */
	int	hper1;		/* Error register 1 */
	int	hpmr;		/* Maintenance */ 
	int	hpas;		/* Attention Summary */
	int	hpda;		/* Desired address register */
	int	hpdt;		/* Drive type */
	int	hpla;		/* Look ahead */
	int	hpsn;		/* serial number */
	int	hpof;		/* Offset register */
	int	hpdc;		/* Desired Cylinder address register */
	int	hpcc;		/* Current Cylinder */
	int	hper2;		/* Error register 2 */
	int	hper3;		/* Error register 3 */
	int	hpec1;		/* Burst error bit position */
	int	hpec2;		/* Burst error bit pattern */
};

#define	HPMBA		PHYSMBA0
#define	HPMBANUM	0

#define	NHP	8
#define	RP	022
#define	RM	024
#define	RM5	027
#define	NSECT	22
#define	NTRAC	19
#define	NRMSECT	32
#define	NRMTRAC	5
#define	SDIST	2
#define	RDIST	6

char	hp_type[NHP] = { 0 };	/* drive type */

#define	GO	01
#define	PRESET	020
#define	RTC	016
#define	OFFSET	014
#define	SEARCH	030
#define	RECAL	06
#define	DCLR	010
#define	WCOM	060
#define	RCOM	070

#define	IE	0100
#define	PIP	020000
#define	DRY	0200
#define	ERR	040000
#define	TRE	040000
#define	DCK	0100000
#define	WLE	04000
#define	ECH	0100
#define	VV	0100
#define	DPR	0400
#define	MOL	010000
#define	FMT22	010000

struct size {
	daddr_t	cyloff;
} hp_sizes[8] = {
	0, 38, 0, -1, -1, -1, 118, -1
}, rm_sizes[8] = {
	0, 100, 0, -1, -1, -1, 310, -1
}, rm5_sizes[9] = {
	0, 27, 0, 562, 589, 681, 562, 82
};

hpopen(io)
register struct iob *io;
{
	register unit = io->i_unit;
	struct device *hpaddr;

	if ((mbaact&(1<<HPMBANUM)) == 0)
		mbainit(HPMBANUM);
	hpaddr = mbadev(HPMBA,unit);
	if (hp_type[unit] == 0)
		hp_type[unit] = hpaddr->hpdt;
	if (hp_sizes[io->i_boff].cyloff == -1 ||
	    io->i_boff < 0 || io->i_boff > 7)
		_stop("hp bad minor");
	switch (hp_type[unit]) {
	case RM:
		io->i_boff = rm_sizes[io->i_boff].cyloff * NRMSECT * NRMTRAC;
		break;
	case RM5:
		io->i_boff = rm5_sizes[io->i_boff].cyloff * NRMSECT * NTRAC;
		break;
	case RP:
		io->i_boff = hp_sizes[io->i_boff].cyloff * NSECT * NTRAC;
		break;
	default:
		_stop("unknown drive type");
	}
}

hpstrategy(io, func)
register struct iob *io;
{
	int unit, nspc, ns, cn, tn, sn;
	daddr_t bn;
	struct device *hpaddr;

	unit = io->i_unit;
	bn = io->i_bn;
	hpaddr = mbadev(HPMBA, unit);
	if (hp_type[unit] == 0)
		hp_type[unit] = hpaddr->hpdt;
	if((hpaddr->hpds & VV) == 0) {
		hpaddr->hpcs1 = PRESET|GO;
		hpaddr->hpof = FMT22;
	}
	switch (hp_type[unit]) {
	case RM:
		nspc = NRMSECT*NRMTRAC; ns = NRMSECT; break;
	case RM5:
		nspc = NRMSECT*NTRAC; ns = NRMSECT; break;
	case RP:
		nspc = NSECT*NTRAC; ns = NSECT; break;
	}
	cn = bn/nspc;
	sn = bn%nspc;
	tn = sn/ns;
	sn = sn%ns;
	hpaddr->hpdc = cn;
	hpaddr->hpda = (tn << 8) + sn;
	mbastart(io, (int *)hpaddr, func);
	while((hpaddr->hpds & DRY) == 0)
		;
	if(hpaddr->hpds&ERR) {
		printf("disk error: cyl=%d track=%d sect=%d ds=%X, er1=%X\n",
		    cn, tn, sn,
		    hpaddr->hpds, hpaddr->hper1);
		return (-1);
	}
	return(io->i_cc);
}
