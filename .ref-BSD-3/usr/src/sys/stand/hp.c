/*
 * RP04/RP06/RM03 disk driver
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/mba.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
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

#define	HPADDR	((struct device *)(PHYSMBA0 + MBA_ERB))
#define	NHP	8
#define	RP	022
#define	RM	024
#define	NSECT	22
#define	NTRAC	19
#define	NRMSECT	32
#define	NRMTRAC	5
#define	SDIST	2
#define	RDIST	6

char	hp_type[NHP];	/* drive type */

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
	0, 38, 98, -1, -1, -1, 118, -1
}, rm_sizes[8] = {
	0, 100, -1, -1, -1, -1, 310, -1
};

hpopen(io)
register struct iob *io;
{
	register unit = io->i_unit;
	struct device *hpaddr;

	hpaddr = (struct device *)((int *)HPADDR + 32*unit);
	if (hp_type[unit] == 0)
		hp_type[unit] = hpaddr->hpdt;
	if (hp_sizes[io->i_boff].cyloff == -1 ||
	    io->i_boff < 0 || io->i_boff > 7)
		_stop("hp bad minor");
	if (hp_type[unit] == RM)
		io->i_boff = rm_sizes[io->i_boff].cyloff * NRMSECT * NRMTRAC;
	else
		io->i_boff = hp_sizes[io->i_boff].cyloff * NSECT * NTRAC;
}

hpstrategy(io, func)
register struct iob *io;
{
	int unit, nspc, ns, cn, tn, sn;
	daddr_t bn;
	struct device *hpaddr;

	unit = io->i_unit;
	bn = io->i_bn;
	hpaddr = (struct device *)((int *)HPADDR + 32*unit);
	if (hp_type[unit] == 0)
		hp_type[unit] = hpaddr->hpdt;
	if((hpaddr->hpds & VV) == 0) {
		hpaddr->hpcs1 = PRESET|GO;
		hpaddr->hpof = FMT22;
	}
	if (hp_type[unit] == RM) {
		nspc = NRMSECT*NRMTRAC;
		ns = NRMSECT;
	} else {
		nspc = NSECT*NTRAC;
		ns = NSECT;
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
