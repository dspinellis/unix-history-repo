/*
 * RP04/RP06 disk driver
 */

#include <sys/param.h>
#include <sys/inode.h>
#include "saio.h"

struct	device
{
	union {
		int	w;
		char	c[2];
	} hpcs1;		/* Control and Status register 1 */
	int	hpwc;		/* Word count register */
	caddr_t	hpba;		/* UNIBUS address register */
	int	hpda;		/* Desired address register */
	union {
		int	w;
		char	c[2];
	} hpcs2;		/* Control and Status register 2*/
	int	hpds;		/* Drive Status */
	int	hper1;		/* Error register 1 */
	int	hpas;		/* Attention Summary */
	int	hpla;		/* Look ahead */
	int	hpdb;		/* Data buffer */
	int	hpmr;		/* Maintenance register */
	int	hpdt;		/* Drive type */
	int	hpsn;		/* Serial number */
	int	hpof;		/* Offset register */
	int	hpdc;		/* Desired Cylinder address register*/
	int	hpcc;		/* Current Cylinder */
	int	hper2;		/* Error register 2 */
	int	hper3;		/* Error register 3 */
	int	hpec1;		/* Burst error bit position */
	int	hpec2;		/* Burst error bit pattern */
	int	hpbae;		/* 11/70 bus extension */
	int	hpcs3;
};

#define	HPADDR	((struct device *)0176700)
#define	NSECT	22
#define	NTRAC	19
#define	SDIST	2
#define	RDIST	6

#define	P400	020
#define	M400	0220
#define	P800	040
#define	M800	0240
#define	P1200	060
#define	M1200	0260

#define	GO	01
#define	PRESET	020
#define	RTC	016
#define	OFFSET	014
#define	SEARCH	030
#define	RECAL	06
#define DCLR	010
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
#define VV	0100
#define FMT22	010000

hpstrategy(io, func)
register struct iob *io;
{
	register unit;
	register i;
	daddr_t bn;
	int sn, cn, tn;

	if (((unit = io->i_unit) & 04) == 0)
		bn = io->i_bn;
	else {
		unit &= 03;
		bn = io->i_bn;
		bn -= io->i_boff;
		i = unit + 1;
		unit = bn%i;
		bn /= i;
		bn += io->i_boff;
	}

	HPADDR->hpcs2.w = unit;

	if((HPADDR->hpds & VV) == 0) {
		HPADDR->hpcs1.c[0] = PRESET|GO;
		HPADDR->hpof = FMT22;
	}
	cn = bn/(NSECT*NTRAC);
	sn = bn%(NSECT*NTRAC);
	tn = sn/NSECT;
	sn = sn%NSECT;

	HPADDR->hpdc = cn;
	HPADDR->hpda = (tn << 8) + sn;
	HPADDR->hpba = io->i_ma;
	HPADDR->hpwc = -(io->i_cc>>1);
	unit = (segflag << 8) | GO;
	if (func == READ)
		unit |= RCOM;
	else if (func == WRITE)
		unit |= WCOM;
	HPADDR->hpcs1.w = unit;
	while ((HPADDR->hpcs1.w&DRY) == 0)
			;
	if (HPADDR->hpcs1.w & TRE) {
		printf("disk error: cyl=%d track=%d sect=%d cs2=%o, er1=%o\n",
		    cn, tn, sn, HPADDR->hpcs2, HPADDR->hper1);
		return(-1);
	}
	return(io->i_cc);
}
