#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "saio.h"

#define	EMULEX
#ifdef	EMULEX
#define DELAY(N)	{ register int d; d = N; while (--d > 0); }
#else
#define	DELAY(N)
#endif

struct upregs {
	short	upcs1;	/* Control and Status register 1 */
	short	upwc;	/* Word count register */
	short	upba;	/* UNIBUS address register */
	short	upda;	/* Desired address register */
	short	upcs2;	/* Control and Status register 2*/
	short	upds;	/* Drive Status */
	short	uper1;	/* Error register 1 */
	short	upas;	/* Attention Summary */
	short	upla;	/* Look ahead */
	short	updb;	/* Data buffer */
	short	upmr;	/* Maintenance register */
	short	updt;	/* Drive type */
	short	upsn;	/* Serial number */
	short	upof;	/* Offset register */
	short	upca;	/* Desired Cylinder address register*/
	short	upcc;	/* Current Cylinder */
	short	uper2;	/* Error register 2 */
	short	uper3;	/* Error register 3 */
	short	uppos;	/* Burst error bit position */
	short	uppat;	/* Burst error bit pattern */
	short	upbae;	/* 11/70 bus extension */
};

#define	UPADDR ((struct upregs *)(PHYSUMEM + 0776700 - UNIBASE))
char	up_openf;

/* Drive Commands */
#define	GO	01
#define	PRESET	020
#define	RECAL	06
#define	RCLR	010
#define	OFFSET	014
#define	RCOM	070
#define	WCOM	060

#define	IENABLE	0100
#define	READY	0200		/* upds - drive ready */
#define	PIP	020000		/* upds - Positioning Operation in Progress */
#define	ERR	040000		/* upcs1 - composite error */
#define	DRY	0200		/* upcs1 - drive ready */

#define	DTE	010000  	/* uper1 - Drive Timing Error	*/
#define	OPI	020000  	/* uper1 - Operation Incomplete */
#define	DU	040000		/* uper1 - Drive Unsafe */

/* Error Correction Code errors */
#define	DCK	0100000		/* uper1 - Data Check error */
#define	ECH	0100    	/* uper1 - ECC hard error */

#define	CLR	040		/* upcs2 - Controller Clear */

#define	FMT22	010000		/* upof - 16 bit /word format */

struct devsize {
	daddr_t	cyloff;
} up_sizes[] = {
	0, 27, 68, -1, -1, -1, -1, 82
};

upopen(io)
register struct iob *io;
{

	if (up_sizes[io->i_boff].cyloff == -1 ||
	    io->i_boff < 0 || io->i_boff > 7)
		_stop("up bad unit");
	io->i_boff = up_sizes[io->i_boff].cyloff * 32 * 19;
}

upstrategy(io, func)
register struct iob *io;
{
	int unit, nspc, ns, cn, tn, sn;
	daddr_t bn;
	int info;
	register short *rp;
	int occ = io->i_cc;

	unit = io->i_unit;
	bn = io->i_bn;
	nspc = 32 * 19;
	ns = 32;
	cn = bn/nspc;
	sn = bn%nspc;
	tn = sn/ns;
	sn = sn%ns;
	if (!up_openf) {
		up_openf++;
		UPADDR->upcs2 = CLR;
		DELAY(500);
		UPADDR->upcs1 = RCLR|GO;
		DELAY(500);
		UPADDR->upcs1 = PRESET|GO;
		DELAY(500);
		UPADDR->upof = FMT22;
		DELAY(500);
		while ((UPADDR->upcs1 & DRY) == 0)
			DELAY(500);
	}
	UPADDR->upcs2 = unit;
	DELAY(500);
	info = ubasetup(io, 1);
	rp = (short *) &UPADDR->upda;
	UPADDR->upca = cn;
	*rp = (tn << 8) + sn;
	*--rp = info;
	*--rp = -io->i_cc / sizeof (short);
	if (func == READ) {
		*--rp = GO|RCOM;
	} else {
		*--rp = GO|WCOM;
	}
	DELAY(500);
	do {
		DELAY(200);
	} while ((UPADDR->upcs1 & DRY) == 0);
	DELAY(200);
	if (UPADDR->upcs1&ERR) {
		printf("disk error: cyl=%d track=%d sect=%d cs1=%X, er1=%X\n",
		    cn, tn, sn,
		    UPADDR->upcs1, UPADDR->uper1);
		return (-1);
	}
	if (io->i_cc != occ)
	printf("returned %d\n", io->i_cc);
	ubafree(info);
	return (io->i_cc);
}
