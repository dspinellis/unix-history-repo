/*	tm.c	4.1	%G%	*/
/*
 * TM tape driver
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "saio.h"

struct device {
	short	tmer;
	short	tmcs;
	short	tmbc;
	u_short	tmba;
	short	tmdb;
	short	tmrd;
};

#define TMADDR ((struct device *)(PHYSUMEM + 0772520 - UNIBASE))

#define GO	01
#define RCOM	02
#define WCOM	04
#define WEOF	06
#define SFORW	010
#define SREV	012
#define WIRG	014
#define REW	016
#define DENS	060000		/* 9-channel */
#define IENABLE 0100
#define CRDY	0200
#define GAPSD	010000
#define TUR	1
#define SDWN	010
#define HARD	0102200 /* ILC, EOT, NXM */
#define EOF	0040000

#define SSEEK	1
#define SIO	2

tmopen(io)
register struct iob *io;
{
	register skip;

	tmstrategy(io, REW);
	skip = io->i_boff;
	while (skip--) {
		io->i_cc = 0;
		while (tmstrategy(io, SFORW))
			;
	}
}

tmclose(io)
register struct iob *io;
{
	tmstrategy(io, REW);
}

tmstrategy(io, func)
register struct iob *io;
{
	register int com, unit, errcnt;
	int info;

	unit = io->i_unit;
	errcnt = 0;
retry:
	tmquiet();
	com = (unit<<8)|DENS;
	info = ubasetup(io, 1);
	TMADDR->tmbc = -io->i_cc;
	TMADDR->tmba = info;
	if (func == READ)
		TMADDR->tmcs = com | RCOM | GO;
	else if (func == WRITE)
		TMADDR->tmcs = com | WCOM | GO;
	else if (func == SREV) {
		TMADDR->tmbc = -1;
		TMADDR->tmcs = com | SREV | GO;
		return(0);
	} else
		TMADDR->tmcs = com | func | GO;
	while ((TMADDR->tmcs&CRDY) == 0)
		;
	ubafree(info);
	if (TMADDR->tmer&EOF)
		return(0);
	if (TMADDR->tmer < 0) {
		if (errcnt == 0)
			printf("tape error: er=%o", TMADDR->tmer);
		if (errcnt==10) {
			printf("\n");
			return(-1);
		}
		errcnt++;
		tmstrategy(io, SREV);
		goto retry;
	}
	if (errcnt)
		printf(" recovered by retry\n");
	return( io->i_cc+TMADDR->tmbc );
}

tmquiet()
{
	while ((TMADDR->tmcs&CRDY) == 0)
		;
	while ((TMADDR->tmer&TUR) == 0)
		;
	while ((TMADDR->tmer&SDWN) != 0)
		;
}
