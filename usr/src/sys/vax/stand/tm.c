/*	tm.c	4.2	%G%	*/
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
#define WEOT	06
#define SFORW	010
#define SREV	012
#define WIRG	014
#define REW	016

#define DENS	0		/* 1600 bpi */

#define CRDY	0200		/* tmcs: control unit ready */
#define TUR	1		/* tmer: tape unit ready */
#define SDWN	010		/* tmer: tape settle down */
#define HARD	0102200		/* tmer: ILC, EOT, NXM */
#define EOT	0040000		/* tmer: at end of tape */

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
	int word;
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
		return (0);
	} else
		TMADDR->tmcs = com | func | GO;
	for (;;) {
		word = TMADDR->tmcs;
		if (word&CRDY)
			break;
	}
		;
	ubafree(info);
	word = TMADDR->tmer;
	if (word&EOT)
		return(0);
	if (word < 0) {
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
	return (io->i_cc+TMADDR->tmbc);
}

tmquiet()
{
	register word;
	for (;;) {
		word = TMADDR->tmcs;
		if (word&CRDY)
			break;
	}
	for (;;) {
		word = TMADDR->tmer;
		if ((word&TUR) && (word&SDWN)==0)
			break;
	}
}
