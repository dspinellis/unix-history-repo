/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <stand/saio.h>

extern int	nullsys(), nodev(), noioctl();

#ifdef BOOT
#define	ctstrategy	nullsys
#define	ctopen		nodev
#define	ctclose		nullsys
#else
int	ctstrategy(), ctopen(), ctclose();
#endif
#define	ctioctl	noioctl

int	rdstrategy(), rdopen();
#define	rdioctl	noioctl

int	sdstrategy(), sdopen();
#define	sdioctl	noioctl


struct devsw devsw[] = {
	{ "ct",	ctstrategy,	ctopen,	ctclose,	ctioctl }, /*0*/
	{ "??",	nullsys,	nodev,	nullsys,	noioctl }, /*1*/
	{ "rd",	rdstrategy,	rdopen,	nullsys,	rdioctl }, /*2*/
	{ "??",	nullsys,	nodev,	nullsys,	noioctl }, /*3*/
	{ "sd",	sdstrategy,	sdopen,	nullsys,	sdioctl }, /*4*/
};

int	ndevs = (sizeof(devsw)/sizeof(devsw[0]));

/*
 * Convert old style unit syntax into adaptor/controller/unit
 */
devconvert(io)
	register struct iob *io;
{
	if (io->i_unit == 0 || io->i_adapt || io->i_ctlr)
		return;
	io->i_adapt = io->i_unit / 8;
	io->i_ctlr = io->i_unit % 8;
	io->i_unit = 0;
}
