/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.2 (Berkeley) %G%
 */

#include "saio.h"

devread(io)
	register struct iob *io;
{
	int cc;

	/* check for interrupt */
	(void) peekchar();

	io->i_flgs |= F_RDDATA;
	io->i_error = 0;
	cc = (*devsw[io->i_dev].dv_strategy)(io, READ);
	io->i_flgs &= ~F_TYPEMASK;
	return (cc);
}

devwrite(io)
	register struct iob *io;
{
	int cc;

	io->i_flgs |= F_WRDATA;
	io->i_error = 0;
	cc = (*devsw[io->i_dev].dv_strategy)(io, WRITE);
	io->i_flgs &= ~F_TYPEMASK;
	return (cc);
}

devopen(io)
	register struct iob *io;
{

	(*devsw[io->i_dev].dv_open)(io);
}

devclose(io)
	register struct iob *io;
{

	(*devsw[io->i_dev].dv_close)(io);
}

devioctl(io, cmd, arg)
	register struct iob *io;
	int cmd;
	caddr_t arg;
{

	return ((*devsw[io->i_dev].dv_ioctl)(io, cmd, arg));
}

/*ARGSUSED*/
nullsys(io)
	struct iob *io;
{

	;
}

/*ARGSUSED*/
nullioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}

int	nullsys(), nullioctl();
int	rdstrategy(), rdopen(), rdioctl();
int	sdstrategy(), sdopen(), sdioctl();
#ifndef BOOT
int	ctstrategy(), ctopen(), ctclose();
#endif

struct devsw devsw[] = {
	{ "rd",	rdstrategy,	rdopen,		nullsys,	nullioctl },
	{ "sd",	sdstrategy,	sdopen,		nullsys,	nullioctl },
#ifndef BOOT
	{ "ct",	ctstrategy,	ctopen,		ctclose,	nullioctl },
#endif
	{ 0, 0, 0, 0, 0 },
};
