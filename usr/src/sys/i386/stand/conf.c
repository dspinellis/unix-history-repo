/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.noredist.c%
 *
 *	@(#)conf.c	7.1 (Berkeley) %G%
 */


/*#include "../machine/pte.h"*/

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"


#include "saio.h"

devread(io)
	register struct iob *io;
{
	int cc;

	io->i_flgs |= F_RDDATA;
	io->i_error = 0;
	cc = (*devsw[major(io->i_ino.i_dev)].dv_strategy)(io, READ);
	io->i_flgs &= ~F_TYPEMASK;
	return (cc);
}

devwrite(io)
	register struct iob *io;
{
	int cc;

	io->i_flgs |= F_WRDATA;
	io->i_error = 0;
	cc = (*devsw[major(io->i_ino.i_dev)].dv_strategy)(io, WRITE);
	io->i_flgs &= ~F_TYPEMASK;
	return (cc);
}

devopen(io)
	register struct iob *io;
{

	(*devsw[major(io->i_ino.i_dev)].dv_open)(io);
}

devclose(io)
	register struct iob *io;
{

	(*devsw[major(io->i_ino.i_dev)].dv_close)(io);
}

devioctl(io, cmd, arg)
	register struct iob *io;
	int cmd;
	caddr_t arg;
{

	return ((*devsw[major(io->i_ino.i_dev)].dv_ioctl)(io, cmd, arg));
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
int	wdstrategy(), wdopen()/*, wdioctl()*/;

struct devsw devsw[] = {
	/*{ "xx",	xxstrategy,	xxopen,		nullsys, nullioctl },*/
	{ "wd",	wdstrategy,	wdopen,		nullsys,/*wdioctl*/ nullioctl },
	{ 0, 0, 0, 0, 0 },
};
int ndevs = 2 ;
