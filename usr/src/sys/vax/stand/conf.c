/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)conf.c	6.2 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"

#include "../vaxmba/mbareg.h"

#include "saio.h"

devread(io)
	register struct iob *io;
{
	int cc;

	io->i_flgs |= F_RDDATA;
	io->i_error = 0;
	cc = (*devsw[io->i_ino.i_dev].dv_strategy)(io, READ);
	io->i_flgs &= ~F_TYPEMASK;
	return (cc);
}

devwrite(io)
	register struct iob *io;
{
	int cc;

	io->i_flgs |= F_WRDATA;
	io->i_error = 0;
	cc = (*devsw[io->i_ino.i_dev].dv_strategy)(io, WRITE);
	io->i_flgs &= ~F_TYPEMASK;
	return (cc);
}

devopen(io)
	register struct iob *io;
{

	(*devsw[io->i_ino.i_dev].dv_open)(io);
}

devclose(io)
	register struct iob *io;
{

	(*devsw[io->i_ino.i_dev].dv_close)(io);
}

devioctl(io, cmd, arg)
	register struct iob *io;
	int cmd;
	caddr_t arg;
{

	return ((*devsw[io->i_ino.i_dev].dv_ioctl)(io, cmd, arg));
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
#if defined(VAX780) || defined(VAX750)
int	hpstrategy(), hpopen(), hpioctl();
#endif
#if defined(VAX780) || defined(VAX750)
int	upstrategy(), upopen(), upioctl();
#endif
int	rkstrategy(), rkopen(), rkioctl();
int	rastrategy(), raopen(), raioctl();
#if defined(VAX730)
int	idcstrategy(), idcopen(), idcioctl();
#endif
int	rlstrategy(), rlopen(), rlioctl();
#ifndef BOOT
int	tmstrategy(), tmopen(), tmclose();
int	tsstrategy(), tsopen(), tsclose();
#if defined(VAX780) || defined(VAX750)
int	htstrategy(), htopen(), htclose();
int	mtstrategy(), mtopen(), mtclose();
#endif
int	utstrategy(), utopen(), utclose();
#endif

struct devsw devsw[] = {
#if defined(VAX780) || defined(VAX750)
	{ "hp",	hpstrategy,	hpopen,		nullsys,	hpioctl },
#endif
#if defined(VAX780) || defined(VAX750)
	{ "up",	upstrategy,	upopen,		nullsys,	upioctl },
#endif
	{ "hk",	rkstrategy,	rkopen,		nullsys,	rkioctl },
	{ "ra",	rastrategy,	raopen,		nullsys,	raioctl },
#if defined(VAX730)
	{ "rb",	idcstrategy,	idcopen,	nullsys,	idcioctl },
#endif
	{ "rl",	rlstrategy,	rlopen,		nullsys,	rlioctl },
#ifndef BOOT
	{ "ts",	tsstrategy,	tsopen,		tsclose,	nullioctl },
#if defined(VAX780) || defined(VAX750)
	{ "ht",	htstrategy,	htopen,		htclose,	nullioctl },
	{ "mt",	mtstrategy,	mtopen,		mtclose,	nullioctl },
#endif
	{ "tm",	tmstrategy,	tmopen,		tmclose,	nullioctl },
	{ "ut",	utstrategy,	utopen,		utclose,	nullioctl },
#endif
	{ 0, 0, 0, 0, 0 },
};
