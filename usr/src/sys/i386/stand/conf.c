/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.2 (Berkeley) %G%
 */

#include "param.h"

#include "saio.h"
#ifdef BOOT
extern exception;
extern int debugflag;
#endif

devread(io)
	register struct iob *io;
{
	int cc;
	char c;

	io->i_flgs |= F_RDDATA;
	io->i_error = 0;
	cc = (*devsw[io->i_dev].dv_strategy)(io, READ);
	io->i_flgs &= ~F_TYPEMASK;

#ifdef BOOT
if(/*io->i_error || */(c=scankbd()))
	_longjmp(&exception,1);
#endif
	return (cc);
}

devwrite(io)
	register struct iob *io;
{
	int cc;
	char c;

	io->i_flgs |= F_WRDATA;
	io->i_error = 0;
	cc = (*devsw[io->i_dev].dv_strategy)(io, WRITE);
	io->i_flgs &= ~F_TYPEMASK;
#ifdef BOOT
if(/* io->i_error || */ (c=scankbd()))
	_longjmp(&exception,1);
#endif
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
int	wdstrategy(), wdopen()/*, wdioctl()*/;
int	fdstrategy(), fdopen()/*, fdioctl()*/;

struct devsw devsw[] = {
	/*{ "xx",	xxstrategy,	xxopen,		nullsys, nullioctl },*/
	{ "wd",	wdstrategy,	wdopen,		nullsys,/*wdioctl*/ nullioctl },
	{ "", 0, 0, 0, 0 }, /* swapdev place holder */
	{ "fd",	fdstrategy,	fdopen,		nullsys,/*fdioctl*/ nullioctl },
	{ 0, 0, 0, 0, 0 },
};
int ndevs = 3 ;
