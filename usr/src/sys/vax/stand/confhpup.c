/*	confhpup.c	4.1	83/02/04	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "saio.h"

devread(io)
	register struct iob *io;
{
	int error;

	io->i_flgs |= F_RDDATA;
	error = (*devsw[io->i_ino.i_dev].dv_strategy)(io, READ);
	io->i_flgs &= ~F_TYPEMASK;
	return (error);
}

devwrite(io)
	register struct iob *io;
{
	int error;

	io->i_flgs |= F_WRDATA;
	error = (*devsw[io->i_ino.i_dev].dv_strategy)(io, WRITE);
	io->i_flgs &= ~F_TYPEMASK;
	return (error);
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
int	hpstrategy(), hpopen(), hpioctl();
int	upstrategy(), upopen(), upioctl();

struct devsw devsw[] = {
	{ "hp",	hpstrategy,	hpopen,		nullsys,	hpioctl },
	{ "up",	upstrategy,	upopen,		nullsys,	upioctl },
	{ 0, 0, 0, 0, 0 }
};
