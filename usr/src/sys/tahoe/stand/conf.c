/*	conf.c	1.4	87/10/27	*/
/*	conf.c	6.1	83/07/29	*/

#include "../machine/pte.h"

#include "param.h"
#include "inode.h"
#include "fs.h"

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
nullsys(io) struct iob *io; {}
nullopen(io) struct iob *io; { _stop("bad device type"); }

/*ARGSUSED*/
noioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}

int	udstrategy(), udopen();
int	vdstrategy(), vdopen();
int	hdstrategy(), hdopen();
int	cystrategy(), cyopen(), cyclose();

struct devsw devsw[] = {
	{ "ud",	udstrategy,	udopen,		nullsys,	noioctl },
	{ "dk",	vdstrategy,	vdopen,		nullsys,	noioctl },
	{ "hd",	hdstrategy,	hdopen,		nullsys,	noioctl },
#ifdef notdef
	{ "xp",	xpstrategy,	xpopen,		nullsys,	noioctl },
#else
	{ "xp",	nullopen,	nullsys,	nullsys,	noioctl },
#endif
	{ "cy",	cystrategy,	cyopen,		cyclose,	noioctl },
	{ 0 }
};
int	ndevs = (sizeof(devsw) / sizeof(devsw[0]) - 1);
