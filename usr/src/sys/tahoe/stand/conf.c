/*	conf.c	1.2	86/07/16	*/
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

/*ARGSUSED*/
nullsys(io) struct iob *io; {}
nullopen(io) struct iob *io; { _stop("bad device type"); }

int	udstrategy(), udopen();
int	vdstrategy(), vdopen();
int	cystrategy(), cyopen(), cyclose();

struct devsw devsw[] = {
	{ "ud",	udstrategy,	udopen,		nullsys },
	{ "dk",	vdstrategy,	vdopen,		nullsys },
#ifdef notdef
	{ "xp",	xpstrategy,	xpopen,		nullsys },
#else
	{ "xp",	nullopen,	nullsys,	nullsys },
#endif
	{ "cy",	cystrategy,	cyopen,		cyclose },
	{ 0 }
};
