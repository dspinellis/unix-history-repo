/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dev.c	7.9 (Berkeley) %G%
 */

#include "sys/param.h"
#include "stand/saio.h"			/* used from machine/stand dir */

/*
 * NB: the value "io->i_dev", used to offset the devsw[] array
 * in the routines below, is munged by the vaxstand Makefile to work
 * for certain boots.
 */

devread(io)
	register struct iob *io;
{
	int cc;

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
	int ret;

	if (!(ret = (*devsw[io->i_dev].dv_open)(io)))
		return (0);
	printf("%s(%d,%d,%d,%d): ", devsw[io->i_dev].dv_name,
		io->i_adapt, io->i_ctlr, io->i_unit, io->i_part);
	switch(ret) {
	case EIO:
		break;		/* already reported */
	case EADAPT:
		printf("bad adaptor number\n");
		break;
	case ECTLR:
		printf("bad controller number\n");
		break;
	case EUNIT:
		printf("bad drive number\n");
		break;
	case EPART:
		printf("bad partition\n");
		break;
	case ERDLAB:
		printf("can't read disk label\n");
		break;
	case EUNLAB:
		printf("unlabeled\n");
		break;
	case ENXIO:
		printf("bad device specification\n");
		break;
	default:
		printf("unknown open error\n");
		break;
	}
	return (ret);
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
{}

/*ARGSUSED*/
nodev(io)
	struct iob *io;
{
	errno = EBADF;
}

/*ARGSUSED*/
noioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{
	return (ECMD);
}
