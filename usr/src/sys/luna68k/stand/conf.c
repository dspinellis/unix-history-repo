/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)conf.c	7.1 (Berkeley) %G%
 */

#include <luna68k/stand/saio.h>

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
int	sdstrategy(), sdopen(), sdioctl();

struct devsw devsw[] = {
	{ "sd",	sdstrategy,	sdopen,		nullsys,	nullioctl },
	{ 0, 0, 0, 0, 0 },
};

dev_t
make_device(str)
	char *str;
{
	char *cp;
	struct devsw *dp;
	int major, unit, part;

	/*
	 * parse path strings
	 */
							/* find end of dev type name */
	for (cp = str; *cp && *cp != '('; cp++)
			;
	if (*cp != '(') {
		return (-1);
	}
							/* compare dev type name */
	*cp = '\0';
	for (dp = devsw; dp->dv_name; dp++)
		if (!strcmp(str, dp->dv_name))
			break;
	*cp++ = '(';
	if (dp->dv_name == NULL) {
		return (-1);
	}
	major = dp - devsw;
							/* get unit number */
	unit = *cp++ - '0';
	if (*cp >= '0' && *cp <= '9')
		unit = unit * 10 + *cp++ - '0';
	if (unit < 0 || unit > 63) {
		return (-1);
	}
							/* get partition offset */
	if (*cp++ != ',') {
		return (-1);
	}
	part = *cp - '0';
							/* check out end of dev spec */
	for (;;) {
		if (*cp == ')')
			break;
		if (*cp++)
			continue;
		return (-1);
	}

	return(major << 8 | unit << 3 | part);
}
