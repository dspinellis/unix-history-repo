/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fdopen.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * Unix routine to do an "fopen" on file descriptor
 * The mode has to be repeated because you can't query its
 * status
 */

#include <sys/types.h>
#include <sys/file.h>
#include <stdio.h>

FILE *
fdopen(fd, mode)
	register char *mode;
{
	extern FILE *_findiop();
	static int nofile = -1;
	register FILE *iop;

	if (nofile < 0)
		nofile = getdtablesize();

	if (fd < 0 || fd >= nofile)
		return (NULL);

	iop = _findiop();
	if (iop == NULL)
		return (NULL);

	iop->_cnt = 0;
	iop->_file = fd;
	iop->_bufsiz = 0;
	iop->_base = iop->_ptr = NULL;

	switch (*mode) {
	case 'r':
		iop->_flag = _IOREAD;
		break;
	case 'a':
		lseek(fd, (off_t)0, L_XTND);
		/* fall into ... */
	case 'w':
		iop->_flag = _IOWRT;
		break;
	default:
		return (NULL);
	}

	if (mode[1] == '+')
		iop->_flag = _IORW;

	return (iop);
}
