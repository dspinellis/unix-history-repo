/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)access_.c	5.1	%G%
 */

/*
 * determine accessability of a file
 *
 * calling format:
 *	integer access
 *	ierror = access(filename, mode)
 * where:
 *	ierror will be 0 for successful access; an error number otherwise.
 *	filename is a character string
 *	mode is a character string which may include any combination of
 *	'r', 'w', 'x', ' '. (' ' => test for existence)
 */

#include "../libI77/f_errno.h"
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif

long access_(name, mode, namlen, modlen)
char *name, *mode;
long namlen, modlen;
{
	char buf[MAXPATHLEN];
	int m = 0;

	if (namlen >= sizeof buf)
		return((long)(errno=F_ERARG));
	g_char(name, namlen, buf);
	if (buf[0] == '\0')
		return((long)(errno=ENOENT));
	if (access(buf, 0) < 0)
		return((long)errno);
	while (modlen--) switch(*mode++)
	{
		case 'x':
			m |= 1;
			break;

		case 'w':
			m |= 2;
			break;

		case 'r':
			m |= 4;
			break;
	}
	if (m > 0 && access(buf, m) < 0)
		return((long)errno);
	return(0L);
}
