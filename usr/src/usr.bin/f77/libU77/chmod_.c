/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)chmod_.c	5.1	%G%
 */

/*
 * chmod - change file mode bits
 *
 * synopsis:
 *	integer function chmod (fname, mode)
 *	character*(*) fname, mode
 */

#include "../libI77/f_errno.h"
#include <sys/param.h>
#ifndef	MAXPATHLEN
#define MAXPATHLEN	128
#endif

long chmod_(name, mode, namlen, modlen)
char	*name, *mode;
long	namlen, modlen;
{
	char	nambuf[MAXPATHLEN];
	char	modbuf[32];
	int	retcode;

	if (namlen >= sizeof nambuf || modlen >= sizeof modbuf)
		return((long)(errno=F_ERARG));
	g_char(name, namlen, nambuf);
	g_char(mode, modlen, modbuf);
	if (nambuf[0] == '\0')
		return((long)(errno=ENOENT));
	if (modbuf[0] == '\0')
		return((long)(errno=F_ERARG));
	if (fork())
	{
		if (wait(&retcode) == -1)
			return((long)errno);
		return((long)retcode);
	}
	else
		execl("/bin/chmod", "chmod", modbuf, nambuf, (char *)0);
}
