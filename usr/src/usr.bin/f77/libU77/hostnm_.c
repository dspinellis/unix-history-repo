/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)hostnm_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * hostnm - return this machines hostname
 *
 * synopsis:
 *	integer function hostnm (name)
 *	character(*) name
 *
 * where:
 *	name	will receive the host name
 *	The returned value will be 0 if successful, an error number otherwise.
 */

extern int	errno;

long
hostnm_ (name, len)
char	*name;
long	len;
{
	char	buf[64];
	register char	*bp;
	int	blen	= sizeof buf;

	if (gethostname (buf, blen) == 0)
	{
		b_char (buf, name, len);
		return (0L);
	}
	else
		return((long)errno);
}
