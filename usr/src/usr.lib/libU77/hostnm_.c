/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)hostnm_.c	5.1	6/7/85
 */

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
