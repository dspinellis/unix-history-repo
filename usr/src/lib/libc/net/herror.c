/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* !lint */

#ifndef lint
static char sccsid[] = "@(#)herror.c	1.2 (Berkeley) %G%";
#endif /* !lint */

#include <sys/types.h>
#include <sys/uio.h>

char	*h_errlist[] = {
	"Error 0",
	"Unknown host",				/* 1 HOST_NOT_FOUND */
	"Host name lookup failure",		/* 2 TRY_AGAIN */
	"Unknown server error",			/* 3 NO_RECOVERY */
	"No address associated with name",	/* 4 NO_ADDRESS */
};
int	h_nerr = { sizeof(h_errlist)/sizeof(h_errlist[0]) };

extern int	h_errno;

/*
 * herror --
 *	print the error indicated by the h_errno value.
 */
herror(s)
	char *s;
{
	struct iovec iov[4];
	register struct iovec *v = iov;

	if (s && *s) {
		v->iov_base = s;
		v->iov_len = strlen(s);
		v++;
		v->iov_base = ": ";
		v->iov_len = 2;
		v++;
	}
	v->iov_base = h_errno < h_nerr ? h_errlist[h_errno] : "Unknown error";
	v->iov_len = strlen(v->iov_base);
	v++;
	v->iov_base = "\n";
	v->iov_len = 1;
	writev(2, iov, (v - iov) + 1);
}
