/* @(#)perror.c	4.2 (Berkeley) %G% */
/*
 * Print the error indicated
 * in the cerror cell.
 */
#include <sys/types.h>
#include <sys/uio.h>

int	errno;
int	sys_nerr;
char	*sys_errlist[];
perror(s)
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
	v->iov_base = errno < sys_nerr ? sys_errlist[errno] : "Unknown error";
	v->iov_len = strlen(v->iov_base);
	v++;
	v->iov_base = "\n";
	v->iov_len = 1;
	writev(2, iov, (v - iov) + 1);
}
