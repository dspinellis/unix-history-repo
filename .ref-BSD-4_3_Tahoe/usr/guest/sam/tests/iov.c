#include <stdio.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>

char	buf[BUFSIZ];
struct	iovec iov[16];

main()
{
	int sv[2], i;
	char c1, c2, c3;
	register struct iovec *v;

	if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0)
		perror("socketpair");
	iov[0].iov_base = &c1;
	iov[0].iov_len = sizeof (c1);
	iov[1].iov_base = &c2;
	iov[1].iov_len = sizeof (c2);
	iov[2].iov_base = &c3;
	iov[2].iov_len = sizeof (c3);
	iov[3].iov_base = buf;
	iov[3].iov_len = sizeof (buf);
	if (fork() == 0) {
		close(sv[0]);
		c1 = '1'; c2 = '2'; c3 = '3';
		strcpy(buf, "this is a test");
		iov[3].iov_len = strlen(buf);
		i = writev(sv[1], iov, 4);
		if (i < 0)
			perror("writev");
		exit(0);
	}
	close(sv[1]);
	i = readv(sv[0], iov, 4);
	if (i < 0) {
		perror("readv");
		exit(1);
	}
	for (v = iov; v < iov + 4; v++) {
		if (i < v->iov_len)
			v->iov_len = i;
		printf("iov[%d] <%d, %.*s>\n", v - iov,
			v->iov_len, v->iov_len, v->iov_base);
		i -= v->iov_len;
	}
}
