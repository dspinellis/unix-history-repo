#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/uio.h>
#include <sys/un.h>

#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <errno.h>

struct	sockaddr_un sun;
jmp_buf	j;
int	catchsig();

main(argc, argv)
	char *argv[];
{
	int s, n;
	char buf[BUFSIZ];

	if (argc < 2) {
		printf("usage: %s socket\n", argv[0]);
		exit(1);
	}
	s = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, argv[1]);
	if (bind(s, &sun, strlen(argv[1]) + 2) < 0) {
		perror("fdserver: bind");
		exit(1);
	}
	signal(SIGINT, catchsig);
	signal(SIGTERM, catchsig);
	signal(SIGPIPE, catchsig);
	if (setjmp(j)) {
		unlink(argv[1]);
		exit(1);
	}
	for (;;) {
		struct sockaddr_un from;
		int fromlen = sizeof (from), f;
		extern int errno;

		n = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
		if (n < 0) {
			if (errno != EINTR)
				perror("fdserver: recvfrom");
			continue;
		}
		printf("Server: message received");
		if (strlen(from.sun_path) > 0)
			printf(" from %s", from.sun_path);
		if (n > 0)
			printf(" \"%.*s\"", n, buf);
		putchar('\n'); fflush(stdout);
		if (n > 0)
			shake(s, buf);
	}
}

catchsig(s)
	int s;
{

	psignal(s, "Server");
	longjmp(j, 1);
}

shake(s, name)
	int s;
	char *name;
{
	char buf[BUFSIZ], filename[BUFSIZ];
	struct msghdr msg;
	struct iovec iov[1];
	struct sockaddr_un to, *sun = &to;
	int newfd, n;

	strcpy(buf, "gotcha...");
	sun->sun_family = AF_UNIX;
	strcpy(sun->sun_path, name);
	n = sizeof (sun->sun_family) + strlen(sun->sun_path);
	printf("Server: send ack\n"); fflush(stdout);
	if (sendto(s, buf, strlen(buf), 0, sun, n) < 0) {
		perror("fdserver: sendto");
		return;
	}
#ifdef notdef
	newfd = sizeof (to);
	n = recvfrom(s, buf, sizeof (buf), 0, sun, n);
	if (n < 0) {
		perror("fdserver: recvfrom");
		return;
	}
	printf("Server: recvfrom %d, from %s, buf \"%.*s\"\n", n,
	    sun->sun_path, n, buf);
#endif
	msg.msg_name = (caddr_t)sun;
	msg.msg_namelen = sizeof (*sun);
	iov->iov_base = filename;
	iov->iov_len = sizeof (filename);
	msg.msg_iov = iov;
	msg.msg_iovlen = 1;
	msg.msg_accrights = (caddr_t)&newfd;
	msg.msg_accrightslen = sizeof (newfd);
	n = recvmsg(s, &msg, 0);
	if (n < 0) {
		perror("fdserver: recvmsg");
		return;
	}
	printf("Server: received file");
	if (iov->iov_len > 0)
		printf(" \"%.*s\"", iov->iov_len, iov->iov_base);
	if (strlen(sun->sun_path) > 0)
		printf(" from %s", sun->sun_path);
	printf("; n %d\n", n); fflush(stdout);
	if (n <= 0 || newfd < 0 || newfd >= getdtablesize())
		printf("fdserver: bad new fd %d\n", newfd);
	else {
		printf("Server: file contents:\n");
		while ((n = read(newfd, buf, sizeof (buf))) > 0)
			write(1, buf, n);
		close(newfd);
	}
	strcpy(buf, "thank you...");
	strcpy(sun->sun_path, name);
	n = sizeof (sun->sun_family) + strlen(sun->sun_path);
	printf("Server: send thank you\n"); fflush(stdout);
	if (sendto(s, buf, strlen(buf), 0, sun, n) < 0)
		perror("fdserver: sendto");
}
