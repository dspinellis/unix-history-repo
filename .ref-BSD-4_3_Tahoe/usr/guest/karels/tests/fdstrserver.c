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
	int s0, s, n;
	char buf[BUFSIZ];

	if (argc < 2) {
		printf("usage: %s socket\n", argv[0]);
		exit(1);
	}
	s0 = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s0 < 0) {
		perror("socket");
		exit(1);
	}
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, argv[1]);
	if (bind(s0, &sun, strlen(argv[1]) + 2) < 0) {
		perror("fdserver: bind");
		exit(1);
	}
	signal(SIGINT, catchsig);
	signal(SIGTERM, catchsig);
	signal(SIGPIPE, catchsig);
	listen(s0, 1);
	if (setjmp(j)) {
		unlink(argv[1]);
		exit(1);
	}
	for (;;) {
		struct sockaddr_un from;
		int fromlen = sizeof (from), f;
		extern int errno;

		s = accept(s0, &from, &fromlen);
		if (s < 0) {
			if (errno != EINTR)
				perror("fdserver: accept");
			continue;
		}
		printf("Server: connection received");
		if (strlen(from.sun_path) > 0)
			printf(" from %s", from.sun_path);
		putchar('\n'); fflush(stdout);
		n = recv(s, buf, sizeof (buf), 0);
		if (n < 0) {
			if (errno != EINTR)
				perror("fdserver: recv");
			continue;
		}
		if (n > 0)
			printf("; message \"%.*s\"", n, buf);
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
	int newfd = -1, n;

	strcpy(buf, "gotcha...");
	printf("Server: send ack\n"); fflush(stdout);
	if (send(s, buf, strlen(buf), 0) < 0) {
		perror("fdserver: send");
		return;
	}
	for (;;) {
#ifdef dataonly
		newfd = sizeof (to);
		n = recv(s, buf, sizeof (buf), 0);
		if (n == 0) {
			printf("Server: got EOF\n");
			close(s);
			return;
		}
		if (n < 0) {
			perror("fdserver: recv");
			return;
		}
		printf("Server: recv %d, buf \"%.*s\"\n", n, n, buf);
#else
		iov->iov_base = filename;
		iov->iov_len = sizeof (filename);
		msg.msg_iov = iov;
		msg.msg_iovlen = 1;
		msg.msg_accrights = (caddr_t)&newfd;
		msg.msg_accrightslen = sizeof (newfd);
		n = recvmsg(s, &msg, 0);
		if (n == 0 && msg.msg_accrightslen == 0) {
			printf("Server: got EOF\n");
			/*
			close(s);
			return;
			*/
			continue;
		}
		if (n < 0) {
			perror("fdserver: recvmsg");
			return;
		}
		printf("Server: received file");
		if (n > 0)
			printf(" \"%.*s\"", n, iov->iov_base);
		if (msg.msg_accrightslen > 0)
			printf(", newfd %d", newfd);
		printf("\n");
		if (newfd < 0 || newfd >= getdtablesize())
			printf("fdserver: bad new fd\n");
		else {
			printf("Server: file contents:\n");
			while ((n = read(newfd, buf, sizeof (buf))) > 0)
				write(1, buf, n);
			close(newfd);
		}
	}
#endif
/*
	strcpy(buf, "thank you...");
	strcpy(sun->sun_path, name);
	n = sizeof (sun->sun_family) + strlen(sun->sun_path);
	printf("Server: send thank you\n"); fflush(stdout);
	if (sendto(s, buf, strlen(buf), 0, sun, n) < 0)
		perror("fdserver: sendto");
*/
}
