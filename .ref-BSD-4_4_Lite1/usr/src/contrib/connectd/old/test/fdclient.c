#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/un.h>

#include <stdio.h>
#include <signal.h>

struct	sockaddr_un to;
struct	sockaddr_un from;
struct	msghdr msg;
struct	iovec iov[1];
char	myname[1024];

int	catchsig();

main(argc, argv)
	char *argv[];
{
	int s, len, tolen, fromlen, fd;
	char buf[BUFSIZ];
	struct stat sb;

	if (argc < 3) {
		printf("usage: %s socket file\n", argv[0]);
		exit(1);
	}
	if (stat(argv[1], &sb) < 0) {
		perror(argv[1]);
		exit(1);
	}
	if ((sb.st_mode & S_IFMT) != S_IFSOCK) {
		printf("%s: not a socket\n", argv[1]);
		exit(1);
	}
	fd = open(argv[2], O_RDONLY);
	if (fd < 0) {
		perror(argv[2]);
		exit(1);
	}
	s = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	from.sun_family = AF_UNIX;
	sprintf(myname, "client%d", getpid());
	strcpy(from.sun_path, myname);
	if (bind(s, &from, strlen(from.sun_path) + 2) < 0) {
		perror("fdclient: bind");
		exit(1);
	}
	signal(SIGPIPE, catchsig);
	signal(SIGINT, catchsig);
	to.sun_family = AF_UNIX;
	strcpy(to.sun_path, argv[1]);
	strcpy(buf, myname);
	len = strlen(buf);
	tolen = strlen(to.sun_path) + 2;
	printf("Client: send greeting from %s\n", myname); fflush(stdout);
	if (sendto(s, buf, len, 0, &to, tolen) < 0) {
		perror("fdclient: sendto");
		goto bad;
	}
	*from.sun_path = '\0';
	fromlen = sizeof (from);
	len = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
	if (len < 0) {
		perror("fdclient: recvfrom");
		goto bad;
	}
	printf("Client: message received");
	if (strlen(from.sun_path) > 0)
		printf(" from %s", from.sun_path);
	if (len > 0)
		printf(" \"%.*s\"", len, buf);
	putchar('\n'); fflush(stdout);
	msg.msg_name = (caddr_t)&to;
	msg.msg_namelen = tolen;
	iov->iov_base = argv[2];
	iov->iov_len = strlen(argv[2]);
	msg.msg_iov = iov;
	msg.msg_iovlen = 1;
	msg.msg_accrights = (caddr_t)&fd;
	msg.msg_accrightslen = sizeof (fd);
	printf("Client: send \"%s\"\n", argv[2]); fflush(stdout);
	if (sendmsg(s, &msg, 0) < 0) {
		perror("fdclient: sendmsg");
		goto bad;
	}
/*
	*from.sun_path = '\0';
	fromlen = sizeof (from);
	len = recvfrom(s, buf, sizeof (buf), 0, &from, &fromlen);
	if (len < 0) {
		perror("fdclient: recvfrom");
		goto bad;
	}
	printf("Client: message received");
	if (strlen(from.sun_path) > 0)
		printf(" from %s", from.sun_path);
	if (len > 0)
		printf(" \"%.*s\"", len, buf);
	putchar('\n'); fflush(stdout);
*/
bad:
	unlink(myname);
}

catchsig(s)
	int s;
{

	psignal(s, "Client");
	unlink(myname);
	exit(1);
}
