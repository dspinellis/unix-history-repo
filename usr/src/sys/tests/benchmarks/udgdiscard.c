/*
 * IPC benchmark,
 * read and discard using UNIX domain datagram sockets.
 *
 * Process forks with parent sending and child
 * receiving (and discarding) messages.
 */

#include <stdio.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/un.h>
#include <sys/resource.h>

struct	sockaddr_un sun;
struct	sockaddr_un myname;

int	catchsig();
char	*malloc();

main(argc, argv)
	char *argv[];
{
	register char *buf;
	register int i, msgs = 0;
	int msglen = 0, ms;
	int pid, s, sunlen;

	if (argc < 3) {
		printf("usage: %s #msgs msglen\n", argv[0]);
		exit(1);
	}
	msgs = atoi(argv[1]);
	msglen = atoi(argv[2]);
	buf = malloc(msglen);
	if (buf == 0) {
		printf("Couldn't allocate data buffer\n");
		exit(1);
	}
	myname.sun_family = AF_UNIX;
	signal(SIGINT, catchsig);
	s = socket(AF_UNIX, SOCK_DGRAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	sprintf(myname.sun_path, "unixdg%d", getpid());
	sunlen = strlen(myname.sun_path) + 2;
	if (bind(s, &myname, sunlen) < 0) {
		perror("bind");
		exit(1);
	}
	pid = fork();
	if (pid == 0)
		for (i = 0; i < msgs; i++) {
			sunlen = sizeof (sun);
			if (recvfrom(s, buf, msglen, 0, &sun, &sunlen) < 0)
				perror("recvfrom (child)");
		}
	else
		for (i = 0; i < msgs; i++) {
			if (sendto(s, buf, msglen, 0, &myname, sunlen) < 0)
				perror("sendto (parent)");
		}
	close(s);
	unlink(myname.sun_path);
}

catchsig(s)
	int s;
{

	unlink(myname.sun_path);
	exit(1);
}
