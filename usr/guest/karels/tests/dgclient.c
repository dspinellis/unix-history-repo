#include <stdio.h>
#include <strings.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

struct	sockaddr_un sun;
struct	sockaddr_un myname;

int	catchsig();

main(argc, argv)
	char *argv[];
{
	int s, sunlen;
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
	sunlen = strlen(argv[1]) + 2;
	myname.sun_family = AF_UNIX;
	sprintf(myname.sun_path, "dgclient%d", getpid());
	if (bind(s, &myname, strlen(myname.sun_path) + 2) < 0) {
		perror("dgclient: bind");
		exit(1);
	}
	signal(SIGPIPE, catchsig);
	signal(SIGINT, catchsig);
	signal(SIGTERM, catchsig);
	while (fgets(buf, sizeof (buf), stdin) != NULL) {
		register char *cp = index(buf, '\n');

		if (cp)
			*cp = '\0';
		if (sendto(s, buf, strlen(buf), 0, &sun, sunlen) < 0)
			perror("dgclient: sendto");
	}
	unlink(myname.sun_path);
}

catchsig(s)
	int s;
{

	psignal(s, "Client");
	unlink(myname.sun_path);
	exit(1);
}
