#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

struct	sockaddr_un sun;

main(argc, argv)
	char *argv[];
{
	int s;
	char buf[BUFSIZ];

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, argv[1]);
#ifdef notdef
	if (bind(s, &sun1, strlen(argv[1]) + 2) < 0) {
		perror("bind");
		exit(1);
	}
#endif
	if (connect(s, &sun, strlen(argv[1]) + 2) < 0) {
		perror("connect");
		exit(1);
	}
	while (fgets(buf, sizeof (buf), stdin) != NULL)
		write(s, buf, strlen(buf));
}
