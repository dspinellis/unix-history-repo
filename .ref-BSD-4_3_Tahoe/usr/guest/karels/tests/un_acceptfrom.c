#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

struct	sockaddr_un sun, from;

main(argc, argv)
	char *argv[];
{
	int s, s2, fromlen;

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, "SOCK");
	bind(s, &sun, 6);
	listen(s, 2);
	for (;;) {
		fromlen = sizeof(from);
		s2 = accept(s, &from, &fromlen);
		printf("fromlen %d, from %.*s\n", fromlen, fromlen, from.sun_path);
		close(s2);
	}
}
