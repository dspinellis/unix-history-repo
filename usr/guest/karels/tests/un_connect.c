#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

struct	sockaddr_un sun;

main(argc, argv)
	char *argv[];
{
	int s;

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	sun.sun_family = AF_UNIX;
	strcpy(sun.sun_path, "SOCK");
	connect(s, &sun, 6);
}
