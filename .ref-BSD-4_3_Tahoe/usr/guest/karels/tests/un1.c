#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
int s0, s1;
struct sockaddr_un sun[2] = {0, "s0", 0, "s1"};
main()
{
	unlink("s0"); unlink("s1");
	s0 = socket(AF_UNIX, SOCK_STREAM, 0);
	s1 = socket(AF_UNIX, SOCK_STREAM, 0);
	bind(s0,&sun[0],5);
	listen(s0, 1);
	connect(s1, &sun[0], 5);
	printf("connect\n");
	sync();
}
