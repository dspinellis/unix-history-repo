#include <sys/param.h>
#include <sys/socket.h>

main()
{
	int s, optlen, ret;
	int buf[12];

	s = socket(AF_INET, SOCK_DGRAM, 0);
	optlen = sizeof(buf);
	bzero(buf, sizeof(buf));
	if ((ret = getsockopt(s, SOL_SOCKET, SO_DEBUG, buf, &optlen)) < 0)
		perror("getsockopt SO_DEBUG");
	else
		printf("ret = %d, debug = %x (len = %d)\n", ret, buf[0], optlen);

	if (setsockopt(s, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
		perror("setsockopt");
	optlen = sizeof(buf);
	bzero(buf, sizeof(buf));
	if ((ret = getsockopt(s, SOL_SOCKET, SO_DEBUG, buf, &optlen)) < 0)
		perror("getsockopt SO_DEBUG");
	else
		printf("ret = %d, debug = %x (len = %d)\n", ret, buf[0], optlen);

	if (setsockopt(s, SOL_SOCKET, SO_DEBUG, buf, sizeof(buf[0])) < 0)
		perror("setsockopt2");

	optlen = sizeof(buf);
	bzero(buf, sizeof(buf));
	if ((ret = getsockopt(s, SOL_SOCKET, SO_DEBUG, buf, &optlen)) < 0)
		perror("getsockopt SO_DEBUG");
	else
		printf("ret = %d, debug = %x (len = %d)\n", ret, buf[0], optlen);
}
