#include <sys/param.h>
#include <sys/time.h>

main(ac, av)
	char *av[];
{
	int nofile = getdtablesize();
	register i, fd;
	int ret;
	struct timeval tv;

	tv.tv_sec = 3;
	tv.tv_usec = 0;
	ret = select(nofile, 0, 0, 0, &tv);
	printf("select none returned %d\n", ret);
	if (ret < 0) {
		perror(select);
		exit(1);
	}
}
