#include <sys/file.h>

main(argc, argv)
	char *argv[];
{
	int fd = open(argv[1], O_RDONLY);

	if (fd < 0)
		exit(1);
	if (flock(fd, LOCK_SH) < 0)
		perror("flock");
	printf("got shared lock\n");
	sigpause(0);
}
