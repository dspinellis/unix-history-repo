#include <sys/file.h>

main(argc, argv)
	char *argv[];
{
	int fd, uid, gid;

	if (argc < 4) {
		printf("usage: chown file uid gid\n");
		exit(1);
	}
	fd = open(argv[1], O_WRONLY);
	if (fd < 0) {
		perror("open");
		exit(2);
	}
	uid = atoi(argv[2]);
	gid = atoi(argv[3]);
	if (fchown(fd, uid, gid) < 0)
		perror("fchown");
}
