#include <sys/file.h>
#include <stdio.h>

main(argc, argv)
	char *argv[];
{
	int fd, mode;

	if (argc < 3) {
		printf("usage: chmod file mode\n");
		exit(1);
	}
	fd = open(argv[1], O_WRONLY);
	if (fd < 0) {
		perror("open");
		exit(2);
	}
	sscanf(argv[2], "%o", &mode);
	fprintf(stderr, "chmod %s to 0%o", argv[1], mode);
	if (fchmod(fd, mode) < 0)
		perror(" ");
	else
		fprintf(stderr, "\n");
}
