#include <stdio.h>
#include <sys/file.h>

main(argc, argv)
	char *argv[];
{
	int fd, n, i;
	char buf[BUFSIZ];

	if (argc < 2)
		exit(1);
	fd = open(argv[1], O_RDWR|O_CREAT|O_TRUNC, 0666);
	if (write(fd, "test\n", 5) < 0)
		perror("write");
	if (lseek(fd, 1024*100, L_XTND) < 0)
		perror("lseek XTND");
	if (write(fd, "hole\n", 5) < 0)
		perror("write");
	if (lseek(fd, 0, L_SET) < 0)
		perror("lseek SET");
	do {
		n = read(fd, buf, sizeof (buf));
		if (n < 0) {
			perror("read");
			break;
		}
		for (i = 0; i < n; i++)
			if (buf[i])
				printf("%d: %c\n",
				    tell(fd) - (sizeof (buf) - i), buf[i]);
	} while (n > 0);
}
