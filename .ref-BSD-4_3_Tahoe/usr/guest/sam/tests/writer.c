#include <sys/file.h>

char buf[10] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };
main()
{
	int fd, n;

	fd = open("./foo", FRDWR|FCREATE, 0666);
	if (fd < 0)
		perror("open");
	printf("open successful\n");
	n = flock(fd, FEXLOCK);
	if (n < 0)
		perror("write lock");
	else
		printf("got write lock\n");
	n = write(fd, buf, sizeof (buf));
	printf("n = %d ", n);
	if (n < 0)
		perror("write");
	else
		putchar('\n');
	pause();
}
