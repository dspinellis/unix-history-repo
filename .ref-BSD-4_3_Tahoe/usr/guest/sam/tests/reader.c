#include <sys/file.h>

char buf[10] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };
main()
{
	int fd, n;

	fd = open("./foo", FRDWR|FCREATE, 0666);
	if (fd < 0)
		perror("open");
	printf("open successful\n");
	n = flock(fd, FSHLOCK);
	if (n < 0)
		perror("read lock");
	else
		printf("got read lock\n");
	n = read(fd, buf, sizeof (buf));
	printf("n = %d ", n);
	if (n < 0)
		perror("read");
	else
		putchar('\n');
	n = flock(fd, FUNLOCK);
	if (n < 0)
		perror("unlock");
	else
		printf("unlocked\n");
	n = flock(fd, FSHLOCK);
	if (n < 0)
		perror("read lock");
	else
		printf("got read lock\n");
	n = read(fd, buf, sizeof (buf));
	printf("n = %d ", n);
	if (n < 0)
		perror("read");
	else
		putchar('\n');
	pause();
}
