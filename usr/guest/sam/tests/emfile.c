#include <stdio.h>

main(argc,argv)
	char *argv[];
{
	int fd, count, cc, buffer;
	char buf[BUFSIZ + 8];

	fd = open(argv[1], 0);
	if (fd < 0) {
		perror(argv[1]);
		exit(-1);
	}
	buffer = count = 0;
	while ((cc = read(fd, buf, BUFSIZ)) != 0 ) {
		if (cc == -1)
			 perror("read");
		else {
			 count += cc;
			 buf[cc] = '\0';
		}
		buffer++;
	}
	fprintf(stderr,"%s: %d\n", argv[1], count);
}
