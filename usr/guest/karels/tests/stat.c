#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

char *ctime();

main(argc, argv)
char **argv;
{
	struct stat stbuf;

	while (--argc) {
		if (stat(*(++argv), &stbuf) < 0) {
			perror(*argv);
			continue;
		}
		printf("\n%s:\n", *argv);
		printf("atime %d = %s", stbuf.st_atime,
			ctime(&stbuf.st_atime));
		printf("mtime %d = %s", stbuf.st_mtime,
			ctime(&stbuf.st_mtime));
		printf("ctime %d = %s", stbuf.st_ctime,
			ctime(&stbuf.st_ctime));
		printf("size %d\n", stbuf.st_size);
	}
	exit(0);
}

