#include <stdio.h>
#define MAXB 30
int mt;
int fd;
char	buf[MAXB*512];
char	name[50];
int	blksz;

main(argc, argv)
int	argc;
char	*argv[];
{
	int i, j, k;
	FILE *mf;

	if (argc != 3) {
		fprintf(stderr, "Usage: maketape tapedrive makefile\n");
		exit(0);
	}
	if ((mt = creat(argv[1], 0666)) < 0) {
		perror(argv[1]);
		exit(1);
	}
	if ((mf = fopen(argv[2], "r")) == NULL) {
		perror(argv[2]);
		exit(2);
	}

	j = 0;
	k = 0;
	for (;;) {
		if ((i = fscanf(mf, "%s %d", name, &blksz))== EOF)
			exit(0);
		if (i != 2) {
			fprintf(stderr, "Help! Scanf didn't read 2 things (%d)\n", i);
			exit(1);
		}
		if (blksz <= 0 || blksz > MAXB) {
			fprintf(stderr, "Block size %d is invalid\n", blksz);
			continue;
		}
		if (strcmp(name, "*") == 0) {
			close(mt);
			mt = open(argv[1], 2);
			j = 0;
			k++;
			continue;
		}
		fd = open(name, 0);
		if (fd < 0) {
			perror(name);
			continue;
		}
		printf("%s: block %d, file %d\n", name, j, k);
		while (read(fd, buf, 512*blksz) > 0) {
			j++;
			write(mt, buf, 512*blksz);
		}
	}
}
