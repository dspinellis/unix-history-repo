/*
 * Concatenate files.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

char	stdbuf[BUFSIZ];

main(argc, argv)
char **argv;
{
	int fflg = 0;
	register FILE *fi;
	register c;
	int dev, ino = -1;
	struct stat statb;

	setbuf(stdout, stdbuf);
	for( ; argc>1 && argv[1][0]=='-'; argc--,argv++) {
		switch(argv[1][1]) {
		case 0:
			break;
		case 'u':
			setbuf(stdout, (char *)NULL);
			continue;
		}
		break;
	}
	fstat(fileno(stdout), &statb);
	statb.st_mode &= S_IFMT;
	if (statb.st_mode!=S_IFCHR && statb.st_mode!=S_IFBLK) {
		dev = statb.st_dev;
		ino = statb.st_ino;
	}
	if (argc < 2) {
		argc = 2;
		fflg++;
	}
	while (--argc > 0) {
		if (fflg || (*++argv)[0]=='-' && (*argv)[1]=='\0')
			fi = stdin;
		else {
			if ((fi = fopen(*argv, "r")) == NULL) {
				fprintf(stderr, "cat: can't open %s\n", *argv);
				continue;
			}
		}
		fstat(fileno(fi), &statb);
		if (statb.st_dev==dev && statb.st_ino==ino) {
			fprintf(stderr, "cat: input %s is output\n",
			   fflg?"-": *argv);
			fclose(fi);
			continue;
		}
		while ((c = getc(fi)) != EOF)
			putchar(c);
		if (fi!=stdin)
			fclose(fi);
	}
	return(0);
}
