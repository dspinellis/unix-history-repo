/*
 * cp oldfile newfile
 */

#define	BSIZE	1024
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
struct	stat	stbuf1, stbuf2;
char	iobuf[BSIZE];

main(argc, argv)
char *argv[];
{
	register i, r;

	if (argc < 3) 
		goto usage;
	if (argc > 3) {
		if (stat(argv[argc-1], &stbuf2) < 0)
			goto usage;
		if ((stbuf2.st_mode&S_IFMT) != S_IFDIR) 
			goto usage;
	}
	r = 0;
	for(i=1; i<argc-1;i++)
		r |= copy(argv[i], argv[argc-1]);
	exit(r);
usage:
	fprintf(stderr, "Usage: cp: f1 f2; or cp f1 ... fn d2\n");
	exit(1);
}

copy(from, to)
char *from, *to;
{
	int fold, fnew, n;
	register char *p1, *p2, *bp;
	int mode;
	if ((fold = open(from, 0)) < 0) {
		fprintf(stderr, "cp: cannot open %s\n", from);
		return(1);
	}
	fstat(fold, &stbuf1);
	mode = stbuf1.st_mode;
	/* is target a directory? */
	if (stat(to, &stbuf2) >=0 &&
	   (stbuf2.st_mode&S_IFMT) == S_IFDIR) {
		p1 = from;
		p2 = to;
		bp = iobuf;
		while(*bp++ = *p2++)
			;
		bp[-1] = '/';
		p2 = bp;
		while(*bp = *p1++)
			if (*bp++ == '/')
				bp = p2;
		to = iobuf;
	}
	if (stat(to, &stbuf2) >= 0) {
		if (stbuf1.st_dev == stbuf2.st_dev &&
		   stbuf1.st_ino == stbuf2.st_ino) {
			fprintf(stderr, "cp: cannot copy file to itself.\n");
			return(1);
		}
	}
	if ((fnew = creat(to, mode)) < 0) {
		fprintf(stderr, "cp: cannot create %s\n", to);
		close(fold);
		return(1);
	}
	while(n = read(fold,  iobuf,  BSIZE)) {
		if (n < 0) {
			perror("cp: read");
			close(fold);
			close(fnew);
			return(1);
		} else
			if (write(fnew, iobuf, n) != n) {
				perror("cp: write");
				close(fold);
				close(fnew);
				return(1);
			}
	}
	close(fold);
	close(fnew);
	return(0);
}
