/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)tcopy.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

#define SIZE	(64 * 1024)

char buff[SIZE];
int filen=1;
long count, lcount;
int RUBOUT();
long itol();
int nfile;
long size, tsize;
int ln;
char *inf, *outf;

main(argc, argv)
char **argv;
{
	register n, nw, inp, outp;
	struct mtop op;

	if (argc != 3) {
		fprintf(stderr, "Usage: tcopy src dest\n");
		exit(1);
	}
	inf = argv[1];
	outf = argv[2];
	if ((inp=open(inf, O_RDONLY, 0666)) < 0) {
		fprintf(stderr,"Can't open %s\n", inf);
		exit(1);
	}
	op.mt_op = MTREW;
	op.mt_count = (daddr_t)1;
	if(ioctl(inp, MTIOCTOP, &op) < 0) {
		perror(inf);
		exit(2);
	}
	if ((outp=open(outf, O_WRONLY, 0666)) < 0) {
		fprintf(stderr,"Can't open %s\n", outf);
		exit(3);
	}
	if(ioctl(outp, MTIOCTOP, &op) < 0) {
		perror(inf);
		exit(4);
	}
	if (signal(2, 1) != 1)
		signal(2, RUBOUT);
	ln = -2;
	for (;;) {
		count++;
		n = read(inp, buff, SIZE);
		if (n > 0) {
		    nw = write(outp, buff, n);
		    if (nw != n) {
			fprintf(stderr, "write (%d) != read (%d)\n", nw, n);
			fprintf(stderr, "COPY Aborted\n");
			exit(5);
		    }
		    size += n;
		    if (n != ln) {
			if (ln > 0)
			    if (count - lcount > 1)
				printf("file %d: records %ld to %ld: size %d\n",
					filen, lcount, count-1, ln);
			    else
				printf("file %d: record %ld: size %d\n",
					filen, lcount, ln);
			ln = n;
			lcount = count;
		    }
		}
		else {
			if (ln <= 0 && ln != -2) {
				printf("eot\n");
				break;
			}
			if (ln > 0)
			    if (count - lcount > 1)
				printf("file %d: records %ld to %ld: size %d\n",
					filen, lcount, count-1, ln);
			    else
				printf("file %d: record %ld: size %d\n",
					filen, lcount, ln);
			printf("file %d: eof after %ld records: %ld bytes\n",
				filen, count-1, size);
			op.mt_op = MTWEOF;
			op.mt_count = (daddr_t)1;
			if(ioctl(outp, MTIOCTOP, &op) < 0) {
				perror("Write EOF");
				exit(6);
			}
			filen++;
			count = 0;
			lcount = 0;
			tsize += size;
			size = 0;
			if (nfile && filen > nfile)
				break;
			ln = n;
		}
	}
	close(outp);
	printf("total length: %ld bytes\n", tsize);
}

RUBOUT()
{
	if (count > lcount)
		--count;
	if (count)
		if (count > lcount)
			printf("file %d: records %ld to %ld: size %d\n",
				filen, lcount, count, ln);
		else
			printf("file %d: record %ld: size %d\n",
				filen, lcount, ln);
	printf("rubout at file %d: record %ld\n", filen, count);
	printf("total length: %ld bytes\n", tsize+size);
	exit(1);
}

