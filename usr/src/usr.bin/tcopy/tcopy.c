/*
 * Copyright (c) 1985, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1985, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tcopy.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/errno.h>

#define MAXREC	(64 * 1024)
#define NOCOUNT	(-2)
#undef DEFTAPE
#define	DEFTAPE	"/dev/rmt0"

char	*buff;
char	*inf = DEFTAPE;
int	maxblk = MAXREC;
int	filen;
long	record, lastrec;
int	intr();
char	*malloc();
long	size, tsize;
int	nfile;
int	lastread;
int	copy;
extern	int errno;

main(argc, argv)
	int argc;
	char **argv;
{
	register nread, nw, inp, outp;
	struct mtop op;
	int needeof = 0, guesslen = 1;

	while (argc > 1 && *argv[1] == '-') {
		switch (*++argv[1]) {
		case 's':
			if (argc < 3)
				goto usage;
			maxblk = atoi(argv[2]);
			if (maxblk <= 0) {
				fprintf(stderr, "illegal block size\n");
				goto usage;
			}
			argc--;
			argv++;
			guesslen = 0;
			break;
		}
		argc--;
		argv++;
	}
	if (argc < 1 || argc > 3) {
usage:
		fprintf(stderr, "Usage: tcopy [-s maxblk] src [dest]\n");
		exit(1);
	}
	if (argc > 1)
		inf = argv[1];
	if ((inp = open(inf, O_RDONLY, 0)) < 0) {
		perror(inf);
		exit(1);
	}
	if (argc == 3) {
		copy = 1;
		if ((outp = open(argv[2], O_WRONLY, 0666)) < 0) {
			perror(argv[2]);
			exit(3);
		}
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, intr);
	buff = malloc((u_int)maxblk);
	if (buff == NULL) {
		fprintf(stderr, "tcopy: no memory\n");
		exit(11);
	}
	lastread = NOCOUNT;
	for (;;) {
		nread = read(inp, buff, maxblk);
		if (nread == -1) {
			if (errno == EINVAL && guesslen &&
			    maxblk > MAXREC / 2) {
				maxblk -= 1024;
				continue;
			}
			fprintf(stderr, "read error, file %d, record %ld: ",
			    filen, record);
			perror("");
		} else if (nread != lastread) {
			if (lastread != 0 && lastread != NOCOUNT) {
				if (lastrec == 0 && nread == 0)
					printf("%ld records\n", record);
				else if (record - lastrec > 1)
					printf("records %ld to %ld\n",
					    lastrec, record);
				else
					printf("record %ld\n", lastrec);
			}
			if (nread != 0)
				printf("file %d: block size %d: ",
				    filen, nread);
			(void) fflush(stdout);
			lastrec = record;
		}
		guesslen = 0;
		if (nread > 0) {
			if (copy) {
				if (needeof) {
				    op.mt_op = MTWEOF;
				    op.mt_count = (daddr_t) 1;
				    if (ioctl(outp, MTIOCTOP, (char *)&op) < 0) {
					    perror("write tape mark");
					    exit(6);
				    }
				    needeof = 0;
				}
				nw = write(outp, buff, nread);
				if (nw != nread) {
				    fprintf(stderr,
					"write error, file %d, record %ld: ",
					filen, record);
				    if (nw == -1)
					perror("");
				    else
					fprintf(stderr,
					    "write (%d) != read (%d)\n",
					    nw, nread);
				    fprintf(stderr, "copy aborted\n");
				    exit(5);
				}
			}
			size += nread;
			record++;
		} else {
			if (lastread <= 0 && lastread != NOCOUNT) {
				printf("eot\n");
				break;
			}
			printf("file %d: eof after %ld records: %ld bytes\n",
				filen, record, size);
			needeof = 1;
			filen++;
			tsize += size;
			record = 0;
			lastrec = 0;
			lastread = 0;
			size = 0;
			if (nfile && filen > nfile)
				break;
		}
		lastread = nread;
	}
	if (copy)
		(void) close(outp);
	printf("total length: %ld bytes\n", tsize);
}

intr()
{
	if (record)
		if (record - lastrec > 1)
			printf("records %ld to %ld\n", lastrec, record);
		else
			printf("record %ld\n", lastrec);
	printf("interrupt at file %d: record %ld\n", filen, record);
	printf("total length: %ld bytes\n", tsize+size);
	exit(1);
}
