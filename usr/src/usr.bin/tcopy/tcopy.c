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
static char sccsid[] = "@(#)tcopy.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/errno.h>

#define	MAXREC	(64 * 1024)
#define	NOCOUNT	(-2)

#undef DEFTAPE
#define	DEFTAPE	"/dev/rmt0"

int	filen, guesslen, maxblk = MAXREC;
long	lastrec, record, size, tsize;

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind, errno;
	register int lastnread, nread, nw, inp, outp;
	enum {READ, VERIFY, COPY, COPYVERIFY} op = READ;
	int ch, needeof, intr(), (*oldsig)();
	char *buff, *inf, *getspace();

	guesslen = 1;
	while ((ch = getopt(argc, argv, "cs:v")) != EOF)
		switch((char)ch) {
		case 'c':
			op = COPYVERIFY;
			break;
		case 's':
			maxblk = atoi(optarg);
			if (maxblk <= 0) {
				fputs("tcopy: illegal block size\n", stderr);
				usage();
			}
			guesslen = 0;
			break;
		case 'v':
			op = VERIFY;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	switch(argc) {
	case 0:
		if (op != READ)
			usage();
		inf = DEFTAPE;
		break;
	case 1:
		if (op != READ)
			usage();
		inf = argv[0];
		break;
	case 2:
		if (op == READ)
			op = COPY;
		inf = argv[0];
		if ((outp = open(argv[1], O_RDWR, 0666)) < 0) {
			perror(argv[1]);
			exit(3);
		}
		break;
	default:
		usage();
	}

	if ((inp = open(inf, O_RDONLY, 0)) < 0) {
		perror(inf);
		exit(1);
	}

	buff = getspace(maxblk);

	if (op == VERIFY) {
		verify(inp, outp, buff);
		exit(0);
	}

	if ((oldsig = signal(SIGINT, SIG_IGN)) != SIG_IGN)
		(void) signal(SIGINT, intr);

	needeof = 0;
	for (lastnread = NOCOUNT;;) {
		if ((nread = read(inp, buff, maxblk)) == -1) {
			while (errno == EINVAL && (maxblk -= 1024)) {
				nread = read(inp, buff, maxblk);
				if (nread >= 0)
					goto r1;
			}
			fprintf(stderr, "read error, file %d, record %ld: ",
			    filen, record);
			perror("");
			exit(1);
		} else if (nread != lastnread) {
			if (lastnread != 0 && lastnread != NOCOUNT) {
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
r1:		guesslen = 0;
		if (nread > 0) {
			if (op >= COPY) {
				if (needeof) {
					writeop(outp, MTWEOF);
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
			if (lastnread <= 0 && lastnread != NOCOUNT) {
				printf("eot\n");
				break;
			}
			printf("file %d: eof after %ld records: %ld bytes\n",
				filen, record, size);
			needeof = 1;
			filen++;
			tsize += size;
			size = record = lastrec = 0;
			lastnread = 0;
		}
		lastnread = nread;
	}
	printf("total length: %ld bytes\n", tsize);
	(void)signal(SIGINT, oldsig);
	if (op >= COPY) {
		writeop(outp, MTWEOF);
		writeop(outp, MTWEOF);
		if (op == COPYVERIFY) {
			writeop(outp, MTREW);
			writeop(inp, MTREW);
			verify(inp, outp, buff);
		}
	}
	exit(0);
}

static
verify(inp, outp, outb)
	register int inp, outp;
	register char *outb;
{
	register int eot, inmaxblk, inn, outmaxblk, outn;
	register char *inb;
	char *getspace();

	inb = getspace(maxblk);
	inmaxblk = outmaxblk = maxblk;
	for (eot = 0;; guesslen = 0) {
		if ((inn = read(inp, inb, inmaxblk)) == -1) {
			if (guesslen)
				while (errno == EINVAL && (inmaxblk -= 1024)) {
					inn = read(inp, inb, inmaxblk);
					if (inn >= 0)
						goto r1;
				}
			perror("tcopy: read error");
			exit(1);
		}
r1:		if ((outn = read(outp, outb, outmaxblk)) == -1) {
			if (guesslen)
				while (errno == EINVAL && (outmaxblk -= 1024)) {
					outn = read(outp, outb, outmaxblk);
					if (outn >= 0)
						goto r2;
				}
			perror("tcopy: read error");
			exit(1);
		}
r2:		if (inn != outn)
			break;
		if (!inn) {
			if (eot++) {
				puts("tcopy: tapes are identical.");
				return;
			}
		} else {
			if (bcmp(inb, outb, inn))
				break;
			eot = 0;
		}
	}
	puts("tcopy: tapes are different.");
	exit(1);
}

static
intr()
{
	if (record)
		if (record - lastrec > 1)
			printf("records %ld to %ld\n", lastrec, record);
		else
			printf("record %ld\n", lastrec);
	printf("interrupt at file %d: record %ld\n", filen, record);
	printf("total length: %ld bytes\n", tsize + size);
	exit(1);
}

static char *
getspace(blk)
	int blk;
{
	char *bp, *malloc();

	if ((bp = malloc((u_int)blk)) == NULL) {
		fputs("tcopy: no memory\n", stderr);
		exit(11);
	}
	return(bp);
}

static
writeop(fd, type)
	int fd, type;
{
	struct mtop op;

	op.mt_op = type;
	op.mt_count = (daddr_t)1;
	if (ioctl(fd, MTIOCTOP, (char *)&op) < 0) {
		perror("tcopy: tape op");
		exit(6);
	}
}

static
usage()
{
	fputs("usage: tcopy [-cv] [-s maxblk] src [dest]\n", stderr);
	exit(1);
}
