/*
 * Copyright (c) 1985, 1987, 1993, 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1985, 1987, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tcopy.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"

#define	MAXREC	(64 * 1024)
#define	NOCOUNT	(-2)

int	filen, guesslen, maxblk = MAXREC;
long	lastrec, record;
off_t	size, tsize;
FILE	*msg = stdout;

void	*getspace __P((int));
void	 intr __P((int));
void	 usage __P((void));
void	 verify __P((int, int, char *));
void	 writeop __P((int, int));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, needeof, nw, inp, outp;
	ssize_t lastnread, nread;
	enum {READ, VERIFY, COPY, COPYVERIFY} op = READ;
	sig_t oldsig;
	char *buff, *inf;

	guesslen = 1;
	while ((ch = getopt(argc, argv, "cs:vx")) != EOF)
		switch((char)ch) {
		case 'c':
			op = COPYVERIFY;
			break;
		case 's':
			maxblk = atoi(optarg);
			if (maxblk <= 0) {
				warnx("illegal block size");
				usage();
			}
			guesslen = 0;
			break;
		case 'v':
			op = VERIFY;
			break;
		case 'x':
			msg = stderr;
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
		inf = _PATH_DEFTAPE;
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
		if ((outp = open(argv[1], op == VERIFY ? O_RDONLY :
		    op == COPY ? O_WRONLY : O_RDWR, DEFFILEMODE)) < 0) {
			err(3, argv[1]);
		}
		break;
	default:
		usage();
	}

	if ((inp = open(inf, O_RDONLY, 0)) < 0)
		err(1, inf);

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
			err(1, "read error, file %d, record %ld",
			    filen, record);
		} else if (nread != lastnread) {
			if (lastnread != 0 && lastnread != NOCOUNT) {
				if (lastrec == 0 && nread == 0)
					fprintf(msg, "%ld records\n", record);
				else if (record - lastrec > 1)
					fprintf(msg, "records %ld to %ld\n",
					    lastrec, record);
				else
					fprintf(msg, "record %ld\n", lastrec);
			}
			if (nread != 0)
				fprintf(msg, "file %d: block size %d: ",
				    filen, nread);
			(void) fflush(stdout);
			lastrec = record;
		}
r1:		guesslen = 0;
		if (nread > 0) {
			if (op == COPY || op == COPYVERIFY) {
				if (needeof) {
					writeop(outp, MTWEOF);
					needeof = 0;
				}
				nw = write(outp, buff, nread);
				if (nw != nread) {
				    int error = errno;
				    fprintf(stderr,
					"write error, file %d, record %ld: ",
					filen, record);
				    if (nw == -1)
					fprintf(stderr,
						": %s", strerror(error));
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
				fprintf(msg, "eot\n");
				break;
			}
			fprintf(msg,
			    "file %d: eof after %ld records: %qd bytes\n",
			    filen, record, size);
			needeof = 1;
			filen++;
			tsize += size;
			size = record = lastrec = 0;
			lastnread = 0;
		}
		lastnread = nread;
	}
	fprintf(msg, "total length: %qd bytes\n", tsize);
	(void)signal(SIGINT, oldsig);
	if (op == COPY || op == COPYVERIFY) {
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

void
verify(inp, outp, outb)
	int inp, outp;
	char *outb;
{
	int eot, inmaxblk, inn, outmaxblk, outn;
	char *inb;

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
			warn("read error");
			break;
		}
r1:		if ((outn = read(outp, outb, outmaxblk)) == -1) {
			if (guesslen)
				while (errno == EINVAL && (outmaxblk -= 1024)) {
					outn = read(outp, outb, outmaxblk);
					if (outn >= 0)
						goto r2;
				}
			warn("read error");
			break;
		}
r2:		if (inn != outn) {
			fprintf(msg,
			    "%s: tapes have different block sizes; %d != %d.\n",
			    "tcopy", inn, outn);
			break;
		}
		if (!inn) {
			if (eot++) {
				fprintf(msg, "%s: tapes are identical.\n",
					"tcopy");
				return;
			}
		} else {
			if (bcmp(inb, outb, inn)) {
				fprintf(msg,
				    "%s: tapes have different data.\n",
					"tcopy");
				break;
			}
			eot = 0;
		}
	}
	exit(1);
}

void
intr(signo)
	int signo;
{
	if (record)
		if (record - lastrec > 1)
			fprintf(msg, "records %ld to %ld\n", lastrec, record);
		else
			fprintf(msg, "record %ld\n", lastrec);
	fprintf(msg, "interrupt at file %d: record %ld\n", filen, record);
	fprintf(msg, "total length: %qd bytes\n", tsize + size);
	exit(1);
}

void *
getspace(blk)
	int blk;
{
	void *bp;

	if ((bp = malloc((size_t)blk)) == NULL)
		errx(11, "no memory");

	return (bp);
}

void
writeop(fd, type)
	int fd, type;
{
	struct mtop op;

	op.mt_op = type;
	op.mt_count = (daddr_t)1;
	if (ioctl(fd, MTIOCTOP, (char *)&op) < 0)
		err(6, "tape op");
}

void
usage()
{

	fprintf(stderr, "usage: tcopy [-cvx] [-s maxblk] src [dest]\n");
	exit(1);
}
