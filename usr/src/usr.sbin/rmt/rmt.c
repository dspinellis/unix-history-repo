#ifndef lint
static char sccsid[] = "@(#)rmt.c	4.1 82/04/02";
#endif

/*
 * rmt
 */
#include <stdio.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/mtio.h>
#include <errno.h>

int	tape = -1;

#define	MAXRECSIZ	(60*1024)
char	record[MAXRECSIZ];

#define	SSIZE	64
char	device[SSIZE];
char	count[SSIZE], mode[SSIZE], pos[SSIZE], op[SSIZE];

extern	errno;
char	*sys_errlist[];
char	resp[BUFSIZ];

char	*sprintf();
long	lseek();

FILE	*debug;

main(argc, argv)
	int argc;
	char **argv;
{
	long rval;
	char c;
	int n, i, cc;

	argc--, argv++;
	if (argc > 0) {
		debug = fopen(*argv, "w");
		if (debug == 0)
			exit(1);
		(void) setbuf(debug, (char *)0);
	}
top:
	errno = 0;
	rval = 0;
	if (read(0, &c, 1) != 1)
		exit(0);
	switch (c) {

	case 'O':
		if (tape >= 0)
			(void) close(tape);
		gets(device); gets(mode);
if (debug) fprintf(debug, "rmtd: O %s %s\n", device, mode);
		tape = open(device, atoi(mode));
		if (tape < 0)
			goto ioerror;
		break;

	case 'C':
if (debug) fprintf(debug, "rmtd: C\n");
		gets(device);		/* discard */
		if (close(tape) < 0)
			goto ioerror;
		tape = -1;
		break;

	case 'L':
		gets(count); gets(pos);
if (debug) fprintf(debug, "rmtd: L %s %s\n", count, pos);
		rval = lseek(tape, (long) atoi(count), atoi(pos));
		if (rval < 0)
			goto ioerror;
		break;

	case 'W':
		gets(count);
		n = atoi(count);
if (debug) fprintf(debug, "rmtd: W %s\n", count);
		for (i = 0; i < n; i += cc) {
			cc = read(0, &record[i], n - i);
			if (cc <= 0) {
if (debug) fprintf(debug, "rmtd: premature eof\n");
				exit(1);
			}
		}
		rval = write(tape, record, n);
		if (rval < 0)
			goto ioerror;
		break;

	case 'R':
		gets(count);
if (debug) fprintf(debug, "rmtd: R %s\n", count);
		n = atoi(count);
		if (n > sizeof (record))
			n = sizeof (record);
		rval = read(tape, record, n);
		if (rval < 0)
			goto ioerror;
		(void) write(1, record, n);
		break;

	case 'I':
		gets(op); gets(count);
if (debug) fprintf(debug, "rmtd: I %s %s\n", op, count);
		{ struct mtop mtop;
		  mtop.mt_op = atoi(op);
		  mtop.mt_count = atoi(count);
		  if (ioctl(tape, MTIOCTOP, (char *)&mtop) < 0)
			goto ioerror;
		  rval = mtop.mt_count;
		}
		break;

	case 'S':		/* status */
if (debug) fprintf(debug, "rmtd: S\n");
		{ struct mtget mtget;
		  if (ioctl(tape, MTIOCGET, (char *)&mtget) < 0)
			goto ioerror;
		  rval = sizeof (mtget);
		  (void) write(1, (char *)&mtget, sizeof (mtget));
		  break;
		}

	default:
if (debug) fprintf(debug, "rmtd: garbage command %c\n", c);
		exit(1);
	}
	(void) sprintf(resp, "A%d\n", rval);
	(void) write(1, resp, strlen(resp));
	goto top;
ioerror:
	error(errno);
	goto top;
}

gets(bp)
	char *bp;
{
	int i;
	char *cp = bp;

	for (i = 0; i < SSIZE; i++) {
		if (read(0, cp+i, 1) != 1)
			exit(0);
		if (cp[i] == '\n')
			break;
	}
	cp[i] = '\0';
}

error(num)
	int num;
{

	(void) sprintf(resp, "E%d\n%s\n", num, sys_errlist[num]);
	(void) write(1, resp, strlen (resp));
}
