/* @(#)cpnull.c	4.1 82/03/05 */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <stat.h>
#include <wellknown.h>
#include <sys/ioctl.h>
#include <net/in.h>
/*
 * cpnull machine blocks
 */
int	rem;
char	*index(), *rindex(), *malloc();
int	errs;
int	broken();
int	lostconn();

int	errno;
char	sys_errlist[];
int	vflag;

main(argc, argv)
	int argc;
	char **argv;
{
	char *host, *remotedir;
	int x, i, f;
	char buf[BUFSIZ];
	int blocks;
	time_t then, now;

	argc--, argv++;
	if (argc > 0 && !strcmp(*argv, "-r"))
		remote();
	if (argc > 0 && !strcmp(*argv, "-v"))
		vflag++, argc--, argv++;
	if (argc < 2) {
		fprintf(stderr, "usage: cpnull machine blocks\n");
		exit(1);
	}
	signal(SIGPIPE, broken);
	host = *argv++; argc--;
	rem = rexec(&host, IPPORT_SHELLSERVER, "cpnull -r", 0, 0);
	if (rem < 0)
		exit(1);
	blocks = atoi(*argv);
	sprintf(buf, "C%d\n", blocks * BUFSIZ);
	write(rem, buf, strlen(buf));
	if (response() < 0)
		exit(1);
	printf("sending %d blocks\n", blocks);
	time(&then);
	for (i = 0; i < blocks; i++)  {
		if (vflag && (i % 100 == 0))
			printf("%d\n", i);
		write(rem, buf, BUFSIZ);
	}
	(void) response();
	time(&now);
	if (then == now)
		printf("< 1 second to transfer\n");
	else
		printf("%d bytes %d seconds %d baud\n",
		    blocks * BUFSIZ, now - then,
		    (blocks * BUFSIZ * 8) / (now - then));
	exit(errs);
}

response()
{
	char resp, c, rbuf[BUFSIZ], *cp = rbuf;

	if (read(rem, &resp, 1) != 1) {
		printf("Response read != 1\n");
		lostconn();
	}
	switch (resp) {

	case 0:
		return (0);

	default:
		*cp++ = resp;
		/* fall into... */
	case 1:
	case 2:
		do {
			if (read(rem, &c, 1) != 1) {
				printf("response loop != 1\n");
				write(2, rbuf, cp-rbuf);
				write(2, "\n", 1);
				lostconn();
			}
			*cp++ = c;
		} while (cp < &rbuf[BUFSIZ] && c != '\n');
		write(2, rbuf, cp - rbuf);
		if (resp == 1)
			return (-1);
		exit(1);
	}
}

broken()
{

	fprintf(stderr, "SIGPIPE.\n");
}

lostconn()
{

	fprintf(stderr, "Lost connection.\n");
	exit(1);
}

remote()
{
	char namebuf[BUFSIZ], buf[BUFSIZ], *name, *cp, c;
	int i, size;
	char *whopp;
#define	SCREWUP(str)	{ whopp = str; goto screwup; }

	cp = namebuf;
	if (read(0, cp, 1) <= 0)
		exit(0);
	if (*cp++ == '\n')
		SCREWUP("unexpected '\\n'");
	do
		if (read(0, cp, 1) != 1)
			exit(1);
	while (*cp++ != '\n');
	*--cp = 0;
	cp = namebuf;
	if (*cp++ != 'C')
		SCREWUP("expected control record");
	size = 0;
	while (*cp >= '0' && *cp <= '9')
		size = size * 10 + (*cp++ - '0');
	if (*cp++ != 0)
		SCREWUP("size not delimited");
	write(rem, "\0", 1);
	for (i = 0; i < size; i += BUFSIZ) {
		int amt = BUFSIZ;
		char *cp = buf;

		if (i + amt > size)
			amt = size - i;
		do {
			int j = read(0, cp, amt);

			if (j <= 0)
				exit(1);
			amt -= j;
			cp += j;
		} while (amt > 0);
		amt = BUFSIZ;
		if (i + amt > size)
			amt = size - i;
	}
	write(rem, "\0", 1);
	exit(0);
perr:
	sprintf(buf, "\01%s: %s\n", cp, sys_errlist[errno]);
	write(rem, buf, strlen(buf));
	exit(1);
screwup:
	sprintf(buf, "\02Procotol screwup: %s.\n", whopp);
	write(rem, buf, strlen(buf));
	exit(1);
}
