/* @(#)cpto.c	4.1 82/03/05 */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <wellknown.h>
#include <sys/ioctl.h>
#include <net/in.h>
/*
 * cpto machine file ... directory
 */
int	rem;
char	*index(), *rindex(), *malloc();
int	errs;
int	lostconn();

int	errno;
char	*sys_errlist[];

main(argc, argv)
	int argc;
	char **argv;
{
	char *host, *remotedir;
	int x, i, f;
	char buf[BUFSIZ];

	argc--, argv++;
	if (argc > 0 && !strcmp(*argv, "-r"))
		remote();
	if (argc < 3) {
		fprintf(stderr, "usage: cpto machine file... remotedir\n");
		exit(1);
	}
	signal(SIGPIPE, lostconn);
	remotedir = argv[argc-1]; argc--;
	host = *argv++; argc--;
	rem = rexec(&host, IPPORT_SHELLSERVER, "cpto -r", 0, 0);
	if (rem < 0)
		exit(1);
	for (x = 0; x < argc; x++) {
		char *name, *last, *dest;
		struct stat stb;
		char buf[1024];

		name = argv[x];
		f = open(name, 0);
		if (f < 0) {
			perror(name);
			continue;
		}
		if (fstat(f, &stb) < 0 ||
		    (stb.st_mode&S_IFMT) != S_IFREG) {
			fprintf(stderr, "%s: not a plain file\n", name);
			close(f);
			continue;
		}
		last = rindex(name, '/');
		if (last == 0)
			last = name;
		else
			last++;
		sprintf(buf, "C%3o %d %s/%s\n",
		  stb.st_mode&0777, stb.st_size, remotedir, last);
		write(rem, buf, strlen(buf));
		if (response() < 0) {
			close(f);
			continue;
		}
		for (i = 0; i < stb.st_size; i += BUFSIZ) {
			int amt = BUFSIZ;
			if (i + amt > stb.st_size)
				amt = stb.st_size - i;
			if (read(f, buf, amt) != amt) {
				fprintf(stderr, "%s: file changed size.\n", *argv);
				exit(1);
			}
			write(rem, buf, amt);
		}
		close(f);
		(void) response();
	}
	exit(errs);
}

response()
{
	char resp, c, rbuf[BUFSIZ], *cp = rbuf;

	if (read(rem, &resp, 1) != 1)
		lostconn();
	switch (resp) {

	case 0:
		return (0);

	default:
		*cp++ = resp;
		/* fall into... */
	case 1:
	case 2:
		do {
			if (read(rem, &c, 1) != 1)
				lostconn();
			*cp++ = c;
		} while (cp < &rbuf[BUFSIZ] && c != '\n');
		write(2, rbuf, cp - rbuf);
		if (resp == 1)
			return (-1);
		exit(1);
	}
}

lostconn()
{

	fprintf(stderr, "Lost connection.\n");
	exit(1);
}

remote()
{
	char namebuf[BUFSIZ], buf[BUFSIZ], *name, *cp, c;
	int of, mode, i, size, errs;
	char *whopp;
#define	SCREWUP(str)	{ whopp = str; goto screwup; }

	for (;;) {
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
		mode = 0;
		for (; cp < namebuf+4; cp++) {
			if (*cp < '0' || *cp > '7')
				SCREWUP("bad mode");
			mode = (mode << 3) | (*cp - '0');
		}
		if (*cp++ != ' ')
			SCREWUP("mode not delimited");
		size = 0;
		while (*cp >= '0' && *cp <= '9')
			size = size * 10 + (*cp++ - '0');
		if (*cp++ != ' ')
			SCREWUP("size not delimited");
		name = cp;
		of = creat(name, mode);
		if (of < 0)
			goto perr;
		errs = 0;
		write(rem, "\0", 1);
		for (i = 0; i < size; i += BUFSIZ) {
			int amt = BUFSIZ;
			char *cp = buf;
			struct stat stb;

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
			if (write(of, buf, amt) != amt)
				errs++;
		}
		close(of);
		if (errs == 0) {
			write(rem, "\0", 1);
			continue;
		}
perr:
		sprintf(buf, "\01%s: %s\n", cp, sys_errlist[errno]);
		write(rem, buf, strlen(buf));
	}
screwup:
	sprintf(buf, "\02Procotol screwup: %s.\n", whopp);
	write(rem, buf, strlen(buf));
	exit(1);
}
