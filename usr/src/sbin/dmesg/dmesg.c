/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dmesg.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/cdefs.h>
#include <sys/msgbuf.h>
#include <fcntl.h>
#include <limits.h>
#include <time.h>
#include <nlist.h>
#include <kvm.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <vis.h>

struct nlist nl[] = {
#define	X_MSGBUF	0
	{ "_msgbufp" },
	{ NULL },
};

void err __P((const char *, ...));
void usage __P((void));

#define	KREAD(addr, var) \
	kvm_read(kd, addr, (void *)&var, sizeof(var)) != sizeof(var)

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int ch, newl, skip;
	register char *p, *ep;
	struct msgbuf *bufp, cur;
	char *memf, *nlistf;
	kvm_t *kd;
	char buf[_POSIX2_LINE_MAX];

	memf = nlistf = NULL;
	while ((ch = getopt(argc, argv, "M:N:")) != EOF)
		switch(ch) {
		case 'M':
			memf = optarg;
			break;
		case 'N':
			nlistf = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/*
	 * Discard setgid privileges if not the running kernel so that bad
	 * guys can't print interesting stuff from kernel memory.
	 */
	if (memf != NULL || nlistf != NULL)
		setgid(getgid());

	/* Read in kernel message buffer, do sanity checks. */
	buf[0] = 0;
	kd = kvm_open(nlistf, memf, NULL, O_RDONLY, buf);
	if (kd == NULL)
		err("kvm_open: %s", buf);
	if (kvm_nlist(kd, nl) == -1)
		err("kvm_nlist: %s", kvm_geterr(kd));
	if (nl[X_MSGBUF].n_type == 0)
		err("%s: msgbufp not found", nlistf ? nlistf : "namelist");
	if (KREAD(nl[X_MSGBUF].n_value, bufp) || KREAD((long)bufp, cur))
		err("kvm_read: %s", kvm_geterr(kd));
	kvm_close(kd);
	if (cur.msg_magic != MSG_MAGIC)
		err("magic number incorrect");
	if (cur.msg_bufx >= MSG_BSIZE)
		cur.msg_bufx = 0;

	/*
	 * The message buffer is circular; start at the read pointer, and
	 * go to the write pointer - 1.
	 */
	p = cur.msg_bufc + cur.msg_bufx;
	ep = cur.msg_bufc + cur.msg_bufx - 1;
	for (newl = skip = 0; p != ep; ++p) {
		if (p == cur.msg_bufc + MSG_BSIZE)
			p = cur.msg_bufc;
		ch = *p;
		/* Skip "\n<.*>" syslog sequences. */
		if (skip) {
			if (ch == '>')
				newl = skip = 0;
			continue;
		}
		if (newl && ch == '<') {
			skip = 1;
			continue;
		}
		if (ch == '\0')
			continue;
		newl = ch == '\n';
		(void) vis(buf, ch, 0, 0);
		if (buf[1] == 0)
			(void) putchar(buf[0]);
		else
			(void) fputs(buf, stdout);
	}
	if (!newl)
		(void)putchar('\n');
	exit(0);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "dmesg: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}

void
usage()
{
	(void)fprintf(stderr, "usage: dmesg [-M core] [-N system]\n");
	exit(1);
}
