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
static char sccsid[] = "@(#)dmesg.c	5.9 (Berkeley) %G%";
#endif /* not lint */

#include <sys/cdefs.h>
#include <sys/msgbuf.h>
#include <time.h>
#include <nlist.h>
#include <kvm.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

struct nlist nl[] = {
#define	X_MSGBUF	0
	{ "_msgbuf" },
	{ NULL },
};

void usage(), vputc();
void err __P((const char *, ...));

main(argc, argv)
	int argc;
	char **argv;
{
	register int ch, newl, skip;
	register char *p, *ep;
	struct msgbuf cur;
	char *core, *namelist;

	core = namelist = NULL;
	while ((ch = getopt(argc, argv, "M:N:")) != EOF)
		switch(ch) {
		case 'M':
			core = optarg;
			break;
		case 'N':
			namelist = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/* Read in kernel message buffer, do sanity checks. */
	if (kvm_openfiles(namelist, core, NULL) == -1)
		err("kvm_openfiles: %s", kvm_geterr());
	if (kvm_nlist(nl) == -1)
		err("kvm_nlist: %s", kvm_geterr());
	if (nl[X_MSGBUF].n_type == 0)
		err("msgbuf not found namelist");

        kvm_read((void *)nl[X_MSGBUF].n_value, (void *)&cur, sizeof(cur));
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
		newl = (ch = *p) == '\n';
		vputc(ch);
	}
	if (!newl)
		(void)putchar('\n');
	exit(0);
}

void
vputc(ch)
	register int ch;
{
	int meta;

	if (!isascii(ch)) {
		(void)putchar('M');
		(void)putchar('-');
		ch = toascii(ch);
		meta = 1;
	} else
		meta = 0;
	if (isprint(ch) || !meta && (ch == ' ' || ch == '\t' || ch == '\n'))
		(void)putchar(ch);
	else {
		(void)putchar('^');
		(void)putchar(ch == '\177' ? '?' : ch | 0100);
	}
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
