/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kdump.c	1.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/dir.h>
/*#include <time.h>*/
#include <sys/user.h>
#include <sys/ktrace.h>
#include <stdio.h>

#define FLG_TIME	0x1
#define FLG_PERSIST	0x2
int flags;

#define DEF_TRACEFILE	"trace.out"
char *tracefile;

struct ktr_header ktr_header;
int size = 1024;

#define eqs(s1, s2)	(strcmp((s1), (s2)) == 0)

main(argc, argv)
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	int ch, ktrlen;
	register char *m;

	while ((ch = getopt(argc,argv,"tf")) != EOF)
		switch((char)ch) {
			case 't':
				flags |= FLG_TIME;
				break;
			case 'f':
				flags |= FLG_PERSIST;
			default:
				fprintf(stderr,"usage: \n",*argv);
				exit(1);
		}
	argv += optind, argc -= optind;

	if (argc > 1) {
		fprintf(stderr, "kdump: usage\n");
		exit(1);
	}
	tracefile = argc ? argv[0] : DEF_TRACEFILE;
	if (!eqs(tracefile, "-")) {
		if (freopen(tracefile, "r", stdin) == NULL) {
			fprintf(stderr, "kdump: %s:", tracefile);
			perror("");
			exit(1);
		}
	}
	m = (char *)malloc(size);
	if (m == NULL) {
		fprintf(stderr, "out of money\n");
		exit(1);
	}
	while (fread(&ktr_header, sizeof(struct ktr_header), 1, stdin)) {
		dumpheader(&ktr_header);
		if ((ktrlen = ktr_header.ktr_len) > 30000) {	/* XXX */
			fprintf(stderr, "bogus length %d\n", 
				ktrlen);
			exit(1);
		}
		if (ktrlen > size) {
			m = (char *)realloc(m, ktrlen);
			if (m == NULL) {
				fprintf(stderr, "out of money\n");
				exit(1);
			}
			size = ktrlen;
		}
		if (fread(m, ktrlen, 1, stdin) == 0) {
			fprintf(stderr, "kdump: out of data\n");
			exit(1);
		}
		switch (ktr_header.ktr_type) {
		case KTR_SYSCALL:
			ktrsyscall((struct ktr_syscall *)m, ktrlen);
			break;
		case KTR_SYSRET:
			ktrsysret((struct ktr_sysret *)m, ktrlen);
			break;
		case KTR_NAMEI:
			ktrnamei(m, ktrlen);
			break;
		}
	}
}

dumpheader(kth)
	struct ktr_header *kth;
{
	static char unknown[64];
	char *type;

	switch (kth->ktr_type) {
	case KTR_SYSCALL:
		type = "C";
		break;
	case KTR_SYSRET:
		type = "R";
		break;
	case KTR_NAMEI:
		type = "N";
		break;
	default:
		sprintf(unknown, "UNKNOWN %d", kth->ktr_type);
		type = unknown;
	}

	printf("%s %d (%s) ",
		type, kth->ktr_pid, kth->ktr_comm);
	if (flags&FLG_TIME)
		printf("%d.%d ", kth->ktr_time.tv_sec, kth->ktr_time.tv_usec);
}

#include "/sys/sys/syscalls.c"
int nsyscalls = sizeof (syscallnames) / sizeof (syscallnames[0]);

ktrsyscall(ktr, len)
	register struct ktr_syscall *ktr;
{
	register narg = ktr->ktr_narg;
	register int *ip;
	register char c = '(';

	if (ktr->ktr_code >= nsyscalls || ktr->ktr_code < 0)
		printf("%d ", ktr->ktr_code);
	else
		printf("%s ", syscallnames[ktr->ktr_code]);
	ip = (int *)((char *)ktr + sizeof(struct ktr_syscall));
	while (narg) {
		printf("%c%x", c, *ip);
		ip++; narg--; c = ',';
	}
	if (ktr->ktr_narg)
		putchar(')');
	putchar('\n');
}

ktrsysret(ktr, len)
	struct ktr_sysret *ktr;
{
	extern char *sys_errlist[];

	if (ktr->ktr_code >= nsyscalls || ktr->ktr_code < 0)
		printf("%d ", ktr->ktr_code);
	else
		printf("%s ", syscallnames[ktr->ktr_code]);
	if (ktr->ktr_error)
		printf("-1 (%d) %s", ktr->ktr_error,
			sys_errlist[ktr->ktr_error]);
	else
		printf("%d (0x%x)", ktr->ktr_retval, ktr->ktr_retval);
	putchar('\n');
}

ktrnamei(cp, len) {
	printf("%.*s\n", len, cp);
}
