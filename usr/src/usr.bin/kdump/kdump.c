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
static char sccsid[] = "@(#)kdump.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include "ktrace.h"

int timestamp, decimal, fancy = 1, loop, maxdata;
char *tracefile = DEF_TRACEFILE;
struct ktr_header ktr_header;
int size = 1024;	/* initial size of buffer - will grow as needed */

#define USAGE	\
	"usage: kdump [-dnlT] [-t facilitystring] [-f tracefile] [-m maxdata]\n\
	facilities: c = syscalls, n = namei, g = generic-i/o, a = everything\n"

#define eqs(s1, s2)	(strcmp((s1), (s2)) == 0)

main(argc, argv)
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	int ch, ktrlen;
	register char *m;
	int facs = DEF_FACS;

	while ((ch = getopt(argc,argv,"t:f:dnlTm")) != EOF)
		switch((char)ch) {
			case 't':
				facs = getfacs(optarg);
				if (facs < 0) {
					fprintf(stderr, 
					     "kdump: unknown facility in %s\n",
						optarg);
					exit(1);
				}
				break;
			case 'f':
				tracefile = optarg;
				break;
			case 'd':
				decimal = 1;
				break;
			case 'n':
				fancy = 0;
				break;
			case 'l':
				loop = 1;
				break;
			case 'T':
				timestamp = 1;
				break;
			case 'm':
				maxdata = atoi(optarg);
				break;
			default:
				fprintf(stderr, USAGE);
				exit(1);
		}
	argv += optind, argc -= optind;

	if (argc > 1) {
		fprintf(stderr, USAGE);
		exit(1);
	}
	if (!eqs(tracefile, "-")) {
		if (freopen(tracefile, "r", stdin) == NULL) {
			fprintf(stderr, "kdump: %s: ", tracefile);
			perror("");
			exit(1);
		}
	}
	m = (char *)malloc(size);
	if (m == NULL) {
		fprintf(stderr, "kdump: ain't gots no memory\n");
		exit(1);
	}
	while (myfread(&ktr_header, sizeof(struct ktr_header), 1, stdin)) {
		if (facs & (1<<ktr_header.ktr_type))
			dumpheader(&ktr_header);
		if ((ktrlen = ktr_header.ktr_len) > 80000) {	/* XXX */
			fprintf(stderr, "kdump: bogus length %d\n", 
				ktrlen);
			exit(1);
		}
		if (ktrlen > size) {
			m = (char *)realloc(m, ktrlen);
			if (m == NULL) {
				fprintf(stderr,"kdump: ain't gots no memory\n");
				exit(1);
			}
			size = ktrlen;
		}
		if (myfread(m, ktrlen, 1, stdin) == 0) {
			fprintf(stderr, "kdump: out of data\n");
			exit(1);
		}
		if ((facs & (1<<ktr_header.ktr_type)) == 0)
			continue;
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
		case KTR_GENIO:
			ktrgenio((struct ktr_genio *)m, ktrlen);
			break;
		}
		if (loop)
			fflush(stdout);
	}
}

myfread(buf, size, num, stream)
	char *buf;
	FILE *stream;
{
	int i;
again:
	if (i = fread(buf, size, num, stream))
		return (i);
	else {
		if (loop) {
			sleep(1);
			clearerr(stream);
			goto again;
		} else
			return 0;
	}
}

dumpheader(kth)
	struct ktr_header *kth;
{
	static char unknown[64];
	char *type;

	switch (kth->ktr_type) {
	case KTR_SYSCALL:
		type = "CALL";
		break;
	case KTR_SYSRET:
		type = "RET ";
		break;
	case KTR_NAMEI:
		type = "NAMI";
		break;
	case KTR_GENIO:
		type = "GIO ";
		break;
	default:
		sprintf(unknown, "UNKNOWN(%d)", kth->ktr_type);
		type = unknown;
	}

	printf("%6d %-8s ",
		kth->ktr_pid, kth->ktr_comm);
	if (timestamp)
		printf("%d.%d ", kth->ktr_time.tv_sec, kth->ktr_time.tv_usec);
	printf("%s  ", type);
}

#include <sys/syscall.h>
#define KTRACE
#include "/sys/sys/syscalls.c"
#undef KTRACE
int nsyscalls = sizeof (syscallnames) / sizeof (syscallnames[0]);

ktrsyscall(ktr, len)
	register struct ktr_syscall *ktr;
{
	register narg = ktr->ktr_narg;
	register int *ip;
	char *ioctlname();

	if (ktr->ktr_code >= nsyscalls || ktr->ktr_code < 0)
		printf("[%d]", ktr->ktr_code);
	else
		printf("%s", syscallnames[ktr->ktr_code]);
	ip = (int *)((char *)ktr + sizeof(struct ktr_syscall));
	if (narg) {
		char c = '(';
		if (fancy && ktr->ktr_code == SYS_ioctl) {
			char *cp;
			if (decimal)
				printf("(%d", *ip);
			else
				printf("(%#x", *ip);
			ip++; narg--;
			if ((cp = ioctlname(*ip)) != NULL)
				printf(",%s", cp);
			else {
				if (decimal)
					printf(",%d", *ip);
				else
					printf(",%#x ", *ip);
			}
			c = ',';
			ip++; narg--;
		}
		while (narg) {
			if (decimal)
				printf("%c%d", c, *ip);
			else
				printf("%c%#x", c, *ip);
			c = ',';
			ip++; narg--;
		}
		putchar(')');
	}
	putchar('\n');
}

ktrsysret(ktr, len)
	struct ktr_sysret *ktr;
{
	extern char *sys_errlist[];
	int ret = ktr->ktr_retval;

	if (ktr->ktr_code >= nsyscalls || ktr->ktr_code < 0)
		printf("[%d] ", ktr->ktr_code);
	else
		printf("%s ", syscallnames[ktr->ktr_code]);
	if (ktr->ktr_error) {
		printf("-1 errno %d", ktr->ktr_error);
		if (fancy)
			printf(" %s", sys_errlist[ktr->ktr_error]);
	} else {
		if (fancy) {
			printf("%d", ret);
			if (ret < 0 || ret > 9)
				printf(" %#x", ret);
		} else {
			if (decimal)
				printf("%d", ret);
			else
				printf("%#x", ret);
		}
	}
	putchar('\n');
}

ktrnamei(cp, len) 
	char *cp;
{
	printf("\"%.*s\"\n", len, cp);
}

ktrgenio(ktr, len)
	struct ktr_genio *ktr;
{
	int datalen = len - sizeof (struct ktr_genio);
	char *cp = (char *)ktr + sizeof (struct ktr_genio);
	register int col = 0;
	register char c;

	printf("fd %d %s %d bytes\n", ktr->ktr_fd,
		ktr->ktr_rw == UIO_READ ? "read" : "wrote", datalen);
	if (maxdata && datalen > maxdata)
		datalen = maxdata;
	for (;datalen > 0; datalen--, cp++) {
		c = *cp;

		if (col == 0) {
			putchar('\t');
			col = 1;
		}
		if (c == '\n' || c == '\t') {
			if (c == '\n')
				col = 0;
			putchar(c);
			continue;
		}
		if (c & 0200) {
			putchar('M');
			putchar('-');
			c &= 0177;
		}
		if (c < 040 || c == 0177) {
			putchar('^');
			if (c == 0177)
				putchar('?');
			else
				putchar(c+'@');
		} else
			putchar(c);
	}
	if (col != 0)
		putchar('\n');
}
