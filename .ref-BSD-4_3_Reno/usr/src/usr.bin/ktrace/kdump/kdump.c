/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)kdump.c	1.9 (Berkeley) 6/29/90";
#endif /* not lint */

#include <sys/ioctl.h>
#include <vis.h>
#define KERNEL
#include <errno.h>
#undef KERNEL
#include "ktrace.h"

int timestamp, decimal, fancy = 1, tail, maxdata;
char *tracefile = DEF_TRACEFILE;
struct ktr_header ktr_header;
int size = 1024;	/* initial size - grow as needed */

#define USAGE	\
	"usage: kdump [-dnlT] [-t trops] [-f trfile] [-m maxdata]\n\
	trops: c = syscalls, n = namei, g = generic-i/o, a = everything\n"

#define eqs(s1, s2)	(strcmp((s1), (s2)) == 0)

main(argc, argv)
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	int ch, ktrlen;
	register char *m;
	int trpoints = ALL_POINTS;

	while ((ch = getopt(argc,argv,"t:f:dnlTRm:")) != EOF)
		switch((char)ch) {
		case 't':
			trpoints = getpoints(optarg);
			if (trpoints < 0) {
				fprintf(stderr, 
				     "kdump: unknown trace point in %s\n",
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
			tail = 1;
			break;
		case 'T':
			timestamp = 1;
			break;
		case 'R':
			timestamp = 2;	/* relative timestamp */
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
	m = (char *)malloc(size+1);
	if (m == NULL) {
		fprintf(stderr, "kdump: ain't gots no memory\n");
		exit(1);
	}
	while (fread_tail(&ktr_header, sizeof(struct ktr_header), 1,
	       stdin, tail)) {
		if (trpoints & (1<<ktr_header.ktr_type))
			dumpheader(&ktr_header);
		if ((ktrlen = ktr_header.ktr_len) < 0) {
			fprintf(stderr, "kdump: bogus length 0x%x\n", 
				ktrlen);
			exit(1);
		}
		if (ktrlen > size) {
			m = (char *)realloc(m, ktrlen+1);
			if (m == NULL) {
				fprintf(stderr,"kdump: out of memory\n");
				exit(1);
			}
			size = ktrlen;
		}
		if (ktrlen && fread_tail(m, ktrlen, 1, stdin, tail) == 0) {
			fprintf(stderr, "kdump: data too short\n");
			exit(1);
		}
		if ((trpoints & (1<<ktr_header.ktr_type)) == 0)
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
		case KTR_PSIG:
			ktrpsig((struct ktr_psig *)m, ktrlen);
			break;
		}
		if (tail)
			fflush(stdout);
	}
}

fread_tail(buf, size, num, stream, tail)
	char *buf;
	FILE *stream;
{
	int i;

	while ((i = fread(buf, size, num, stream)) == 0 && tail) {
		sleep(1);
		clearerr(stream);
	}
	return (i);
}

dumpheader(kth)
	struct ktr_header *kth;
{
	static char unknown[64];
	static struct timeval prevtime, temp;
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
	case KTR_PSIG:
		type = "PSIG";
		break;
	default:
		sprintf(unknown, "UNKNOWN(%d)", kth->ktr_type);
		type = unknown;
	}

	printf("%6d %-8s ",
		kth->ktr_pid, kth->ktr_comm);
	if (timestamp) {
		if (timestamp == 2) {
			temp = kth->ktr_time;
			timevalsub(&kth->ktr_time, &prevtime);
			prevtime = temp;
		}
		printf("%d.%06d ", kth->ktr_time.tv_sec, kth->ktr_time.tv_usec);
	}
	printf("%s  ", type);
}

#include <sys/syscall.h>
#define KTRACE
#include "/sys/kern/syscalls.c"
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
	register int ret = ktr->ktr_retval;
	register int error = ktr->ktr_error;
	register int code = ktr->ktr_code;

	if (code >= nsyscalls || code < 0)
		printf("[%d] ", code);
	else
		printf("%s ", syscallnames[code]);

	if (error == 0) {
		if (fancy) {
			printf("%d", ret);
			if (ret < 0 || ret > 9)
				printf("/%#x", ret);
		} else {
			if (decimal)
				printf("%d", ret);
			else
				printf("%#x", ret);
		}
	} else if (error == ERESTART)
		printf("RESTART");
	else if (error == EJUSTRETURN)
		printf("JUSTRETURN");
	else {
		printf("-1 errno %d", ktr->ktr_error);
		if (fancy)
			printf(" %s", strerror(ktr->ktr_error));
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
	register int datalen = len - sizeof (struct ktr_genio);
	register char *dp = (char *)ktr + sizeof (struct ktr_genio);
	register char *cp;
	register int col = 0;
	register char c;
	register width;
	char visbuf[5];
	static screenwidth = 0;

	if (screenwidth == 0) {
		struct winsize ws;

		if (fancy && ioctl(fileno(stderr), TIOCGWINSZ, &ws) != -1 &&
		    ws.ws_col > 8)
			screenwidth = ws.ws_col;
		else
			screenwidth = 80;
	}
	printf("fd %d %s %d bytes\n", ktr->ktr_fd,
		ktr->ktr_rw == UIO_READ ? "read" : "wrote", datalen);
	if (maxdata && datalen > maxdata)
		datalen = maxdata;
	printf("       \"");
	col = 8;
	for (;datalen > 0; datalen--, dp++) {
		(void) vis(visbuf, *dp, VIS_CSTYLE, *(dp+1));
		cp = visbuf;
		/*
		 * Keep track of printables and
		 * space chars (like fold(1)).
		 */
		if (col == 0) {
			putchar('\t');
			col = 8;
		}
		switch(*cp) {
		case '\n':
			col = 0;
			putchar('\n');
			continue;
		case '\t':
			width = 8 - (col&07);
			break;
		default:
			width = strlen(cp);
		}
		if (col + width > (screenwidth-2)) {
			printf("\\\n\t");
			col = 8;
		}
		col += width;
		do {
			putchar(*cp++);
		} while (*cp);
	}
	if (col == 0)
		printf("       ");
	printf("\"\n");
}


char *signames[] = {
	"NULL", "HUP", "INT", "QUIT", "ILL", "TRAP", "IOT",	/*  1 - 6  */
	"EMT", "FPE", "KILL", "BUS", "SEGV", "SYS",		/*  7 - 12 */
	"PIPE", "ALRM",  "TERM", "URG", "STOP", "TSTP",		/* 13 - 18 */
	"CONT", "CHLD", "TTIN", "TTOU", "IO", "XCPU",		/* 19 - 24 */
	"XFSZ", "VTALRM", "PROF", "WINCH", "29", "USR1",	/* 25 - 30 */
	"USR2", NULL,						/* 31 - 32 */
};

ktrpsig(psig, len)
	struct ktr_psig *psig;
{
	printf("SIG%s ", signames[psig->signo]);
	if (psig->action == SIG_DFL)
		printf("SIG_DFL\n");
	else {
		printf("caught handler=0x%x mask=0x%x code=0x%x\n",
			psig->action, psig->mask, psig->code);
	}
}
