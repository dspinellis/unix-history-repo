/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
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
static char sccsid[] = "@(#)xi_sink.c	7.2 (Berkeley) 11/13/90";
#endif /* not lint */

/*
 * This is a test program to be a sink for TP4 connections.
 */
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/route.h>
#include <net/if.h>
#include <netccitt/x25.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>


#define dbprintf if(verbose)printf
#define try(a,b,c) {x = (a b); dbprintf("%s%s returns %d\n",c,"a",x);\
		if(x<0) {perror("a"); myexit(0);}}


struct  ifreq ifr;
short port = 3000;
struct  sockaddr_x25 faddr, laddr = { sizeof(laddr), AF_CCITT };
struct  sockaddr_x25 *sx25 = &laddr;
char **xenvp;

long size, count = 10, forkp, confp, echop, mynamep, verbose = 1, playtag = 0;
long records, intercept = 0, isode_mode;

char buf[2048];
char your_it[] = "You're it!";

char *Servername;

main(argc, argv, envp)
int argc;
char *argv[];
char *envp[];
{
	register char **av = argv;
	register char *cp;

	xenvp = envp;
	while(--argc > 0) {
		av++;
		if (strcmp(*av,"host")==0) {
			av++;
			ccitt_addr(*av, sx25);
			argc--;
		} else if (strcmp(*av,"count")==0) {
			av++;
			sscanf(*av,"%ld",&count);
			argc--;
		} else if (strcmp(*av,"size")==0) {
			av++;
			sscanf(*av,"%ld",&size);
			argc--;
		} else if (strcmp(*av, "intercept")==0) {
			intercept++;
		}
	}
	tisink();
}
#define BIG 2048
#define MIDLIN 512
char readbuf[BIG];
char x25info[1];
struct iovec iov[2] = {
	x25info,
	sizeof x25info,
	readbuf,
	sizeof readbuf,
};
char name[MIDLIN];
union {
    struct {
	    struct cmsghdr	cmhdr;
	    char		cmdata[128 - sizeof(struct cmsghdr)];
    } cm;
    char data[128];
} cbuf;
#define control cbuf.data
struct msghdr msghdr = {
	name, sizeof(name),
	iov, sizeof(iov)/sizeof(iov[1]),
	control, sizeof control,
	0 /* flags */
};

tisink()
{
	int x, s, pid, on = 1, loop = 0, n;
	extern int errno;

	try(socket, (AF_CCITT, SOCK_STREAM, 0),"");

	s = x;

	sx25->x25_opts.op_flags |= X25_MQBIT;
	try(bind, (s, (struct sockaddr *) sx25, sx25->x25_len), "");

	/*try(setsockopt, (s, SOL_SOCKET, SO_DEBUG, &on, sizeof (on)), ""); */

	try(listen, (s, 5), "");
	for(;;) {
		int child, ns;
		int addrlen = sizeof(faddr);
		char childname[50];

		try (accept, (s, &faddr, &addrlen), "");
		ns = x;
		dumpit("connection from:", &faddr, sizeof faddr);
		if (mynamep || intercept) {
			addrlen = sizeof(faddr);
			try (getsockname, (ns, &faddr, &addrlen), "");
			dumpit("connected as:", &faddr, addrlen);
		}
		loop++;
		if (loop > 3) myexit(0);
		if (forkp) {
			try(fork, (), "");
		} else
			x = 0;
		if (x == 0)  {
		    long n, count = 0, cn, flags;
		    records = 0;
#ifdef ISODE_MODE
		    if (isode_mode) {
			static char fdbuf[10];
			static char *nargv[4] =
			    {"/usr/sbin/isod.tsap", fdbuf, "", 0};
			sprintf(fdbuf, "Z%d", ns);
			old_isod_main(3, nargv, xenvp);
		    } else
#endif
		    for (;;) {
			msghdr.msg_iovlen = 1;
			msghdr.msg_controllen = sizeof(control);
			iov[0].iov_len = sizeof(x25info);
			iov[1].iov_len = sizeof(readbuf);
			/* n = recvmsg(ns, &msghdr, 0); */
			n = readv(ns, iov, 2);
			flags = msghdr.msg_flags;
			count++;
			dbprintf("recvmsg from child %d got %d ctl %d x25info %x\n",
				    getpid(), n, (cn = msghdr.msg_controllen),
					x25info[0]);
			fflush(stdout);
			/* if (cn && verbose)
				dumpit("control data:\n", control, cn); */
			if (n < 0) {
				fprintf(stderr, "errno is %d\n", errno);
				perror("recvmsg");
				/*sleep (10);*/
				break;
			} else {
				if (verbose)
					dumpit("data:\n", readbuf, n - 1);
			}
			if (echop) {
				n = answerback(flags, n, ns);
			}
			if (flags & MSG_EOR)
				records++;
			if (playtag && n == sizeof(your_it) && (flags & MSG_EOR)
			    && bcmp(readbuf, your_it, n) == 0) {
				printf("Answering back!!!!\n");
				answerback(flags, n, ns);
				answerback(flags, n, ns);
			}
			errno = 0;
		    }
		}
		myexit(0);
	}
}
answerback(flags, n, ns)
{
	msghdr.msg_controllen = 0;
	msghdr.msg_iovlen = 1;
	iov->iov_len = n;
	n = sendmsg(ns, &msghdr, flags);
	dbprintf("echoed %d\n", n);
	return n;
}

dumpit(what, where, n)
char *what; unsigned short *where; int n;
{
	unsigned short *s = where;
	unsigned short *z = where + (n+1)/2;
	int count = 0;
	printf(what);
	while(s < z) {
		count++;
		printf("%x ",*s++);
		if ((count & 15) == 0)
			putchar('\n');
	}
	if (count & 15)
		putchar('\n');
	fflush(stdout);
}
myexit(n)
{
	fflush(stderr);
	printf("got %d records\n", records);
	fflush(stdout);
	exit(n);
}
