/*-
 * Copyright (c) 1988, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)Tesis.c	7.1 (Berkeley) 5/7/91";
#endif /* not lint */

/* Modifications
 * Copyright (c) 1990 Robert Hagens
 */

/*
 * This is a test program to listen to esis packets.
 */
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <net/route.h>
#include <net/if.h>
#define  TCPT_NTIMERS 4
#include <netiso/iso.h>
#include <netiso/tp_param.h>
#include <netiso/tp_user.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>


#define dbprintf if(verbose)printf
#define try(a,b,c) {x = (a b); dbprintf("%s%s returns %d\n",c,"a",x);\
		if(x<0) {perror("a"); myexit(0);}}


struct  ifreq ifr;
short port = 3000;
struct  sockaddr_iso faddr, laddr = { sizeof(laddr), AF_ISO };
struct  sockaddr_iso *siso = &laddr;
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
	tisink();
}
#define BIG 2048
#define MIDLIN 512
char readbuf[BIG];
struct iovec iov[1] = {
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
	int x, s, pid, on = 1, loop = 0, n, fromlen, flags = 0;
	extern int errno;

	try(socket, (AF_ISO, SOCK_DGRAM, ISOPROTO_ESIS),"");
	s = x;

	for(;;) {
		fromlen = sizeof(name);
		try(recvfrom, (s, readbuf, sizeof(readbuf), flags,
			name, &fromlen), "");
		n = x;
		dumpit("connection from:", name, fromlen);
		dumpit("packet is:", readbuf, n);
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
