/*
 * Copyright (c) 1989, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */
#ifndef lint
static char sccsid[] = "%W% (Berkeley) %G%";
#endif /* not lint */

/*
 * This is a test program to be a sink for ISO packets.
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

long size, count = 10, forkp, confp, echop, mynamep, verbose = 1, playtag = 0;
long records, dgramp = 1;

char buf[2048];
char your_it[] = "You're it!";

char *Servername;

main(argc, argv)
int argc;
char *argv[];
{
	register char **av = argv;
	register char *cp;
	struct iso_addr iso_addr();

	while(--argc > 0) {
		av++;
		if(strcmp(*av,"Servername")==0) {
			av++;
			Servername = *av;
			argc--;
		} else if (strcmp(*av,"host")==0) {
			av++;
			laddr.siso_addr = iso_addr(*av);
			argc--;
		} else if (strcmp(*av,"count")==0) {
			av++;
			sscanf(*av,"%ld",&count);
			argc--;
		} else if (strcmp(*av,"port")==0) {
			av++;
			sscanf(*av,"%hd",&port);
			argc--;
		} else if (strcmp(*av,"size")==0) {
			av++;
			sscanf(*av,"%ld",&size);
			argc--;
		}
	}
	if (Servername) {
		int tlen = laddr.siso_tlen = strlen(Servername);
		int len =  TSEL(siso) + tlen - (caddr_t) &siso;
		if (len > sizeof(*siso)) {
			siso = (struct sockaddr_iso *)malloc(len);
			*siso = laddr;
			siso->siso_len = len;
		}
		bcopy(Servername, TSEL(siso), tlen);
	} else {
		port = htons(port);
		laddr.siso_tlen = sizeof(port);
		bcopy((char *)&port, TSEL(siso), sizeof(port));
	}
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
	int x, s, pid, on = 1, loop = 0, n, ns;
	extern int errno;
	int socktype = (dgramp ? SOCK_DGRAM : SOCK_SEQPACKET);
	int addrlen = sizeof(faddr);

	try(socket, (AF_ISO, socktype, 0),"");

	s = x;

	try(bind, (s, (struct sockaddr *) siso, siso->siso_len), "");

	/*try(setsockopt, (s, SOL_SOCKET, SO_DEBUG, &on, sizeof (on)), ""); */
	if (dgramp) {
		ns  =  s;
		pid = getpid();
		goto dgram1;
	}

	try(listen, (s, 5), "");
	for(;;) {
		int child;
		char childname[50];

		try (accept, (s, &faddr, &addrlen), "");
		ns = x;
		dumpit("connection from:", &faddr, sizeof faddr);
		if (mynamep) {
			addrlen = sizeof(faddr);
			try (getsockname, (ns, &faddr, &addrlen), "");
			dumpit("connected as:", &faddr, addrlen);
		}
		loop++;
		if(loop > 3) myexit(0);
		if (forkp) {
			try(fork, (), "");
		} else
			x = 0;
		pid = getpid();
		if (x == 0)  {
		    long n, count = 0, cn, flags;
		    records = 0;
		    if (confp) {
			msghdr.msg_iovlen = 0;
			msghdr.msg_namelen = 0;
			msghdr.msg_controllen = 
			    cbuf.cm.cmhdr.cmsg_len = sizeof (cbuf.cm.cmhdr);
			cbuf.cm.cmhdr.cmsg_level = SOL_TRANSPORT;
			cbuf.cm.cmhdr.cmsg_type = TPOPT_CFRM_DATA;
			n = sendmsg(ns, &msghdr, 0);
			if (n <= 0) {
				printf("confirm: errno is %d\n", errno);
				fflush(stdout);
				perror("Confirm error");
			} else {
				dbprintf("confim ok\n");
			}
			sleep(10);
		    }
		    for (;;) {
		    dgram1:
			msghdr.msg_iovlen = 1;
			msghdr.msg_controllen = sizeof(control);
			msghdr.msg_namelen = (dgramp ? (sizeof name) : 0);
			iov->iov_len = sizeof(readbuf);
			n = recvmsg(ns, &msghdr, 0);
			flags = msghdr.msg_flags;
			count++;
			dbprintf("recvmsg from pid %d got %d ctl %d flags %x\n",
				pid, n, (cn = msghdr.msg_controllen), flags);
			fflush(stdout);
			if (dgramp && msghdr.msg_namelen && verbose)
				dumpit("from:\n", name, msghdr.msg_namelen);
			if (cn && verbose)
				dumpit("control data:\n", control, cn);
			if (n <= 0) {
				fprintf(stderr, "errno is %d\n", errno);
				perror("recvmsg");
				/*sleep (10);*/
				break;
			} else {
				if (verbose)
					dumpit("data:\n", readbuf, n);
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
