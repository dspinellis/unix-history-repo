/*-
 * Copyright (c) 1988, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988, 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tisink.c	7.11 (Berkeley) %G%";
#endif /* not lint */

/*
 * This is a test program to be a sink for ISO packets.
 */
#include <unistd.h>
#include <sys/param.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
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
#ifdef __STDC__
#define try(a,b,c) {x = (a b); dbprintf("%s%s returns %d\n",c,#a,x);\
		if (x<0) {perror(#a); myexit(0);}}
#else
#define try(a,b,c) {x = (a b); dbprintf("%s%s returns %d\n",c,"a",x);\
		if (x<0) {perror("a"); myexit(0);}}
#endif


struct  ifreq ifr;
short port = 3000;
struct  sockaddr_iso faddr, laddr = { sizeof(laddr), AF_ISO };
struct  sockaddr_iso *siso = &laddr;
char **xenvp;

long size, forkp = 0, confp = 0, mynamep, verbose = 1, echop = 0;
long records, intercept = 0, isode_mode = 0, dgramp = 0, tp0mode = 0;
long dumpnodata = 0, playtag = 0, select_mode = 0, tuba = 0;
void savedata();

char buf[2048];
char your_it[] = "You're it!";
fd_set readfds, exceptfds;

char *Servername;

main(argc, argv, envp)
int argc;
char *argv[];
char *envp[];
{
	register char **av = argv;
	register char *cp;
	struct iso_addr *iso_addr();

	xenvp = envp;
	while(--argc > 0) {
		av++;
		if(strcmp(*av,"Servername")==0) {
			av++;
			Servername = *av;
			argc--;
		} else if (strcmp(*av,"host")==0) {
			av++;
			laddr.siso_addr = *iso_addr(*av);
			argc--;
		} else if (strcmp(*av,"port")==0) {
			av++;
			sscanf(*av,"%hd",&port);
			argc--;
		} else if (strcmp(*av,"size")==0) {
			av++;
			sscanf(*av,"%ld",&size);
			argc--;
		} else if (strcmp(*av, "echo")==0) {
			echop++;
		} else if (strcmp(*av, "intercept")==0) {
			intercept++;
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
	int socktype = (dgramp ? SOCK_DGRAM :
			(tuba ? SOCK_STREAM :SOCK_SEQPACKET));
	int proto = (tp0mode ? ISOPROTO_TP0 : (tuba ? ISOPROTO_TCP : 0 ));
	int addrlen = sizeof(faddr);

	try(socket, (AF_ISO, socktype, proto),"");

	s = x;

	try(bind, (s, (struct sockaddr *) siso, siso->siso_len), "");

	/*try(setsockopt, (s, SOL_SOCKET, SO_DEBUG, &on, sizeof (on)), ""); */
	if (dgramp) {
		ns  =  s;
		goto dgram1;
	}

	try(listen, (s, 5), "");
	if (intercept) {
	    try(setsockopt,
		(s, SOL_TRANSPORT, TPOPT_INTERCEPT, &on, sizeof(on)), "");
	}
	for(;;) {
		int child;
		char childname[50];

		try (accept, (s, (struct sockaddr *)&faddr, &addrlen), "");
		ns = x;
		dumpit("connection from:", &faddr, sizeof faddr);
		if (mynamep || intercept) {
			addrlen = sizeof(faddr);
			try (getsockname,
			      (ns, (struct sockaddr *)&faddr, &addrlen), "");
			dumpit("connected as:", &faddr, addrlen);
		}
		loop++;
		if(loop > 3) myexit(0);
		if (forkp) {
			try(fork, (), "");
		} else
			x = 0;
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
			if (n < 0) {
				printf("confirm: errno is %d\n", errno);
				fflush(stdout);
				perror("Confirm error");
			} else {
				dbprintf("confim ok\n");
			}
			sleep(3);
		    }
#ifdef ISODE_MODE
		    if (isode_mode) {
			static char fdbuf[10];
			static char *nargv[4] =
			    {"/usr/sbin/isod.tsap", fdbuf, "", 0};
			sprintf(fdbuf, "Z%d", ns);
			old_isod_main(3, nargv, xenvp);
			myexit(0);
		    }
#endif
		    for (;;) {
		    dgram1:
			msghdr.msg_iovlen = 1;
			msghdr.msg_controllen = sizeof(control);
			msghdr.msg_namelen = (dgramp ? (sizeof name) : 0);
			iov->iov_len = sizeof(readbuf);
			if (select_mode)
			    sel_recvwait(ns);
			n = recvmsg(ns, &msghdr, 0);
			flags = msghdr.msg_flags;
			count++;
			dbprintf("recvmsg from child %d got %d ctl %d flags %x\n",
				getpid(), n, (cn = msghdr.msg_controllen), flags);
			fflush(stdout);
			if (dgramp && msghdr.msg_namelen && verbose)
				dumpit("from:\n", name, msghdr.msg_namelen);
			if (cn && verbose)
				dumpit("control data:\n", control, cn);
			if (n < 0) {
				fprintf(stderr, "errno is %d\n", errno);
				perror("recvmsg");
				/*sleep (10);*/
				break;
			} else {
				if (verbose)
					dumpit("data:\n", readbuf, n);
			}
			if (echop)
				savedata(n, flags);
			if (flags & MSG_EOR) {
				records++;
				if (echop)
					answerback(ns);
			}
			errno = 0;
		    }
		    myexit(0);
		}
	}
}
struct savebuf {
	struct savebuf *s_next;
	struct savebuf *s_prev;
	int	s_n;
	int	s_flags;
} savebuf = {&savebuf, &savebuf};

void
savedata(n, flags)
int n, flags;
{
	register struct savebuf *s = (struct savebuf *)malloc(n + sizeof *s);
	if (s == 0)
		return;
	insque(s, savebuf.s_prev);
	s->s_n = n;
	s->s_flags = flags;
	bcopy(readbuf, (char *)(s + 1), n);
}

answerback(ns)
{
	int n;
	register struct savebuf *s = savebuf.s_next, *t;
	static struct iovec iov[1];
	static struct msghdr msghdr = { 0, 0, iov, 1, 0, 0, 0};
	while (s != &savebuf) {
		iov->iov_len = s->s_n;
		iov->iov_base = (char *)(s + 1);
		n = sendmsg(ns, &msghdr, s->s_flags);
		dbprintf("echoed %d\n", n);
		t = s; s = s->s_next; remque(t); free((char *)t);
	}
}

dumpit(what, where, n)
char *what; unsigned short *where; int n;
{
	unsigned short *s = where;
	unsigned short *z = where + (n+1)/2;
	int count = 0;
	if (dumpnodata)
		return;
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

sel_recvwait(fd)
int fd;
{
	int x;
	do {
		FD_ZERO(&readfds);
		FD_ZERO(&exceptfds);
		FD_SET(fd, &readfds);
		FD_SET(fd, &exceptfds);
		x = select(fd+1, &readfds, (fd_set *)0, &exceptfds, (void *)0);
		dbprintf("select returns %d\n", x);
	} while (x <= 0 ||
		 (FD_ISSET(fd,&readfds) == 0 && FD_ISSET(fd,&exceptfds) == 0));
}

#include <sys/syscall.h>
/* Here for gdb trapping */
setsockopt(s, level, optname, optval, optlen)
int s, level, optname, optlen;
const void *optval;
{

	dbprintf("setsocket called s %d, level 0x%x, optname %d, optlen %d\n",
			s, level, optname, optlen);
	dumpit("", optval, optlen);
	return syscall(SYS_setsockopt, s, level, optname, optval, optlen);
}
