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
static char sccsid[] = "@(#)tisrc.c	7.8 (Berkeley) 2/25/93";
#endif /* not lint */

/*
 * This is a test program to be a source for ISO transport.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <sys/ioctl.h>
#include <net/route.h>
#include <net/if.h>
#define  TCPT_NTIMERS 4
#include <netiso/iso.h>
#include <netiso/tp_user.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>


#define dbprintf if(verbose)printf
#ifdef __STDC__
#define try(a,b,c) {x = (a b); dbprintf("%s%s returns %d\n",c,#a,x);\
		if (x<0) {perror(#a); exit(1);}}
#else
#define try(a,b,c) {x = (a b); dbprintf("%s%s returns %d\n",c,"a",x);\
		if (x<0) {perror("a");exit(1);}}
#endif

struct	iso_addr eon = {20, 0x47, 0, 6, 3, 0, 0, 0, 25 /*EGP for Berkeley*/};
struct	iso_addr *iso_addr();
struct  sockaddr_iso to_s = { sizeof(to_s), AF_ISO }, *to = &to_s;
struct  sockaddr_iso from_s = { sizeof(from_s), AF_ISO }, *from = 0;
struct  sockaddr_iso old_s = { sizeof(to_s), AF_ISO }, *old = &old_s;
struct	tp_conn_param tp_params;
fd_set	readfds, writefds, exceptfds;
long size, count = 0;
int verbose = 1, selectp, socktype = SOCK_SEQPACKET, nobuffs, errno, playtag = 0;
int echop = 0, dgramp = 0, debug = 0, tp0mode = 0, dumpnodata  = 0, tuba = 0;
short portnumber = 3000;
char your_it[] = "You're it!";
char *Servername, *conndata, data_msg[8192];
char Serverbuf[128];
char name[128];
struct iovec iov[1] = {data_msg};
union {
    struct {
	    struct cmsghdr	cmhdr;
	    char		cmdata[128 - sizeof(struct cmsghdr)];
    } cm;
    char data[128];
} cm;
struct msghdr msg = { 0, 0, iov, 1, 0, 0, 0};

main(argc, argv)
int argc;
char *argv[];
{
	register char **av = argv;
	register char *cp;
	u_long len;
	int handy;

	while(--argc > 0) {
		av++;
		if(strcmp(*av,"Servername")==0) {
			av++;
			Servername = *av;
			argc--;
		} else if(strcmp(*av,"conndata")==0) {
			av++;
			conndata = *av;
			argc--;
		} else if(strcmp(*av,"host")==0) {
			av++;
			to_s.siso_addr = *iso_addr(*av);
			argc--;
		} else if(strcmp(*av,"from")==0) {
			av++;
			from_s.siso_addr = *iso_addr(*av);
			from = &from_s;
			argc--;
		} else if(strcmp(*av,"port")==0) {
			av++;
			sscanf(*av,"%hd",&portnumber);
			argc--;
		} else if(strcmp(*av,"count")==0) {
			av++;
			sscanf(*av,"%ld",&count);
			argc--;
		} else if(strcmp(*av,"size")==0) {
			av++;
			sscanf(*av,"%ld",&size);
			iov->iov_len = size;
		} else if(strcmp(*av,"stream")==0) {
			socktype = SOCK_STREAM;
		} else if (strcmp(*av, "echo")==0) {
			echop++;
		} else if (strcmp(*av,"eon") == 0) {
			unsigned long l, inet_addr();

			l = inet_addr(*++av); argc--;
			to_s.siso_addr = eon;
			bcopy((char *)&l, &to_s.siso_data[15], 4);
		}
	}
	maketoaddr();
	tisrc();
}

maketoaddr()
{
	if (Servername) {
		int tlen = strlen(Servername);
		int len =  tlen + TSEL(to) - (caddr_t) to;
		if (len < sizeof(*to)) len = sizeof(*to);
		if (len > to->siso_len) {
			old = to;
			to = (struct sockaddr_iso *)malloc(len);
			*to = *old; /* We dont care if all old tsel is copied*/
			if (old != &to_s) free(old);
		}
		bcopy(Servername, TSEL(to), tlen);
		to->siso_tlen = tlen;
	} else {
		to->siso_tlen = sizeof(portnumber);
		portnumber = htons(portnumber);
		bcopy((char *)&portnumber, TSEL(to), sizeof(portnumber));
	}
}

tisrc() {
	int x, s, pid, on = 1, flags = 8, n;
	int proto = (tp0mode ? ISOPROTO_TP0 : (tuba ? ISOPROTO_TCP : 0 ));
	int socktype = (dgramp ? SOCK_DGRAM :
			(tuba ? SOCK_STREAM :SOCK_SEQPACKET));

	try(socket, (AF_ISO, socktype, proto),"");
	s = x;

	if (debug)
		try(setsockopt, (s, SOL_SOCKET, SO_DEBUG, &on, sizeof on), "");
	if (dgramp == 0) {
		if (conndata)
			doconndata(s);
		if (from) {
			try(bind,
			    (s, (struct sockaddr *)from, from->siso_len), "");
		}
		try(connect, (s, (struct sockaddr *) to, to->siso_len), "");
		recv_cdata(s);
	}
	if (selectp) {
		FD_ZERO(&writefds); FD_SET(s, &writefds);
		select(1, &writefds, 0, 0, 0);
	}
	do {
		if (size <= 0 && get_record(&flags) == EOF)
			exit(0);
		n = put_record(s, flags);
		if (n < iov->iov_len) {
			if (n == -1 && errno == 55) {
				nobuffs++;
				if (count) ++count;
				continue;
			}
			fprintf(stderr, "wrote %d < %d, count %d,",
						n, iov->iov_len, count);
			perror("due to");
		}
	} while (count == 0 || --count >= 1);
	if (playtag) {
		printf("Tag time!\n");
		iov->iov_base = your_it;
		iov->iov_len = sizeof your_it;
		sendmsg(s, &msg, MSG_EOR);
		sendmsg(s, &msg, MSG_EOR);
		iov->iov_base = data_msg;
		iov->iov_len = sizeof data_msg;
		try(recvmsg, (s, &msg, flags), " playtag ");
	}
	if(nobuffs) {
		printf("looped %d times waiting for bufs\n", nobuffs);
	}
}

recv_cdata(s)
int s;
{
	int x;
	iov->iov_len = 0;
	msg.msg_controllen = sizeof(cm);
	msg.msg_control = (char *)&cm;
	try(recvmsg,(s, &msg, 0), "confirm data?");
	if (msg.msg_controllen)
		dumpit("", (u_short *)&cm, msg.msg_controllen);
	msg.msg_control = 0;
	msg.msg_controllen = 0;
}

int localsize;
char dupbuf[4096];

struct savebuf {
	struct savebuf *s_next;
	struct savebuf *s_prev;
	int	s_n;
	int	s_flags;
} savebuf = {&savebuf, &savebuf};

void
savedata(n, flags)
int n;
{
	register struct savebuf *s = (struct savebuf *)malloc(n + sizeof *s);
	if (s == 0)
		return;
	insque(s, savebuf.s_prev);
	s->s_n = n;
	s->s_flags = flags;
	bcopy(iov->iov_base, (char *)(s + 1), n);
}

checkback(fd)
int fd;
{
	int n, nn;
	register struct savebuf *s = savebuf.s_next, *t;
	register char *cp = data_msg;
	while (s != &savebuf) {
		nn = s->s_n;
		do {
			msg.msg_flags = 0;
			iov->iov_len = nn;
			iov->iov_base = cp;
			n = recvmsg(fd, &msg, 0);
			cp += n;
			nn -= n;
		} while (dgramp == 0 && nn > 0 && !(msg.msg_flags & MSG_EOR));
		iov->iov_base = data_msg;
		if (dgramp) {
			if (msg.msg_namelen)
				dumpit("from: ", to, msg.msg_namelen);
			msg.msg_namelen = old->siso_len;
		}
		n = s->s_n - nn;
		dbprintf("echoed %d", n);
		if (nn)
			dbprintf(" instead of %d", s->s_n);
		if (bcmp((char *)(s + 1), data_msg, n))
			dbprintf(", with mismatched data");
		if (nn && (msg.msg_flags & MSG_EOR))
			dbprintf(" and with %d unchecked after EOR", nn);
		dbprintf("\n");
		t = s; s = s->s_next; remque(t); free((char *)t);
	}
}


put_record(s, flags)
int s, flags;
{
	char *buf;
	int x, saved_x;

	msg.msg_flags = flags;
	if (verbose) {
		if (msg.msg_controllen) {
			printf("(CMessage Type is %x) ", cm.cm.cmhdr.cmsg_type);
			dumpit("CMsg data:\n", &msg.msg_control, msg.msg_controllen);
		}
		if (iov->iov_len) {
			printf("sending: %s %s",
			(flags & MSG_OOB ? "(OOB Data)" : ""),
				(flags & MSG_EOR ? "(Record Mark)" : ""));
			dumpit("data: ", data_msg, localsize);
		}
	}
	if (echop)
		savedata(iov->iov_len, flags);
	if (dgramp) {
		msg.msg_name = (caddr_t)to;
		msg.msg_namelen = to->siso_len;
	}
	try(sendmsg, (s, &msg, flags), " put_record ");
	saved_x = x;
	if (echop && (flags & MSG_EOR))
		checkback(s);
	bcopy(old, to, old->siso_len);
	msg.msg_control = 0;
	return (saved_x);
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
int *datasize = &iov->iov_len;
char *cp, *cplim;

get_control_data(type)
{

	datasize = (int *)&msg.msg_controllen;
	cp = cm.cm.cmdata;
	cplim = cp + sizeof(cm.cm.cmdata);
	cm.cm.cmhdr.cmsg_level = SOL_TRANSPORT;
	cm.cm.cmhdr.cmsg_type = type;
	msg.msg_control = cm.data;
}

doconndata(s)
{
	get_control_data(TPOPT_CONN_DATA);
	*datasize = strlen(conndata) + sizeof(cm.cm.cmhdr);
	cm.cm.cmhdr.cmsg_len = *datasize;
	bcopy(conndata, cp, *datasize);
	put_record(s, 0);
}

get_altbuf(addrbuf)
char *addrbuf;
{
	if (dgramp == 0) {
		printf("illegal option for stream\n");
		return 1;
	}
	return (scanf("%s", addrbuf) == EOF ? 1 : 0);
}

get_record(flags)
int *flags;
{
	int factor = 1, x = 0, newaddr = 0;
	static repeatcount, repeatsize;
	char workbuf[10240];
	char addrbuf[128];

	if (repeatcount > 0) {
		repeatcount--;
		return;
	}

	*flags = 0;
	*datasize = 0;
	datasize = &iov->iov_len;
	cp = data_msg;
	cplim  = cp + sizeof(data_msg);

	for(;;) {
		x = scanf("%s", workbuf);
		if (x == EOF)
			break;
		if (strcmp(workbuf, "host") == 0) {
			if (get_altbuf(addrbuf))
				break;
			to->siso_addr = *iso_addr(addrbuf);
			newaddr = 1;
		} else if (strcmp(workbuf, "Servername") == 0) {
			if (get_altbuf(Serverbuf))
				break;
			Servername = Serverbuf;
			newaddr = 1;
		} else if (strcmp(workbuf, "port") == 0) {
			x = scanf("%hd", &portnumber);
			if (x == EOF)
				break;
			Servername = 0;
			newaddr = 1;
		} else if (strcmp(workbuf, "repeat") == 0) {
			x = scanf("%d", &repeatcount);
			if (repeatcount <= 0) repeatcount = 1;
			repeatcount--;
			if (x == EOF)
				break;
		} else if (strcmp(workbuf, "disc") == 0)
			x = get_control_data(TPOPT_DISC_DATA);
		else if (strcmp(workbuf, "cfrm") == 0)
			x = get_control_data(TPOPT_CFRM_DATA);
		else if (strcmp(workbuf, "oob") == 0)
			*flags |= MSG_OOB;
		else if (strcmp(workbuf, "eom") == 0)
			*flags |= MSG_EOR;
		else if (strcmp(workbuf, "factor") == 0) {
			x = scanf("%d", &factor);
			if (factor <= 0) factor = 1;
			if (x == EOF)
				break;
		} else {
			int len = strlen(workbuf);
			localsize = 0;
			while ((factor-- > 0) &&
			       ((cp + len) < cplim)) {
					strcpy(cp, workbuf);
					cp += len;
					localsize += len;
			}
			*datasize = localsize;
			if (datasize != &iov->iov_len) {
				*datasize += sizeof(cm.cm.cmhdr);
				repeatsize = cm.cm.cmhdr.cmsg_len = *datasize;
			}
			break;
		}
	}
	errno = 0;
	if (newaddr)
		maketoaddr();
	return (x);
}
