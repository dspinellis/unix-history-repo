/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */
#ifndef lint
static char sccsid[] = "@(#)tisrc.c	7.1 (Berkeley) %G%";
#endif /* not lint */

/*
 * This is a test program to be a source for TP4 connections.
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
#define try(a,b,c) {x = (a b);dbprintf("%s%s returns %d\n",c,"a",x);\
		    if (x < 0) {perror("a"); exit(1);}}

struct	iso_addr eon = {20, 0x47, 0, 6, 3, 0, 0, 0, 25 /*EGP for Berkeley*/};
struct  sockaddr_iso to_s = { sizeof(to_s), AF_ISO }, *to = &to_s;
fd_set	readfds, writefds, exceptfds;
long size, count = 10;
int verbose = 1, selectp, type = SOCK_SEQPACKET, nobuffs, errno, playtag = 0;
int verify = 0;
short portnumber = 3000;
char your_it[] = "You're it!";
char *port, *conndata, data_msg[2048];
struct iovec iov[1] = {data_msg};
union {
    struct {
	    struct cmsghdr	cmhdr;
	    char		cmdata[128 - sizeof (struct cmsghdr)];
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
	struct iso_addr iso_addr();
	u_long len;
	int handy;

	while(--argc > 0) {
		av++;
		if(strcmp(*av,"Servername")==0) {
			av++;
			port = *av;
			argc--;
		} else if(strcmp(*av,"conndata")==0) {
			av++;
			conndata = *av;
			argc--;
		} else if(strcmp(*av,"host")==0) {
			av++;
			to_s.siso_addr = iso_addr(*av);
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
			type = SOCK_STREAM;
		} else if (strcmp(*av,"eon") == 0) {
			unsigned long l, inet_addr();

			l = inet_addr(*++av); argc--;
			to_s.siso_addr = eon;
			bcopy((char *)&l, &to_s.siso_data[15], 4);
		}
	}
	if (port) {
		to_s.siso_tlen = strlen(port);
		len =  1 + to_s.siso_nlen + strlen(port)
				+ sizeof(*to) - sizeof(struct iso_addr);
		if (len > sizeof(*to)) {
			to = (struct sockaddr_iso *)malloc(len);
			bzero(to, len);
			*to = to_s;
			to->siso_len = len;
		}
		bcopy(port, TSEL(to), strlen(port));
	} else {
		to_s.siso_tlen = sizeof(portnumber);
		portnumber = htons(portnumber);
		bcopy((char *)&portnumber, TSEL(to), sizeof(portnumber));
	}

	tisrc();
}

tisrc() {
	int x, s, pid, on = 1, flags = 8, n;

	try(socket, (AF_ISO, type, 0),"");
	s = x;

	/*try(setsockopt, (s, SOL_SOCKET, SO_DEBUG, &on, sizeof (on)), "");*/

	if (conndata) doconndata(s);

	try(connect, (s, (struct sockaddr *) to, to->siso_len), "");

	if (selectp) {
		FD_ZERO(&writefds); FD_SET(s, &writefds);
		select(1, &writefds, 0, 0, 0);
	}
	while (count-- > 0) {
		if (size <= 0 && get_record(&flags) == EOF)
			exit(0);
		n = put_record(s, flags);
		if (n < iov->iov_len) {
			if (n==-1 && errno == 55) {
				nobuffs++;
				count++;
				continue;
			}
			fprintf(stderr, "wrote %d < %d, count %d,",
						n, iov->iov_len, count);
			perror("due to");
		}
	}
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
int localsize;
char dupbuf[4096];

put_record(s, flags)
int s, flags;
{
	int fd, buflen;
	char *buf;
	struct sockaddr *to;
	int x, saved_x;

	msg.msg_flags = flags;
	if (verbose) {
		unsigned short *zp, *zlim;
		if (msg.msg_controllen) {
			zp = (unsigned short *)&(cm.cm.cmhdr.cmsg_len);
			printf("(CMessage Type is %x) ", cm.cm.cmhdr.cmsg_type);
			printf("CMsg data: ");
			x = msg.msg_controllen;
			zlim = zp + ((x + 1) / 2);
			while (zp < zlim) printf("%x ", *zp++);
			putchar ('\n');
		}
		if (iov->iov_len) {
			printf("sending: %s %s",
			(flags & MSG_OOB ? "(OOB Data)" : ""),
				(flags & MSG_EOR ? "(Record Mark)" : ""));
			x = localsize;
			zp = (unsigned short *)data_msg;
			zlim = zp + ((x + 1) / 2);
			while (zp < zlim) printf("%x ", *zp++);
			putchar ('\n');
		}
	}
	if (verify) {
		buflen = iov->iov_len;
		bcopy(iov->iov_base, dupbuf, buflen);
	}
	try(sendmsg, (s, &msg, flags), " put_record ");
	saved_x = x;
	while (verify && buflen > 0) {
		iov->iov_len = buflen;
		iov->iov_base = dupbuf;
		try(recvmsg, (s, &msg, flags), " put_record ");
		printf("verify got %d\n", x);
		buflen -= x;
	}
	msg.msg_control = 0;
	return (saved_x);
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



get_record(flags)
int *flags;
{
	int factor = 1, x = 0;
	char workbuf[10240];

	*flags = 0;
	*datasize = 0;
	datasize = &iov->iov_len;
	cp = data_msg;
	cplim  = cp + sizeof(data_msg);

	for(;;) {
		x = scanf("%s", workbuf);
		if (x == EOF)
			break;
		if (strcmp(workbuf, "disc") == 0)
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
				cm.cm.cmhdr.cmsg_len = *datasize;
			}
			break;
		}
	}
	errno = 0;
	return (x);
}
