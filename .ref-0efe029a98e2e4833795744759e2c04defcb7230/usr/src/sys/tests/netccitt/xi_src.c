/*
 * Copyright (c) 1988, 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */
#ifndef lint
static char sccsid[] = "@(#)xi_src.c	7.2 (Berkeley) %G%";
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
#include <netccitt/x25.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>


#define dbprintf if(verbose)printf
#define try(a,b,c) {x = (a b);dbprintf("%s%s returns %d\n",c,"a",x);\
		    if (x < 0) {perror("a"); exit(1);}}

fd_set	readfds, writefds, exceptfds;
long size, count = 10;
int verbose = 1, selectp, type = SOCK_STREAM, nobuffs, errno, playtag = 0;
int verify = 0, mqdata;
short portnumber = 3000;
struct sockaddr_x25 to;
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
	u_long len;
	int handy;

	while(--argc > 0) {
		av++;
		if (strcmp(*av,"dest")==0) {
			av++;
			ccitt_addr(*av, &to);
			argc--;
		} else if (strcmp(*av,"count")==0) {
			av++;
			sscanf(*av,"%ld",&count);
			argc--;
		} else if (strcmp(*av,"size")==0) {
			av++;
			sscanf(*av,"%ld",&size);
		}
	}
	tisrc();
}

tisrc() {
	int x, s, pid, on = 1, flags = 8, n;

	try(socket, (AF_CCITT, type, 0),"");
	s = x;

	/*try(setsockopt, (s, SOL_SOCKET, SO_DEBUG, &on, sizeof (on)), "");*/

	to.x25_opts.op_flags |= X25_MQBIT;
	try(connect, (s, (struct sockaddr *) &to, to.x25_len), "");

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
	if (nobuffs) {
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

get_control_data(type, level)
{

	datasize = (int *)&msg.msg_controllen;
	cp = cm.cm.cmdata;
	cplim = cp + sizeof(cm.cm.cmdata);
	cm.cm.cmhdr.cmsg_level = level;
	cm.cm.cmhdr.cmsg_type = type;
	msg.msg_control = cm.data;
}



get_record(flags)
int *flags;
{
	int factor = 1, x = 0;
	char workbuf[10240];

	*flags = 0;
	*datasize = 0;
	datasize = &iov->iov_len;
	cp = data_msg + 1;
	cplim  = data_msg + sizeof(data_msg);

	*data_msg = 0;
	for(;;) {
		x = scanf("%s", workbuf);
		if (x == EOF)
			break;
		if (strcmp(workbuf, "oob") == 0)
			*flags |= MSG_OOB;
		else if (strcmp(workbuf, "qbit") == 0)
			*data_msg |= 0x80;
		else if (strcmp(workbuf, "mbit") == 0)
			*data_msg |= 0x40;
		else if (strcmp(workbuf, "eom") == 0)
			*flags |= MSG_EOR;
		else if (strcmp(workbuf, "factor") == 0) {
			x = scanf("%d", &factor);
			if (factor <= 0) factor = 1;
			if (x == EOF)
				break;
		} else {
			int len = strlen(workbuf);
			localsize = 1;
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
