/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tftp.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * TFTP User Program -- Protocol Machines
 */
#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/tftp.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <netdb.h>

extern	int errno;
extern	struct sockaddr_in sin;
extern	char mode[];
int	f;
int	trace;
int	connected;
char	sbuf[BUFSIZ];			/* send buffer */
char	rbuf[BUFSIZ];			/* receive buffer */
int	rexmtval;
int	maxtimeout;
int	timeout;
jmp_buf	toplevel;
jmp_buf	timeoutbuf;

timer()
{

	timeout += rexmtval;
	if (timeout >= maxtimeout) {
		printf("Transfer timed out.\n");
		longjmp(toplevel, -1);
	}
	longjmp(timeoutbuf, 1);
}

/*
 * Send the requested file.
 */
sendfile(fd, name)
	int fd;
	char *name;
{
	register struct tftphdr *stp = (struct tftphdr *)sbuf;
	register struct tftphdr *rtp = (struct tftphdr *)rbuf;
	register int block = 0, size, n, amount = 0;
	struct sockaddr_in from, to;
	time_t start = time(0), delta;
	int fromlen, aborted = 0;

	to = sin;
	signal(SIGALRM, timer);
	do {
		if (block == 0)
			size = makerequest(WRQ, name) - 4;
		else {
			size = read(fd, stp->th_data, SEGSIZE);
			if (size < 0) {
				nak(&to, errno + 100);
				break;
			}
			stp->th_opcode = htons((u_short)DATA);
			stp->th_block = htons((u_short)block);
		}
		timeout = 0;
		(void) setjmp(timeoutbuf);
		if (trace)
			tpacket("sent", &to, stp, size + 4);
		n = sendto(f, sbuf, size + 4, 0, (caddr_t)&to, sizeof (to));
		if (n != size + 4) {
			perror("tftp: sendto");
			aborted = 1;
			goto done;
		}
		do {
again:
			alarm(rexmtval);
			do {
				fromlen = sizeof (from);
				n = recvfrom(f, rbuf, sizeof (rbuf), 0,
				    (caddr_t)&from, &fromlen);
			} while (n <= 0);
			alarm(0);
			if (n < 0) {
				perror("tftp: recvfrom");
				aborted = 1;
				goto done;
			}
			if (to.sin_addr.s_addr != from.sin_addr.s_addr) {
				tpacket("discarded (wrong host)",
				    &from, rtp, n);
				goto again;
			}
			if (to.sin_port = sin.sin_port)
				to.sin_port = from.sin_port;
			if (to.sin_port != from.sin_port) {
				tpacket("discarded (wrong port)",
				    &from, rtp, n);
				goto again;
			}
			if (trace)
				tpacket("received", &from, rtp, n);
			/* should verify packet came from server */
			rtp->th_opcode = ntohs(rtp->th_opcode);
			rtp->th_block = ntohs(rtp->th_block);
			if (rtp->th_opcode == ERROR) {
				printf("Error code %d: %s\n", rtp->th_code,
					rtp->th_msg);
				aborted = 1;
				goto done;
			}
		} while (rtp->th_opcode != ACK && block != rtp->th_block);
		if (block > 0)
			amount += size;
		block++;
	} while (size == SEGSIZE || block == 1);
	if (!aborted && amount > 0) {
		delta = time(0) - start;
		printf("Sent %d bytes in %d seconds.\n", amount, delta);
	}
done:
	(void) close(fd);
	return (aborted);
}

/*
 * Receive a file.
 */
recvfile(fd, name)
	int fd;
	char *name;
{
	register struct tftphdr *stp = (struct tftphdr *)sbuf;
	register struct tftphdr *rtp = (struct tftphdr *)rbuf;
	register int block = 1, n, size, amount = 0;
	struct sockaddr_in from, to;
	time_t start = time(0), delta;
	int fromlen, firsttrip = 1, aborted = 0;

	to = sin;
	signal(SIGALRM, timer);
	do {
		if (firsttrip) {
			size = makerequest(RRQ, name);
			firsttrip = 0;
		} else {
			stp->th_opcode = htons((u_short)ACK);
			stp->th_block = htons((u_short)(block));
			size = 4;
			block++;
		}
		timeout = 0;
		(void) setjmp(timeoutbuf);
		if (trace)
			tpacket("sent", &to, stp, size);
		if (sendto(f, sbuf, size, 0, (caddr_t)&to,
		    sizeof (to)) != size) {
			alarm(0);
			perror("tftp: sendto");
			aborted = 1;
			goto done;
		}
		do {
again:
			alarm(rexmtval);
			do {
				fromlen = sizeof (from);
				n = recvfrom(f, rbuf, sizeof (rbuf), 0,
				    (caddr_t)&from, &fromlen);
			} while (n <= 0);
			alarm(0);
			if (n < 0) {
				perror("tftp: recvfrom");
				aborted = 1;
				goto done;
			}
			if (to.sin_addr.s_addr != from.sin_addr.s_addr) {
				tpacket("discarded (wrong host)",
				    &from, rtp, n);
				goto again;
			}
			if (to.sin_port = sin.sin_port)
				to.sin_port = from.sin_port;
			if (to.sin_port != from.sin_port) {
				tpacket("discarded (wrong port)",
				    &from, rtp, n);
				goto again;
			}
			if (trace)
				tpacket("received", &from, rtp, n);
			rtp->th_opcode = ntohs(rtp->th_opcode);
			rtp->th_block = ntohs(rtp->th_block);
			if (rtp->th_opcode == ERROR) {
				printf("Error code %d: %s\n", rtp->th_code,
					rtp->th_msg);
				aborted = 1;
				goto done;
			}
		} while (rtp->th_opcode != DATA && rtp->th_block != block);
		size = write(fd, rtp->th_data, n - 4);
		if (size < 0) {
			perror("tftp: write");
			nak(&to, errno + 100);
			aborted = 1;
			goto done;
		}
		amount += size;
	} while (size == SEGSIZE);
done:
	stp->th_opcode = htons((u_short)ACK);
	stp->th_block = htons((u_short)block);
	(void) sendto(f, sbuf, 4, 0, &to, sizeof (to));
	(void) close(fd);
	if (!aborted && amount > 0) {
		delta = time(0) - start;
		printf("Received %d bytes in %d seconds.\n", amount, delta);
	}
	return (aborted);
}

makerequest(request, name)
	int request;
	char *name;
{
	register struct tftphdr *stp;
	int size;
	register char *cp;

	stp = (struct tftphdr *)sbuf;
	stp->th_opcode = htons((u_short)request);
	strcpy(stp->th_stuff, name);
	size = strlen(name);
	cp = stp->th_stuff + strlen(name);
	*cp++ = '\0';
	strcpy(cp, mode);
	cp += sizeof ("netascii") - 1;
	*cp++ = '\0';
	return (cp - sbuf);
}

struct errmsg {
	int	e_code;
	char	*e_msg;
} errmsgs[] = {
	{ EUNDEF,	"Undefined error code" },
	{ ENOTFOUND,	"File not found" },
	{ EACCESS,	"Access violation" },
	{ ENOSPACE,	"Disk full or allocation exceeded" },
	{ EBADOP,	"Illegal TFTP operation" },
	{ EBADID,	"Unknown transfer ID" },
	{ EEXISTS,	"File already exists" },
	{ ENOUSER,	"No such user" },
	{ -1,		0 }
};

/*
 * Send a nak packet (error message).
 * Error code passed in is one of the
 * standard TFTP codes, or a UNIX errno
 * offset by 100.
 */
nak(to, error)
	struct sockaddr_in *to;
	int error;
{
	register struct tftphdr *stp;
	int length;
	register struct errmsg *pe;
	extern char *sys_errlist[];

	stp = (struct tftphdr *)sbuf;
	stp->th_opcode = htons((u_short)ERROR);
	stp->th_code = htons((u_short)error);
	for (pe = errmsgs; pe->e_code >= 0; pe++)
		if (pe->e_code == error)
			break;
	if (pe->e_code < 0)
		pe->e_msg = sys_errlist[error - 100];
	strcpy(stp->th_msg, pe->e_msg);
	length = strlen(pe->e_msg) + 4;
	if (trace)
		tpacket("sent", to, stp, length);
	if (sendto(f, sbuf, length, 0, to, sizeof (*to)) != length)
		perror("tftp: nak");
}

tpacket(s, sin, tp, n)
	struct sockaddr_in *sin;
	struct tftphdr *tp;
	int n;
{
	static char *opcodes[] =
	   { "#0", "RRQ", "WRQ", "DATA", "ACK", "ERROR" };
	register char *cp, *file;
	u_short op = ntohs(tp->th_opcode);
	char *index();

	printf("%s ", s);
	if (sin) {
		struct hostent *hp = gethostbyaddr(&sin->sin_addr,
		     sizeof (sin->sin_addr), AF_INET);

		printf("%s.%d ",
		    hp == 0 ? inet_ntoa(sin->sin_addr) : hp->h_name,
		    ntohs(sin->sin_port));
	}
	if (op < RRQ || op > ERROR)
		printf("opcode=%x ", op);
	else
		printf("%s ", opcodes[op]);
	switch (op) {

	case RRQ:
	case WRQ:
		n -= 2;
		file = cp = tp->th_stuff;
		cp = index(cp, '\0');
		printf("<file=%s, mode=%s>\n", file, cp + 1);
		break;

	case DATA:
		printf("<block=%d, %d bytes>\n", ntohs(tp->th_block), n - 4);
		break;

	case ACK:
		printf("<block=%d>\n", ntohs(tp->th_block));
		break;

	case ERROR:
		printf("<code=%d, msg=%s>\n", ntohs(tp->th_code), tp->th_msg);
		break;

	default:
		putchar('\n');
		break;
	}
}
