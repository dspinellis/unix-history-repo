#ifndef lint
static char sccsid[] = "@(#)tftp.c	4.7 (Berkeley) 8/11/83";
#endif

/*
 * TFTP User Program -- Protocol Machines
 */
#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <arpa/tftp.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>

extern	int errno;
extern	struct sockaddr_in sin;
extern	char mode[];
int	f;
int	trace;
int	verbose;
int	connected;
char	buf[BUFSIZ];
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
	register struct tftphdr *tp = (struct tftphdr *)buf;
	register int block = 0, size, n, amount = 0;
	struct sockaddr_in from;
	time_t start = time(0), delta;
	int fromlen;

	signal(SIGALRM, timer);
	do {
		if (block == 0)
			size = makerequest(WRQ, name) - 4;
		else {
			size = read(fd, tp->th_data, SEGSIZE);
			if (size < 0) {
				nak(errno + 100);
				break;
			}
			tp->th_opcode = htons((u_short)DATA);
			tp->th_block = htons((u_short)block);
		}
		timeout = 0;
		(void) setjmp(timeoutbuf);
		if (trace)
			tpacket("sent", tp, size + 4);
		n = sendto(f, buf, size + 4, 0, (caddr_t)&sin, sizeof (sin));
		if (n != size + 4) {
			perror("tftp: sendto");
			goto abort;
		}
		do {
			alarm(rexmtval);
			do {
				fromlen = sizeof (from);
				n = recvfrom(f, buf, sizeof (buf), 0,
				    (caddr_t)&from, &fromlen);
			} while (n <= 0);
			alarm(0);
			if (n < 0) {
				perror("tftp: recvfrom");
				goto abort;
			}
			if (trace)
				tpacket("received", tp, n);
			/* should verify packet came from server */
			tp->th_opcode = ntohs(tp->th_opcode);
			tp->th_block = ntohs(tp->th_block);
			if (tp->th_opcode == ERROR) {
				printf("Error code %d: %s\n", tp->th_code,
					tp->th_msg);
				goto abort;
			}
		} while (tp->th_opcode != ACK && block != tp->th_block);
		if (block > 0)
			amount += size;
		block++;
	} while (size == SEGSIZE || block == 1);
abort:
	(void) close(fd);
	if (amount > 0) {
		delta = time(0) - start;
		printf("Sent %d bytes in %d seconds.\n", amount, delta);
	}
}

/*
 * Receive a file.
 */
recvfile(fd, name)
	int fd;
	char *name;
{
	register struct tftphdr *tp = (struct tftphdr *)buf;
	register int block = 1, n, size, amount = 0;
	struct sockaddr_in from;
	time_t start = time(0), delta;
	int fromlen, firsttrip = 1;

	signal(SIGALRM, timer);
	do {
		if (firsttrip) {
			size = makerequest(RRQ, name);
			firsttrip = 0;
		} else {
			tp->th_opcode = htons((u_short)ACK);
			tp->th_block = htons((u_short)(block));
			size = 4;
			block++;
		}
		timeout = 0;
		(void) setjmp(timeoutbuf);
		if (trace)
			tpacket("sent", tp, size);
		if (sendto(f, buf, size, 0, (caddr_t)&sin,
		    sizeof (sin)) != size) {
			alarm(0);
			perror("tftp: sendto");
			goto abort;
		}
		do {
			alarm(rexmtval);
			do
				n = recvfrom(f, buf, sizeof (buf), 0,
				    (caddr_t)&from, &fromlen);
			while (n <= 0);
			alarm(0);
			if (n < 0) {
				perror("tftp: recvfrom");
				goto abort;
			}
			if (trace)
				tpacket("received", tp, n);
			/* should verify client address */
			tp->th_opcode = ntohs(tp->th_opcode);
			tp->th_block = ntohs(tp->th_block);
			if (tp->th_opcode == ERROR) {
				printf("Error code %d: %s\n", tp->th_code,
					tp->th_msg);
				goto abort;
			}
		} while (tp->th_opcode != DATA && block != tp->th_block);
		size = write(fd, tp->th_data, n - 4);
		if (size < 0) {
			nak(errno + 100);
			break;
		}
		amount += size;
	} while (size == SEGSIZE);
abort:
	tp->th_opcode = htons((u_short)ACK);
	tp->th_block = htons((u_short)block);
	(void) sendto(f, buf, 4, 0, &sin, sizeof (sin));
	(void) close(fd);
	if (amount > 0) {
		delta = time(0) - start;
		printf("Received %d bytes in %d seconds.\n", amount, delta);
	}
}

makerequest(request, name)
	int request;
	char *name;
{
	register struct tftphdr *tp;
	int size;
	register char *cp;

	tp = (struct tftphdr *)buf;
	tp->th_opcode = htons((u_short)request);
	strcpy(tp->th_stuff, name);
	size = strlen(name);
	cp = tp->th_stuff + strlen(name);
	*cp++ = '\0';
	strcpy(cp, mode);
	cp += sizeof ("netascii") - 1;
	*cp++ = '\0';
	return (cp - buf);
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
nak(error)
	int error;
{
	register struct tftphdr *tp;
	int length;
	register struct errmsg *pe;
	extern char *sys_errlist[];

	tp = (struct tftphdr *)buf;
	tp->th_opcode = htons((u_short)ERROR);
	tp->th_code = htons((u_short)error);
	for (pe = errmsgs; pe->e_code >= 0; pe++)
		if (pe->e_code == error)
			break;
	if (pe->e_code < 0)
		pe->e_msg = sys_errlist[error - 100];
	strcpy(tp->th_msg, pe->e_msg);
	length = strlen(pe->e_msg) + 4;
	if (trace)
		tpacket("sent", tp, length);
	if (send(f, &sin, buf, length) != length)
		perror("nak");
}

tpacket(s, tp, n)
	struct tftphdr *tp;
	int n;
{
	static char *opcodes[] =
	   { "#0", "RRQ", "WRQ", "DATA", "ACK", "ERROR" };
	register char *cp, *file;
	u_short op = ntohs(tp->th_opcode);
	char *index();

	if (op < RRQ || op > ERROR)
		printf("%s opcode=%x ", s, op);
	else
		printf("%s %s ", s, opcodes[op]);
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
	}
}
