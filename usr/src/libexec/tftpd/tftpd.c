/*	tftpd.c	4.1	82/08/16	*/

/*
 * Trivial file transfer protocol server.
 */
#include <sys/types.h>
#include <net/in.h>
#include <sys/socket.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <stat.h>
#include <stdio.h>
#include <wait.h>
#include <errno.h>
#include <ctype.h>
#include "tftp.h"

#define	DEBUG		1

extern	int errno;
struct	sockaddr_in sin = { AF_INET, IPPORT_TFTP };
int	f;
int	options;
char	buf[BUFSIZ];

main(argc, argv)
	char *argv[];
{
	union wait status;
	struct sockaddr_in from;
	register struct tftphdr *tp;
	register int n;

#ifndef DEBUG
	if (fork())
		exit(0);
	for (f = 0; f < 10; f++)
		(void) close(f);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int t = open("/dev/tty", 2);
	  if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	  }
	}
#endif
#if vax || pdp11
	sin.sin_port = htons(sin.sin_port);
#endif
	argc--, argv++;
	if (argc > 0 && !strcmp(argv[0], "-d"))
		options |= SO_DEBUG;
	for (;;) {
		errno = 0;
		f = socket(SOCK_DGRAM, 0, &sin, options);
		if (f < 0) {
			perror("socket");
			sleep(5);
			continue;
		}
again:
		n = receive(f, &from, buf, sizeof (buf));
		if (n <= 0) {
			if (n < 0)
				perror("receive");
			goto again;
		}
		tp = (struct tftphdr *)buf;
#if vax || pdp11
		tp->th_opcode = ntohs(tp->th_opcode);
#endif
		if (tp->th_opcode == RRQ || tp->th_opcode == WRQ)
			if (fork() == 0)
				tftp(&from, tp, n);
		(void) close(f);
#ifdef notdef
		while (wait3(status, WNOHANG, 0) > 0)
#else
		while (wait3(status, 0, 0) > 0)
			continue;
	}
}

int	validate_access();
int	sendfile(), recvfile();

struct formats {
	char	*f_mode;
	int	(*f_validate)();
	int	(*f_send)();
	int	(*f_recv)();
} formats[] = {
	{ "netascii",	validate_access,	sendfile,	recvfile },
	{ "octet",	validate_access,	sendfile,	recvfile },
#ifdef notdef
	{ "mail",	validate_user,		sendmail,	recvmail },
#endif
	{ 0 }
};

int	fd;			/* file being transferred */

/*
 * Handle initial connection protocol.
 */
tftp(client, tp, size)
	struct sockaddr_in *client;
	struct tftphdr *tp;
	int size;
{
	register char *cp;
	int first = 1, ecode;
	register struct formats *pf;
	char *filename, *mode;

	if (connect(f, client) < 0) {
		perror("connect");
		exit(1);
	}
	filename = cp = tp->th_stuff;
again:
	while (cp < buf + size) {
		if (*cp == '\0')
			break;
		cp++;
	}
	if (*cp != '\0') {
		nak(EBADOP);
		exit(1);
	}
	if (first) {
		mode = ++cp;
		first = 0;
		goto again;
	}
	for (cp = mode; *cp; cp++)
		if (isupper(*cp))
			*cp = tolower(*cp);
	for (pf = formats; pf->f_mode; pf++)
		if (strcmp(pf->f_mode, mode) == 0)
			break;
	if (pf->f_mode == 0) {
		nak(EBADOP);
		exit(1);
	}
	ecode = (*pf->f_validate)(filename, client, tp->th_opcode);
	if (ecode) {
		nak(ecode);
		exit(1);
	}
	if (tp->th_opcode == WRQ)
		(*pf->f_recv)(pf);
	else
		(*pf->f_send)(pf);
	exit(0);
}

/*
 * Validate file access.  Since we
 * have no uid or gid, for now require
 * file to exist and be publicly
 * readable/writable.
 * Note also, full path name must be
 * given as we have no login directory.
 */
validate_access(file, client, mode)
	char *file;
	struct sockaddr_in *client;
	int mode;
{
	struct stat stbuf;

	if (*file != '/')
		return (EACCESS);
	if (stat(file, &stbuf) < 0)
		return (errno == ENOENT ? ENOTFOUND : EACCESS);
	if (mode == RRQ) {
		if ((stbuf.st_mode&(S_IREAD >> 6)) == 0)
			return (EACCESS);
	} else {
		if ((stbuf.st_mode&(S_IWRITE >> 6)) == 0)
			return (EACCESS);
	}
	fd = open(file, mode == RRQ ? 0 : 1);
	if (fd < 0)
		return (errno + 100);
	return (0);
}

int timeout;

timer()
{
	timeout += TIMEOUT;
	if (timeout >= MAXTIMEOUT)
		exit(1);
	alarm(TIMEOUT);
}

/*
 * Send the requested file.
 */
sendfile(pf)
	struct format *pf;
{
	register struct tftphdr *tp;
	register int block = 1, size, n;

	sigset(SIGALRM, timer);
	tp = (struct tftphdr *)buf;
	do {
		size = read(fd, tp->th_data, SEGSIZE);
		if (size < 0) {
			nak(errno + 100);
			break;
		}
		tp->th_opcode = htons((u_short)DATA);
		tp->th_block = htons((u_short)block);
		timeout = 0;
		alarm(TIMEOUT);
rexmt:
		if (write(f, buf, size + 4) != size + 4) {
			perror("send");
			break;
		}
again:
		n = read(f, buf, sizeof (buf));
		if (n <= 0) {
			if (n == 0)
				goto again;
			if (errno == EINTR)
				goto rexmt;
			alarm(0);
			perror("receive");
			break;
		}
		alarm(0);
#if vax || pdp11
		tp->th_opcode = ntohs(tp->th_opcode);
		tp->th_block = ntohs(tp->th_block);
#endif
		if (tp->th_opcode == ERROR)
			break;
		if (tp->th_opcode != ACK || tp->th_block != block)
			goto again;
		block++;
	} while (size == SEGSIZE);
	(void) close(fd);
}

/*
 * Receive a file.
 */
recvfile(pf)
	struct format *pf;
{
	register struct tftphdr *tp;
	register int block = 0, n, size;

	sigset(SIGALRM, timer);
	tp = (struct tftphdr *)buf;
	do {
		timeout = 0;
		alarm(TIMEOUT);
		tp->th_opcode = htons((u_short)ACK);
		tp->th_block = htons((u_short)block);
		block++;
rexmt:
		if (write(f, buf, 4) != 4) {
			perror("ack");
			break;
		}
again:
		n = read(f, buf, sizeof (buf));
		if (n <= 0) {
			if (n == 0)
				goto again;
			if (errno == EINTR)
				goto rexmt;
			alarm(0);
			perror("receive");
			break;
		}
		alarm(0);
#if vax || pdp11
		tp->th_opcode = ntohs(tp->th_opcode);
		tp->th_block = ntohs(tp->th_block);
#endif
		if (tp->th_opcode == ERROR)
			break;
		if (tp->th_opcode != DATA || block != tp->th_block)
			goto again;
		size = write(fd, tp->th_data, n - 4);
		if (size < 0) {
			nak(errno + 100);
			break;
		}
	} while (size == SEGSIZE);
	tp->th_opcode = htons((u_short)ACK);
	tp->th_block = htons((u_short)(block));
	(void) write(f, buf, 4);
	(void) close(fd);
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
	length = strlen(pe->e_msg);
	tp->th_msg[length] = '\0';
	length += 5;
	if (write(f, buf, length) != length)
		perror("nak");
}
