/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)tftpd.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Trivial file transfer protocol server.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include <netinet/in.h>

#include <arpa/tftp.h>

#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>
#include <setjmp.h>
#include <syslog.h>

#define	TIMEOUT		5

extern	int errno;
struct	sockaddr_in sin = { AF_INET };
int	peer;
int	rexmtval = TIMEOUT;
int	maxtimeout = 5*TIMEOUT;
char	buf[BUFSIZ];
struct	sockaddr_in from;
int	fromlen;

main()
{
	register struct tftphdr *tp;
	register int n;

	alarm(10);
	fromlen = sizeof (from);
	n = recvfrom(0, buf, sizeof (buf), 0,
	    (caddr_t)&from, &fromlen);
	if (n < 0) {
		perror("tftpd: recvfrom");
		exit(1);
	}
	from.sin_family = AF_INET;
	alarm(0);
	close(0);
	close(1);
	peer = socket(AF_INET, SOCK_DGRAM, 0);
	if (peer < 0) {
		openlog("tftpd", LOG_PID, 0);
		syslog(LOG_ERR, "socket: %m");
		exit(1);
	}
	if (bind(peer, (caddr_t)&sin, sizeof (sin)) < 0) {
		openlog("tftpd", LOG_PID, 0);
		syslog(LOG_ERR, "bind: %m");
		exit(1);
	}
	if (connect(peer, (caddr_t)&from, sizeof(from)) < 0) {
		openlog("tftpd", LOG_PID, 0);
		syslog(LOG_ERR, "connect: %m");
		exit(1);
	}
	tp = (struct tftphdr *)buf;
	tp->th_opcode = ntohs(tp->th_opcode);
	if (tp->th_opcode == RRQ || tp->th_opcode == WRQ)
		tftp(tp, n);
	exit(1);
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

/*
 * Handle initial connection protocol.
 */
tftp(tp, size)
	struct tftphdr *tp;
	int size;
{
	register char *cp;
	int first = 1, ecode;
	register struct formats *pf;
	char *filename, *mode;

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
	ecode = (*pf->f_validate)(filename, tp->th_opcode);
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

int	fd;

/*
 * Validate file access.  Since we
 * have no uid or gid, for now require
 * file to exist and be publicly
 * readable/writable.
 * Note also, full path name must be
 * given as we have no login directory.
 */
validate_access(file, mode)
	char *file;
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

int	timeout;
jmp_buf	timeoutbuf;

timer()
{

	timeout += rexmtval;
	if (timeout >= maxtimeout)
		exit(1);
	longjmp(timeoutbuf, 1);
}

/*
 * Send the requested file.
 */
sendfile(pf)
	struct format *pf;
{
	register struct tftphdr *tp;
	register int block = 1, size, n;

	signal(SIGALRM, timer);
	tp = (struct tftphdr *)buf;
	do {
		size = read(fd, tp->th_data, SEGSIZE);
		if (size < 0) {
			nak(errno + 100);
			return;
		}
		tp->th_opcode = htons((u_short)DATA);
		tp->th_block = htons((u_short)block);
		timeout = 0;
		(void) setjmp(timeoutbuf);
		if (send(peer, buf, size + 4, 0) != size + 4) {
			perror("tftpd: send");
			return;
		}
		do {
			alarm(rexmtval);
			n = recv(peer, buf, sizeof (buf), 0);
			alarm(0);
			if (n < 0) {
				perror("tftpd: recv");
				return;
			}
			tp->th_opcode = ntohs((u_short)tp->th_opcode);
			tp->th_block = ntohs((u_short)tp->th_block);
			if (tp->th_opcode == ERROR)
				return;
		} while (tp->th_opcode != ACK || tp->th_block != block);
		block++;
	} while (size == SEGSIZE);
}

/*
 * Receive a file.
 */
recvfile(pf)
	struct format *pf;
{
	register struct tftphdr *tp;
	register int block = 0, n, size;

	signal(SIGALRM, timer);
	tp = (struct tftphdr *)buf;
	do {
		timeout = 0;
		tp->th_opcode = htons((u_short)ACK);
		tp->th_block = htons((u_short)block);
		block++;
		(void) setjmp(timeoutbuf);
		if (send(peer, buf, 4, 0) != 4) {
			perror("tftpd: send");
			goto abort;
		}
		do {
			alarm(rexmtval);
			n = recv(peer, buf, sizeof (buf), 0);
			alarm(0);
			if (n < 0) {
				perror("tftpd: recv");
				goto abort;
			}
			tp->th_opcode = ntohs((u_short)tp->th_opcode);
			tp->th_block = ntohs((u_short)tp->th_block);
			if (tp->th_opcode == ERROR)
				goto abort;
		} while (tp->th_opcode != DATA || block != tp->th_block);
		size = write(fd, tp->th_data, n - 4);
		if (size < 0) {
			nak(errno + 100);
			goto abort;
		}
	} while (size == SEGSIZE);
abort:
	tp->th_opcode = htons((u_short)ACK);
	tp->th_block = htons((u_short)(block));
	(void) send(peer, buf, 4, 0);
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
	if (send(peer, buf, length, 0) != length)
		perror("nak");
	exit(1);
}
