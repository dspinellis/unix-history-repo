#ifndef lint
static char sccsid[] = "@(#)rexec.c	4.3 82/04/01";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <errno.h>

extern	errno;
char	*index(), *sprintf();
int	rexecoptions;
char	*getpass(), *getlogin();

rexec(ahost, rport, name, pass, cmd, fd2p)
	char **ahost;
	int rport;
	char *name, *pass, *cmd;
	int *fd2p;
{
	int s, addr, timo = 1;
	struct sockaddr_in sin, sin2, from;
	char c;
	short port;

	addr = rhost(ahost);
	if (addr == -1) {
		fprintf(stderr, "%s: unknown host\n", *ahost);
		return (-1);
	}
	ruserpass(*ahost, &name, &pass);
retry:
	sin.sin_family = AF_INET;
	sin.sin_port = 0;
	sin.sin_addr.s_addr = 0;
	s = socket(SOCK_STREAM, 0, &sin, rexecoptions|SO_KEEPALIVE);
	if (s < 0)
		return (-1);
	sin.sin_addr.s_addr = addr;
	sin.sin_port = rport;
#if vax
	sin.sin_port =
	    ((sin.sin_port << 8) & 0xff00) | ((sin.sin_port >> 8) & 0x00ff);
#endif
	if (connect(s, &sin) < 0) {
		if (errno == ECONNREFUSED && timo <= 16) {
			(void) close(s);
			sleep(timo);
			timo *= 2;
			goto retry;
		}
		perror(*ahost);
		return (-1);
	}
	if (fd2p == 0) {
		(void) write(s, "", 1);
		port = 0;
	} else {
		char num[8];
		int s2;
		
		sin.sin_family = AF_INET;
		sin.sin_port = 0;
		sin.sin_addr.s_addr = 0;
		s2 = socket(SOCK_STREAM, 0, &sin, rexecoptions|SO_ACCEPTCONN);

		if (s2 < 0) {
			(void) close(s);
			return (-1);
		}
		socketaddr(s2, &sin2);
		port = sin2.sin_port;
#if vax
		port = ((port << 8) & 0xff00) | ((port >> 8) & 0x00ff);
#endif
		(void) sprintf(num, "%d", port);
		(void) write(s, num, strlen(num)+1);
		if (accept(s2, &from) < 0) {
			perror("accept");
			goto bad;
		}
		*fd2p = s2;
	}
	(void) write(s, name, strlen(name) + 1);
	/* should public key encypt the password here */
	(void) write(s, pass, strlen(pass) + 1);
	(void) write(s, cmd, strlen(cmd) + 1);
	if (read(s, &c, 1) != 1) {
		perror(*ahost);
		goto bad;
	}
	if (c != 0) {
		while (read(s, &c, 1) == 1) {
			(void) write(2, &c, 1);
			if (c == '\n')
				break;
		}
		goto bad;
	}
	return (s);
bad:
	if (port)
		(void) close(*fd2p);
	(void) close(s);
	return (-1);
}
