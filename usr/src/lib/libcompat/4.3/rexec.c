/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)rexec.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <stdio.h>
#include <netdb.h>
#include <errno.h>

extern	errno;
char	*index();
int	rexecoptions;
char	*getpass(), *getlogin();

rexec(ahost, rport, name, pass, cmd, fd2p)
	char **ahost;
	int rport;
	char *name, *pass, *cmd;
	int *fd2p;
{
	int s, timo = 1, s3;
	struct sockaddr_in sin, sin2, from;
	char c;
	short port;
	struct hostent *hp;

	hp = gethostbyname(*ahost);
	if (hp == 0) {
		fprintf(stderr, "%s: unknown host\n", *ahost);
		return (-1);
	}
	*ahost = hp->h_name;
	ruserpass(hp->h_name, &name, &pass);
retry:
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("rexec: socket");
		return (-1);
	}
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = rport;
	bcopy(hp->h_addr, (caddr_t)&sin.sin_addr, hp->h_length);
	if (connect(s, &sin, sizeof(sin)) < 0) {
		if (errno == ECONNREFUSED && timo <= 16) {
			(void) close(s);
			sleep(timo);
			timo *= 2;
			goto retry;
		}
		perror(hp->h_name);
		return (-1);
	}
	if (fd2p == 0) {
		(void) write(s, "", 1);
		port = 0;
	} else {
		char num[8];
		int s2, sin2len;
		
		s2 = socket(AF_INET, SOCK_STREAM, 0);
		if (s2 < 0) {
			(void) close(s);
			return (-1);
		}
		listen(s2, 1);
		sin2len = sizeof (sin2);
		if (getsockname(s2, (char *)&sin2, &sin2len) < 0 ||
		  sin2len != sizeof (sin2)) {
			perror("getsockname");
			(void) close(s2);
			goto bad;
		}
		port = ntohs((u_short)sin2.sin_port);
		(void) sprintf(num, "%d", port);
		(void) write(s, num, strlen(num)+1);
		{ int len = sizeof (from);
		  s3 = accept(s2, &from, &len, 0);
		  close(s2);
		  if (s3 < 0) {
			perror("accept");
			port = 0;
			goto bad;
		  }
		}
		*fd2p = s3;
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
