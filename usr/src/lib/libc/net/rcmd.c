#ifndef lint
static char sccsid[] = "@(#)rcmd.c	4.4 %G%";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#include <netdb.h>
#include <errno.h>

extern	errno;
char	*index(), *sprintf();
int	rcmdoptions;

rcmd(ahost, rport, locuser, remuser, cmd, fd2p)
	char **ahost;
	int rport;
	char *locuser, *remuser, *cmd;
	int *fd2p;
{
	int s, timo = 1;
	struct sockaddr_in sin, sin2, from;
	char c;
	short port;
	int lport = IPPORT_RESERVED - 1;
	struct hostent *hp;

	hp = gethostbyname(*ahost);
	if (hp == 0) {
		fprintf(stderr, "%s: unknown host\n", *ahost);
		return (-1);
	}
	*ahost = hp->h_name;
retry:
	s = rresvport(rcmdoptions|SO_KEEPALIVE, &lport);
	if (s < 0)
		return (-1);
	sin.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr, (caddr_t)&sin.sin_addr, hp->h_length);
	sin.sin_port = rport;
	if (connect(s, (caddr_t)&sin, sizeof (sin), 0) < 0) {
		if (errno == EADDRINUSE) {
			close(s);
			lport--;
			goto retry;
		}
		if (errno == ECONNREFUSED && timo <= 16) {
			(void) close(s);
			sleep(timo);
			timo *= 2;
			goto retry;
		}
		perror(hp->h_name);
		return (-1);
	}
	lport--;
	if (fd2p == 0) {
		write(s, "", 1);
		port = 0;
	} else {
		char num[8];
		int s2 = rresvport(rcmdoptions|SO_ACCEPTCONN, &lport), s3;

		if (s2 < 0) {
			(void) close(s);
			return (-1);
		}
		listen(s2, 1);
		socketaddr(s2, &sin2);
		port = htons((u_short)sin2.sin_port);
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
		from.sin_port = ntohs((u_short)from.sin_port);
		if (from.sin_family != AF_INET ||
		    from.sin_port >= IPPORT_RESERVED) {
			fprintf(stderr,
			    "socket: protocol failure in circuit setup.\n");
			goto bad2;
		}
	}
	(void) write(s, locuser, strlen(locuser)+1);
	(void) write(s, remuser, strlen(remuser)+1);
	(void) write(s, cmd, strlen(cmd)+1);
	if (read(s, &c, 1) != 1) {
		perror(*ahost);
		goto bad2;
	}
	if (c != 0) {
		while (read(s, &c, 1) == 1) {
			(void) write(2, &c, 1);
			if (c == '\n')
				break;
		}
		goto bad2;
	}
	return (s);
bad2:
	if (port)
		(void) close(*fd2p);
bad:
	(void) close(s);
	return (-1);
}

rresvport(options, alport)
	int options, *alport;
{
	struct sockaddr_in sin;
	int s;

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = 0;
	s = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (s < 0)
		return (-1);
	for (;;) {
		sin.sin_port = htons((u_short)*alport);
		if (bind(s, (caddr_t)&sin, sizeof (sin), 0) >= 0)
			return (s);
		if (errno != EADDRINUSE && errno != EADDRNOTAVAIL) {
			perror("socket");
			return (-1);
		}
		(*alport)--;
		if (*alport == IPPORT_RESERVED/2) {
			fprintf(stderr, "socket: All ports in use\n");
			return (-1);
		}
	}
}

ruserok(rhost, ruser, luser)
	char *rhost, *ruser, *luser;
{
	FILE *hostf;
	char ahost[32];
	int first = 1;

	hostf = fopen("/etc/hosts.equiv", "r");
again:
	if (hostf) {
		while (fgets(ahost, sizeof (ahost), hostf)) {
			char *user;
			if (index(ahost, '\n'))
				*index(ahost, '\n') = 0;
			user = index(ahost, ' ');
			if (user)
				*user++ = 0;
			if (!strcmp(rhost, ahost) &&
			    !strcmp(ruser, user ? user : luser))
				goto ok;
		}
		(void) fclose(hostf);
	}
	if (first == 1) {
		first = 0;
		hostf = fopen(".rhosts", "r");
		goto again;
	}
	return (-1);
ok:
	(void) fclose(hostf);
	return (0);
}

socketaddr(x, y)
{

	syscall(103,x,y);
}
