#ifndef lint
static char sccsid[] = "@(#)rcmd.c	4.2 82/10/07";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include <errno.h>
#include <netdb.h>

extern	errno;
char	*index(), *sprintf();
int	rcmdoptions;

rcmd(ahost, rport, locuser, remuser, cmd, fd2p)
	char **ahost;
	int rport;
	char *locuser, *remuser, *cmd;
	int *fd2p;
{
	int s, addr, timo = 1;
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
retry:
	s = rresvport(rcmdoptions|SO_KEEPALIVE);
	if (s < 0)
		return (-1);
	sin.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr, &sin.sin_addr, hp->h_length);
	sin.sin_port = htons(rport);
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
		write(s, "", 1);
		port = 0;
	} else {
		char num[8];
		int s2 = rresvport(rcmdoptions|SO_ACCEPTCONN);

		if (s2 < 0) {
			(void) close(s);
			return (-1);
		}
		socketaddr(s2, &sin2);
		port = htons((u_short)sin2.sin_port);
		(void) sprintf(num, "%d", port);
		(void) write(s, num, strlen(num)+1);
		if (accept(s2, &from) < 0) {
			perror("accept");
			goto bad;
		}
		from.sin_port = ntohs(from.sin_port);
		if (from.sin_family != AF_INET ||
		    from.sin_port >= IPPORT_RESERVED) {
			fprintf(stderr,
			    "socket: protocol failure in circuit setup.\n");
			goto bad;
		}
		*fd2p = s2;
	}
	(void) write(s, locuser, strlen(locuser)+1);
	(void) write(s, remuser, strlen(remuser)+1);
	(void) write(s, cmd, strlen(cmd)+1);
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

rresvport(options)
	int options;
{
	struct sockaddr_in sin;
	short lport = IPPORT_RESERVED - 1;
	int s;

	for (;;) {
		sin.sin_family = AF_INET;
		sin.sin_port = htons(lport);
		sin.sin_addr.s_addr = 0;
		s = socket(SOCK_STREAM, 0, &sin, options);
		if (s >= 0)
			return (s);
		if (errno != EADDRINUSE && errno != EADDRNOTAVAIL) {
			perror("socket");
			return (-1);
		}
		lport--;
		if (lport == IPPORT_RESERVED/2) {
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
