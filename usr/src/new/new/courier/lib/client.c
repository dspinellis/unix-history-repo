#ifndef lint
static char sccsid[] = "@(#)client.c	4.2 (Berkeley) 7/7/83";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include "courier.h"

#if DEBUG
int CourierClientDebuggingFlag = 0;
#endif

SendCallMessage(f, procedure, nwords, arguments)
	int f;
	Cardinal procedure, nwords;
	Unspecified *arguments;
{
	Cardinal p, n;

#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr, "[SendCallMessage %d %d]\n", procedure, nwords);
#endif
	PackCardinal(&procedure, &p, 1);
	PackCardinal(&nwords, &n, 1);
	write(f, &p, sizeof(Cardinal));
	write(f, &n, sizeof(Cardinal));
	write(f, arguments, nwords*sizeof(Unspecified));
}

Unspecified *
ReceiveReturnMessage(f)
	int f;
{
	Cardinal nwords, n;
	Unspecified *bp;

	if (ClientRead(f, &nwords, 1) == 0)
		goto eof;
	UnpackCardinal(&n, &nwords);
#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr, "[ReceiveReturnMessage %d]\n", n);
#endif
	bp = Allocate(n);
	if (ClientRead(f, bp, n) == 0)
		goto eof;
	return (bp);
eof:
	fprintf(stderr, "\n\r\7Lost connection to server.\n");
	exit(1);
}

static
ClientRead(f, addr, nwords)
	int f;
	char *addr;
	int nwords;
{
	register int nbytes, n;
	register char *p;

	for (p = addr, nbytes = 2*nwords; nbytes > 0; nbytes -= n, p += n) {
		n = read(f, p, nbytes);
		if (n <= 0)
			return (0);
	}
	return (1);
}

CourierActivate(program_name, host)
	String program_name, host;
{
	struct hostent *hp;
	struct servent *srvp;
	int f;
	struct sockaddr_in sin;
	Unspecified buf[50];
	Cardinal n;
	char c;

	hp = gethostbyname(host);
	if (hp == 0) {
		fprintf(stderr, "%s: unknown host\n", host);
		exit(1);
	}
	srvp = getservbyname("courier", "tcp");
	if (srvp == 0) {
		fprintf(stderr, "tcp/courier: unknown service\n");
		exit(1);
	}
	f = socket(AF_INET, SOCK_STREAM, 0, 0);
	if (f < 0) {
		perror("socket");
		exit(1);
	}
	sin.sin_family = AF_INET;
	sin.sin_port = 0;
	sin.sin_addr.s_addr = 0;
	if (bind(f, (caddr_t)&sin, sizeof (sin), 0) < 0) {
		perror("bind");
		exit(1);
	}
	sin.sin_family = hp->h_addrtype;
	sin.sin_addr = *(struct in_addr *) hp->h_addr;
	sin.sin_port = srvp->s_port;
	if (connect(f, (caddr_t)&sin, sizeof(sin), 0) < 0) {
		perror(hp->h_name);
		exit(1);
	}
#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr, "[CourierActivate: connected to %s]\n", hp->h_name);
#endif
	n = PackString(&program_name, buf, 1);
	write(f, buf, n*sizeof(Unspecified));
	if (read(f, &c, 1) != 1) {
		perror(host);
		exit(1);
	}
	if (c != 0) {
		do write(fileno(stderr), &c, 1);
		while (read(f, &c, 1) == 1 && c != 0);
		exit(1);
	}
#if DEBUG
	if (CourierClientDebuggingFlag)
		fprintf(stderr, "[CourierActivate: running %s]\n", program_name);
#endif
	return (f);
}
