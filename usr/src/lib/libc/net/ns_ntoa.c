/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 */

#ifndef lint
static char sccsid[] = "@(#)ns_ntoa.c	6.1 (Berkeley) %G%";
#endif not lint

#include <sys/types.h>
#include <netns/ns.h>

char *
ns_ntoa(addr)
struct ns_addr addr;
{
	static char obuf[40];
	char *spectHex();
	u_long net;
	u_short port = htons(addr.x_port);
	register char *cp;
	char *cp2;
	register u_char *up = addr.x_host.c_host;
	u_char *uplim = up + 6;

	* (union ns_net *) &net = addr.x_net;
	net = ntohl(net);
	sprintf(obuf, "%lx", net);
	cp = spectHex(obuf);
	cp2 = cp + 1;
	while (*up==0 && up < uplim) up++;
	if (up == uplim) return (obuf);
	sprintf(cp, ".%x", *up++);
	while (up < uplim) {
		while (*cp) cp++;
		sprintf(cp, "%02x", *up++);
	}
	cp = spectHex(cp2);
	if (port) {
		sprintf(cp, ".%x", port);
		spectHex(cp + 1);
	}
	return (obuf);
}

static char *
spectHex(p0)
char *p0;
{
	int ok = 0;
	int nonzero = 0;
	register char *p = p0;
	for (; *p; p++) switch (*p) {

	case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
		*p += ('A' - 'a');
		/* fall into . . . */
	case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
		ok = 1;
	case '1': case '2': case '3': case '4': case '5':
	case '6': case '7': case '8': case '9':
		nonzero = 1;
	}
	if (nonzero && !ok) { *p++ = 'H'; *p = 0; }
	return (p);
}
