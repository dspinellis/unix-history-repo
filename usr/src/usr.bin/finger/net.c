/*
 * Copyright (c) 1989 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)net.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <ctype.h>

netfinger(name)
	char *name;
{
	extern int lflag;
	register FILE *fp;
	register int c, lastc;
	struct in_addr defaddr;
	struct hostent *hp, def;
	struct servent *sp;
	struct sockaddr_in sin;
	int s;
	char *alist[1], *host, *rindex();
	u_long inet_addr();

	if (!(host = rindex(name, '@')))
		return;
	*host++ = NULL;
	if (!(hp = gethostbyname(host))) {
		defaddr.s_addr = inet_addr(host);
		if (defaddr.s_addr == -1) {
			(void)fprintf(stderr,
			    "finger: unknown host: %s\n", host);
			return;
		}
		def.h_name = host;
		def.h_addr_list = alist;
		def.h_addr = (char *)&defaddr;
		def.h_length = sizeof(struct in_addr);
		def.h_addrtype = AF_INET;
		def.h_aliases = 0;
		hp = &def;
	}
	if (!(sp = getservbyname("finger", "tcp"))) {
		(void)fprintf(stderr, "finger: tcp/finger: unknown service\n");
		return;
	}
	sin.sin_family = hp->h_addrtype;
	bcopy(hp->h_addr, (char *)&sin.sin_addr, hp->h_length);
	sin.sin_port = sp->s_port;
	if ((s = socket(hp->h_addrtype, SOCK_STREAM, 0)) < 0) {
		perror("finger: socket");
		return;
	}

	/* have network connection; identify the host connected with */
	(void)printf("[%s]\n", hp->h_name);
	if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
		perror("finger: connect");
		(void)close(s);
		return;
	}

	/* -l flag for remote fingerd  */
	if (lflag)
		write(s, "/W ", 3);
	/* send the name followed by <CR><LF> */
	(void)write(s, name, strlen(name));
	(void)write(s, "\r\n", 2);

	/*
	 * Read from the remote system; once we're connected, we assume some
	 * data.  If none arrives, we hang until the user interrupts.
	 *
	 * If we see a <CR> or a <CR> with the high bit set, treat it as
	 * a newline; if followed by a newline character, only output one
	 * newline.
	 *
	 * Otherwise, all high bits are stripped; if it isn't printable and
	 * it isn't a space, we can simply set the 7th bit.  Every ASCII
	 * character with bit 7 set is printable.
	 */ 
	if (fp = fdopen(s, "r"))
		while ((c = getc(fp)) != EOF) {
			c &= 0x7f;
			if (c == 0x0d) {
				c = '\n';
				lastc = '\r';
			} else {
				if (!isprint(c) && !isspace(c))
					c |= 0x40;
				if (lastc != '\r' || c != '\n')
					lastc = c;
				else {
					lastc = '\n';
					continue;
				}
			}
			putchar(c);
		}
	if (lastc != '\n')
		putchar('\n');
	(void)fclose(fp);
}
