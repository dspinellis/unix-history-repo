/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)implogd.c	5.10 (Berkeley) 3/2/91";
#endif /* not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <sys/file.h>

#include <net/if.h>

#include <netinet/in.h>
#include <netimp/if_imp.h>

#include <sgtty.h>
#include "pathnames.h"

/*
 * Socket address, internet style, with
 * unused space taken by timestamp and packet
 * size.
 */
struct sockstamp {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	time_t	sin_time;
	int	sin_len;
};

main()
{
	register int len, log, s;
	struct sockstamp from;
	int fromlen;
	u_char request[1024];
	time_t time();

	openlog("implogd", LOG_PID|LOG_ODELAY|LOG_PERROR, LOG_DAEMON);
	log = open(_PATH_IMPLOG, O_CREAT|O_WRONLY|O_APPEND, 0644);
	if (log < 0) {
		syslog(LOG_ERR, "%s: %m\n", _PATH_IMPLOG);
		exit(1);
	}
	from.sin_time = time((time_t *)NULL);
	from.sin_len = sizeof(time_t);
	(void)write(log, (char *)&from, sizeof(from));
	if ((s = socket(AF_IMPLINK, SOCK_RAW, 0)) < 0) {
		syslog(LOG_ERR, "socket: %m\n");
		exit(1);
	}
#ifndef DEBUG
	{
		register int i, tt;

		if (fork())
			exit(0);
		for (i = 0; i < 10; i++)
			if (i != log && i != s)
				(void) close(i);
		(void) open("/", O_RDONLY, 0);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		tt = open(_PATH_TTY, O_RDWR, 0);
		if (tt > 0) {
			ioctl(tt, TIOCNOTTY, 0);
			(void)close(tt);
		}
	}
#endif
	for (fromlen = sizeof(from);;) {
		len = recvfrom(s, request, sizeof(request), 0,
		    (struct sockaddr *)&from, &fromlen);
		if (len < 0) {
			syslog(LOG_ERR, "recvfrom: %m\n");
			continue;
		}
		if (len == 0 || len > IMPMTU)		/* sanity */
			continue;
		from.sin_len = len;
		from.sin_time = time((time_t *)NULL);
		(void)write(log, (char *)&from, sizeof(from));
		(void)write(log, request, len);
	}
	/*NOTREACHED*/
}
