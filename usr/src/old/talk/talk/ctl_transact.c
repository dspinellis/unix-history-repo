/*-
 * Copyright (c) 1983, 1985
 *	The Regents of the University of California.  All rights reserved.
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
static char sccsid[] = "@(#)ctl_transact.c	5.1 (Berkeley) 6/6/85";
#endif not lint

#include "talk_ctl.h"
#include <sys/time.h>

#define CTL_WAIT 2	/* time to wait for a response, in seconds */

/*
 * SOCKDGRAM is unreliable, so we must repeat messages if we have
 * not recieved an acknowledgement within a reasonable amount
 * of time
 */
ctl_transact(target, msg, type, response)
	struct in_addr target;
	CTL_MSG msg;
	int type;
	CTL_RESPONSE *response;
{
	struct sockaddr junk;
	int read_mask;
	int ctl_mask;
	int nready;
	int cc;
	int junk_size;
	struct timeval wait;

	msg.type = type;
	daemon_addr.sin_addr = target;
	daemon_addr.sin_port = daemon_port;
	ctl_mask = 1 << ctl_sockt;

	/*
	 * keep sending the message until a response of the right
	 * type is obtained
	 */
	do {
		wait.tv_sec = CTL_WAIT;
		wait.tv_usec = 0;

		/* keep sending the message until a response is obtained */
		do {
			cc = sendto(ctl_sockt, (char *)&msg, sizeof(CTL_MSG), 0,
			    (struct sockaddr *)&daemon_addr,
			    sizeof(daemon_addr));
			if (cc != sizeof(CTL_MSG)) {
				if (errno == EINTR)
					continue;
				p_error("Error on write to talk daemon");
			}
			read_mask = ctl_mask;
			if ((nready = select(32, &read_mask, 0, 0, &wait)) < 0) {
				if (errno == EINTR)
					continue;
				p_error("Error waiting for daemon response");
			}
		} while (nready == 0);
		/* keep reading while there are queued messages 
		   (this is not necessary, it just saves extra
		   request/acknowledgements being sent)
		 */
		do {
			junk_size = sizeof(junk);
			cc = recvfrom(ctl_sockt, (char *)response,
			    sizeof (CTL_RESPONSE), 0, &junk, &junk_size);
			if (cc < 0) {
				if (errno == EINTR)
					continue;
				p_error("Error on read from talk daemon");
			}
			read_mask = ctl_mask;
			/* an immediate poll */
			timerclear(&wait);
			nready = select(32, &read_mask, 0, 0, &wait);
		} while (nready > 0 && response->type != type);
	} while (response->type != type);
}
