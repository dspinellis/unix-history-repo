/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include "bsd.h"

#if	defined(TALK_43) || defined(TALK_42)

#ifndef lint
static char sccsid[] = "@(#)ctl_transact.c	5.2 (Berkeley) 3/13/86";
#endif

#include "talk_ctl.h"
#include <sys/time.h>

#define CTL_WAIT 2	/* time to wait for a response, in seconds */
#define MAX_RETRY 5

/*
 * SOCKDGRAM is unreliable, so we must repeat messages if we have
 * not recieved an acknowledgement within a reasonable amount
 * of time
 */
ctl_transact(target, msg, type, rp)
	struct in_addr target;
	CTL_MSG msg;
	int type;
	CTL_RESPONSE *rp;
{
	int read_mask, ctl_mask, nready, cc, retries;
	struct timeval wait;

	msg.type = type;
	daemon_addr.sin_addr = target;
	daemon_addr.sin_port = daemon_port;
	ctl_mask = 1 << ctl_sockt;

	/*
	 * Keep sending the message until a response of
	 * the proper type is obtained.
	 */
	do {
		wait.tv_sec = CTL_WAIT;
		wait.tv_usec = 0;
		/* resend message until a response is obtained */
		for (retries = MAX_RETRY; retries > 0; retries -= 1) {
			cc = sendto(ctl_sockt, (char *)&msg, sizeof (msg), 0,
				&daemon_addr, sizeof (daemon_addr));
			if (cc != sizeof (msg)) {
				if (errno == EINTR)
					continue;
				p_error("Error on write to talk daemon");
			}
			read_mask = ctl_mask;
			nready = select(32, &read_mask, 0, 0, &wait);
			if (nready < 0) {
				if (errno == EINTR)
					continue;
				p_error("Error waiting for daemon response");
			}
			if (nready != 0)
				break;
		}
		if (retries <= 0)
			break;
		/*
		 * Keep reading while there are queued messages 
		 * (this is not necessary, it just saves extra
		 * request/acknowledgements being sent)
		 */
		do {
			cc = recv(ctl_sockt, (char *)rp, sizeof (*rp), 0);
			if (cc < 0) {
				if (errno == EINTR)
					continue;
				p_error("Error on read from talk daemon");
			}
			read_mask = ctl_mask;
			/* an immediate poll */
			timerclear(&wait);
			nready = select(32, &read_mask, 0, 0, &wait);
		} while (nready > 0 && (
#ifdef	TALK_43
		    rp->vers != TALK_VERSION ||
#endif
		    rp->type != type));
	} while (
#ifdef	TALK_43
	    rp->vers != TALK_VERSION ||
#endif
	    rp->type != type);
	rp->id_num = ntohl(rp->id_num);
#ifdef	TALK_43
	rp->addr.sa_family = ntohs(rp->addr.sa_family);
# else
	rp->addr.sin_family = ntohs(rp->addr.sin_family);
# endif
}
#endif
