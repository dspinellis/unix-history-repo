#ifndef lint
static char sccsid[] = "@(#)ctl_transact.c	1.3 (Berkeley) %G%";
#endif

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
				&daemon_addr, sizeof(daemon_addr));
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
