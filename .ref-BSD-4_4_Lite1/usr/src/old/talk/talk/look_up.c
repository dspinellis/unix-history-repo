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

/* from "@(#)look_up.c	5.1 (Berkeley) 6/6/85"; */

#ifndef lint
static char sccsid[] = "@(#)look_up.c	6.4 (Berkeley) 4/2/94";
#endif not lint

#include "talk_ctl.h"


/*
 * See if the local daemon has a invitation for us
 */
check_local()
{
	CTL_RESPONSE response;

	/* the rest of msg was set up in get_names */
	msg.ctl_addr = ctl_addr;
	/* must be initiating a talk */
	if (!look_for_invite(&response))
		return (0);
	/*
	 * There was an invitation waiting for us, 
	 * so connect with the other (hopefully waiting) party 
	 */
	current_state = "Waiting to connect with caller";
again:
	swapresponse(&response);
	response.addr.sin_family = AF_INET;
	if (connect(sockt,
	    (struct sockaddr *)&response.addr, sizeof(response.addr)) != -1)
		return (1);
	if (errno == EINTR)
		goto again;
	if (errno == ECONNREFUSED) {
		/*
		 * The caller gave up, but his invitation somehow
		 * was not cleared. Clear it and initiate an 
		 * invitation. (We know there are no newer invitations,
		 * the talkd works LIFO.)
		 */
		ctl_transact(his_machine_addr, msg, DELETE, &response);
		close(sockt);
		open_sockt();
		return (0);
	}
	p_error("Unable to connect with initiator");
	/*NOTREACHED*/
}

/*
 * Look for an invitation on 'machine'
 */
look_for_invite(response)
	CTL_RESPONSE *response;
{
	struct in_addr machine_addr;

	current_state = "Checking for invitation on caller's machine";
	ctl_transact(his_machine_addr, msg, LOOK_UP, response);
	/* the switch is for later options, such as multiple invitations */
	switch (response->answer) {

	case SUCCESS:
		msg.id_num = response->id_num;
		return (1);

	default :
		/* there wasn't an invitation waiting for us */
		return (0);
	}
}

/*  
 * heuristic to detect if need to reshuffle CTL_RESPONSE structure
 */

#define swapshort(a) (((a << 8) | ((unsigned short) a >> 8)) & 0xffff)
#define swaplong(a) ((swapshort(a) << 16) | (swapshort(((unsigned)a >> 16))))

#ifdef sun
struct ctl_response_vax {
	char type;
	char answer;
	short junk;
	int id_num;
	struct sockaddr_in addr;
};

swapresponse(rsp)
	CTL_RESPONSE *rsp;
{
	struct ctl_response_vax swaprsp;
	
	if (rsp->addr.sin_family != AF_INET) {
		bcopy(rsp, &swaprsp, sizeof(CTL_RESPONSE));
		swaprsp.addr.sin_family = swapshort(swaprsp.addr.sin_family);
		if (swaprsp.addr.sin_family == AF_INET) {
			rsp->addr = swaprsp.addr;
			rsp->type = swaprsp.type;
			rsp->answer = swaprsp.answer;
			rsp->id_num = swaplong(swaprsp.id_num);
		}
	}
}
#endif

#ifdef vax
struct ctl_response_sun {
	char type;
	char answer;
	unsigned short id_num2;
	unsigned short id_num1;
	short sin_family;
	short sin_port;
	short sin_addr2;
	short sin_addr1;
};

swapresponse(rsp)
	CTL_RESPONSE *rsp;
{
	struct ctl_response_sun swaprsp;
	
	if (rsp->addr.sin_family != AF_INET) {
		bcopy(rsp, &swaprsp, sizeof(struct ctl_response_sun));
		if (swaprsp.sin_family == swapshort(AF_INET)) {
			rsp->type = swaprsp.type;
			rsp->answer = swaprsp.answer;
			rsp->id_num = swapshort(swaprsp.id_num1)
			    | (swapshort(swaprsp.id_num2) << 16);
			rsp->addr.sin_family = swapshort(swaprsp.sin_family);
 			rsp->addr.sin_port = swaprsp.sin_port;
			rsp->addr.sin_addr.s_addr =
			    swaprsp.sin_addr2 | (swaprsp.sin_addr1 << 16);
		}
	}
}
#endif
