/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)look_up.c	5.1 (Berkeley) %G%";
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
	if (connect(sockt, &response.addr, sizeof(response.addr)) != -1)
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
