/*
 * Copyright (c) 1983 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)talkd.h	5.3 (Berkeley) %G%
 */

#include <sys/types.h>
#include <sys/socket.h>
/*
 * This describes the protocol used by the talk server and clients.
 *
 * The talk server acts a repository of invitations, responding to
 * requests by clients wishing to rendezvous for the purpose of
 * holding a conversation.  In normal operation, a client, the caller,
 * initiates a rendezvous by sending a CTL_MSG to the server of
 * type LOOK_UP.  This causes the server to search its invitation
 * tables to check if an invitation currently exists for the caller
 * (to speak to the callee specified in the message).  If the lookup
 * fails, the caller then sends an ANNOUNCE message causing the server
 * to broadcast an announcement on the callee's login ports requesting
 * contact.  When the callee responds, the local server uses the
 * recorded invitation to respond with the appropriate rendezvous
 * address and the caller and callee client programs establish a
 * stream connection through which the conversation takes place.
 */

/*
 * Client->server request message format.
 */
typedef struct {
	u_char	vers;		/* protocol version */
	u_char	type;		/* request type, see below */
	u_char	answer;		/* not used */
	u_char	pad;
	u_long	id_num;		/* message id */
	struct	sockaddr addr;
	struct	sockaddr ctl_addr;
	long	pid;		/* caller's process id */
#define	NAME_SIZE	12
	char	l_name[NAME_SIZE];/* caller's name */
	char	r_name[NAME_SIZE];/* callee's name */
#define	TTY_SIZE	16
	char	r_tty[TTY_SIZE];/* callee's tty name */
} CTL_MSG;

/*
 * Server->client response message format.
 */
typedef struct {
	u_char	vers;		/* protocol version */
	u_char	type;		/* type of request message, see below */
	u_char	answer;		/* respose to request message, see below */
	u_char	pad;
	u_long	id_num;		/* message id */
	struct	sockaddr addr;	/* address for establishing conversation */
} CTL_RESPONSE;

#define	TALK_VERSION	1		/* protocol version */

/* message type values */
#define LEAVE_INVITE	0	/* leave invitation with server */
#define LOOK_UP		1	/* check for invitation by callee */
#define DELETE		2	/* delete invitation by caller */
#define ANNOUNCE	3	/* announce invitation by caller */

/* answer values */
#define SUCCESS		0	/* operation completed properly */
#define NOT_HERE	1	/* callee not logged in */
#define FAILED		2	/* operation failed for unexplained reason */
#define MACHINE_UNKNOWN	3	/* caller's machine name unknown */
#define PERMISSION_DENIED 4	/* callee's tty doesn't permit announce */
#define UNKNOWN_REQUEST	5	/* request has invalid type value */
#define	BADVERSION	6	/* request has invalid protocol version */
#define	BADADDR		7	/* request has invalid addr value */
#define	BADCTLADDR	8	/* request has invalid ctl_addr value */

/*
 * Operational parameters.
 */
#define MAX_LIFE	60	/* max time daemon saves invitations */
/* RING_WAIT should be 10's of seconds less than MAX_LIFE */
#define RING_WAIT	30	/* time to wait before resending invitation */
