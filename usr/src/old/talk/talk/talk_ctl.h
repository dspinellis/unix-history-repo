/*-
 * Copyright (c) 1983, 1985
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)talk_ctl.h	5.1 (Berkeley) 6/6/85
 */

#include "ctl.h"
#include "talk.h"
#include <errno.h>

extern	int errno;

extern	struct sockaddr_in daemon_addr;
extern	struct sockaddr_in ctl_addr;
extern	struct sockaddr_in my_addr;
extern	struct in_addr my_machine_addr;
extern	struct in_addr his_machine_addr;
extern	u_short daemon_port;
extern	int ctl_sockt;
extern	CTL_MSG msg;

void	ctl_transact __P((struct in_addr, CTL_MSG, int, CTL_RESPONSE *));
