/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)talk_ctl.h	5.5 (Berkeley) %G%
 */

#include <sys/types.h>
#include <protocols/talkd.h>
#include <netinet/in.h>
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
