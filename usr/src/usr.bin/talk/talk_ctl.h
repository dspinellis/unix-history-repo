/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)talk_ctl.h	5.3 (Berkeley) %G%
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
