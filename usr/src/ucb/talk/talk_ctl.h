/* $Header: talk_ctl.h 1.1 83/03/26 14:36:39 moore Exp $ */

#include "ctl.h"
#include "talk.h"
#include <errno.h>

extern int errno;

extern struct sockaddr_in daemon_addr;
extern struct sockaddr_in ctl_addr;
extern struct sockaddr_in my_addr;
extern struct in_addr my_machine_addr;
extern struct in_addr his_machine_addr;
extern u_short daemon_port;
extern int ctl_sockt;
extern CTL_MSG msg;
