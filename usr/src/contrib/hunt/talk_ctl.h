/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)talk_ctl.h	5.2 (Berkeley) 3/13/86
 */

#include <sys/types.h>
#include <netinet/in.h>
#include <sys/socket.h>

#ifdef TALK_43
#include <protocols/talkd.h>
#else

#define	NAME_SIZE	9
#define	TTY_SIZE	16
#ifndef MAXHOSTNAMELEN
#define	MAXHOSTNAMELEN	256
#endif

#define MAX_LIFE	60	/* max time daemon saves invitations */
/* RING_WAIT should be 10's of seconds less than MAX_LIFE */
#define RING_WAIT	30	/* time to wait before refreshing invitation */

/* type values */
#define LEAVE_INVITE	0
#define LOOK_UP		1
#define DELETE		2
#define ANNOUNCE	3

/* answer values */
#define SUCCESS		0
#define NOT_HERE	1
#define FAILED		2
#define MACHINE_UNKNOWN	3
#define PERMISSION_DENIED 4
#define UNKNOWN_REQUEST	5

typedef struct ctl_response {
	char	type;
	char	answer;
	int	id_num;
	struct	sockaddr_in addr;
} CTL_RESPONSE;

typedef struct ctl_msg {
	char	type;
	char	l_name[NAME_SIZE];
	char	r_name[NAME_SIZE];
	int	id_num;
	int	pid;
	char	r_tty[TTY_SIZE];
	struct	sockaddr_in addr;
	struct	sockaddr_in ctl_addr;
} CTL_MSG;
#endif

#include <errno.h>
#ifdef LOG
#include <syslog.h>
#endif

extern	int errno;

extern	struct sockaddr_in daemon_addr;
extern	struct sockaddr_in ctl_addr;
extern	struct sockaddr_in my_addr;
extern	struct in_addr my_machine_addr;
extern	struct in_addr his_machine_addr;
extern	u_short daemon_port;
extern	int ctl_sockt;
extern	CTL_MSG msg;

#ifdef LOG
#define	p_error(str)	syslog(LOG_WARNING, "faketalk %s: %m", str)
#else
#define	p_error(str)	perror(str)
#endif
