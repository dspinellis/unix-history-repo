/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include "bsd.h"

#if	defined(TALK_43) || defined(TALK_42)

# include	<stdio.h>
# include	<string.h>
# include	"talk_ctl.h"
# include	<sys/param.h>
# include	<netdb.h>

extern	CTL_MSG	msg;

struct	hostent	*gethostbyname();
struct	servent	*getservbyname();

static	char	hostname[MAXHOSTNAMELEN];
char		*my_machine_name;

/*
 * Determine the local user and machine
 */
get_local_name(my_name)
	char	*my_name;
{
	struct	hostent	*hp;
	struct	servent	*sp;

	/* Load these useful values into the standard message header */
	msg.id_num = 0;
	(void) strncpy(msg.l_name, my_name, NAME_SIZE);
	msg.l_name[NAME_SIZE - 1] = '\0';
	msg.r_tty[0] = '\0';
	msg.pid = getpid();
# ifdef TALK_43
	msg.vers = TALK_VERSION;
	msg.addr.sa_family = htons(AF_INET);
	msg.ctl_addr.sa_family = htons(AF_INET);
# else
	msg.addr.sin_family = htons(AF_INET);
	msg.ctl_addr.sin_family = htons(AF_INET);
# endif

	(void) gethostname(hostname, sizeof (hostname));
	my_machine_name = hostname;
	/* look up the address of the local host */
	hp = gethostbyname(my_machine_name);
	if (hp == (struct hostent *) 0) {
		printf("This machine doesn't exist. Boy, am I confused!\n");
		exit(-1);
	}
	memcpy(&my_machine_addr, hp->h_addr, hp->h_length);
	/* find the daemon portal */
# ifdef TALK_43
	sp = getservbyname("ntalk", "udp");
# else
	sp = getservbyname("talk", "udp");
# endif
	if (sp == 0) {
# ifdef LOG
		syslog(LOG_ERR, "This machine doesn't support talk");
# else
		perror("This machine doesn't support talk");
# endif
		exit(-1);
	}
	daemon_port = sp->s_port;
}

/*
 * Determine the remote user and machine
 */
get_remote_name(his_address)
	char	*his_address;
{
	char		*his_name;
	char		*his_machine_name;
	char		*ptr;
	struct	hostent	*hp;


	/* check for, and strip out, the machine name of the target */
	for (ptr = his_address; *ptr != '\0' && *ptr != '@' && *ptr != ':'
					&& *ptr != '!' && *ptr != '.'; ptr++)
		continue;
	if (*ptr == '\0') {
		/* this is a local to local talk */
		his_name = his_address;
		his_machine_name = my_machine_name;
	} else {
		if (*ptr == '@') {
			/* user@host */
			his_name = his_address;
			his_machine_name = ptr + 1;
		} else {
			/* host.user or host!user or host:user */
			his_name = ptr + 1;
			his_machine_name = his_address;
		}
		*ptr = '\0';
	}
	/* Load these useful values into the standard message header */
	(void) strncpy(msg.r_name, his_name, NAME_SIZE);
	msg.r_name[NAME_SIZE - 1] = '\0';

	/* if he is on the same machine, then simply copy */
	if (bcmp((char *) &his_machine_name, (char *) &my_machine_name,
						sizeof(his_machine_name)) == 0)
		memcpy(&his_machine_addr, &my_machine_addr,
						sizeof(his_machine_name));
	else {
		/* look up the address of the recipient's machine */
		hp = gethostbyname(his_machine_name);
		if (hp == (struct hostent *) 0)
			return 0;			/* unknown host */
		memcpy(&his_machine_addr, hp->h_addr, hp->h_length);
	}
	return 1;
}
#endif
