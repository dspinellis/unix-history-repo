/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

#include "bsd.h"

#if	defined(TALK_43) || defined(TALK_42)

# include	<stdio.h>
# include	<netdb.h>
# include	"talk_ctl.h"
# include	<ctype.h>
# include	<signal.h>
# include	<sys/time.h>
extern	int	errno;

extern	char	*index(), *rindex();

# define	TRUE		1
# define	FALSE		0

/* defines for fake talk message to announce start of game */
# ifdef TALK_43
# define	MASQUERADE	"\"Hunt Game\""
# else
# define	MASQUERADE	"HuntGame"
# endif
# define	RENDEZVOUS	"hunt-players"
# define	ARGV0		"HUNT-ANNOUNCE"

extern	char		*my_machine_name;
extern	char		*First_arg, *Last_arg;

/*
 *	exorcise - disspell zombies
 */

exorcise()
{
	(void) wait(0);
}

/*
 *	query the local SMTP daemon to expand the RENDEZVOUS mailing list
 *	and fake a talk request to each address thus found.
 */

faketalk()
{
	struct	servent		*sp;
	char			buf[BUFSIZ];
	FILE			*f;
	int			service;	/* socket of service */
	struct	sockaddr_in	des;		/* address of destination */
	char			*a, *b;
	extern	char		**environ;

	(void) signal(SIGCHLD, exorcise);

	if (fork() != 0)
		return;

	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGPIPE, SIG_IGN);

	/*
	 *	change argv so that a ps shows ARGV0
	 */
	*environ = NULL;
	for (a = First_arg, b = ARGV0; a < Last_arg; a++) {
		if (*b)
			*a = *b++;
		else
			*a = ' ';
	}

	/*
	 *	initialize "talk"
	 */
	get_local_name(MASQUERADE);
	open_ctl();

	/*
	 *	start fetching addresses
	 */

	if ((sp = getservbyname("smtp", (char *) NULL)) == NULL) {
# ifdef LOG
		syslog(LOG_ERR, "faketalk: smtp protocol not supported\n");
# else LOG
		fprintf(stderr, "faketalk: stmp protocol not supported\n");
# endif LOG
		_exit(1);
	}

	bzero((char *) &des, sizeof (des));
	des.sin_family = AF_INET;
	des.sin_addr = my_machine_addr;
	des.sin_port = sp->s_port;

	if ((service = socket(des.sin_family, SOCK_STREAM, 0)) < 0) {
# ifdef LOG
		syslog(LOG_ERR, "falktalk:  socket");
# else LOG
		perror("falktalk:  socket");
# endif LOG
		_exit(-1);
	}

	if (connect(service, (struct sockaddr *) &des, sizeof(des)) != 0) {
# ifdef LOG
		syslog(LOG_ERR, "faketalk:  connect");
# else LOG
		perror("faketalk:  connect");
# endif LOG
		_exit(-1);
	}
	if ((f = fdopen(service, "r")) == NULL) {
# ifdef LOG
		syslog(LOG_ERR, "fdopen failed\n");
# else LOG
		fprintf(stderr, "fdopen failed\n");
# endif LOG
		_exit(-2);
	}

	(void) fgets(buf, BUFSIZ, f);
	(void) sprintf(buf, "HELO HuntGame@%s\r\n", my_machine_name);
	(void) write(service, buf, strlen(buf));
	(void) fgets(buf, BUFSIZ, f);
	(void) sprintf(buf, "EXPN %s@%s\r\n", RENDEZVOUS, my_machine_name);
	(void) write(service, buf, strlen(buf));
	while (fgets(buf, BUFSIZ, f) != NULL) {
		char	*s, *t;

		if (buf[0] != '2' || buf[1] != '5' || buf[2] != '0')
			break;
		if ((s = index(buf + 4, '<')) == NULL)
			s = buf + 4, t = buf + strlen(buf) - 1;
		else {
			s += 1;
			if ((t = rindex(s, '>')) == NULL)
				t = s + strlen(s) - 1;
			else
				t -= 1;
		}
		while (isspace(*s))
			s += 1;
		if (*s == '\\')
			s += 1;
		while (isspace(*t))
			t -= 1;
		*(t + 1) = '\0';
		do_announce(s);		/* construct and send talk request */
		if (buf[3] == ' ')
			break;
	}
	(void) shutdown(service, 2);
	(void) close(service);
	_exit(0);
}

/*
 * The msg.id's for the invitations on the local and remote machines.
 * These are used to delete the invitations.
 */

do_announce(s)
	char	*s;
{
	CTL_RESPONSE			response;
	extern	struct	sockaddr_in	ctl_addr;

	get_remote_name(s);	/* setup his_machine_addr, msg.r_name */

# ifdef TALK_43
# if BSD_RELEASE >= 44
	msg.ctl_addr = *(struct osockaddr *) &ctl_addr;
# else
	msg.ctl_addr = *(struct sockaddr *) &ctl_addr;
# endif
	msg.ctl_addr.sa_family = htons(msg.ctl_addr.sa_family);
# else
	msg.ctl_addr = ctl_addr;
	msg.ctl_addr.sin_family = htons(msg.ctl_addr.sin_family);
# endif
	msg.id_num = (int) htonl((u_long) -1);	/* an impossible id_num */
	ctl_transact(his_machine_addr, msg, ANNOUNCE, &response);
	if (response.answer != SUCCESS)
		return;

	/*
	 * Have the daemons delete the invitations now that we
	 * have announced.
	 */

	/* we don't care if cleanup doesn't make it. */
	msg.type = DELETE;
	msg.id_num = (int) htonl(response.id_num);
	daemon_addr.sin_addr = his_machine_addr;
	if (sendto(ctl_sockt, (char *) &msg, sizeof (msg), 0,
			(struct sockaddr *) &daemon_addr, sizeof(daemon_addr))
			!= sizeof(msg))
		p_error("send delete remote");
}
#else
faketalk()
{
	return;
}
#endif
