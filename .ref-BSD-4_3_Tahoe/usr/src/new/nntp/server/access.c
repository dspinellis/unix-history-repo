#ifndef lint
static char	*sccsid = "@(#)access.c	1.16	(Berkeley) 10/15/87";
#endif

#include "common.h"
#include <sys/socket.h>

#define	SNETMATCH	1
#define	NETMATCH	2

/*
 * host_access -- determine if the client has permission to
 * read, transfer, and/or post news.  read->transfer.
 * We switch on socket family so as to isolate network dependent code.
 *
 *	Parameters:	"canread" is a pointer to storage for
 *			an integer, which we set to 1 if the
 *			client can read news, 0 otherwise.
 *
 *			"canpost" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can post news, 0 otherwise.
 *
 *			"canxfer" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can transfer news, 0 otherwise.
 *
 *			"gdlist" is a comma separated list of
 *			newsgroups/distributions which the client
 *			can access.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	None.
 */

#ifdef LOG
char	hostname[256];
#endif

host_access(canread, canpost, canxfer, gdlist)
	int		*canread, *canpost, *canxfer;
	char		*gdlist;
{
	int		sockt;
	int		length;
	struct sockaddr	sa;
	int		match;
	int		count;
	char		hostornet[MAX_STRLEN];
	char		readperm[MAX_STRLEN];
	char		postperm[MAX_STRLEN];
	char		groups[MAX_STRLEN];
	char		host_name[MAX_STRLEN];
	char		net_name[MAX_STRLEN];
	char		snet_name[MAX_STRLEN];
	char		line[MAX_STRLEN];
	register char	*cp;
	register FILE	*acs_fp;

	gdlist[0] = '\0';

#ifdef DEBUG
	*canread = *canpost = *canxfer = 1;
	return;
#endif

	*canread = *canpost = *canxfer = 0;

	sockt = fileno(stdin);
	length = sizeof (sa);

	if (getpeername(sockt, &sa, &length) < 0) {
		if (isatty(sockt)) {
#ifdef LOG
			(void) strcpy(hostname, "stdin");
#endif
			*canread = 1;
		} else {
#ifdef SYSLOG
			syslog(LOG_ERR, "host_access: getpeername: %m");
#endif
#ifdef LOG
			(void) strcpy(hostname, "unknown");
#endif
		}
		return;
	}

	switch (sa.sa_family) {
	case AF_INET:
		inet_netnames(sockt, &sa, net_name, snet_name, host_name);
		break;

#ifdef DECNET
	case AF_DECnet:
		dnet_netnames(sockt, &sa, net_name, snet_name, host_name);
		break;
#endif

	default:
#ifdef SYSLOG
		syslog(LOG_ERR, "unknown address family %ld", sa.sa_family);
#endif
		return;
	};

	/* Normalize host name to lower case */

	for (cp = host_name; *cp; cp++)
		if (isupper(*cp))
			*cp = tolower(*cp);

#ifdef LOG
	syslog(LOG_INFO, "%s connect\n", host_name);
	(void) strcpy(hostname, host_name);
#endif

	/*
	 * We now we have host_name, snet_name, and net_name.
	 * Our strategy at this point is:
	 *
	 * for each line, get the first word
	 *
	 *	If it matches "host_name", we have a direct
	 *		match; parse and return.
	 *
	 *	If it matches "snet_name", we have a subnet match;
	 *		parse and set flags.
	 *
	 *	If it matches "net_name", we have a net match;
	 *		parse and set flags.
	 *
	 *	If it matches the literal "default", note we have
	 *		a net match; parse.
	 */

	acs_fp = fopen(accessfile, "r");
	if (acs_fp == NULL) {
#ifdef SYSLOG
		syslog(LOG_ERR, "access: fopen %s: %m", accessfile);
#endif
		return;
	}

	while (fgets(line, sizeof(line), acs_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if ((cp = index(line, '#')) != NULL)
			*cp = '\0';
		if (*line == '\0')
			continue;

		count = sscanf(line, "%s %s %s %s",
				hostornet, readperm, postperm, groups);

		if (count < 4) {
			if (count < 3)
				continue;
			groups[0] = '\0';	/* No groups specified */
		}

		if (streql(hostornet, host_name)) {
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (*canread || readperm[0] == 'X'
					     || readperm[0] == 'x');
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
			break;
		}

		if (*snet_name && streql(hostornet, snet_name)) {
			match = SNETMATCH;
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (*canread || readperm[0] == 'X'
					     || readperm[0] == 'x');
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
		}

		if (match != SNETMATCH && (streql(hostornet, net_name) ||
		    streql(hostornet, "default"))) {
			match = NETMATCH;
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (*canread || readperm[0] == 'X'
					     || readperm[0] == 'x');
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) strcpy(gdlist, groups);
		}
	}

	(void) fclose(acs_fp);
}
