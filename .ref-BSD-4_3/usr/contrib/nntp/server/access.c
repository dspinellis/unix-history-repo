#ifndef lint
static char	*sccsid = "@(#)access.c	1.4	(Berkeley) 3/12/86";
#endif

#include "common.h"
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

/*
 * host_access -- determine if the client has permission to
 *	read, transfer, and/or post news.  read->transfer.
 *
 *	Parameters:	"read" is a pointer to storage for
 *			an integer, which we set to 1 if the
 *			client can read news, 0 otherwise.
 *
 *			"post" is a pointer to storage for
 *			an integer,which we set to 1 if the
 *			client can post news, 0 otherwise.
 *
 *	Returns:	Nothing.
 *
 *	Side effects:	None.
 */

#ifdef LOG
char	hostname[256];
#endif

host_access(canread, canpost, canxfer)
int	*canread, *canpost, *canxfer;
{
	char		hostornet[MAX_STRLEN];
	char		readperm[MAX_STRLEN], postperm[MAX_STRLEN];
	char		host_name[MAX_STRLEN], net_name[MAX_STRLEN];
	char		line[MAX_STRLEN];
	char		*cp;
	int		ncanread, ncanpost, ncanxfer;
	int		netmatch;
	int		count, sockt, length;
	unsigned long	net_addr;
	struct netent	*np;
	struct sockaddr_in sin;
	struct hostent	*hp;
	FILE		*acs_fp;

	*canread = *canpost = *canxfer = 0;

	sockt = fileno(stdin);
	length = sizeof (struct sockaddr_in);
#ifdef DEBUG
	*canread = *canpost = *canxfer = 1;
	return;
#endif

	if (getpeername(sockt, (struct sockaddr *) &sin, &length) < 0) {
		if (isatty(sockt)) {
#ifdef LOG
			(void) strcpy(hostname, "stdin");
#endif
			*canread = 1;
		} else {
			syslog(LOG_ERR, "host_access: getpeername: %m");
#ifdef LOG
			(void) strcpy(hostname, "unknown");
#endif
		}
		return;
	}

	/*
	 * At this point, sin.sin_addr.s_addr is the address of
	 * the host in network order.
	 */

	net_addr = inet_netof(sin.sin_addr);	/* net_addr in host order */

	np = getnetbyaddr(net_addr, AF_INET);
	if (np != NULL)
		(void) strcpy(net_name, np->n_name);
	else
		(void) strcpy(net_name,inet_ntoa(*(struct in_addr *)&net_addr));

	hp = gethostbyaddr((char *) &sin.sin_addr.s_addr, sizeof(long),
		AF_INET);
	if (hp != NULL)
		(void) strcpy(host_name, hp->h_name);
	else
		(void) strcpy(host_name, inet_ntoa(sin.sin_addr));

#ifdef LOG
	syslog(LOG_INFO, "%s connect\n", host_name);
	(void) strcpy(hostname, host_name);
#endif

	/*
	 * So, now we have host_name and net_name.
	 * Our strategy at this point is:
	 *
	 * for each line, get the first word
	 *
	 *	If it matches "host_name", we have a direct
	 *		match; parse and return.
	 *
	 *	If it matches "net_name", we have a net match;
	 *		parse and set flags.
	 *
	 *	If it matches the literal "default", note we have
	 *		a net match; parse.
	 */

	acs_fp = fopen(ACCESS_FILE, "r");
	if (acs_fp == NULL)
		return;

	while (fgets(line, sizeof(line), acs_fp) != NULL) {
		if ((cp = index(line, '\n')) != NULL)
			*cp = '\0';
		if ((cp = index(line, '#')) != NULL)
			*cp = '\0';
		if (*line == '\0')
			continue;

		count = sscanf(line, "%s %s %s", hostornet, readperm, postperm);

		if (count < 3)
			continue;

		if (strcmp(hostornet, host_name) == 0) {
			*canread = (readperm[0] == 'r' || readperm[0] == 'R');
			*canxfer = (*canread || readperm[0] == 'X'
					     || readperm[0] == 'x');
			*canpost = (postperm[0] == 'p' || postperm[0] == 'P');
			(void) fclose(acs_fp);
			return;
		}

		if (strcmp(hostornet, net_name) == 0 ||
		    strcmp(hostornet, "default") == 0) {
			netmatch = 1;
			ncanread = (readperm[0] == 'r' || readperm[0] == 'R');
			ncanxfer = (ncanread || readperm[0] == 'X'
					     || readperm[0] == 'x');
			ncanpost = (postperm[0] == 'p' || postperm[0] == 'P');
		}
	}

	(void) fclose(acs_fp);

	if (netmatch) {
		*canread = ncanread;
		*canpost = ncanpost;
		*canxfer = ncanxfer;
	}
}
