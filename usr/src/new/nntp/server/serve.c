#ifndef lint
static char	*sccsid = "@(#)serve.c	1.24	(Berkeley) 10/15/87";
#endif

/*
 * Main server routine
 */

#include "common.h"
#include <signal.h>
#include <sys/time.h>

#ifdef LOG
#include <sys/resource.h>
#endif

extern	int	ahbs(), group(), help(), ihave();
extern	int	list(), newgroups(), newnews(), nextlast(), post();
extern	int	slave(), stat(), xhdr();

static struct cmdent {
	char	*cmd_name;
	int	(*cmd_fctn)();
} cmdtbl[] = {
	"article",	ahbs,
	"body",		ahbs,
	"group",	group,
	"head",		ahbs,
	"help",		help,
	"ihave",	ihave,
	"last",		nextlast,
	"list",		list,
	"newgroups",	newgroups,
	"newnews",	newnews,
	"next",		nextlast,
	"post",		post,
	"slave",	slave,
	"stat",		ahbs,
#ifdef XHDR
	"xhdr",		xhdr,
#endif XHDR
};
#define NUMCMDS (sizeof(cmdtbl) / sizeof(struct cmdent))


/*
 * serve -- given a connection on stdin/stdout, serve
 *	a client, executing commands until the client
 *	says goodbye.
 *
 *	Parameters:	None.
 *
 *	Returns:	Exits.
 *
 *	Side effects:	Talks to client, does a lot of
 *			stuff.
 */

serve()
{
	char		line[MAX_STRLEN];
	char		host[MAX_STRLEN];
	char		gdbuf[MAX_STRLEN];
	char		**argp;
	char		*timeptr, *cp;
	int		argnum, i;
	struct timeval	clock, now;
	extern char	*ctime();
#ifdef POSTER
	struct passwd	*pp;
#endif
#ifdef TIMEOUT
	void		timeout();
#endif
#ifdef LOG
	struct rusage	me, children;
	
	grps_acsd = arts_acsd = 0;
#endif

	/* Not all systems pass fd's 1 and 2 from inetd ... */

	(void) close(1);
	(void) close(2);
	(void) dup(0);
	(void) dup(0);

	/* If we're ALONE, then we've already opened syslog */

#ifndef ALONE
# ifdef SYSLOG
#  ifdef BSD_42
	openlog("nntpd", LOG_PID);
#  else
	openlog("nntpd", LOG_PID, SYSLOG);
#  endif
# endif
#endif

#ifdef ALONE
	(void) signal(SIGCHLD, SIG_IGN);
#endif

	/* Ignore SIGPIPE, since we'll see closed connections with read */

	(void) signal(SIGPIPE, SIG_IGN);

	/* Get permissions and see if we can talk to this client */

	host_access(&canread, &canpost, &canxfer, gdbuf);

	if (gethostname(host, sizeof(host)) < 0)
		(void) strcpy(host, "Amnesiac");

	if (!canread && !canxfer) {
		printf("%d %s NNTP server can't talk to you.  Goodbye.\r\n",
			ERR_ACCESS, host);
		(void) fflush(stdout);
#ifdef LOG
		syslog(LOG_INFO, "%s refused connection", hostname);
#endif
		exit(1);
	}

	/* If we can talk, proceed with initialization */

	ngpermcount = get_nglist(&ngpermlist, gdbuf);

#ifdef POSTER
	pp = getpwnam(POSTER);
	if (pp != NULL) {
		uid_poster = pp->pw_uid;
		gid_poster = pp->pw_gid;
	} else
#endif
		uid_poster = gid_poster = 0;

#ifndef FASTFORK
	num_groups = 0;
	num_groups = read_groups();	/* Read in the active file */
#else
	signal(SIGALRM, SIG_IGN);	/* Children don't deal with */
					/* these things */
#endif

	art_fp = NULL;
	argp = (char **) NULL;		/* for first time */

	(void) gettimeofday(&clock, (struct timezone *)NULL);
	timeptr = ctime(&clock.tv_sec);
	if ((cp = index(timeptr, '\n')) != NULL)
		*cp = '\0';
	else
		timeptr = "Unknown date";

	printf("%d %s NNTP server version %s ready at %s (%s).\r\n",
		canpost ? OK_CANPOST : OK_NOPOST,
		host, nntp_version,
		timeptr,
		canpost ? "posting ok" : "no posting");
	(void) fflush(stdout);

	/*
	 * Now get commands one at a time and execute the
	 * appropriate routine to deal with them.
	 */

#ifdef TIMEOUT
	(void) signal(SIGALRM, timeout);
	(void) alarm(TIMEOUT);
#endif TIMEOUT

	while (fgets(line, sizeof(line), stdin) != NULL) {
#ifdef TIMEOUT
		(void) alarm(0);
#endif TIMEOUT

		cp = index(line, '\r');		/* Zap CR-LF */
		if (cp != NULL)
			*cp = '\0';
		else {
			cp = index(line, '\n');
			if (cp != NULL)
				*cp = '\0';
		}

		if ((argnum = parsit(line, &argp)) == 0)
			continue;		/* Null command */
		else {
			for (i = 0; i < NUMCMDS; ++i)
				if (streql(cmdtbl[i].cmd_name, argp[0]))
					break;
			if (i < NUMCMDS)
				(*cmdtbl[i].cmd_fctn)(argnum, argp);
			else {
				if (streql(argp[0], "quit"))
					break;
#ifdef LOG
				syslog(LOG_INFO, "%s unrecognized %s",
					hostname,
					line);
#endif
				printf("%d Command unrecognized.\r\n",
					ERR_COMMAND);
				(void) fflush(stdout);
			}
		}
#ifdef TIMEOUT
		(void) alarm(TIMEOUT);
#endif TIMEOUT
	}

	printf("%d %s closing connection.  Goodbye.\r\n", OK_GOODBYE, host);
	(void) fflush(stdout);


#ifdef LOG
	if (ferror(stdout))
		syslog(LOG_ERR, "%s disconnect: %m", hostname);

	if (getrusage(RUSAGE_SELF, &me) < 0) {
		syslog(LOG_ERR, "getrusage RUSAGE_SELF: %m");
		if (grps_acsd)
			syslog(LOG_INFO, "%s exit %d articles %d groups",
				hostname, arts_acsd, grps_acsd);
		exit(1);
	}
	if (getrusage(RUSAGE_CHILDREN, &children) < 0) {
		syslog(LOG_ERR, "getrusage RUSAGE_CHILDREN: %m");
		if (grps_acsd)
			syslog(LOG_INFO, "%s exit %d articles %d groups",
				hostname, arts_acsd, grps_acsd);
		exit(1);
	}
	if (grps_acsd)
		syslog(LOG_INFO, "%s exit %d articles %d groups",
			hostname, arts_acsd, grps_acsd);
	if (nn_told)
		syslog(LOG_INFO, "%s newnews_stats told %d took %d",
			hostname, nn_told, nn_took);
	if (ih_accepted || ih_rejected || ih_failed)
		syslog(LOG_INFO,
			"%s ihave_stats accepted %d rejected %d failed %d",
			hostname,
			ih_accepted,
			ih_rejected,
			ih_failed);
	(void) gettimeofday(&now, (struct timezone *)NULL);
	(void) sprintf(line, "user %.1f system %.1f elapsed %.1f",
		(float) me.ru_utime.tv_sec +
			(float) me.ru_utime.tv_usec/1000000.0 +
			(float) children.ru_utime.tv_sec +
			(float) children.ru_utime.tv_usec/1000000.0,
		(float) me.ru_stime.tv_sec +
			(float) me.ru_stime.tv_usec/1000000.0 +
			(float) children.ru_stime.tv_sec +
			(float) children.ru_stime.tv_usec/1000000.0,
		(float) (now.tv_sec - clock.tv_sec) +
			(float) now.tv_usec/1000000.0 -
			(float) clock.tv_usec/1000000.0);
			
	syslog(LOG_INFO, "%s times %s", hostname, line);
#endif

#ifdef PROFILE
	profile();
#endif

	exit(0);
}


#ifdef TIMEOUT
/*
 * No activity for TIMEOUT seconds, so print an error message
 * and close the connection.
 */

void
timeout()
{
	printf("%d Timeout after %d seconds, closing connection.\r\n",
		ERR_FAULT, TIMEOUT);
	(void) fflush(stdout);

#ifdef LOG
	syslog(LOG_ERR, "%s timeout", hostname);
#endif LOG

	exit(1);
}
#endif TIMEOUT
