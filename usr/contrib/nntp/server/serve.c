#ifndef lint
static char	*sccsid = "@(#)serve.c	1.6	(Berkeley) 3/12/86";
#endif

/*
 * Main server routine
 */

#include "common.h"

#ifdef LOG
#include <sys/time.h>
#include <sys/resource.h>
#endif

#ifdef FASTFORK
#include <signal.h>
#endif

struct cmdent {
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
};
#define NUMCMDS (sizeof(cmdtbl) / sizeof(struct cmdent))

char	*homedir = SPOOLDIR;

char	*group_array[MAX_GROUPS];
int	num_groups;
int	ingroup = 0;
int	art_ptr;
int	num_arts, art_array[MAX_ARTICLES];
FILE	*art_fp;
int	uid_poster, gid_poster;
int	canpost, canread, canxfer;
extern	char *version;

#ifdef LOG
int	grps_acsd, arts_acsd;
#endif

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
	char	line[MAX_STRLEN], host[MAX_STRLEN];
	char	**argp;
	char	*timeptr, *cp;
	int	argnum, i;
	struct	passwd *pp;
	long	clock, time();
	char	*ctime();

#ifdef LOG
	struct	rusage	rusagebuf;

	grps_acsd = arts_acsd = 0;
#endif

#ifdef BSD_42
	openlog("nntpd", LOG_PID);
#else
	openlog("nntpd", LOG_PID, LOG_FACILITY);
#endif

	/* Get permissions and see if we can talk to this client */

	host_access(&canread, &canpost, &canxfer);

	if (gethostname(host, sizeof(host)) < 0)
		(void) strcpy(host, "Unknown host");

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

	(void) time(&clock);
	timeptr = ctime(&clock);
	if ((cp = index(timeptr, '\n')) != NULL)
		*cp = '\0';
	else
		timeptr = "Unknown date";

	printf("%d %s NNTP server version %s ready at %s (%s).\r\n",
		canpost ? OK_CANPOST : OK_NOPOST,
		host, version,
		timeptr,
		canpost ? "posting ok" : "no posting");
	(void) fflush(stdout);

	/*
	 * Now get commands one at a time and execute the
	 * appropriate routine to deal with them.
	 */

	while (fgets(line, sizeof(line), stdin) != NULL) {

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
				printf("%d Command unrecognized.\r\n",
					ERR_COMMAND);
				(void) fflush(stdout);
			}
		}
	}

	printf("%d %s closing connection.  Goodbye.\r\n", OK_GOODBYE, host);
	(void) fflush(stdout);

#ifdef LOG
	if (getrusage(RUSAGE_SELF, &rusagebuf) < 0) {
		syslog(LOG_ERR, "getrusage RUSAGE_SELF: %m");
		syslog(LOG_INFO, "%s exit %d articles %d groups", hostname,
			arts_acsd, grps_acsd);
	} else {
		syslog(LOG_INFO, "%s exit %d articles %d groups",
			hostname,
			arts_acsd,
			grps_acsd);
		syslog(LOG_INFO, "%s times user %d system %d elapsed %ld",
			hostname,
			rusagebuf.ru_utime.tv_sec,
			rusagebuf.ru_stime.tv_sec,
			time((long *) 0) - clock);
	}
#endif
	exit(0);
}
