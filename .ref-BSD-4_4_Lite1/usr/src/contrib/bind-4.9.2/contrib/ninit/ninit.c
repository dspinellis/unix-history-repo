/*
 * Named init --- sits around and restarts named when necessary
 *
 * Written by Theodore Ts'o.  Copyright 1991.
 * 
 * Use this code however you want, as long as you don't try to make
 * money off of it and as long as you don't claim it's yours.
 *
 *	$Header: /afs/net.mit.edu/project/bind/named/RCS/ninit.c,v 1.3 91/05/13 17:30:58 tytso Exp $
 * 	$Source: /afs/net.mit.edu/project/bind/named/RCS/ninit.c,v $
 *
 * Note that ninit requires that named be modified so that it accepts
 * the -n option.  This option to named asks named not to fork into
 * the background when it is started up.  This is necessary because
 * ninit wants to be notified when the named process exits.
 *
 * Usage: ninit [options] [named boot file]
 *
 * Options:
 * 	-n <negative niceness>	Run the named process with the nice level
 * 				*reduced* argument level.  This is useful
 * 				on mailhub, when you have 30+ sendmail
 * 				processes and one named process, and you
 * 				want to give the named process a
 * 				higher priority.
 *
 * 	-s <stats interval>	This option controls how often (in
 * 				seconds) named should be strobed to
 * 				update the named.stats file.
 *
 * 	-c <check interval>	This option controls how often (in
 * 				seconds) ninit should check to make
 * 				sure named is still working.
 *
 * 	-f <syslog facility>	This option controls which syslog facility
 * 				ninit should use.
 *
 * 	-d <debug level>	This option is passed to named to turn
 * 				on nameserver debugging.
 *
 * 	-N			This option tells ninit not fork and go
 * 				into the background when it is started
 * 				up.
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <syslog.h>
#include <errno.h>
#include <signal.h>
#include <netdb.h>
#include <ctype.h>
#include <sys/socket.h>
#include <arpa/nameser.h>
#include <netinet/in.h>
#include <resolv.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>

/*
 * Configuration section.  It should be fairly obvious.....
 *
 * FORKING_NAMED is intended for site who do not have source code to
 * their named, and so cannot add the -n option to named.
 *
 * TEST_HOST specifies the host which ninit should try to resolve when
 * checking to see if named is working.
 */

#define TEST_HOST "MIT.EDU"
#define NAMED "/etc/named"
#define NAMED_PID_FILE "/etc/named.pid"
#define PID_FILE "/etc/ninit.pid"
#define DEFAULT_FACILITY LOG_LOCAL4
/* #define FORKING_NAMED */

extern int h_errno;

int	named_pid = -1;
int	terminating_named = 0;
int	named_restarting = 0;
int	child_died = 0;

int	stats_timer = 0;
int	check_timer = 0;

char	*named_file = 0;
int	named_nice = 0;
int	debug = 0;
int	syslog_facility = DEFAULT_FACILITY;
char	*progname = 0;
int	nofork = 0;
int	stats_interval = 300;
int	check_interval = 60;

struct in_addr local_addr;
	
void	start_named(), PRS(), usage();
int	decode_facility();

struct code {
        char    *name;
        int     facility;
};

struct code     FacNames[] = {
        "kern",         LOG_KERN,
        "user",         LOG_USER,
        "mail",         LOG_MAIL,
        "daemon",       LOG_DAEMON,
        "auth",         LOG_AUTH,
        "syslog",       LOG_SYSLOG,
        "lpr",          LOG_LPR,
#ifdef LOG_NEWS
        "news",         LOG_NEWS,
#endif
#ifdef LOG_UUCP
        "uucp",         LOG_UUCP,
#endif
        "local0",       LOG_LOCAL0,
        "local1",       LOG_LOCAL1,
        "local2",       LOG_LOCAL2,
        "local3",       LOG_LOCAL3,
        "local4",       LOG_LOCAL4,
        "local5",       LOG_LOCAL5,
        "local6",       LOG_LOCAL6,
        "local7",       LOG_LOCAL7,
        "security",     LOG_AUTH,
#ifdef LOG_MARK
        "mark",         LOG_MARK,
#endif
        NULL,           -1
};

int alarm_chk()
{
	int	named_active = 1;
	int	do_check = 0;
	int	next_alarm;
	
	if ((named_pid <= 0) || named_restarting || terminating_named) {
		named_active = 0;
	}
	/*
	 * We could put something here to increment the timers by alarm(0),
	 * but we won't bother for now.
	 *
	 * Check to see if the timers have expired; if so, run the events
	 */
	if (named_active && (stats_timer <= 0)) {
		(void) rename("/usr/tmp/named.stats",
			      "/usr/tmp/named.stats.old");
		kill(named_pid, SIGIOT);
		stats_timer = stats_interval;
	}
	if (named_active && (check_timer <= 0)) {
		/*
		 * We run this one after restarting the alarm because
		 * it might take a while
		 */
		do_check++;
		check_timer = check_interval;
	}
	/*
	 * Now figure out when the next time we need to run.
	 */
	if (stats_timer < check_timer)
		next_alarm = stats_timer;
	else
		next_alarm = check_timer;
	stats_timer -= next_alarm;
	check_timer -= next_alarm;
	alarm(next_alarm);
	/*
	 * Now, perform the named check if necessary
	 */
	if (do_check) {
		(void) gethostbyname(TEST_HOST);
		if (h_errno == TRY_AGAIN) {
			syslog(LOG_ERR, "Named hosed!  Restarting...");
			restart_named();
		}
	}
}

int restart_named()
{
	if ((named_pid <= 0) || named_restarting || terminating_named)
		return;
	terminating_named++;
	kill(named_pid, SIGTERM);
}

int die()
{
	syslog(LOG_NOTICE, "/etc/ninit received SIGTERM, exiting");
	if (named_pid > 0) {
		kill(named_pid, SIGTERM);
		syslog(LOG_NOTICE, "named killed as part of ninit shutdown");
	}
	(void) unlink(NAMED_PID_FILE);
	(void) unlink(PID_FILE);
	exit(0);
}

int mourner()
{
	child_died++;
}

main(argc, argv)
	int	argc;
	char	**argv;
{
	int	pid;
	union wait status;
	
	PRS(argc, argv);
	daemon_setup();
	openlog("ninit", LOG_PID|LOG_CONS, syslog_facility);
	syslog(LOG_INFO, "ninit started, pid = %d", getpid());
	signal(SIGALRM, alarm_chk);
	signal(SIGHUP, restart_named);
	signal(SIGTERM, die);
	signal(SIGCHLD, mourner);
	start_named();
	alarm_chk();
	
	while (1) {
		pid = wait(&status);

		if (pid == -1) {
			if (errno == ECHILD) {
				named_pid = -1;
				start_named();
				continue;
			} else {
				syslog(LOG_CRIT,
				       "Error in wait: %s",
				       sys_errlist[errno]);
				sleep(120);
				continue;
			}
		}
		named_pid = -1;
		if (terminating_named || (status.w_termsig == SIGTERM)) {
			terminating_named = 0;
		} else if (status.w_termsig) {
			syslog(LOG_ERR, "Named terinated with signal %d%s",
			       status.w_termsig,
			       status.w_coredump ? " (core dumped)" : "");
		} else {
			syslog(status.w_retcode ? LOG_ERR : LOG_NOTICE,
			       "named termined with exit status %d",
			       status.w_retcode);
		}
		start_named();
	}
	
}

void start_named()
{
	int	pid;
	int	tries = 3;
	FILE	*f;
	char	debug_buf[24];
	char	*argv[10];
	int	argc = 0;

	if (f = fopen(NAMED_PID_FILE, "r")) {
		fscanf(f, "%d", &pid);
		fclose(f);
		if ((pid > 0) && !kill(pid, 0)) {
			/* There is a running named already, kill it. */
			syslog(LOG_WARNING,
			       "Killing already existing named, pid = %d",
			       pid);
			kill(pid, 15);
		}
	}

	if (named_restarting++ > 3) {
		syslog(LOG_ALERT,
		       "Couldn't start named after three tries, sleeping");
		sleep(180);
		named_restarting = 1;
	}
	child_died = 0;
	if ((pid = vfork()) == 0) {
		argv[argc++]  = "named";
		if (debug) {
			argv[argc++] = "-d";
			sprintf(debug_buf, "%d", debug);
			argv[argc++] = debug_buf;
		}
#ifdef FORKING_NAMED
		else
			argv[argc++] = "-d";
#else
		argv[argc++] = "-n";
#endif
		argv[argc++] = named_file;
		argv[argc++] = 0;
		execv(NAMED, argv);
		syslog(LOG_ERR, "Couldn't start named: %s",
		       sys_errlist[errno]);
		exit(1);
	} else if (pid == -1) {
		syslog(LOG_ERR, "Couldn't fork to start named: %s",
		       sys_errlist[errno]);
		sleep(180);
		return;
	} else {
		syslog(LOG_DEBUG, "Named process started, pid = %d", pid);
		named_pid = pid;
		setpriority(PRIO_PROCESS, pid, named_nice);
		named_restarting++;
		/* Stall until the named is really working */
		do {
			if (child_died)
				return;
			(void) gethostbyname(TEST_HOST);
		} while (h_errno == TRY_AGAIN);
		syslog(LOG_INFO, "Named successfully started, pid = %d\n",
		       pid);
		named_restarting = 0;
#ifdef FORKING_NAMED
		/* Turn off debugging */
		if (!debug)
			kill(named_pid, SIGUSR2);
#endif
		/*
		 * Reset timers...
		 */
		stats_timer = stats_interval;
		check_timer = check_interval;
		alarm_chk();
	}
}

void PRS(argc, argv)
	int	argc;
	char	**argv;
{
	register char	*word, ch;
	char	*buf;
	char	*fac = NULL;
	
	local_addr.s_addr = inet_addr("127.0.0.1");
	res_init();
	_res.nscount = 1;	/* One nameserver --- the local one */
	_res.nsaddr.sin_addr = local_addr;
	_res.retry = 2;
	_res.retrans = RES_TIMEOUT;
	progname = *argv++;
	while (word = *argv++) {
		if (*word == '-') {
			word++;
			while (word && (ch = *word++)) {
				switch(ch){
				case 'n': 	/* Niceness */
					if (*word)
						buf = word;
					else
						buf = *argv++;
					if (!buf)
						usage();
					named_nice = 0 - atoi(buf);
					word = 0;
					break;
				case 's': 	/* Stats int. */
					if (*word)
						buf = word;
					else
						buf = *argv++;
					if (!buf)
						usage();
					stats_interval = atoi(buf);
					word = 0;
					break;
				case 'c': 	/* Check int. */
					if (*word)
						buf = word;
					else
						buf = *argv++;
					if (!buf)
						usage();
					check_interval = atoi(buf);
					word = 0;
					break;
				case 'f': 	/* Syslog facility */
					if (*word)
						fac = word;
					else
						fac = *argv++;
					if (!fac)
						usage();
					word = 0;
					break;
				case 'd': 	/* Debug */
					if (*word)
						buf = word;
					else
						buf = *argv++;
					if (!buf)
						usage();
					debug = atoi(buf);
					word = 0;
					break;
				case 'N': 	/* Nofork */
					nofork++;
					break;
				default:
					usage();
				}
				
			}
		} else {
			if (named_file)
				usage();
			else
				named_file = word;
		}
	}
	if (fac) {
		syslog_facility = decode_facility(fac);
		if (syslog_facility < 0) {
			fprintf(stderr, "%s is not a valid facility\n", fac);
			exit(1);
		}
	}
}

void usage()
{
	fprintf(stderr,
		"Usage: %s [OPTIONS] named_boot_file\n\n", progname);
	fprintf(stderr, "Where the options can be: \n");
	fprintf(stderr, "\t-d debug_level\tTurns on debugging\n");
	fprintf(stderr, "\t-n nice arg\tRenices the named by -nice arg\n");
	fprintf(stderr, "\t-f facility\tUses the specified syslog facility\n");
	fprintf(stderr, "\t-s stats_interval\tSpecifies the statistics polling interval\n");
	fprintf(stderr, "\t-c check_interval\tSpecifies the named check interval\n");
	fprintf(stderr, "\t-N\t\tCauses ninit not to fork on startup.\n");
	exit(1);
}

daemon_setup()
{
	int	n, pid;
	FILE	*f;

	if (f = fopen(PID_FILE, "r")) {
		fscanf(f, "%d", &pid);
		fclose(f);
		if (!kill(pid, 0)) {
			fprintf(stderr,
				"ninit is already running on pid = %d\n",
				pid);
			exit(1);
		}
	}
	
	if (!nofork) {
		if (fork() > 0)
			exit(0);
		n = open("/dev/null", O_RDONLY);
		(void) dup2(n, 0);
		(void) dup2(n, 1);
		(void) dup2(n, 2);
		if (n > 2)
			(void) close(n);
	}
	
	if ((f = fopen(PID_FILE, "w")) == NULL)
		syslog(LOG_ERR, "Couldn't open pid file for write, %s",
		       sys_errlist[errno]);
	fprintf(f, "%d\n", getpid());
	fclose(f);
}

int decode_facility(name)
	char	*name;
{
	char	buf[40], *src, *dest;
	struct code	*p;
	int	i;

	for (src = name, dest = buf, i = 0; *src && i < 40; src++, dest++, i++)
		*dest = (isupper(*src)) ? tolower(*src) : *src;
	for (p = FacNames; p->name; p++)
		if (!strcmp(buf, p->name))
			return(p->facility);
	return(-1);
}

		
	
	
