/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Donn Seeley at UUNET Technologies, Inc.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)init.c	6.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/wait.h>
#include <db.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <time.h>
#include <ttyent.h>
#include <unistd.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifdef SECURE
#include <pwd.h>
#endif

#include "pathnames.h"

/*
 * Until the mythical util.h arrives...
 */
extern int login_tty __P((int));
extern int logout __P((const char *));
extern void logwtmp __P((const char *, const char *, const char *));

/*
 * Sleep times; used to prevent thrashing.
 */
#define	GETTY_SPACING		10	/* fork getty on a port every N secs */
#define	WINDOW_WAIT		 3	/* wait N secs after starting window */
#define	STALL_TIMEOUT		30	/* wait N secs after warning */
#define	DEATH_WATCH		10	/* wait N secs for procs to die */

void handle __P((sig_t, ...));
void delset __P((sigset_t *, ...));

void stall __P((char *, ...));
void warning __P((char *, ...));
void emergency __P((char *, ...));
void disaster __P((int));

/*
 * We really need a recursive typedef...
 * The following at least guarantees that the return type of (*state_t)()
 * is sufficiently wide to hold a function pointer.
 */
typedef long (*state_func_t) __P((void));
typedef state_func_t (*state_t) __P((void));

state_func_t single_user __P((void));
state_func_t runcom __P((void));
state_func_t read_ttys __P((void));
state_func_t multi_user __P((void));
state_func_t clean_ttys __P((void));
state_func_t catatonia __P((void));
state_func_t death __P((void));

enum { AUTOBOOT, FASTBOOT } runcom_mode = AUTOBOOT;

void transition __P((state_t));
state_t requested_transition = runcom;

void setctty __P((char *));

typedef struct session {
	int	se_index;		/* index of entry in ttys file */
	pid_t	se_process;		/* controlling process */
	time_t	se_started;		/* used to avoid thrashing */
	int	se_flags;		/* status of session */
#define	SE_SHUTDOWN	0x1		/* session won't be restarted */
	char	*se_device;		/* filename of port */
	char	*se_getty;		/* what to run on that port */
	char	**se_getty_argv;	/* pre-parsed argument array */
	char	*se_window;		/* window system (started only once) */
	char	**se_window_argv;	/* pre-parsed argument array */
	struct	session *se_prev;
	struct	session *se_next;
} session_t;

void free_session __P((session_t *));
session_t *new_session __P((session_t *, int, struct ttyent *));
session_t *sessions;

char **construct_argv __P((char *));
void start_window_system __P((session_t *));
void collect_child __P((int));
pid_t start_getty __P((session_t *));
void transition_handler __P((int));
void alrm_handler __P((int));
int clang;

int start_logger __P((void));
void clear_session_logs __P((session_t *));
int logger_enable;

int start_session_db __P((void));
void add_session __P((session_t *));
void del_session __P((session_t *));
session_t *find_session __P((pid_t));
DB *session_db;

/*
 * The mother of all processes.
 */
int
main(argc, argv)
	int argc;
	char **argv;
{
	int c;
	struct sigaction sa;
	sigset_t mask;

	/*
	 * Silently dispose of random users running this program.
	 */
	if (getuid() != 0)
		return 1;

	/*
	 * Note that this does NOT open a file...
	 * Does 'init' deserve its own facility number?
	 */
	openlog("init", LOG_CONS|LOG_ODELAY, LOG_AUTH);

	/*
	 * Create an initial session.
	 */
	if (setsid() < 0)
		syslog(LOG_ERR, "setsid failed (initial) %m");

	/*
	 * This code assumes that we always get arguments through flags,
	 * never through bits set in some random machine register.
	 */
	while ((c = getopt(argc, argv, "sf")) != -1)
		switch (c) {
		case 's':
			requested_transition = single_user;
			break;
		case 'f':
			runcom_mode = FASTBOOT;
			break;
		default:
			warning("unrecognized flag '-%c'", c);
			break;
		}

	if (optind != argc)
		warning("ignoring excess arguments");

	/*
	 * We catch or block signals rather than ignore them,
	 * so that they get reset on exec.
	 */
	handle(disaster, SIGABRT, SIGFPE, SIGILL, SIGSEGV,
	       SIGBUS, SIGSYS, SIGXCPU, SIGXFSZ, 0);
	handle(transition_handler, SIGHUP, SIGTERM, SIGTSTP, 0);
	handle(alrm_handler, SIGALRM, 0);
	sigfillset(&mask);
	delset(&mask, SIGABRT, SIGFPE, SIGILL, SIGSEGV, SIGBUS, SIGSYS,
		SIGXCPU, SIGXFSZ, SIGHUP, SIGTERM, SIGTSTP, SIGALRM, 0);
	sigprocmask(SIG_SETMASK, &mask, (sigset_t *) 0);
	sigemptyset(&sa.sa_mask);
	sa.sa_flags = 0;
	sa.sa_handler = SIG_IGN;
	(void) sigaction(SIGTTIN, &sa, (struct sigaction *)0);
	(void) sigaction(SIGTTOU, &sa, (struct sigaction *)0);

	/*
	 * Paranoia.
	 */
	close(0);
	close(1);
	close(2);

	/*
	 * Start the state machine.
	 */
	transition(requested_transition);

	/*
	 * Should never reach here.
	 */
	return 1;
}

/*
 * Associate a function with a signal handler.
 */
void
#ifdef __STDC__
handle(sig_t handler, ...)
#else
handle(va_alist)
	va_dcl
#endif
{
	int sig;
	struct sigaction sa;
	int mask_everything;
	va_list ap;
#ifndef __STDC__
	sig_t handler;

	va_start(ap);
	handler = va_arg(ap, sig_t);
#else
	va_start(ap, handler);
#endif

	sa.sa_handler = handler;
	sigfillset(&mask_everything);

	while (sig = va_arg(ap, int)) {
		sa.sa_mask = mask_everything;
		/* XXX SA_RESTART? */
		sa.sa_flags = sig == SIGCHLD ? SA_NOCLDSTOP : 0;
		sigaction(sig, &sa, (struct sigaction *) 0);
	}
}

/*
 * Delete a set of signals from a mask.
 */
void
#ifdef __STDC__
delset(sigset_t *maskp, ...)
#else
delset(va_alist)
	va_dcl
#endif
{
	int sig;
	va_list ap;
#ifndef __STDC__
	sigset_t *maskp;

	va_start(ap);
	maskp = va_arg(ap, sigset_t *);
#else
	va_start(ap, maskp);
#endif

	while (sig = va_arg(ap, int))
		sigdelset(maskp, sig);
}

/*
 * Log a message and sleep for a while (to give someone an opportunity
 * to read it and to save log or hardcopy output if the problem is chronic).
 * We fork so that we can't block on I/O when writing the message,
 * and so that init proper doesn't acquire another open file.
 * If the fork fails, or the child can't finish, too bad.
 */
void
#ifdef __STDC__
stall(char *message, ...)
#else
stall(va_alist)
	va_dcl
#endif
{
	pid_t pid;
	va_list ap;
#ifndef __STDC__
	char *message;

	va_start(ap);
	message = va_arg(ap, char *);
#else
	va_start(ap, message);
#endif

	if ((pid = fork()) == 0) {
		vsyslog(LOG_ALERT, message, ap);
		_exit(0);
	}
	va_end(ap);
	sleep(STALL_TIMEOUT);
	if (pid != -1)
		waitpid(pid, (int *) 0, WNOHANG);
}

/*
 * Like stall(), but doesn't sleep.
 * If cpp had variadic macros, the two functions could be #defines for another.
 */
void
#ifdef __STDC__
warning(char *message, ...)
#else
warning(va_alist)
	va_dcl
#endif
{
	va_list ap;
#ifndef __STDC__
	char *message;

	va_start(ap);
	message = va_arg(ap, char *);
#else
	va_start(ap, message);
#endif

	if (fork() == 0) {
		vsyslog(LOG_ALERT, message, ap);
		_exit(0);
	}
	va_end(ap);
}

/*
 * Log a message, no forking, no waiting.
 * We take care to close syslog's file descriptor, however.
 */
void
#ifdef __STDC__
emergency(char *message, ...)
#else
emergency(va_alist)
	va_dcl
#endif
{
	va_list ap;
#ifndef __STDC__
	char *message;

	va_start(ap);
	message = va_arg(ap, char *);
#else
	va_start(ap, message);
#endif

	vsyslog(LOG_EMERG, message, ap);
	va_end(ap);
	closelog();
	openlog("init", LOG_CONS|LOG_ODELAY, LOG_AUTH);
}

/*
 * Catch an unexpected signal.
 */
void
disaster(sig)
	int sig;
{
	emergency("fatal signal: %s",
		sig < (unsigned) NSIG ? sys_siglist[sig] : "unknown signal");

	sleep(STALL_TIMEOUT);
	_exit(sig);		/* reboot */
}

/*
 * Change states in the finite state machine.
 * The initial state is passed as an argument.
 */
void
transition(s)
	state_t s;
{
	for (;;)
		s = (state_t) (*s)();
}

/*
 * We send requests for session logging to another process for two reasons.
 * First, we don't want to block if the log files go away (e.g. because
 * one or more are on hard-mounted NFS systems whose server crashes).
 * Second, despite all the crud already contained in init, it still isn't
 * right that init should care about session logging record formats and files.
 * We could use explicit 'Unix' IPC for this, but let's try to be POSIX...
 */
int
start_logger()
{
	static char *argv[] = { _PATH_SLOGGER, 0 };
	int fd, pfd[2];
	pid_t pid;
	sigset_t mask;

	if (pipe(pfd) == -1) {
		warning("session logging disabled: can't make pipe to %s: %m",
			  argv[0]);
		return -1;
	}
	if ((pid = fork()) == -1) {
		emergency("session logging disabled: can't fork for %s: %m",
			  argv[0]);
		return -1;
	}

	if (pid == 0) {
		close(pfd[1]);
		if (pfd[0] != 0) {
			dup2(pfd[0], 0);
			close(pfd[0]);
		}
		if ((fd = open(_PATH_DEVNULL, O_WRONLY)) != -1) {
			if (fd != 1)
				dup2(fd, 1);
			if (fd != 2)
				dup2(fd, 2);
			if (fd != 1 && fd != 2)
				close(fd);
		} else {
			/* paranoid */
			close(1);
			close(2);
		}
		sigemptyset(&mask);
		sigprocmask(SIG_SETMASK, &mask, (sigset_t *) 0);
		execv(argv[0], argv);
		stall("can't exec %s: %m", argv[0]);
		_exit(1);
	}

	close(pfd[0]);
	fcntl(pfd[1], F_SETFD, FD_CLOEXEC);
	fcntl(pfd[1], F_SETFL, O_NONBLOCK);

	return pfd[1];
}

/*
 * Close out the accounting files for a login session.
 */
void
clear_session_logs(sp)
	session_t *sp;
{
	if (fork() == 0 && logout(sp->se_device))
		logwtmp(sp->se_device, "", "");
}

/*
 * Start a session and allocate a controlling terminal.
 * Only called by children of init after forking.
 */
void
setctty(name)
	char *name;
{
	int fd;

	(void) revoke(name);
	if ((fd = open(name, O_RDWR)) == -1) {
		stall("can't open %s: %m", name);
		_exit(1);
	}
	if (login_tty(fd) == -1) {
		stall("can't get %s for controlling terminal: %m", name);
		_exit(1);
	}
}

/*
 * Bring the system up single user.
 */
state_func_t
single_user()
{
	pid_t pid, wpid;
	int status;
	sigset_t mask;
	char *shell = _PATH_BSHELL;
	char *argv[2];
#ifdef SECURE
	struct ttyent *typ;
	struct passwd *pp;
	static const char banner[] =
		"Enter root password, or ^D to go multi-user\n";
	char *password;
#endif

	if ((pid = fork()) == 0) {
		/*
		 * Start the single user session.
		 */
		setctty(_PATH_CONSOLE);

#ifdef SECURE
		/*
		 * Check the root password.
		 * We don't care if the console is 'on' by default;
		 * it's the only tty that can be 'off' and 'secure'.
		 */
		typ = getttynam("console");
		pp = getpwnam("root");
		if (typ && (typ->ty_status & TTY_SECURE) == 0 && pp) {
			write(2, banner, sizeof banner - 1);
			for (;;) {
				password = getpass("Password:");
				if (password == 0 || *password == '\0')
					_exit(0);
				if (strcmp(password, pp->pw_passwd) == 0)
					break;
			}
		}
#if 0
		/*
		 * Make the single-user shell be root's standard shell?
		 */
		if (pp && pp->pw_shell)
			shell = pp->pw_shell;
#endif
		endttyent();
		endpwent();
#endif

		/*
		 * Unblock signals.
		 * We catch all the interesting ones,
		 * and those are reset to SIG_DFL on exec.
		 */
		sigemptyset(&mask);
		sigprocmask(SIG_SETMASK, &mask, (sigset_t *) 0);

		/*
		 * Fire off a shell.
		 * If the default one doesn't work, try the Bourne shell.
		 */
		argv[0] = "-sh";
		argv[1] = 0;
		execv(shell, argv);
		emergency("can't exec %s for single user: %m", shell);
		execv(_PATH_BSHELL, argv);
		emergency("can't exec %s for single user: %m", _PATH_BSHELL);
		sleep(STALL_TIMEOUT);
		_exit(1);
	}

	if (pid == -1) {
		/*
		 * We are seriously hosed.  Do our best.
		 */
		emergency("can't fork single-user shell, trying again");
		while (waitpid(-1, (int *) 0, WNOHANG) > 0)
			;
		return (state_func_t) single_user;
	}

	do {
		if ((wpid = waitpid(-1, &status, WUNTRACED)) != -1)
			collect_child(wpid);
		if (wpid == -1) {
			if (errno == EINTR)
				continue;
			warning("wait for single-user shell failed: %m; restarting");
			return (state_func_t) single_user;
		}
		if (wpid == pid && WIFSTOPPED(status)) {
			warning("init: shell stopped, restarting\n");
			kill(pid, SIGCONT);
			wpid = -1;
		}
	} while (wpid != pid);

	if (!WIFEXITED(status)) {
		warning("single user shell terminated abnormally, restarting");
		return (state_func_t) single_user;
	}

	runcom_mode = FASTBOOT;
	return (state_func_t) runcom;
}

/*
 * Run the system startup script.
 */
state_func_t
runcom()
{
	pid_t pid, wpid;
	int status;
	char *argv[4];
	struct sigaction sa;

	if ((pid = fork()) == 0) {
		sigemptyset(&sa.sa_mask);
		sa.sa_flags = 0;
		sa.sa_handler = SIG_IGN;
		(void) sigaction(SIGTSTP, &sa, (struct sigaction *)0);
		(void) sigaction(SIGHUP, &sa, (struct sigaction *)0);

		setctty(_PATH_CONSOLE);

		argv[0] = "sh";
		argv[1] = _PATH_RUNCOM;
		argv[2] = runcom_mode == AUTOBOOT ? "autoboot" : 0;
		argv[3] = 0;

		sigprocmask(SIG_SETMASK, &sa.sa_mask, (sigset_t *) 0);

		execv(_PATH_BSHELL, argv);
		stall("can't exec %s for %s: %m", _PATH_BSHELL, _PATH_RUNCOM);
		_exit(1);	/* force single user mode */
	}

	if (pid == -1) {
		emergency("can't fork for %s on %s: %m",
			_PATH_BSHELL, _PATH_RUNCOM);
		while (waitpid(-1, (int *) 0, WNOHANG) > 0)
			;
		sleep(STALL_TIMEOUT);
		return (state_func_t) single_user;
	}

	/*
	 * Copied from single_user().  This is a bit paranoid.
	 */
	do {
		if ((wpid = waitpid(-1, &status, WUNTRACED)) != -1)
			collect_child(wpid);
		if (wpid == -1) {
			if (errno == EINTR)
				continue;
			warning("wait for %s on %s failed: %m; going to single user mode",
				_PATH_BSHELL, _PATH_RUNCOM);
			return (state_func_t) single_user;
		}
		if (wpid == pid && WIFSTOPPED(status)) {
			warning("init: %s on %s stopped, restarting\n",
				_PATH_BSHELL, _PATH_RUNCOM);
			kill(pid, SIGCONT);
			wpid = -1;
		}
	} while (wpid != pid);

	if (!WIFEXITED(status)) {
		warning("%s on %s terminated abnormally, going to single user mode",
			_PATH_BSHELL, _PATH_RUNCOM);
		return (state_func_t) single_user;
	}

	if (WEXITSTATUS(status))
		return (state_func_t) single_user;

	runcom_mode = AUTOBOOT;		/* the default */
	logwtmp("~", "reboot", "");	/* XXX */
	return (state_func_t) read_ttys;
}

/*
 * Open the session database.
 *
 * NB: We could pass in the size here; is it necessary?
 */
int
start_session_db()
{
	if (session_db && (*session_db->close)(session_db))
		emergency("session database close: %s", strerror(errno));
	if ((session_db = hash_open(NULL, O_RDWR, 0, NULL)) == 0) {
		emergency("session database open: %s", strerror(errno));
		return (1);
	}
	return (0);
		
}

/*
 * Add a new login session.
 */
void
add_session(sp)
	session_t *sp;
{
	DBT key;
	DBT data;

	key.data = &sp->se_process;
	key.size = sizeof sp->se_process;
	data.data = &sp;
	data.size = sizeof sp;

	if ((*session_db->put)(session_db, &key, &data, R_PUT))
		emergency("insert %d: %s", sp->se_process, strerror(errno));
}

/*
 * Delete an old login session.
 */
void
del_session(sp)
	session_t *sp;
{
	DBT key;

	key.data = &sp->se_process;
	key.size = sizeof sp->se_process;

	if ((*session_db->del)(session_db, &key, 0))
		emergency("delete %d: %s", sp->se_process, strerror(errno));
}

/*
 * Look up a login session by pid.
 */
session_t *
#ifdef __STDC__
find_session(pid_t pid)
#else
find_session(pid)
	pid_t pid;
#endif
{
	DBT key;
	DBT data;

	key.data = &pid;
	key.size = sizeof pid;
	if ((*session_db->get)(session_db, &key, &data, 0) != 0)
		return 0;
	return *(session_t **)data.data;
}

/*
 * Construct an argument vector from a command line.
 */
char **
construct_argv(command)
	char *command;
{
	register int argc = 0;
	register char **argv = (char **) malloc(((strlen(command) + 1) / 2 + 1)
						* sizeof (char *));
	static const char separators[] = " \t";

	if ((argv[argc++] = strtok(command, separators)) == 0)
		return 0;
	while (argv[argc++] = strtok((char *) 0, separators))
		;
	return argv;
}

/*
 * Deallocate a session descriptor.
 */
void
free_session(sp)
	register session_t *sp;
{
	free(sp->se_device);
	free(sp->se_getty);
	free(sp->se_getty_argv);
	if (sp->se_window) {
		free(sp->se_window);
		free(sp->se_window_argv);
	}
	free(sp);
}

/*
 * Allocate a new session descriptor.
 */
session_t *
new_session(sprev, session_index, typ)
	session_t *sprev;
	int session_index;
	register struct ttyent *typ;
{
	register session_t *sp;

	if ((typ->ty_status & TTY_ON) == 0 ||
	    typ->ty_name == 0 ||
	    typ->ty_getty == 0)
		return 0;

	sp = (session_t *) malloc(sizeof (session_t));

	sp->se_index = session_index;
	sp->se_process = 0;
	sp->se_started = 0;
	sp->se_flags = 0;
	sp->se_window = 0;

	sp->se_device = malloc(6 + strlen(typ->ty_name));
	memcpy(sp->se_device, _PATH_DEV, 5);
	strcpy(sp->se_device + 5, typ->ty_name);

	sp->se_getty = strdup(typ->ty_getty);
	sp->se_getty_argv = construct_argv(sp->se_getty);
	if (sp->se_getty_argv == 0) {
		warning("can't parse getty for port %s",
			sp->se_device);
		free_session(sp);
		return 0;
	}
	if (typ->ty_window) {
		sp->se_window = strdup(typ->ty_window);
		sp->se_window_argv = construct_argv(sp->se_window);
		if (sp->se_window_argv == 0) {
			warning("can't parse window for port %s",
				sp->se_device);
			free_session(sp);
			return 0;
		}
	}

	sp->se_next = 0;
	if (sprev == 0) {
		sessions = sp;
		sp->se_prev = 0;
	} else {
		sprev->se_next = sp;
		sp->se_prev = sprev;
	}

	return sp;
}

/*
 * Walk the list of ttys and create sessions for each active line.
 */
state_func_t
read_ttys()
{
	int session_index = 0;
	register session_t *sp, *snext;
	register struct ttyent *typ;

	/*
	 * Destroy any previous session state.
	 * There shouldn't be any, but just in case...
	 */
	for (sp = sessions; sp; sp = snext) {
		if (sp->se_process)
			clear_session_logs(sp);
		snext = sp->se_next;
		free_session(sp);
	}
	sessions = 0;
	if (start_session_db())
		return (state_func_t) single_user;

	/*
	 * Allocate a session entry for each active port.
	 * Note that sp starts at 0.
	 */
	while (typ = getttyent())
		if (snext = new_session(sp, ++session_index, typ))
			sp = snext;

	endttyent();

	logger_enable = 1;
	return (state_func_t) multi_user;
}

/*
 * Start a window system running.
 */
void
start_window_system(sp)
	session_t *sp;
{
	pid_t pid;
	sigset_t mask;

	if ((pid = fork()) == -1) {
		emergency("can't fork for window system on port %s: %m",
			sp->se_device);
		/* hope that getty fails and we can try again */
		return;
	}

	if (pid)
		return;

	sigemptyset(&mask);
	sigprocmask(SIG_SETMASK, &mask, (sigset_t *) 0);

	if (setsid() < 0)
		emergency("setsid failed (window) %m");

	execv(sp->se_window_argv[0], sp->se_window_argv);
	stall("can't exec window system '%s' for port %s: %m",
		sp->se_window_argv[0], sp->se_device);
	_exit(1);
}

/*
 * Start a login session running.
 */
pid_t
start_getty(sp)
	session_t *sp;
{
	pid_t pid;
	sigset_t mask;
	time_t current_time = time((time_t *) 0);

	/*
	 * fork(), not vfork() -- we can't afford to block.
	 */
	if ((pid = fork()) == -1) {
		emergency("can't fork for getty on port %s: %m", sp->se_device);
		return -1;
	}

	if (pid)
		return pid;

	if (current_time > sp->se_started &&
	    current_time - sp->se_started < GETTY_SPACING) {
		warning("getty repeating too quickly on port %s, sleeping",
		        sp->se_device);
		sleep((unsigned) GETTY_SPACING -
		      (current_time - sp->se_started));
	}

	if (sp->se_window) {
		start_window_system(sp);
		sleep(WINDOW_WAIT);
	}

	setctty(sp->se_device);

	sigemptyset(&mask);
	sigprocmask(SIG_SETMASK, &mask, (sigset_t *) 0);

	execv(sp->se_getty_argv[0], sp->se_getty_argv);
	stall("can't exec getty '%s' for port %s: %m",
		sp->se_getty_argv[0], sp->se_device);
	_exit(1);
}

/*
 * Collect exit status for a child.
 * If an exiting login, start a new login running.
 */
void
collect_child(pid)
	pid_t pid;
{
	register session_t *sp, *sprev, *snext;

	if (! sessions)
		return;

	if (! (sp = find_session(pid)))
		return;

	clear_session_logs(sp);
	del_session(sp);
	sp->se_process = 0;

	if (sp->se_flags & SE_SHUTDOWN) {
		if (sprev = sp->se_prev)
			sprev->se_next = sp->se_next;
		else
			sessions = sp->se_next;
		if (snext = sp->se_next)
			snext->se_prev = sp->se_prev;
		free_session(sp);
		return;
	}

	if ((pid = start_getty(sp)) == -1) {
		/* serious trouble */
		requested_transition = clean_ttys;
		return;
	}

	sp->se_process = pid;
	sp->se_started = time((time_t *) 0);
	add_session(sp);
}

/*
 * Catch a signal and request a state transition.
 */
void
transition_handler(sig)
	int sig;
{
	switch (sig) {
	case SIGHUP:
		requested_transition = clean_ttys;
		break;
	case SIGTERM:
		requested_transition = death;
		break;
	case SIGTSTP:
		requested_transition = catatonia;
		break;
	default:
		requested_transition = 0;
		break;
	}
}

/*
 * Take the system multiuser.
 */
state_func_t
multi_user()
{
	pid_t pid;
	register session_t *sp;

	requested_transition = 0;
	logger_enable = 1;

	for (sp = sessions; sp; sp = sp->se_next) {
		if (sp->se_process)
			continue;
		if ((pid = start_getty(sp)) == -1) {
			/* serious trouble */
			requested_transition = clean_ttys;
			break;
		}
		sp->se_process = pid;
		sp->se_started = time((time_t *) 0);
		add_session(sp);
	}

	while (!requested_transition)
		if ((pid = waitpid(-1, (int *) 0, 0)) != -1)
			collect_child(pid);

	return (state_func_t) requested_transition;
}

/*
 * This is an n-squared algorithm.  We hope it isn't run often...
 */
state_func_t
clean_ttys()
{
	register session_t *sp, *sprev;
	register struct ttyent *typ;
	register int session_index = 0;

	if (! sessions)
		return (state_func_t) multi_user;

	while (typ = getttyent()) {
		++session_index;

		for (sp = sessions; sp; sprev = sp, sp = sp->se_next)
			if (strcmp(typ->ty_name, sp->se_device) == 0)
				break;

		if (sp) {
			if (sp->se_index != session_index) {
				warning("port %s changed utmp index from %d to %d",
				       sp->se_device, sp->se_index,
				       session_index);
				sp->se_index = session_index;
			}
			if (typ->ty_status & TTY_ON)
				sp->se_flags &= ~SE_SHUTDOWN;
			else {
				sp->se_flags |= SE_SHUTDOWN;
				kill(sp->se_process, SIGHUP);
			}
			continue;
		}

		new_session(sprev, session_index, typ);
	}

	endttyent();

	return (state_func_t) multi_user;
}

/*
 * Block further logins.
 */
state_func_t
catatonia()
{
	register session_t *sp;

	for (sp = sessions; sp; sp = sp->se_next)
		sp->se_flags |= SE_SHUTDOWN;

	return (state_func_t) multi_user;
}

/*
 * Note SIGALRM.
 */
void
alrm_handler(sig)
	int sig;
{
	clang = 1;
}

/*
 * Bring the system down to single user.
 */
state_func_t
death()
{
	register session_t *sp;
	register int i;
	pid_t pid;
	static const int death_sigs[3] = { SIGHUP, SIGTERM, SIGKILL };

	for (sp = sessions; sp; sp = sp->se_next)
		sp->se_flags |= SE_SHUTDOWN;

	logwtmp("~", "shutdown", "");	/* XXX */
	logger_enable = 0;

	for (i = 0; i < 3; ++i) {
		if (kill(-1, death_sigs[i]) == -1 && errno == ESRCH)
			return (state_func_t) single_user;

		clang = 0;
		alarm(DEATH_WATCH);
		do
			if ((pid = waitpid(-1, (int *)0, 0)) != -1)
				collect_child(pid);
		while (clang == 0 && errno != ECHILD);

		if (errno == ECHILD)
			return (state_func_t) single_user;
	}

	warning("some processes wouldn't die");

	return (state_func_t) single_user;
}
