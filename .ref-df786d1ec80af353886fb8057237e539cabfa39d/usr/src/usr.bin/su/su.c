#ifndef lint
static char *sccsid = "@(#)su.c	4.6 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

char	userbuf[16]	= "USER=";
char	homebuf[128]	= "HOME=";
char	shellbuf[128]	= "SHELL=";
char	pathbuf[128]	= "PATH=:/usr/ucb:/bin:/usr/bin";
char	*cleanenv[] = { userbuf, homebuf, shellbuf, pathbuf, 0, 0 };
char	*user = "root";
char	*shell = "/bin/sh";
int	fulllogin;
int	fastlogin;

extern char	**environ;
struct	passwd *pwd,*getpwnam();
char	*crypt();
char	*getpass();
char	*getenv();

main(argc,argv)
	int argc;
	char *argv[];
{
	char *password;

again:
	if (argc > 1 && strcmp(argv[1], "-f") == 0) {
		fastlogin++;
		argc--, argv++;
		goto again;
	}
	if (argc > 1 && strcmp(argv[1], "-") == 0) {
		fulllogin++;
		argc--, argv++;
		goto again;
	}
	if (argc > 1 && argv[1][0] != '-') {
		user = argv[1];
		argc--, argv++;
	}
	if (strcmp(user, "root") == 0)
		setpriority(PRIO_PROCESS, 0, -2);
	if ((pwd = getpwnam(user)) == NULL) {
		fprintf(stderr, "Unknown login: %s\n", user);
		exit(1);
	}
	if (pwd->pw_passwd[0] == '\0' || getuid() == 0)
		goto ok;
	password = getpass("Password:");
	if (strcmp(pwd->pw_passwd, crypt(password, pwd->pw_passwd)) != 0) {
		fprintf(stderr, "Sorry\n");
		if (pwd->pw_uid == 0) {
			FILE *console = fopen("/dev/console", "w");
			if (console != NULL) {
				fprintf(console, "BADSU: %s %s\r\n",
					getlogin(), ttyname(2));
				fclose(console);
			}
		}
		exit(2);
	}
ok:
	endpwent();
	if (pwd->pw_uid == 0) {
		FILE *console = fopen("/dev/console", "w");
		if (console != NULL) {
			fprintf(console, "SU: %s %s\r\n",
				getlogin(), ttyname(2));
			fclose(console);
		}
	}
	if (setgid(pwd->pw_gid) < 0) {
		perror("su: setgid");
		exit(3);
	}
	if (initgroups(user, pwd->pw_gid)) {
		fprintf(stderr, "su: initgroups failed\n");
		exit(4);
	}
	if (setuid(pwd->pw_uid) < 0) {
		perror("su: setuid");
		exit(5);
	}
	if (pwd->pw_shell && *pwd->pw_shell)
		shell = pwd->pw_shell;
	if (fulllogin) {
		cleanenv[4] = getenv("TERM");
		environ = cleanenv;
	}
	if (strcmp(user, "root"))
		setenv("USER", pwd->pw_name, userbuf);
	setenv("SHELL", shell, shellbuf);
	setenv("HOME", pwd->pw_dir, homebuf);
	setpriority(PRIO_PROCESS, 0, 0);
	if (fastlogin) {
		*argv-- = "-f";
		*argv = "su";
	} else if (fulllogin) {
		if (chdir(pwd->pw_dir) < 0) {
			fprintf(stderr, "No directory\n");
			exit(6);
		}
		*argv = "-su";
	} else
		*argv = "su";
	execv(shell, argv);
	fprintf(stderr, "No shell\n");
	exit(7);
}

setenv(ename, eval, buf)
	char *ename, *eval, *buf;
{
	register char *cp, *dp;
	register char **ep = environ;

	/*
	 * this assumes an environment variable "ename" already exists
	 */
	while (dp = *ep++) {
		for (cp = ename; *cp == *dp && *cp; cp++, dp++)
			continue;
		if (*cp == 0 && (*dp == '=' || *dp == 0)) {
			strcat(buf, eval);
			*--ep = buf;
			return;
		}
	}
}

char *
getenv(ename)
	char *ename;
{
	register char *cp, *dp;
	register char **ep = environ;

	while (dp = *ep++) {
		for (cp = ename; *cp == *dp && *cp; cp++, dp++)
			continue;
		if (*cp == 0 && (*dp == '=' || *dp == 0))
			return (*--ep);
	}
	return ((char *)0);
}
