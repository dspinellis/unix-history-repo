#ifndef lint
static char *sccsid = "@(#)su.c	4.4 (Berkeley) 4.4";
#endif

#include <stdio.h>
#include <pwd.h>

struct	passwd *pwd,*getpwnam();
char	*crypt();
char	*getpass();

main(argc,argv)
	int argc;
	char **argv;
{
	char *nptr = "root";
	char *password;
	char *shell = "/bin/sh";

	if (argc > 1)
		nptr = argv[1];
	if ((pwd = getpwnam(nptr)) == NULL) {
		printf("Unknown id: %s\n",nptr);
		exit(1);
	}
	if (pwd->pw_passwd[0] == '\0' || getuid() == 0)
		goto ok;
	password = getpass("Password:");
	if (strcmp(pwd->pw_passwd, crypt(password, pwd->pw_passwd)) != 0) {
		printf("Sorry\n");
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
	if (initgroups(nptr, pwd->pw_gid)) {
		fprintf(stderr, "su: initgroups failed\n");
		exit(4);
	}
	if (setuid(pwd->pw_uid) < 0) {
		perror("su: setuid");
		exit(5);
	}
	if (pwd->pw_shell && *pwd->pw_shell)
		shell = pwd->pw_shell;
	homeis(pwd->pw_dir);
	shellis(shell);
	execl(shell, "su", 0);
	printf("No shell\n");
	exit(3);
}

char	**environ;

homeis(hp)
	char *hp;
{
	register char *cp, *dp;
	register char **ep = environ;
	static char homebuf[128];

	while (dp = *ep++) {
		for (cp = "HOME"; *cp == *dp && *cp; cp++, dp++)
			continue;
		if (*cp == 0 && (*dp == '=' || *dp == 0)) {
			strcpy(homebuf, "HOME=");
			strcat(homebuf, hp);
			*--ep = homebuf;
			return;
		}
	}
}

shellis(sp)
	char *sp;
{
	register char *cp, *dp;
	register char **ep = environ;
	static char shellbuf[128];

	while (dp = *ep++) {
		for (cp = "SHELL"; *cp == *dp && *cp; cp++, dp++)
			continue;
		if (*cp == 0 && (*dp == '=' || *dp == 0)) {
			strcpy(shellbuf, "SHELL=");
			strcat(shellbuf, sp);
			*--ep = shellbuf;
			return;
		}
	}
}
