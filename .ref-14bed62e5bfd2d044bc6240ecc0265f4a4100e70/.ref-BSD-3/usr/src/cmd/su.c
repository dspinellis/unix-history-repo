#include <stdio.h>
#include <pwd.h>

struct	passwd *pwd,*getpwnam();
char	*crypt();
char	*getpass();

main(argc,argv)
int	argc;
char	**argv;
{
	char *nptr;
	char *password;
	int badsw = 0;
	char *shell = "/bin/sh";

	if(argc > 1)
		nptr = argv[1];
	else
		nptr = "root";
	if((pwd=getpwnam(nptr)) == NULL) {
		printf("Unknown id: %s\n",nptr);
		exit(1);
	}
	if(pwd->pw_passwd[0] == '\0' || getuid() == 0)
		goto ok;
	password = getpass("Password:");
	if(badsw || (strcmp(pwd->pw_passwd, crypt(password, pwd->pw_passwd)) != 0)) {
bad:
		printf("Sorry\n");
		if(pwd->pw_uid == 0) {
			FILE *console = fopen("/dev/console", "w");
			if (console != NULL) {
				fprintf(console, "BADSU: %s %s\r\n", getlogin(), ttyname(2));
				fclose(console);
			}
		}
		exit(2);
	}
	if(pwd->pw_uid == 0 && badroot(getgid(),getuid()))
		goto bad;

ok:
	endpwent();
	if(pwd->pw_uid == 0) {
		FILE *console = fopen("/dev/console", "w");
		if (console != NULL) {
			fprintf(console, "SU: %s %s\r\n", getlogin(), ttyname(2));
			fclose(console);
		}
	}
	setgid(pwd->pw_gid);
	setuid(pwd->pw_uid);
	if (pwd->pw_shell && *pwd->pw_shell)
		shell = pwd->pw_shell;
	homeis(pwd->pw_dir);
	shellis(shell);
	execl(shell, "su", 0);
	printf("No shell\n");
	exit(3);
}
badroot(gid,uid)
{
/*
	if(gid!=10 || (uid > 15 && (uid!=40 && uid!=209 && uid!=203
				    && uid!=54 && uid!=245)))
		return(1);
	else 
*/
		return(0);
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
