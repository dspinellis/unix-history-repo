#include <stdio.h>
#include <pwd.h>

struct	passwd *pwd,*getpwnam();
char	*crypt();
char	*getpass();
char	**environ;

main(argc,argv)
int	argc;
char	**argv;
{
	register char **p;
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
		printf("Sorry\n");
		exit(2);
	}

ok:
	endpwent();
	setgid(pwd->pw_gid);
	setuid(pwd->pw_uid);
	if (pwd->pw_shell && *pwd->pw_shell)
		shell = pwd->pw_shell;
	for (p=environ; *p; p++) {
		if (strncmp("PS1=", *p, 4) == 0) {
			*p = "PS1=# ";
			break;
		}
	}
	execl(shell, "su", 0);
	printf("No shell\n");
	exit(3);
}
