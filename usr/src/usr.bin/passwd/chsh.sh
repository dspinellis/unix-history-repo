#ifndef lint
static char *sccsid = "@(#)chsh.sh	4.8 (Berkeley) %G%";
#endif

/*
 * chsh
 */
#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>

char	temp[] = "/etc/ptmp";
char	temp_pag[] = "/etc/ptmp.pag";
char	temp_dir[] = "/etc/ptmp.dir";
char	passwd[] = "/etc/passwd";
char	passwd_pag[] = "/etc/passwd.pag";
char	passwd_dir[] = "/etc/passwd.dir";
struct	passwd *pwd;
struct	passwd *getpwent();
int	endpwent();
char	*crypt();
char	*getpass();
char	*strcat();
char	buf[BUFSIZ];

main(argc, argv)
register char *argv[];
{
	register int u, fd;
	register FILE *tf;

	if (argc < 2 || argc > 3) {
		printf("Usage: chsh user [ /bin/csh ]\n");
		exit(1);
	}
	if (argc == 2)
		argv[2] = "";
	else {
		if (argv[2][0] != '/')
			argv[2] = strcat(
			    "/bin/\0 12345678901234567890123456789", argv[2]);
		if (strcmp(argv[2], "/bin/csh") &&
		    strcmp(argv[2], "/bin/oldcsh") &&
		    strcmp(argv[2], "/bin/newcsh") &&
		    strcmp(argv[2], "/usr/new/csh") &&
			/* and, for cretins who can't read the manual */
		    strcmp(argv[2], "/bin/sh") &&
		    getuid()) {
			printf(
			    "Only /bin/csh may be specified\n"
			);
			exit(1);
		}
		if (access(argv[2], 1) < 0) {
			printf("%s is not available\n", argv[2]);
			exit(1);
		}
	}
	unlimit(RLIMIT_CPU);
	unlimit(RLIMIT_FSIZE);
	u = getuid();
	if (u != 0 && ((pwd = getpwnam(argv[1])) == NULL || u != pwd->pw_uid)) {
		printf("Permission denied.\n");
		exit(1);
	}

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTSTP, SIG_IGN);
	(void) umask(0);
	if ((fd = open(temp, O_CREAT|O_EXCL|O_RDWR, 0644)) < 0) {
		printf("Temporary file busy -- try again\n");
		exit(1);
	}
	if ((tf=fdopen(fd, "w")) == NULL) {
		printf("Absurd fdopen failure - seek help\n");
		goto out;
	}
	/*
	 * Copy passwd to temp, replacing matching lines
	 * with new shell.
	 */
	while ((pwd=getpwent()) != NULL) {
		if (strcmp(pwd->pw_name, argv[1]) == 0) {
			if (u != 0 && u != pwd->pw_uid) {
				printf("Permission denied.\n");
				goto out;
			}
			pwd->pw_shell = argv[2];
		}
		if (strcmp(pwd->pw_shell, "/bin/sh") == 0)
			pwd->pw_shell = "";
		fprintf(tf, "%s:%s:%d:%d:%s:%s:%s\n"
			, pwd->pw_name
			, pwd->pw_passwd
			, pwd->pw_uid
			, pwd->pw_gid
			, pwd->pw_gecos
			, pwd->pw_dir
			, pwd->pw_shell
		);
	}
	endpwent();
	fclose(tf);
	if (makedb(temp) < 0)
		fprintf(stderr, "chsh: mkpasswd failed\n");
	else if (rename(temp_pag, passwd_pag) < 0)
		fprintf(stderr, "chsh: "), perror(temp_pag);
	else if (rename(temp_dir, passwd_dir) < 0)
		fprintf(stderr, "chsh: "), perror(temp_dir);
	else if (rename(temp, passwd) < 0)
		fprintf(stderr, "chsh: "), perror("rename");
	else
		exit(0);
out:
	unlink(temp_pag);
	unlink(temp_dir);
	unlink(temp);
	exit(1);
}

unlimit(lim)
{
	struct rlimit rlim;

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	setrlimit(lim, &rlim);
}

makedb(file)
	char *file;
{
	int status, pid, w;

	if ((pid = vfork()) == 0) {
		execl("/etc/mkpasswd", "mkpasswd", file, 0);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1 || status != 0)
		status = -1;
	return(status);
}
