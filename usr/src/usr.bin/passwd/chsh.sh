#ifndef lint
static char *sccsid = "@(#)chsh.sh	4.9 (Berkeley) %G%";
#endif

/*
 * chsh
 */
#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <ndbm.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>

char	temp[] = "/etc/ptmp";
char	passwd[] = "/etc/passwd";
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
	DBM *dp;

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
	if ((tf = fdopen(fd, "w")) == NULL) {
		printf("Absurd fdopen failure - seek help\n");
		goto out;
	}
	if ((dp = ndbmopen(passwd, O_RDWR, 0644)) == NULL) {
		fprintf(stderr, "Warning: dbminit failed: ");
		perror(passwd);
	} else if (flock(dp->db_dirf, LOCK_EX) < 0) {
		perror("Warning: lock failed");
		ndbmclose(dp);
		dp = NULL;
	}
	/*
	 * Copy passwd to temp, replacing matching lines
	 * with new shell.
	 */
	while ((pwd = getpwent()) != NULL) {
		if (strcmp(pwd->pw_name, argv[1]) == 0) {
			if (u != 0 && u != pwd->pw_uid) {
				printf("Permission denied.\n");
				goto out;
			}
			pwd->pw_shell = argv[2];
			replace(dp, pwd);
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
	(void) fclose(tf);
	ndbmclose(dp);
	if (rename(temp, passwd) < 0) {
		fprintf(stderr, "chsh: "), perror("rename");
	out:
		(void) unlink(temp);
		exit(1);
	}
	exit(0);
}

unlimit(lim)
{
	struct rlimit rlim;

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	setrlimit(lim, &rlim);
}

/*
 * Replace the password entry in the dbm data base with pwd.
 */
replace(dp, pwd)
	DBM *dp;
	struct passwd *pwd;
{
	datum key, content;
	register char *cp, *tp;
	char buf[BUFSIZ];

	if (dp == NULL)
		return;

	cp = buf;
#define	COMPACT(e)	tp = pwd->pw_/**/e; while (*cp++ = *tp++);
	COMPACT(name);
	COMPACT(passwd);
	*(int *)cp = pwd->pw_uid; cp += sizeof (int);
	*(int *)cp = pwd->pw_gid; cp += sizeof (int);
	*(int *)cp = pwd->pw_quota; cp += sizeof (int);
	COMPACT(comment);
	COMPACT(gecos);
	COMPACT(dir);
	COMPACT(shell);
	content.dptr = buf;
	content.dsize = cp - buf;
	key.dptr = pwd->pw_name;
	key.dsize = strlen(pwd->pw_name);
	dbmstore(dp, key, content, DB_REPLACE);
	key.dptr = (char *)&pwd->pw_uid;
	key.dsize = sizeof (int);
	dbmstore(dp, key, content, DB_REPLACE);
}
