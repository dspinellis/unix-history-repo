#ifndef lint
static	char *sccsid = "@(#)mkpasswd.c	4.3 (Berkeley) 84/01/25";
#endif

#include <sys/file.h>
#include <stdio.h>
#include <pwd.h>
#include <ndbm.h>

char	buf[BUFSIZ];
char	line[BUFSIZ+1];
char	EMPTY[] = "";
struct	passwd passwd;

struct	passwd *fgetpwent();

main(argc, argv)
	char *argv[];
{
	FILE *pwf;
	DBM *dp;
	datum key, content;
	register char *cp, *tp;
	register struct passwd *pwd;
	int verbose = 0, entries = 0, maxlen = 0;

	if (argc > 1 && strcmp(argv[1], "-v") == 0) {
		verbose++;
		argv++, argc--;
	}
	if (argc != 2) {
		fprintf(stderr, "usage: mkpasswd [ -v ] file\n");
		exit(1);
	}
	if ((pwf = fopen(argv[1], "r" )) == NULL) {
		fprintf(stderr, "mkpasswd: ");
		perror(argv[1]);
		exit(1);
	}
	umask(0);
	dp = ndbmopen(argv[1], O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (dp == NULL) {
		fprintf(stderr, "mkpasswd: ");
		perror(argv[1]);
		exit(1);
	}
	dp->db_maxbno = 0;
	while (pwd = fgetpwent(pwf)) {
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
		if (verbose)
			printf("store %s, uid %d\n", pwd->pw_name, pwd->pw_uid);
		key.dptr = pwd->pw_name;
		key.dsize = strlen(pwd->pw_name);
		dbmstore(dp, key, content, DB_INSERT);
		key.dptr = (char *)&pwd->pw_uid;
		key.dsize = sizeof (int);
		dbmstore(dp, key, content, DB_INSERT);
		entries++;
		if (cp - buf > maxlen)
			maxlen = cp - buf;
	}
	printf("%d password entries, maximum length %d\n", entries, maxlen);
	exit(0);
}

static char *
pwskip(p)
register char *p;
{
	while( *p && *p != ':' )
		++p;
	if( *p ) *p++ = 0;
	return(p);
}

struct passwd *
fgetpwent(pwf)
	FILE *pwf;
{
	register char *p;

	p = fgets(line, BUFSIZ, pwf);
	if (p==NULL)
		return(0);
	passwd.pw_name = p;
	p = pwskip(p);
	passwd.pw_passwd = p;
	p = pwskip(p);
	passwd.pw_uid = atoi(p);
	p = pwskip(p);
	passwd.pw_gid = atoi(p);
	passwd.pw_quota = 0;
	passwd.pw_comment = EMPTY;
	p = pwskip(p);
	passwd.pw_gecos = p;
	p = pwskip(p);
	passwd.pw_dir = p;
	p = pwskip(p);
	passwd.pw_shell = p;
	while(*p && *p != '\n') p++;
	*p = '\0';
	return(&passwd);
}
