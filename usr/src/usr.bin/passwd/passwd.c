#ifndef lint
static char sccsid[] = "@(#)passwd.c	4.1 (Berkeley) %G%";
#endif

/*
 * enter a password in the password file
 * this program should be suid with owner
 * with an owner with write permission on /etc/passwd
 */
#include <sys/file.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <errno.h>

char	passwd[] = "/etc/passwd";
char	temp[]	 = "/etc/ptmp";
struct	passwd *pwd;
struct	passwd *getpwent();
int	endpwent();
char	*strcpy();
char	*crypt();
char	*getpass();
char	*getlogin();
char	*pw;
char	pwbuf[10];
extern	int errno;

main(argc, argv)
	char *argv[];
{
	char *p;
	int i;
	char saltc[2];
	long salt;
	int u;
	int insist;
	int ok, flags;
	int c, pwlen, fd;
	FILE *tf;
	char *uname;

	insist = 0;
	if (argc < 2) {
		if ((uname = getlogin()) == NULL) {
			printf ("Usage: passwd user\n");
			exit(1);
		}
		printf("Changing password for %s\n", uname);
	} else
		uname = argv[1];
	while (((pwd = getpwent()) != NULL) && strcmp(pwd->pw_name, uname))
		;
	u = getuid();
	if (pwd == NULL || (u != 0 && u != pwd->pw_uid)) {
		printf("Permission denied.\n");
		exit(1);
	}
	endpwent();
	if (pwd->pw_passwd[0] && u != 0) {
		strcpy(pwbuf, getpass("Old password:"));
		pw = crypt(pwbuf, pwd->pw_passwd);
		if (strcmp(pw, pwd->pw_passwd) != 0) {
			printf("Sorry.\n");
			exit(1);
		}
	}
tryagain:
	strcpy(pwbuf, getpass("New password:"));
	pwlen = strlen(pwbuf);
	if (pwlen == 0) {
		printf("Password unchanged.\n");
		exit(1);
	}
	/*
	 * Insure password is of reasonable length and
	 * composition.  If we really wanted to make things
	 * sticky, we could check the dictionary for common
	 * words, but then things would really be slow.
	 */
	ok = 0;
	flags = 0;
	p = pwbuf;
	while (c = *p++) {
		if (c >= 'a' && c <= 'z')
			flags |= 2;
		else if (c >= 'A' && c <= 'Z')
			flags |= 4;
		else if (c >= '0' && c <= '9')
			flags |= 1;
		else
			flags |= 8;
	}
	if (flags >= 7 && pwlen >= 4)
		ok = 1;
	if ((flags == 2 || flags == 4) && pwlen >= 6)
		ok = 1;
	if ((flags == 3 || flags == 5 || flags == 6) && pwlen >= 5)
		ok = 1;
	if (!ok && insist < 2) {
		if (flags == 1)
		printf("Please use %s.\n", flags == 1 ?
			"at least one non-numeric character" :
			"a longer password");
		insist++;
		goto tryagain;
	}
	if (strcmp(pwbuf, getpass("Retype new password:")) != 0) {
		printf("Mismatch - password unchanged.\n");
		exit(1);
	}
	time(&salt);
	salt = 9 * getpid();
	saltc[0] = salt & 077;
	saltc[1] = (salt>>6) & 077;
	for (i = 0; i < 2; i++) {
		c = saltc[i] + '.';
		if (c > '9')
			c += 7;
		if (c > 'Z')
			c += 6;
		saltc[i] = c;
	}
	pw = crypt(pwbuf, saltc);
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	/*
	 * The mode here could be 644 except then old versions
	 * of passwd that don't honor the advisory locks might
	 * sneak in and mess things up.  If we could believe the
	 * locking were honored, then we could also eliminate the
	 * chmod below after the rename.
	 */
	fd = open(temp, FWRONLY|FCREATE|FEXLOCK|FNBLOCK, 0600);
	if (fd < 0) {
		fprintf(stderr, "passwd: ");
		if (errno == EBUSY)
			fprintf(stderr, "password file busy - try again.\n");
		else
			perror(temp);
		exit(1);
	}
	signal(SIGTSTP, SIG_IGN);
	if ((tf = fdopen(fd, "w")) == NULL) {
		fprintf(stderr, "passwd: fdopen failed?\n");
		exit(1);
	}
	/*
	 * Copy passwd to temp, replacing matching lines
	 * with new password.
	 */
	while ((pwd = getpwent()) != NULL) {
		if (strcmp(pwd->pw_name,uname) == 0) {
			u = getuid();
			if (u && u != pwd->pw_uid) {
				fprintf(stderr, "passwd: permission denied.\n");
				unlink(temp);
				exit(1);
			}
			pwd->pw_passwd = pw;
			if (pwd->pw_gecos[0] == '*')
				pwd->pw_gecos++;
		}
		fprintf(tf,"%s:%s:%d:%d:%s:%s:%s\n",
			pwd->pw_name,
			pwd->pw_passwd,
			pwd->pw_uid,
			pwd->pw_gid,
			pwd->pw_gecos,
			pwd->pw_dir,
			pwd->pw_shell);
	}
	endpwent();
	if (rename(temp, passwd) < 0) {
		fprintf(stderr, "passwd: "); perror("rename");
		unlink(temp);
		exit(1);
	}
	chmod(passwd, 0644);
	fclose(tf);
}
