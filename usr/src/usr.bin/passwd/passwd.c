/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)passwd.c	4.27 (Berkeley) %G%";
#endif not lint

/*
 * Modify a field in the password file (either
 * password, login shell, or gecos field).
 * This program should be suid with an owner
 * with write permission on /etc/passwd.
 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <ndbm.h>
#include <errno.h>
#include <strings.h>
#include <ctype.h>

/*
 * This should be the first thing returned from a getloginshells()
 * but too many programs know that it is /bin/sh.
 */
#define DEFSHELL "/bin/sh"

char	temp[] = "/etc/ptmp";
char	passwd[] = "/etc/passwd";
char	*getpass();
char	*getfingerinfo();
char	*getloginshell();
char	*getnewpasswd();
char	*malloc();
char	*getusershell();
extern	int errno;

main(argc, argv)
	char *argv[];
{
	struct passwd *pwd;
	char *cp, *uname, *progname;
	uid_t u;
	int fd, dochfn, dochsh, err;
	FILE *tf;
	DBM *dp;
	char *getlogin();

	if ((progname = rindex(argv[0], '/')) == NULL)
		progname = argv[0];
	else
		progname++;
	dochfn = 0, dochsh = 0;
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		for (cp = &argv[0][1]; *cp; cp++) switch (*cp) {

		case 'f':
			if (dochsh)
				goto bad;
			dochfn = 1;
			break;

		case 's':
			if (dochfn) {
		bad:
				fprintf(stderr,
				   "passwd: Only one of -f and -s allowed.\n"); 
				exit(1);
			}
			dochsh = 1;
			break;

		default:
			fprintf(stderr, "passwd: -%c: unknown option.\n", *cp);
			exit(1);
		}
		argc--, argv++;
	}
	if (!dochfn && !dochsh) {
		if (strcmp(progname, "chfn") == 0)
			dochfn = 1;
		else if (strcmp(progname, "chsh") == 0)
			dochsh = 1;
	}
	if (argc < 1) {
		if (uname = getlogin())
			printf("Changing %s for %s.\n", dochfn ? "finger information" : dochsh ? "login shell" : "password", uname);
	}
	else
		uname = *argv++;
	u = getuid();
	if (!(pwd = uname ? getpwnam(uname) : getpwuid(u))) {
		fprintf(stderr, "passwd: %s: unknown user.\n", uname);
		exit(1);
	}
	if (u != 0 && u != pwd->pw_uid) {
		printf("Permission denied.\n");
		exit(1);
	}
	if (dochfn)
		cp = getfingerinfo(pwd);
	else if (dochsh)
		cp = getloginshell(pwd, u, *argv);
	else
		cp = getnewpasswd(pwd, u);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTSTP, SIG_IGN);
	(void) umask(0);
	fd = open(temp, O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (fd < 0) {
		err = errno;

		fprintf(stderr, "passwd: ");
		if (err == EEXIST)
			fprintf(stderr, "password file busy - try again.\n");
		else {
			errno = err;
			perror(temp);
		}
		exit(1);
	}
	if ((tf = fdopen(fd, "w")) == NULL) {
		fprintf(stderr, "passwd: fdopen failed?\n");
		exit(1);
	}
	if ((dp = dbm_open(passwd, O_RDWR, 0644)) == NULL) {
		err = errno;
		fprintf(stderr, "Warning: dbm_open failed: ");
		errno = err;
		perror(passwd);
	} else if (flock(dp->dbm_dirf, LOCK_EX) < 0) {
		perror("Warning: lock failed");
		dbm_close(dp);
		dp = NULL;
	}
	unlimit(RLIMIT_CPU);
	unlimit(RLIMIT_FSIZE);
	/*
	 * Copy passwd to temp, replacing matching lines
	 * with new password.
	 */
	while ((pwd = getpwent()) != NULL) {
		if (strcmp(pwd->pw_name, uname) == 0) {
			if (u && u != pwd->pw_uid) {
				fprintf(stderr, "passwd: permission denied.\n");
				goto out;
			}
			if (dochfn)
				pwd->pw_gecos = cp;
			else if (dochsh)
				pwd->pw_shell = cp;
			else
				pwd->pw_passwd = cp;
			if (pwd->pw_gecos[0] == '*')	/* ??? */
				pwd->pw_gecos++;
			replace(dp, pwd);
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
	if (dp != NULL && dbm_error(dp))
		fprintf(stderr, "Warning: dbm_store failed\n");
	(void) fflush(tf);
	if (ferror(tf)) {
		fprintf(stderr, "Warning: %s write error, %s not updated\n",
		    temp, passwd);
		goto out;
	}
	(void) fclose(tf);
	if (dp != NULL)
		dbm_close(dp);
	if (rename(temp, passwd) < 0) {
		perror("passwd: rename");
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
	(void) setrlimit(lim, &rlim);
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
	bcopy((char *)&pwd->pw_uid, cp, sizeof (int));
	cp += sizeof (int);
	bcopy((char *)&pwd->pw_gid, cp, sizeof (int));
	cp += sizeof (int);
	bcopy((char *)&pwd->pw_quota, cp, sizeof (int));
	cp += sizeof (int);
	COMPACT(comment);
	COMPACT(gecos);
	COMPACT(dir);
	COMPACT(shell);
	content.dptr = buf;
	content.dsize = cp - buf;
	key.dptr = pwd->pw_name;
	key.dsize = strlen(pwd->pw_name);
	dbm_store(dp, key, content, DBM_REPLACE);
	key.dptr = (char *)&pwd->pw_uid;
	key.dsize = sizeof (int);
	dbm_store(dp, key, content, DBM_REPLACE);
}

char *
getnewpasswd(pwd, u)
	register struct passwd *pwd;
	int u;
{
	char saltc[2];
	long salt;
	int i, insist = 0, ok, flags;
	int c, pwlen;
	static char pwbuf[10];
	long time();
	char *crypt(), *pw, *p;

	if (pwd->pw_passwd[0] && u != 0) {
		(void) strcpy(pwbuf, getpass("Old password:"));
		pw = crypt(pwbuf, pwd->pw_passwd);
		if (strcmp(pw, pwd->pw_passwd) != 0) {
			printf("Sorry.\n");
			exit(1);
		}
	}
tryagain:
	(void) strcpy(pwbuf, getpass("New password:"));
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
	(void) time(&salt);
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
	return (crypt(pwbuf, saltc));
}

char *
getloginshell(pwd, u, arg)
	struct passwd *pwd;
	int u;
	char *arg;
{
	static char newshell[BUFSIZ];
	char *cp, *valid, *getusershell();

	if (pwd->pw_shell == 0 || *pwd->pw_shell == '\0')
		pwd->pw_shell = DEFSHELL;
	if (u != 0) {
		do {
			valid = getusershell();
			if (valid == NULL) {
				printf("Cannot change from restricted shell %s\n",
					pwd->pw_shell);
				exit(1);
			}
		} while (strcmp(pwd->pw_shell, valid) != 0);
	}
	if (arg != 0) {
		(void) strncpy(newshell, arg, sizeof newshell - 1);
		newshell[sizeof newshell - 1] = '\0';
	} else {
		printf("Old shell: %s\nNew shell: ", pwd->pw_shell);
		(void)fgets(newshell, sizeof (newshell) - 1, stdin);
		cp = index(newshell, '\n');
		if (cp)
			*cp = '\0';
		if (newshell[0] == 0) {
			puts("Login shell unchanged.");
			exit(1);
		}
	}
	/*
	 * Allow user to give shell name w/o preceding pathname.
	 */
	if (u != 0 || newshell[0] != '/') {
		endusershell();
		do {
			valid = getusershell();
			if (valid == 0) {
				if (u == 0) {
					valid = newshell;
					break;
				}
				printf("%s is unacceptable as a new shell.\n",
					newshell);
				exit(1);
			}
			if (newshell[0] == '/') {
				cp = valid;
			} else {
				cp = rindex(valid, '/');
				if (cp == 0)
					cp = valid;
				else
					cp++;
			}
		} while (strcmp(newshell, cp) != 0);
	} else {
		valid = newshell;
	}
	if (strcmp(valid, pwd->pw_shell) == 0) {
		puts("Login shell unchanged.");
		exit(1);
	}
	if (access(valid, X_OK) < 0) {
		printf("%s is unavailable.\n", valid);
		exit(1);
	}
	if (strcmp(valid, DEFSHELL) == 0)
		valid[0] = '\0';
	return (valid);
}

struct default_values {
	char *name;
	char *office_num;
	char *office_phone;
	char *home_phone;
};

/*
 * Get name, room number, school phone, and home phone.
 */
char *
getfingerinfo(pwd)
	struct passwd *pwd;
{
	char in_str[BUFSIZ];
	struct default_values *defaults, *get_defaults();
	static char answer[4*BUFSIZ];

	answer[0] = '\0';
	defaults = get_defaults(pwd->pw_gecos);
	printf("Default values are printed inside of '[]'.\n");
	printf("To accept the default, type <return>.\n");
	printf("To have a blank entry, type the word 'none'.\n");
	/*
	 * Get name.
	 */
	do {
		printf("\nName [%s]: ", defaults->name);
		(void) fgets(in_str, BUFSIZ, stdin);
		if (special_case(in_str, defaults->name)) 
			break;
	} while (illegal_input(in_str));
	(void) strcpy(answer, in_str);
	/*
	 * Get room number.
	 */
	do {
		printf("Room number (Exs: 597E or 197C) [%s]: ",
			defaults->office_num);
		(void) fgets(in_str, BUFSIZ, stdin);
		if (special_case(in_str, defaults->office_num))
			break;
	} while (illegal_input(in_str) || illegal_building(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	/*
	 * Get office phone number.
	 * Remove hyphens.
	 */
	do {
		printf("Office Phone (Ex: 6426000) [%s]: ",
			defaults->office_phone);
		(void) fgets(in_str, BUFSIZ, stdin);
		if (special_case(in_str, defaults->office_phone))
			break;
		remove_hyphens(in_str);
	} while (illegal_input(in_str) || not_all_digits(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	/*
	 * Get home phone number.
	 * Remove hyphens if present.
	 */
	do {
		printf("Home Phone (Ex: 9875432) [%s]: ", defaults->home_phone);
		(void) fgets(in_str, BUFSIZ, stdin);
		if (special_case(in_str, defaults->home_phone))
			break;
		remove_hyphens(in_str);
	} while (illegal_input(in_str) || not_all_digits(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	if (strcmp(answer, pwd->pw_gecos) == 0) {
		printf("Finger information unchanged.\n");
		exit(1);
	}
	return (answer);
}

/*
 * Prints an error message if a ':' or a newline is found in the string.
 * A message is also printed if the input string is too long.
 * The password file uses :'s as seperators, and are not allowed in the "gcos"
 * field.  Newlines serve as delimiters between users in the password file,
 * and so, those too, are checked for.  (I don't think that it is possible to
 * type them in, but better safe than sorry)
 *
 * Returns '1' if a colon or newline is found or the input line is too long.
 */
illegal_input(input_str)
	char *input_str;
{
	char *ptr;
	int error_flag = 0;
	int length = strlen(input_str);

	if (index(input_str, ':')) {
		printf("':' is not allowed.\n");
		error_flag = 1;
	}
	if (input_str[length-1] != '\n') {
		/* the newline and the '\0' eat up two characters */
		printf("Maximum number of characters allowed is %d\n",
			BUFSIZ-2);
		/* flush the rest of the input line */
		while (getchar() != '\n')
			/* void */;
		error_flag = 1;
	}
	/*
	 * Delete newline by shortening string by 1.
	 */
	input_str[length-1] = '\0';
	/*
	 * Don't allow control characters, etc in input string.
	 */
	for (ptr=input_str; *ptr != '\0'; ptr++) {
		if ((int) *ptr < 040) {
			printf("Control characters are not allowed.\n");
			error_flag = 1;
			break;
		}
	}
	return (error_flag);
}

/*
 * Removes '-'s from the input string.
 */
remove_hyphens(str)
	char *str;
{
	char *hyphen;

	while ((hyphen = index(str, '-')) != NULL)
		(void) strcpy(hyphen, hyphen+1);
}

/*
 *  Checks to see if 'str' contains only digits (0-9).  If not, then
 *  an error message is printed and '1' is returned.
 */
not_all_digits(str)
	char *str;
{
	char *ptr;

	for (ptr = str; *ptr != '\0'; ++ptr)
		if (!isdigit(*ptr)) {
			printf("Phone numbers can only contain digits.\n");
			return (1);
		}
	return (0);
}

/*
 * Deal with Berkeley buildings.  Abbreviating Cory to C and Evans to E.
 * Correction changes "str".
 *
 * Returns 1 if incorrect room format.
 * 
 * Note: this function assumes that the newline has been removed from str.
 */
illegal_building(str)
	register char *str;
{
	int length = strlen(str);
	register char *ptr;

	/*
	 * If the string is [Ee]vans or [Cc]ory or ends in
	 * [ \t0-9][Ee]vans or [ \t0-9M][Cc]ory, then contract the name
	 * into 'E' or 'C', as the case may be, and delete leading blanks.
	 */
	if (length >= 5 && strcmp(ptr = str + length - 4, "vans") == 0 &&
	    (*--ptr == 'e' || *ptr == 'E') &&
	    (--ptr < str || isspace(*ptr) || isdigit(*ptr))) {
		for (; ptr > str && isspace(*ptr); ptr--)
			;
		ptr++;
		*ptr++ = 'E';
		*ptr = '\0';
	} else
	if (length >= 4 && strcmp(ptr = str + length - 3, "ory") == 0 &&
	    (*--ptr == 'c' || *ptr == 'C') &&
	    (--ptr < str || *ptr == 'M' || isspace(*ptr) || isdigit(*ptr))) {
		for (; ptr > str && isspace(*ptr); ptr--)
			;
		ptr++;
		*ptr++ = 'C';
		*ptr = '\0';
	}
	return (0);
}

/*
 * get_defaults picks apart "str" and returns a structure points.
 * "str" contains up to 4 fields separated by commas.
 * Any field that is missing is set to blank.
 */
struct default_values *
get_defaults(str)
	char *str;
{
	struct default_values *answer;

	answer = (struct default_values *)
		malloc((unsigned)sizeof(struct default_values));
	if (answer == (struct default_values *) NULL) {
		fprintf(stderr,
			"\nUnable to allocate storage in get_defaults!\n");
		exit(1);
	}
	/*
	 * Values if no corresponding string in "str".
	 */
	answer->name = str;
	answer->office_num = "";
	answer->office_phone = "";
	answer->home_phone = "";
	str = index(answer->name, ',');
	if (str == 0) 
		return (answer);
	*str = '\0';
	answer->office_num = str + 1;
	str = index(answer->office_num, ',');
	if (str == 0) 
		return (answer);
	*str = '\0';
	answer->office_phone = str + 1;
	str = index(answer->office_phone, ',');
	if (str == 0) 
		return (answer);
	*str = '\0';
	answer->home_phone = str + 1;
	return (answer);
}

/*
 *  special_case returns true when either the default is accepted
 *  (str = '\n'), or when 'none' is typed.  'none' is accepted in
 *  either upper or lower case (or any combination).  'str' is modified
 *  in these two cases.
 */
special_case(str,default_str)
	char *str, *default_str;
{
	static char word[] = "none\n";
	char *ptr, *wordptr;

	/*
	 *  If the default is accepted, then change the old string do the 
	 *  default string.
	 */
	if (*str == '\n') {
		(void) strcpy(str, default_str);
		return (1);
	}
	/*
	 *  Check to see if str is 'none'.  (It is questionable if case
	 *  insensitivity is worth the hair).
	 */
	wordptr = word-1;
	for (ptr = str; *ptr != '\0'; ++ptr) {
		++wordptr;
		if (*wordptr == '\0')	/* then words are different sizes */
			return (0);
		if (*ptr == *wordptr)
			continue;
		if (isupper(*ptr) && (tolower(*ptr) == *wordptr))
			continue;
		/*
		 * At this point we have a mismatch, so we return
		 */
		return (0);
	}
	/*
	 * Make sure that words are the same length.
	 */
	if (*(wordptr+1) != '\0')
		return (0);
	/*
	 * Change 'str' to be the null string
	 */
	*str = '\0';
	return (1);
}
