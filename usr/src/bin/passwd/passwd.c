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
static char sccsid[] = "@(#)passwd.c	4.32 (Berkeley) 1/21/88";
#endif not lint

/*
 * Modify a field in the password file (either password, login shell, or
 * gecos field).  This program should be suid with an owner with write
 * permission on /etc/passwd.
 */
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
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
#define	DEFSHELL	"/bin/sh"

#define	DICT		"/usr/dict/words"
#define	PASSWD		"/etc/passwd"
#define	PTEMP		"/etc/ptmp"

#define	EOS		'\0';

main(argc, argv)
	int	argc;
	char	**argv;
{
	extern char	*optarg;
	extern int	errno, optind;
	struct passwd	*pwd;
	FILE	*tf;
	DBM	*dp;
	uid_t	uid, getuid();
	int	ch, fd, dochfn, dochsh;
	char	*cp, *uname, *progname, *umsg,
		*getfingerinfo(), *getloginshell(), *getnewpasswd(), *malloc();

	progname = (cp = rindex(*argv, '/')) ? cp + 1 : *argv;
	dochfn = dochsh = 0;
	if (!strcmp(progname, "chfn")) {
		dochfn = 1;
		umsg = "usage: chfn [username]\n";
	}
	else if (!strcmp(progname, "chsh")) {
		dochsh = 1;
		umsg = "usage: chsh [username]\n";
	}
	else
		umsg = "usage: passwd [-fs] [username]\n";

	while ((ch = getopt(argc, argv, "fs")) != EOF)
		switch((char)ch) {
		case 'f':
			if (dochsh)
				goto usage;
			dochfn = 1;
			break;
		case 's':
			if (dochfn)
				goto usage;
			dochsh = 1;
			break;
		case '?':
		default:
usage:			fputs(umsg, stderr);
			exit(1);
		}

	uid = getuid();
	if (argc - optind < 1) {
		if (!(pwd = getpwuid(uid))) {
			fprintf(stderr, "%s: %u: unknown user uid.\n", progname, uid);
			exit(1);
		}
		if (!(uname = malloc((u_int)(strlen(pwd->pw_name) + 1)))) {
			fprintf(stderr, "%s: out of space.\n", progname);
			exit(1);
		}
		(void)strcpy(uname, pwd->pw_name);
	}
	else {
		uname = *(argv + optind);
		if (!(pwd = getpwnam(uname))) {
			fprintf(stderr, "%s: %s: unknown user.\n", progname, uname);
			exit(1);
		}
	}
	if (uid && uid != pwd->pw_uid) {
		fputs("Permission denied.\n", stderr);
		exit(1);
	}
	printf("Changing %s for %s.\n", dochfn ? "finger information" : dochsh ? "login shell" : "password", uname);
	if (dochfn)
		cp = getfingerinfo(pwd);
	else if (dochsh)
		cp = getloginshell(pwd, uid);
	else
		cp = getnewpasswd(pwd, uid);
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTSTP, SIG_IGN);
	(void) umask(0);
	if ((fd = open(PTEMP, O_WRONLY|O_CREAT|O_EXCL, 0644)) < 0) {
		if (errno == EEXIST)
			fprintf(stderr, "%s: password file busy - try again.\n", progname);
		else {
			fprintf(stderr, "%s: %s: ", progname, PTEMP);
			perror((char *)NULL);
		}
		exit(1);
	}
	if ((tf = fdopen(fd, "w")) == NULL) {
		fprintf(stderr, "%s: fdopen failed.\n", progname);
		exit(1);
	}
	if ((dp = dbm_open(PASSWD, O_RDWR, 0644)) == NULL) {
		fprintf(stderr, "Warning: dbm_open failed: %s: ", PASSWD);
		perror((char *)NULL);
	}
	else if (flock(dp->dbm_dirf, LOCK_EX) < 0) {
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
		if (!strcmp(pwd->pw_name, uname)) {
			if (uid && uid != pwd->pw_uid) {
				fprintf(stderr, "%s: permission denied.\n", progname);
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
		fprintf(tf, "%s:%s:%d:%d:%s:%s:%s\n",
			pwd->pw_name,
			pwd->pw_passwd,
			pwd->pw_uid,
			pwd->pw_gid,
			pwd->pw_gecos,
			pwd->pw_dir,
			pwd->pw_shell);
	}
	endpwent();
	if (dp && dbm_error(dp))
		fputs("Warning: dbm_store failed.\n", stderr);
	(void) fflush(tf);
	if (ferror(tf)) {
		fprintf(stderr, "Warning: %s write error, %s not updated.\n",
		    PTEMP, PASSWD);
		goto out;
	}
	(void)fclose(tf);
	if (dp != NULL)
		dbm_close(dp);
	if (rename(PTEMP, PASSWD) < 0) {
		perror(progname);
	out:
		(void)unlink(PTEMP);
		exit(1);
	}
	exit(0);
}

unlimit(lim)
	int	lim;
{
	struct rlimit rlim;

	rlim.rlim_cur = rlim.rlim_max = RLIM_INFINITY;
	(void)setrlimit(lim, &rlim);
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
#define	COMPACT(e)	tp = pwd->e; while (*cp++ = *tp++);
	COMPACT(pw_name);
	COMPACT(pw_passwd);
	bcopy((char *)&pwd->pw_uid, cp, sizeof (int));
	cp += sizeof (int);
	bcopy((char *)&pwd->pw_gid, cp, sizeof (int));
	cp += sizeof (int);
	bcopy((char *)&pwd->pw_quota, cp, sizeof (int));
	cp += sizeof (int);
	COMPACT(pw_comment);
	COMPACT(pw_gecos);
	COMPACT(pw_dir);
	COMPACT(pw_shell);
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
	uid_t u;
{
	time_t	salt, time();
	int	c, i, insist;
	char	*pw, pwbuf[10], pwcopy[10], saltc[2],
		*crypt(), *getpass();

	if (pwd->pw_passwd[0] && u != 0) {
		(void)strcpy(pwbuf, getpass("Old password:"));
		pw = crypt(pwbuf, pwd->pw_passwd);
		if (strcmp(pw, pwd->pw_passwd) != 0) {
			puts("Sorry.");
			exit(1);
		}
	}
	for(;;) {
		(void)strcpy(pwbuf, getpass("New password:"));
		if (!*pwbuf) {
			puts("Password unchanged.");
			exit(1);
		}
		if (strcmp(pwbuf, pwcopy)) {
			insist = 1;
			(void)strcpy(pwcopy, pwbuf);
		}
		else if (++insist == 4)
			break;
		if (strlen(pwbuf) <= 4)
			puts("Please enter a longer password.");
		else {
			for (pw = pwbuf; *pw && islower(*pw); ++pw);
			if (*pw)
				break;
			puts("Please don't use an all-lower case password.\nUnusual capitalization, control characters or digits are suggested.");
		}
	}
	if (strcmp(pwbuf, getpass("Retype new password:"))) {
		puts("Mismatch - password unchanged.");
		exit(1);
	}
	(void)time(&salt);
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
	return(crypt(pwbuf, saltc));
}

char *
getloginshell(pwd, u)
	struct passwd *pwd;
	uid_t u;
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
	printf("Old shell: %s\nNew shell: ", pwd->pw_shell);
	(void)fgets(newshell, sizeof (newshell) - 1, stdin);
	cp = index(newshell, '\n');
	if (cp)
		*cp = '\0';
	if (newshell[0] == 0) {
		puts("Login shell unchanged.");
		exit(1);
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
	}
	else
		valid = newshell;
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
	puts("Default values are printed inside of '[]'.");
	puts("To accept the default, type <return>.");
	puts("To have a blank entry, type the word 'none'.");
	/*
	 * Get name.
	 */
	do {
		printf("\nName [%s]: ", defaults->name);
		(void) fgets(in_str, BUFSIZ - 1, stdin);
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
		(void) fgets(in_str, BUFSIZ - 1, stdin);
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
		(void) fgets(in_str, BUFSIZ - 1, stdin);
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
		(void) fgets(in_str, BUFSIZ - 1, stdin);
		if (special_case(in_str, defaults->home_phone))
			break;
		remove_hyphens(in_str);
	} while (illegal_input(in_str) || not_all_digits(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	if (strcmp(answer, pwd->pw_gecos) == 0) {
		puts("Finger information unchanged.");
		exit(1);
	}
	return (answer);
}

/*
 * Prints an error message if a ':', ',' or a newline is found in the string.
 * A message is also printed if the input string is too long.  The password
 * file uses :'s as separators, and are not allowed in the "gcos" field;
 * commas are used as separators in the gcos field, so are disallowed.
 * Newlines serve as delimiters between users in the password file, and so,
 * those too, are checked for.  (I don't think that it is possible to
 * type them in, but better safe than sorry)
 *
 * Returns '1' if a colon, comma or newline is found or the input line is
 * too long.
 */
illegal_input(input_str)
	char *input_str;
{
	char *ptr;
	int error_flag = 0;
	int length = strlen(input_str);

	if (strpbrk(input_str, ",:")) {
		puts("':' and ',' are not allowed.");
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
	for (ptr = input_str; *ptr; ptr++)
		if (!isprint(*ptr)) {
			puts("Control characters are not allowed.");
			error_flag = 1;
			break;
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
	register char *str;
{
	for (; *str; ++str)
		if (!isdigit(*str)) {
			puts("Phone numbers may only contain digits.");
			return(1);
		}
	return(0);
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
	char	*malloc();

	answer = (struct default_values *)
		malloc((unsigned)sizeof(struct default_values));
	if (answer == (struct default_values *) NULL) {
		fputs("\nUnable to allocate storage in get_defaults!\n", stderr);
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
