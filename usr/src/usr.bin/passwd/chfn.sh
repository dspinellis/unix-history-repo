#ifndef lint
static char *sccsid = "@(#)chfn.sh	4.2 (Berkeley) %G%";
#endif lint

/*
 *	 changefinger - change finger entries
 */
#include <stdio.h>
#include <signal.h>
#include <pwd.h>

char	passwd[] = "/etc/passwd";
char	temp[]	 = "/etc/ptmp";
struct	passwd *pwd;
struct	passwd *getpwent(), *getpwnam(), *getpwuid();
int	endpwent();
char	*crypt();
char	*getpass();
char	buf[BUFSIZ];

#define MAX_STR 52
main(argc, argv)
	int argc;
	char *argv[];
{
	int user_uid;
	int num_bytes, fi, fo;
	char replacement[4*MAX_STR];
	FILE *tf;

	if (argc > 2) {
		printf("Usage: changefinger [user]\n");
		exit(1);
	}
	/*
	 * Error check to make sure the user (foolishly) typed their own name.
	 */
	user_uid = getuid();
	if ((argc == 2) && (user_uid != 0)) {
		pwd = getpwnam(argv[1]);
		if (pwd == NULL) {
			printf("%s%s%s%s%s%s%s%s",
				"There is no account for ", argv[1],
				" on this machine.\n", 
				"You probably mispelled your login name;\n",
				"only root is allowed to change another",
				" person's finger entry.\n",
				"Note:  you do not need to type your login",
				" name as an argument.\n");
			exit(1);
		}
		if (pwd->pw_uid != user_uid) {
			printf("%s%s",
				"You are not allowed to change another",
				" person's password entry.\n");
			exit(1);
		}
	}
	/*
	 * If root is changing a finger entry, then find the uid that
	 * corresponds to the user's login name.
	 */
	if ((argc == 2) && (user_uid == 0)) {
		pwd = getpwnam(argv[1]);
		if (pwd == NULL) {
			printf("There is no account for %s on this machine\n", 
				pwd->pw_name);
			exit(1);
		}
		user_uid = pwd->pw_uid;
	}
	/*
	 * Collect name, room number, school phone, and home phone.
	 */
	get_info(replacement);
	/*
	 * Update the entry in the password file.
	 */
	while (access(temp, 0) >= 0) {
		printf("Password file busy -- waiting for it to be free.\n");
		sleep(10);
	}
	(void) signal(SIGHUP, SIG_IGN);
	(void) signal(SIGINT, SIG_IGN);
	(void) signal(SIGQUIT, SIG_IGN);
	(void) signal(SIGTSTP, SIG_IGN);
	/*
	 * Race condition -- the locking mechinism is not my idea (ns)
	 */
	if(access(temp, 0) >= 0) {
		printf("It's not your day!  Password file is busy again.\n");
		printf("Try again later.\n");
		exit(1);
	}
	if((tf=fopen(temp,"w")) == NULL) {
		printf("Cannot create temporary file\n");
		exit(1);
	}
	/*
	 * There is another race condition here:  if the passwd file
	 * has changed since the error checking at the beginning of the program,
	 * then user_uid may not be in the file.  Of course, the uid might have
	 * been changed, but this is not supposed to happen.
	 */
	if (getpwuid(user_uid) == NULL) {
		printf("%s%d%s\n", "Passwd file has changed. Uid ", user_uid,
			" is no longer in the file!?");
		goto out;
	}
	/*
	 * copy passwd to temp, replacing matching line
	 * with new finger entry (gecos field).
	 */
	while((pwd=getpwent()) != NULL) {
		if(pwd->pw_uid == user_uid) {
			pwd->pw_gecos = replacement;
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
	(void) endpwent();
	(void) fclose(tf);
	/*
	 * Copy temp back to password file.
	 */
	if((fi=open(temp,0)) < 0) {
		printf("Temp file disappeared!\n");
		goto out;
	}
	if((fo=creat(passwd, 0644)) < 0) {
		printf("Cannot recreat passwd file.\n");
		goto out;
	}
	while((num_bytes=read(fi,buf,sizeof(buf))) > 0)
		(void) write(fo,buf,num_bytes);
out:
	(void) unlink(temp);
}

/*
 * Get name, room number, school phone, and home phone.
 */
get_info(answer)
	char *answer;
{
	char *strcpy(), *strcat();
	char in_str[MAX_STR];
	answer[0] = '\0';

	/*
	 * Get name.
	 */
	do {
		printf("\nName: ");
		(void) fgets(in_str, MAX_STR, stdin);
	} while (illegal_input(in_str));
	(void) strcpy(answer, in_str);
	/*
	 * Get room number.
	 */
	do {
		printf("Room number (Exs: 597E or 197C): ");
		(void) fgets(in_str, MAX_STR, stdin);
	} while (illegal_input(in_str) || illegal_building(in_str));
	(void) strcat(strcat(answer, ","), in_str);
	/*
	 * Get office phone number.
	 * Remove hyphens and 642 or x2 prefixes if present.
	 */
	do {
		printf("Office Phone (Ex: 1632): ");
		(void) fgets(in_str, MAX_STR, stdin);
		remove_hyphens(in_str);
		if ((strlen(in_str) == 8) && (strcmpn(in_str, "642", 3) == 0))
			(void) strcpy(in_str, in_str+3);
		if ((strlen(in_str) == 7) && (strcmpn(in_str, "x2", 2) == 0))
			(void) strcpy(in_str, in_str+2);
	} while ((illegal_input(in_str)) || wrong_length(in_str, 4));
	(void) strcat(strcat(answer, ","), in_str);
	/*
	 * Get home phone number.
	 * Remove hyphens if present.
	 */
	do {
		printf("Home Phone (Ex: 9875432): ");
		(void) fgets(in_str, MAX_STR, stdin);
		remove_hyphens(in_str);
	} while (illegal_input(in_str));
	(void) strcat(strcat(answer, ","), in_str);
}

/*
 * Prints an error message if a ':' or a newline is found in the string.
 * A message is also printed if the input string is too long.
 * The password file uses :'s as seperators, and are not allowed in the "gcos"
 * field.  Newlines serve a delimiters between users in the password file,
 * and so, those too, are checked for.  (I don't think that it is possible to
 * type them in, but better safe than sorry)
 *
 * Returns '1' if a colon or newline is found or the input line is too long.
 */
illegal_input(input_str)
	char *input_str;
{
	char *index();
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
			MAX_STR-2);
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
	return(error_flag);
}

/*
 * Removes '-'s from the input string.
 */
remove_hyphens(str)
	char *str;
{
	char *hyphen, *index(), *strcpy();

	while ((hyphen=index(str, '-')) != NULL) {
		(void) strcpy(hyphen, hyphen+1);
	}
}

/*
 * Returns 1 when the length of the input string is not zero or equal to n.
 * Prints an error message in this case.
 */
wrong_length(str, n)
	char *str;
	int n;
{
	if ((strlen(str) != 0) && (strlen(str) != n)) {
		printf("The phone number should be %d digits long.\n", n);
		return(1);
	}
	return(0);
}

/*
 * Make sure that building is 'E' or 'C'.
 * Error correction is done if building is 'e', 'c', "evans", or "cory".
 * Correction changes "str".
 * The finger program determines the building by looking at the last
 * character.  Currently, finger only allows that character to be 'E' or 'C'.
 *
 * Returns 1 if incorrect room format.
 * 
 * Note: this function assumes that the newline has been removed from str.
 */
illegal_building(str)
	char *str;
{
	int length = strlen(str);
	char *last_ch, *ptr;

	/*
	 * Zero length strings are acceptable input.
	 */
	if (length == 0)
		return(0);
	/*
	 * Delete "vans" and "ory".
	 */
	if (strcmpn(str+length-4, "vans", 4) == 0) {
		length -= 4;
		str[length] = '\0';
	}
	if (strcmpn(str+length-3, "ory", 3) == 0) {
		length -= 3;
		str[length] = '\0';
	}
	last_ch = str+length-1;
	/*
	 * Now change e to E or c to C.
	 */
	if (*last_ch == 'e')
		*last_ch = 'E';
	if (*last_ch == 'c')
		*last_ch = 'C';
	/*
	 * Delete any spaces before the E or C.
	 */
	for (ptr=last_ch-1; ptr>str; ptr--) {
		if (*ptr != ' ')
			break;
	}
	(void) strcpy(ptr+1, last_ch);
	/*
	 * Make sure building is evans or cory.
	 */
	if ((*last_ch != 'E') && (*last_ch != 'C')) {
		printf("%s%s%s",
			"The finger program requires that your",
			" office be in Cory or Evans.\n",
			"Enter this as (for example) 597E or 197C.\n");
		return(1);
	}
	return(0);
}
