#ifndef lint
static char *sccsid = "@(#)inews.c	1.4	(Berkeley) 3/20/86";
#endif

/*
 * Itty-bitty inews for talking to remote server.
 * Simply accept input on stdin (or via a named file) and dump this
 * to the server; add a From: and Path: line if missing in the original.
 *
 * Steven Grady (grady@ucbvax.Berkeley.EDU)
 */

#include <stdio.h>
#include <pwd.h>
#include <ctype.h>
#include <strings.h>
#include "../common/conf.h"
#include "../common/response_codes.h"

extern	FILE	*ser_wr_fp;

main(argc, argv)
int	argc;
char	*argv[];
{
	char	line[256], s[256];
	int	seen_fromline, in_header;

	++argv;
	while (argc > 1)
		if (*argv[0] == '-') {
			++argv;
			--argc;
		} else
			break;

	if (argc > 1) {
		if (freopen(*argv, "r", stdin) == NULL) {
			perror(*argv);
			exit(1);
		}
	}

	if (server_init(SERVER_HOST) < 0)
		exit(1);			/* Connect to the server */

	put_server("POST");
	(void) get_server(line, sizeof(line));
	if (*line != CHAR_CONT) {
		if (atoi(line) == ERR_NOPOST) {
			fprintf(stderr,
				"Sorry, you can't post from this machine.\n");
			exit(1);
		} else {
		        fprintf(stderr, "Remote error: %s\n", line);
			exit(1);
		}
	}

	in_header = 1;
	while (gets(s) != NULL) {
		if (s[0] == '.')    /* Single . is eof, so put in extra one */
			(void) fputc('.', ser_wr_fp);
		if (in_header && strneql(s, "From:", sizeof("From:")-1))
			seen_fromline = 1;
		if (in_header && s[0] == '\0') {
			in_header = 0;
			if (!seen_fromline)
				gen_frompath();
		}
		fprintf(ser_wr_fp, "%s\r\n", s);
	}

	append_signature();

	fprintf(ser_wr_fp, ".\r\n");
	(void) fflush(ser_wr_fp);
	(void) get_server(line, sizeof(line));
	if (*line != CHAR_OK) {
		if (atoi(line) == ERR_POSTFAIL) {
			fprintf(stderr,
			    "Article not accepted by server; not posted.\n");
			exit(1);
		} else {
			fprintf(stderr, "Remote error: %s\n", line);
			exit(1);
		}
	}

	/*
	 * Close server sends the server a
	 * "quit" command for us, which is why we don't send it.
	 */

	close_server();
}

/*
 * append_signature -- append the person's .signature file if
 * they have one.
 */

append_signature()
{
	char	line[256], sigfile[256];
	char	*cp;
	struct	passwd	*passwd;
	FILE	*fp;
	char	*index();

	passwd = getpwuid(getuid());
	if (passwd == NULL)
		return;

	(void) strcpy(sigfile, passwd->pw_dir);
	(void) strcat(sigfile, "/");
	(void) strcat(sigfile, ".signature");

	fp = fopen(sigfile, "r");
	if (fp != NULL)
		while (fgets(line, sizeof(line), fp)) {
			if (cp = index(line, '\n'))
				*cp = '\0';
			fprintf(ser_wr_fp, "%s\r\n", line);
		}
}

/*
 * gen_frompath -- generate From: and Path: lines, in the form
 *
 *	From: user@host.domain (full_name)
 *	Path: host!user
 *
 * This routine should only be called if the message doesn't have
 * a From: line in it.
 */

gen_frompath()
{
	char	host_name[256];
	char	*full_name;
	char	*cp;
	struct	passwd *passwd;
	char	*index(), *getenv();

	passwd = getpwuid(getuid());

	full_name = getenv("NAME");
	if (full_name == NULL) {
		full_name = passwd->pw_gecos;
		if ((cp = index(full_name, ',')))
			*cp = '\0';
	}

	(void) gethostname(host_name, sizeof (host_name));
	
#ifdef DOMAIN
	fprintf(ser_wr_fp, "From: %s@%s.%s (",
		passwd->pw_name,
		host_name,
		DOMAIN);
#else
	fprintf(ser_wr_fp, "From: %s@%s (",
		passwd->pw_name,
		host_name);
#endif

	for (cp = full_name; *cp != '\0'; ++cp)
		if (*cp != '&')
			putc(*cp, ser_wr_fp);
		else {		/* Stupid & hack.  God damn it. */
			putc(toupper(passwd->pw_name[0]), ser_wr_fp);
			fprintf(ser_wr_fp, passwd->pw_name+1);
		}

	fprintf(ser_wr_fp, ")\r\n");

	fprintf(ser_wr_fp, "Path: %s!%s\r\n", host_name, passwd->pw_name);
}


/*
 * strneql -- determine if two strings are equal in the first n
 * characters, ignoring case.
 *
 *	Parameters:	"a" and "b" are the pointers
 *			to characters to be compared.
 *			"n" is the number of characters to compare.
 *
 *	Returns:	1 if the strings are equal, 0 otherwise.
 *
 *	Side effects:	None.
 */

strneql(a, b, n)
register char *a, *b;
int	n;
{
	char	lower();

	while (n && lower(*a) == lower(*b)) {
		if (*a == '\0')
			return (1);
		a++;
		b++;
		n--;
	}
	if (n)
		return (0);
	else
		return (1);
}

/*
 * lower -- convert a character to lower case, if it's
 *	upper case.
 *
 *	Parameters:	"c" is the character to be
 *			converted.
 *
 *	Returns:	"c" if the character is not
 *			upper case, otherwise the lower
 *			case eqivalent of "c".
 *
 *	Side effects:	None.
 */

char lower(c)
register char c;
{
	if (isascii(c) && isupper(c))
		c = c - 'A' + 'a';
	return(c);
}
