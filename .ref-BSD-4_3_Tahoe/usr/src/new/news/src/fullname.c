/*
 * fullname.c - this file is made separate so that different local
 * conventions can be applied.  The stock version understands two
 * conventions:
 *
 * (a) Berkeley finger: the gecos field in /etc/passwd begins with
 *     the full name, terminated with comma, semicolon, or end of
 *     field.  & expands to the login name.
 * (b) BTL RJE: the gecos field looks like
 *	: junk - full name ( junk :
 *     where the "junk -" is optional.
 *
 * If you have a different local convention, modify this file accordingly.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)fullname.c	1.12	9/24/87";
#endif /* SCCSID */

#include "params.h"

#ifndef LOCALNAME
/*
 * Figure out who is sending the message and sign it.
 * We attempt to look up the user in the gecos field of /etc/passwd.
 */
char *
fullname(un)
char *un;
{
	static char inbuf[BUFLEN];
	struct passwd *pw;

	pw = getpwnam(un);
	if (pw == NULL)
		return un;
	buildfname(pw->pw_gecos, un, inbuf);
	if (inbuf[0] == 0)
		return un;
	return inbuf;
}

#else

/*
 * Alternative version of fullname which asks the user for his full name.
 * This is mainly suitable for systems that don't have a full name
 * database somewhere.  It puts the answer in $HOME/.name
 */
char *
fullname(un)
char *un;
{
	static char inbuf[BUFLEN];
	char fbuf[BUFLEN];
	FILE *fd;
	char *p, *index(), *getenv();
	int pid;

	if (!isatty(2))
		return un;
	printf("What is your full name (for news article signatures): ");
	fflush(stdout);
	read(2, inbuf, sizeof inbuf);
	if (inbuf[0] == 0)
		return un;
	p = index(inbuf, '\n');
	if (p)
		*p = 0;
	if ((p = getenv("HOME")) == NULL) {
		fprintf(stderr,
		"inews: no HOME environment variable - .name not written\n");
		return inbuf;
	}
	sprintf(fbuf, "%s/%s", p, ".name");
	if ((pid = vfork()) < 0) {
		perror("inews");
		return inbuf;
	}
	else if (pid != 0)
		while (wait((int *)0) != pid)
			;
	else {
		setuid(getuid());	/* become the user */
		if ((fd = fopen(fbuf, "w")) == NULL)
			fprintf(stderr, "inews: can't create %s\n", fbuf);
		else {
			fprintf(fd, "%s\n", inbuf);
			fclose(fd);
		}
		exit(0);
	}
	return inbuf;
}
#endif

#ifndef LOCALNAME
/*
**  BUILDFNAME -- build full name from gecos style entry.
**	(routine lifted from sendmail)
**
**	This routine interprets the strange entry that would appear
**	in the GECOS field of the password file.
**
**	Parameters:
**		p -- name to build.
**		login -- the login name of this user (for &).
**		buf -- place to put the result.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

buildfname(p, login, buf)
	register char *p;
	char *login;
	char *buf;
{
	register char *bp = buf;

	if (*p == '*')
		p++;
	while (*p != '\0' && *p != ',' && *p != ';' && *p != ':' && *p != '(')
	{
		if (*p == '-' && isspace(p[1])) {
			bp = buf;
			p++;
		}
		else if (*p == '&')
		{
			strcpy(bp, login);
			if ((bp == buf || !isalpha(bp[-1])) && islower(*bp))
				*bp = toupper(*bp);
			while (*bp != '\0')
				bp++;
			p++;
		}
		else
			*bp++ = *p++;
	}
	*bp = '\0';
}
#endif
