/* prsvunix.c */

/* This file contains the UNIX-specific parts of the "elvprsv" program. */

#if OSK
#define ELVPRSV
#include "osk.c"
#else
#include <sys/stat.h>
#include <pwd.h>
#endif
#ifndef __386BSD__
extern struct passwd *getpwuid();
#endif

/* This variable is used to add extra error messages for mail sent to root */
char *ps;

/* This function returns the login name of the owner of a file */
char *ownername(filename)
	char	*filename;	/* name of a file */
{
	struct stat	st;
	struct passwd	*pw;

	/* stat the file, to get its uid */
	if (stat(filename, &st) < 0)
	{
		ps = "stat() failed";
		return "root";
	}

	/* get the /etc/passwd entry for that user */
	pw = getpwuid(st.st_uid);
	if (!pw)
	{
		ps = "uid not found in password file";
		return "root";
	}

	/* return the user's name */
	return pw->pw_name;
}


/* This function sends a mail message to a given user, saying that a file
 * has been preserved.
 */
void mail(user, file, when)
	char	*user;	/* name of user who should receive the mail */
	char	*file;	/* name of original text file that was preserved */
	char	*when;	/* description of why the file was preserved */
{
	char	cmd[80];/* buffer used for constructing a "mail" command */
	FILE	*m, *popen();	/* stream used for giving text to the "mail" program */
	char	*base;	/* basename of the file */

	/* separate the directory name from the basename. */
	for (base = file + strlen(file); --base > file && *base != SLASH; )
	{
	}
	if (*base == SLASH)
	{
		*base++ = '\0';
	}

	/* for anonymous buffers, pretend the name was "foo" */
	if (!strcmp(base, "*"))
	{
		base = "foo";
	}

	/* open a pipe to the "mail" program */
#if OSK
	sprintf(cmd, "mail \"-s=%s preserved!\" %s", base, user);
#else /* ANY_UNIX */
	sprintf(cmd, "mail %s >/dev/null 2>/dev/null", user);
#endif
	m = popen(cmd, "w");
	if (!m)
	{
		/* Can't send mail!  Hope the user figures it out. */
		return;
	}

	/* Tell the user that the file was preserved */
	fprintf(m, "A version of your file \"%s%c%s\"\n", file, SLASH, base);
	fprintf(m, "was preserved when %s.\n", when);
	fprintf(m, "To recover this file, do the following:\n");
	fprintf(m, "\n");
#if OSK
	fprintf(m, "     chd %s\n", file);
#else /* ANY_UNIX */
	fprintf(m, "     cd %s\n", file);
#endif
	fprintf(m, "     elvisrecover %s\n", base);
	fprintf(m, "\n");
	fprintf(m, "With fond wishes for a speedy recovery,\n");
	fprintf(m, "                                    Elvis\n");
	if (ps)
	{
		fprintf(m, "\nP.S. %s\n", ps);
		ps = (char *)0;
	}

	/* close the stream */
	pclose(m);
}
