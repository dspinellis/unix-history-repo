#


/*
 * Getname / getuserid for those with no
 * hashed passwd data base).
 * Do not compile this module in if you DO have hashed
 * passwd's -- this is slower.
 *
 * Also provided here is a getpw routine which can share
 * the open file.  This is used for the Version 6 getenv
 * implementation.
 */

#include "rcv.h"

static FILE *pwfile =	NULL;		/* Pw file held open */
static char *pwname = 	"/etc/passwd";	/* Name of passwd file */

/*
 * Search the passwd file for a uid.  Return name through ref parameter
 * if found, indicating success with 0 return.  Return -1 on error.
 * If -1 is passed as the user id, close the passwd file.
 */

getname(uid, namebuf)
	char namebuf[];
{
	register char *cp, *cp2;
	char linebuf[BUFSIZ];

	if (uid == -1) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL)
		return(-1);
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL)
		if (pweval(linebuf) == uid) {
			for (cp = linebuf, cp2 = namebuf; *cp != ':';
			    *cp2++ = *cp++)
				;
			*cp2 = '\0';
			return(0);
		}
	return(-1);
}

/*
 * Read the users password file line into the passed line
 * buffer.
 */

getpw(uid, linebuf)
	char linebuf[];
{
	register char *cp, *cp2;

	if (uid == -1) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL)
		return(-1);
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL)
		if (pweval(linebuf) == uid) {
			if (linebuf[0] != '\0')
				linebuf[strlen(linebuf)-1] = '\0';
			return(0);
		}
	return(-1);
}

/*
 * Look for passwd line belonging to 'name'
 */

getpwnam(name, linebuf)
	char name[], linebuf[];
{
	register char *cp, *cp2;

	if (name == NOSTR) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL) {
		perror(pwname);
		return(-1);
	}
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL) {
		cp = linebuf;
		cp2 = name;
		while (*cp2++ == *cp++)
			;
		if (*--cp == ':' && *--cp2 == 0)
			return(0);
	}
	return(-1);
}

/*
 * Convert the passed name to a user id and return it.  Return -1
 * on error.  Iff the name passed is -1 (yech) close the pwfile.
 */

getuserid(name)
	char name[];
{
	register char *cp, *cp2;
	char linebuf[BUFSIZ];

	if (name == (char *) -1) {
		if (pwfile != NULL)
			fclose(pwfile);
		pwfile = NULL;
		return(0);
	}
	if (pwfile == NULL && (pwfile = fopen(pwname, "r")) == NULL)
		return(-1);
	rewind(pwfile);
	while (fgets(linebuf, BUFSIZ, pwfile) != NULL) {
		for (cp = name, cp2 = linebuf; *cp++ == *cp2++;)
			;
		if (*--cp == '\0' && *--cp2 == ':')
			return(pweval(linebuf));
	}
	return(-1);
}

/*
 * Evaluate the user id of the passed passwd line and return it.
 */

static
pweval(line)
	char line[];
{
	register char *cp;
	register int i;
	register int uid;

	for (cp = line, i = 0; i < 2; i += (*cp++ == ':'))
		;
	uid = atoi(cp);

#ifdef UIDGID
	while (*cp && *cp != ':')
		cp++;
	cp++;
	uid |= atoi(cp) << 8;
#endif

	return(uid);
}
