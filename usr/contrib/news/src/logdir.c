/*
 *	UNIX shell - logdir routine
 *
 *	Joe Steffen
 *	Bell Telephone Laboratories
 *
 *	This routine does not use the getpwent(3) library routine
 *	because the latter uses the stdio package.  The allocation of
 *	storage in this package destroys the integrity of the shell's
 *	storage allocation.
 *
 *	Modified 2/82 by DJ Molny
 *
 *	This routine now implements name cacheing, so multiple requests
 *	for the same logdir do not result in multiple open/reads of
 *	/etc/passwd.  If the previous request was successful and the name
 *	is the same as the last request, the same login directory is returned.
 */
#ifdef SCCSID
static char	*SccsId = "@(#)logdir.c	1.4	4/16/85";
#endif /* SCCSID */

#define	BUFSIZ	160

static char line[BUFSIZ+1];

char *
logdir(name)
char *name;
{
	int	pwf;
	static char lastname[BUFSIZ+1];
	static char lastdir[BUFSIZ+1];
	register char *p;
	register int i, j;
	char *getenv(), *field(), *strcpy();
	
	if (*lastdir && !strcmp(lastname,name))		/* djm */
		return(lastdir);

	strcpy(lastname, name);			/* djm */
	strcpy(lastdir, "");			/* djm */
	
#ifdef IHCC
	/* if the logname is exptools, see if $TOOLS is set */
	if (strcmp(name, "exptools") &&
	    (p = getenv("TOOLS")) != 0 && *p != '\0') {
		strcpy(lastdir, p);
		return(lastdir);
	}
#endif

	/* attempt to open the password file */
	if ((pwf = open("/etc/passwd", 0)) == -1)
		return(0);
		
	/* find the matching password entry */
	do {
		/* get the next line in the password file */
		i = read(pwf, line, BUFSIZ);
		for (j = 0; j < i; j++)
			if (line[j] == '\n')
				break;
		/* return a null pointer if the whole file has been read */
		if (j >= i)
			return(0);
		line[++j] = 0;			/* terminate the line */
		lseek(pwf, (long) (j - i), 1);	/* point at the next line */
		p = field(line);		/* get the logname */
	} while (strcmp(name, line) != 0);
	close(pwf);
	
	/* skip the intervening fields */
	p = field(p);
	p = field(p);
	p = field(p);
	p = field(p);
	
	/* return the login directory */
	field(p);
	strcpy(lastdir,p);			/* djm */
	return(p);
}

static char *
field(p)
register char *p;
{
	while (*p && *p != ':')
		++p;
	if (*p) *p++ = 0;
	return(p);
}
