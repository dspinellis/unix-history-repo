static char Sccsid[] = "@(#)userdir.c	1.2	%G%";
/*
	Gets user's login directory.
	The argument must be an integer.
	Note the assumption about position of directory field in
	password file (no group id in password file).
	Returns pointer to login directory on success,
	0 on failure.
        Remembers user ID and login directory for subsequent calls.
*/

userdir(uid)
register int uid;
{
	char pw[200];
	static int ouid;
	static char ldir[33];
	register int i;
	register char *cp;

	if (ouid!=uid || ouid==0) {
		if (getpw(uid,pw))
			return(0);
		cp = pw;
		while (*cp++ != ':') ; /* login name */
		while (*cp++ != ':') ; /* passwd */
		while (*cp++ != ':') ; /* user ID */
		while (*cp++ != ':') ; /* comment */
		for (i=0; i<32; i++) {
			if ((ldir[i] = *cp)=='\0' || *cp==':') break;
			cp++;
		}
		ldir[i] = '\0';
	}
	return(ldir);
}
