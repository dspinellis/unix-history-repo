/* @(#)path.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 * This file contains routines that a) implement a path mechanism, whereby
 * several places may be searched for files, and b) provide a defaulting
 * mechanism for file name extensions.
 *
 * (modified from code originally written by John Ousterhout for the caesar
 *  design system)
 */

#include "gremlin.h"
#include <pwd.h>
#include <ctype.h>

/* Imports from config.c: */

extern char GLibrary[];

/* Library routines: */

extern char *strcpy(), *strcpyn(), *index(), *sprintf(), *malloc();

/* The following string holds the current path,which consists of a bunch
 * of directory names separated by spaces.
 */

#define PATHSIZE 400
static char path[PATHSIZE] = ".";

/* The following string pointers constitute a cache of recently
 * referenced tilde names.
 */

#define NTILDE 10
static char *tildename[NTILDE] = {NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL};
static char *realname[NTILDE]  = {NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL};
static int discard = 0;


int
PConvertTilde(psource, pdest, size)
char **psource;			/* Pointer to a pointer to the source string */
char **pdest;			/* Pointer to a pointer to the dest. string */
int *size;			/* Pointer to no. bytes available at pdest */

/*---------------------------------------------------------
 *	This routine converts tilde notation into standard directory names.
 *
 *	Results:
 *	If the conversion was done successfully, then TRUE is returned.
 *	If a user name couldn't be found in the password file, then
 *	FALSE is returned.
 *
 *	Side Effects:
 *	If the first character of the string indicated by psource is a
 *	tilde ("~") then the subsequent user name is converted to a login
 *	directory name and stored in the string indicated by pdest.  Then
 *	remaining characters in the file name at psource are copied to
 *	pdest and both pointers are updated.  Upon return, psource points
 *	to the terminating character in the source file name and pdest
 *	points to the next character after the last character in the file
 *	name at that location.  If a tilde cannot be converted because the
 *	user name cannot be found, psource is still advanced past the current
 *	entry, but pdest is unaffected.  At most size characters will be
 *	stored at pdest, and the size is decremented by the number of
 *	characters we actually stored.
 *---------------------------------------------------------
 */

{
    register char *ps, *pd;
    struct passwd *passwd, *getpwnam();
    char username[17], *string;
    int i, length;

    ps = *psource;
    if (*ps == '~')
    {
	/* Copy the user name into a string (at most 16 characters).
	 * If we don't have a cached entry for this name, then
	 * read the password file entry for the user and grab out
 	 * the login directory.
	 */

	pd = username;
	for (i=0; ; i++)
	{
	    *pd = *++ps;
	    if (isspace(*pd) || (*pd=='\0') || (*pd=='/') || (*pd==':'))
		break;
	    if (i < 16) pd++;
	}
	*pd = '\0';
	for (i=0;  i<NTILDE; i++)
	{
	    if (strcmp(tildename[i], username) == 0)
	    {
		string = realname[i];
		goto gotname;
	    }
	}

	/* Since we don't have a cached entry, read the password file
	 * and make a new cache entry for this one.  Note that this
	 * means discarding an old entry.
	 */

	passwd = getpwnam(username);
	string = passwd->pw_dir;
	if (passwd != NULL)
	{
	    discard++;
	    if (discard >= NTILDE) discard = 0;
	    if (tildename[discard] != NULL)
	    {
	    	free(tildename[discard]);
	    	free(realname[discard]);
	    }
	    tildename[discard] = malloc((unsigned) strlen(username)+1);
	    strcpy(tildename[discard], username);
	    realname[discard] = malloc((unsigned) strlen(string)+1);
	    strcpy(realname[discard], string);
	}
	else

	/* The entry can't be found, so skip the source entry and return */

	{
	    while ((*ps != '\0') && !isspace(*ps) && (*ps != ':')) ps++;
	    *psource = ps;
	    return FALSE;
	}
	gotname: length = strlen(string);
	if (length > *size) length = *size;
	strcpyn(*pdest, string, length);
	*size -= length;
	pd = *pdest+length;
    }
    else pd = *pdest;

    /* Copy the rest of the directory name from the source to the dest. */

    while ((*ps != '\0') && !isspace(*ps) && (*ps != ':'))
	if (*size > 0)
	{
	    *pd++ = *ps++;
	    (*size)--;
	}
	else ps++;
    *psource = ps;
    *pdest = pd;
    return TRUE;
}


int
PSetPath(string)
char *string;			/* Pointer to a string that is to become
				 * the new fle search path.  Must consist
				 * of one or more directory names separated
				 * by white space or colons.  Two adjacent
				 * colons are the same as ":.:".  Tilde
				 * notation is ok.
				 */

/*---------------------------------------------------------
 *	PSetPath sets up the current search path.
 *
 *	Results:
 *	-1 is returned if one or more of the paths contained a tilde
 *	notation that couldn't be converted.  -2 is returned if the
 *	path space was exhausted.  Otherwise a value >= 0 is returned.
 *
 *	Side Effects:
 *	The string is stored as the current path, and all entries with
 *	tilde notation are converted to non-tilde notation.  Tilde entries
 *	that cannot be converted are ignored.  Note:  only PATHSIZE total
 *	bytes of path are stored, after tilde conversion.  Excess bytes
 *	are truncated.
 *---------------------------------------------------------
 */

{
    int result, spaceleft;
    char *p;

    result = 0;
    spaceleft = PATHSIZE-1;
    p = path;

    if (*string == '\0')
    {
	path[0] = '.';
	path[1] = '\0';
	return 0;
    }
	
    while (*string != '\0')
    {
	if (*string == ':')
	    if ((*++string == ':') && (spaceleft >= 2))
	    {
		*p++ = '.';
		*p++ = ' ';
		spaceleft -= 2;
	    }
	if (spaceleft <= 0) break;
	while ((*string == ':') || isspace(*string)) string++;
	if (!PConvertTilde(&string, &p, &spaceleft)) result = -1;
	else if (spaceleft-- > 0) *p++ = ' ';
    }
    *p = '\0';
    if (spaceleft < 2) return -2;
    return result;
}


char *
PGetPath()

/*---------------------------------------------------------
 *	This routine merely returns a poiner to the current path.
 *
 *	Results:
 *	The address of the current path (with all tildes expanded).
 *
 *	Side Effects:	None.
 *---------------------------------------------------------
 */

{
    return path;
}


FILE *
POpen(file, prealname, search)
char *file;			/* Name of the file to be opened. */
char **prealname;		/* Pointer to a location that will be filled
				 * in with the address of the real name of
				 * the file that was successfully opened.
				 * If NULL, then nothing is stored.
                                 */
int search;			/* If FALSE, then the search path business
				 * doesn't happen.  A default extension
				 * will still be added.
				 */

/*---------------------------------------------------------
 *	This routine does a file lookup using the current path and
 *	supplying a default extension.
 *
 *	Results:
 *	A pointer to a FILE, or NULL if the file couldn't be found.
 *
 *	Side Effects:
 *	If the first character of the
 *	file name is "~" or "/" or if search is FALSE, then we try
 *	to look up the file with the original name, doing tilde
 *	expansion of course and returning that result.  If none of 
 *	these conditions is met, we go through the current path
 *	trying to look up the file once for each path
 *	entry by prepending the path entry to the original file name.
 *	This concatenated name is stored in a static string and made
 *	available to the caller through prealname if the open succeeds.
 *	Note: the static string will be trashed on the next call to this
 *	routine.  Also, note that no individual file name is allowed to
 *	be more than NAMESIZE characters long.  Excess characters are lost.
 *---------------------------------------------------------
 */

{
#define NAMESIZE 50
    static char realname[NAMESIZE];
    char extendedname[NAMESIZE], *p, *p2;
    int length, spaceleft, size;
    FILE *f;

    if (file == NULL) return (FILE *) NULL;
    if (file[0] == '\0') return (FILE *) NULL;

    if (prealname != NULL) *prealname = realname;

    /* Now try the original name.  If it starts with a ~ or / look 
     * it up directly.
     */

    if (file[0] == '~')
    {
	p = realname;
	length = NAMESIZE-1;
	if (!PConvertTilde(&file, &p, &length)) return NULL;
	*p = '\0';
	return fopen(realname, "r");
    }

    if ((file[0] == '/') || (search == FALSE))
    {
	strcpyn(realname, file, NAMESIZE-1);
	realname[NAMESIZE-1] = '\0';
	return fopen(realname, "r");
    }

    /* Now try going through the path. */

    p = path;
    while (*p != '\0')
    {
	spaceleft = NAMESIZE-1;
	p2 = realname;
	while (isspace(*p)) p++;
	while ((*p != '\0') && !isspace(*p))
	    if (spaceleft-- > 0) *p2++ = *p++;
	    else p++;
	if (spaceleft-- > 0) *p2++ = '/';
	if (spaceleft > 0) strcpyn(p2, file, spaceleft);
	realname[NAMESIZE-1] = '\0';
	f = fopen(realname, "r");
	if (f != NULL) return f;
    }

    /* We've tried the path and that didn't work.  As a last shot,
     * try the library area.  Only use the library for reads.
     */

    p = GLibrary;
    p2 = realname;
    size = NAMESIZE;
    if (!PConvertTilde(&p, &p2, &size)) return FALSE;
    (void) strcpyn(p2, file, size);
    realname[NAMESIZE-1] = '\0';
    return fopen(realname, "r");
}
