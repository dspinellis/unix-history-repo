#include	<stdio.h>
#include	"newsgate.h"

#ifdef	RCSIDENT
static char *rcsid = "$Header: newspath.c,v 1.7 85/01/18 15:40:17 notes Rel $";
#endif	RCSIDENT


/*
 *	char *getpath(dest) char *dest;
 *
 *	look in the routing tables for a path from here to that
 *	specified host.  Return the path.
 *	Return NULL if there is no such path.
 *
 *	The returned path is of the form:
 *		a!b!c!d!dest!		(note trailing !)
 *
 *	The path is saved in a static buffer so you have to save 
 *	it or it is destroyed in the next call.
 *
 *	A re-write of what Jeff Donnelly did a while back.
 *	Ray Essick
 *
 *	The routing table contains lines of the form:
 *	site<space>path
 *		site = destination site
 *		path = a!b!c!d!site!	(not trailing !)
 *
 *	Must be in alphabetical order since the search gives up
 *	after finding a site "after" the one we want!
 */

FILE * fopen ();
extern char *fgets ();

/*
 *	This entire routine is only called when EXPANDPATH is defined
 *	so we surround it with an ifdef to keep down the binary sizes
 *	when EXPANDPATH is not defined.
 */
#ifdef	EXPANDPATH
char   *getpath (dest)
char   *dest;
{

    static char line[BUFSIZ];
    FILE * mapfile;
    register char  *p,
                   *path;


    if ((mapfile = fopen (PATHMAP, "r")) == NULL)
	return (NULL);					/* no file */

    while (fgets (line, BUFSIZ, mapfile) != NULL)
    {
	p = line;
	path = NULL;
	do
	{
	    switch (*p)
	    {
		case ' ': 
		case '\t': 				/* zap and mark path */
		    if (path == NULL)			/* only once */
		    {
			*p++ = '\0';
			path = p;
		    }
		    else
			p++;				/* gotta move over it */
		    break;

		case '\n': 
		    *p = '\0';				/* end of it all */
		    break;

		default: 
		    p++;
		    break;
	    }
	}
	while (*p);					/* terminates after newline */
	if (strcmp (line, dest) == 0)			/* matches */
	{
	    break;					/* jump and return */
	}

	if (strcmp (line, dest) > 0)			/* past it */
	{
	    path = NULL;
	    break;
	}
    }
    fclose (mapfile);					/* don't litter */
    return (path);					/* and our answer */
}
#endif	EXPANDPATH
