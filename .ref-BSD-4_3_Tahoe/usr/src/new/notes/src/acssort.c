#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: acssort.c,v 1.7 85/01/18 15:40:40 notes Rel $";
#endif	RCSIDENT

/*
 *	acssort
 *
 *	Routines in this file are used to sort access lists.
 *	Splintered off from access.c so that nfaccess.c can use
 *	them.
 *
 *	Also contains routines to add entries to an access list
 *	and to parse an ascii representation of an access right.
 *
 *	Ray Essick
 */


acscmp (a, b)
struct perm_f  *a,
               *b;
{
/*
 *	people before groups before systems
 *	Alphabetical within each class
 */
    if (a -> ptype < b -> ptype)
	return (-1);
    if (a -> ptype > b -> ptype)
	return 1;
    if (strcmp ("Other", a -> name) == 0)
	if (strcmp ("Other", b -> name) == 0)
	    return 0;
	else
	    return 1;					/* put "Other" last */
    if (strcmp ("Other", b -> name) == 0)
	return (-1);					/* is correct */
    return strcmp (a -> name, b -> name);
}

/*
 *	acssort
 *
 *	sort the access list
 */

acssort (alist, items)
struct perm_f   alist[];
int     items;
{
    qsort (alist, items, sizeof (struct perm_f), acscmp);
}
/*
 *	addmodes(io) struct io_f *io;
 *
 *	reads the access list and adds the modes specified in the
 *	Newmodes array.
 *	Checks for duplication and merely replaces with the new
 *	permission in those cases.
 */

addmodes (io, nmodes, Newmodes, verbose)
struct io_f *io;
int     nmodes;
struct perm_f  *Newmodes;
int     verbose;
{
    struct perm_f   alist[NPERMS];
    int     pcount;					/* items in list */
    FILE * acs;
    int     i,
            j;
    char    fn[WDLEN];					/* hold a filename */

    sprintf (fn, "%s/%s/%s", io -> basedir, io -> nf, ACCESS);
    x ((acs = fopen (fn, "r")) == NULL, "addmode: no access list");
    x ((pcount = fread (alist, sizeof (struct perm_f), NPERMS, acs)) == 0, "addmode: empty access list");
    fclose (acs);

    for (i = 0; i < nmodes; i++)			/* for each mode */
    {
	for (j = 0; j < pcount; j++)			/* look for match */
	    if (Newmodes[i].ptype == alist[j].ptype &&
		    strcmp (Newmodes[i].name, alist[j].name) == 0)
		break;					/* match */
	if (j == pcount)				/* wasn't there */
	{
	    if (pcount == NPERMS)
	    {
		if (verbose)
		    printf ("%s: access list full\n", io -> nf);
		break;
	    }
	    alist[pcount].ptype = Newmodes[i].ptype;
	    alist[pcount].perms = Newmodes[i].perms;
	    strcpy (alist[pcount].name, Newmodes[i].name);
	    pcount++;
	}
	else						/* update existing one */
	{
	    alist[j].perms = Newmodes[i].perms;
	    if (verbose)
		printf ("%s: replaced extant permission for %s\n",
			io -> nf, Newmodes[i].name);
	}
    }
/*
 *	replace the access list
 */

    acssort (alist, pcount);
    x ((acs = fopen (fn, "w")) == NULL, "addmodes: can't write access list");
    x (fwrite (alist, sizeof (struct perm_f), pcount, acs) != pcount, "addmodes: writing access");
    fclose (acs);
}

/*
 *	parsemode
 *
 *	Parse the supplied (character string) access specification
 *	into the specified perm_f structure.
 *
 *	Ray Essick
 */

parsemode (asciimode, pstuff, verbose)
char   *asciimode;
struct perm_f  *pstuff;
int     verbose;
{
    char   *p;
    char    name[WDLEN];				/* hold the name */
    char    namespec[WDLEN];				/* entire name */
    int     nametype;					/* name class */
    char    mode[WDLEN];				/* and the mode */
    char    imode = 0;					/* internalized */


    if ((p = index (asciimode, '=')) == NULL)		/* find the mode */
    {
	if (verbose)
	    printf ("No mode separator: %s\n", asciimode);
	return (1);
    }

    *p++ = '\0';					/* split out mode */
    strcpy (mode, p);					/* grab mode */
    strcpy (namespec, asciimode);			/* and name */
    *--p = '=';						/* replace marker */


    if ((p = index (namespec, ':')) == NULL)		/* implicitly user? */
    {
	strcpy (name, namespec);			/* user name */
	nametype = PERMUSER;				/* default to user */
    }
    else
    {
	*p++ = '\0';					/* break specification */
	strcpy (name, p);				/* load name */
	switch (namespec[0])				/* determine class */
	{
	    case 'u': 
	    case 'U': 
		nametype = PERMUSER;
		break;

	    case 'g': 
	    case 'G': 
		nametype = PERMGROUP;
		break;

	    case 's': 
	    case 'S': 
		nametype = PERMSYSTEM;
		break;

	    default: 
		if (verbose)
		    printf ("Invalid name class: %s\n", namespec);
		return (1);
		break;
	}

    }
/*
 *	Check that user/group are defined on our system. Don't
 *	want to be filling our tables with bogus stuff.
 */

    switch (nametype)
    {
	case PERMUSER: 
	    if (getpwnam (name) == NULL)		/* does he exist? */
	    {
		if (verbose)
		    printf ("%s: no such user\n", name);
		return (1);
	    }
	    break;

	case PERMGROUP: 
	    if (getgrnam (name) == NULL)		/* does it exist */
	    {
		if (verbose)
		    printf ("%s: no such group\n", name);
		return (1);
	    }
	    break;

	case PERMSYSTEM: 
	default: 
	    break;
    }

/*
 *	Now internalize the mode
 */

    imode = 0;						/* initially null */
    for (p = mode; *p; p++)				/* each specifier */
    {
	switch (*p)
	{
	    case 'd': 					/* director */
		imode = DRCTOK + READOK + WRITOK + RESPOK;
		break;
	    case 'r': 					/* read */
		imode |= READOK;
		break;
	    case 'w': 					/* write (and respond) */
		imode |= WRITOK + RESPOK;
		break;
	    case 'a': 					/* respond */
		imode |= RESPOK;
		break;
	    case 'n': 					/* nullify */
		imode = 0;
		break;
	    default: 
		if (verbose)
		    printf ("%c: Invalid permission mode\n", *p);
		break;
	}

    }
    pstuff -> ptype = nametype;				/* load structure */
    pstuff -> perms = imode;
    strcpy (pstuff -> name, name);
    return 0;
}
