#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: perms.c,v 1.7.0.1 85/04/09 11:42:48 notes Rel $";
#endif	RCSIDENT

/* 
 *	getperms - opens and reads the permission file to
 *	determine the access rights for this user
 *
 *	Uses a struture similar to PLATO's. A list is scanned
 *	until a match is found. The access rights at that point
 *	are then awarded to the user. In the event that no
 *	match is found, no access rights are granted.
 *
 *	Original Coding:	Ray Essick	December 1981
 */
#include	<grp.h>
#include	<sys/param.h>				/* for groups */
/*
 *	Make sure that the name NGROUPS is defined.
 *	pre-4.1a probably doesn't define either.
 *	4.1a uses the NGRPS constant
 *	4.2 uses the NGROUPS constant
 *	We make sure that NGROUPS is defined and use that in the
 *	generic portion of the routine. The emulation for V7, 4.1, 4.1a
 *	and such use the NGRPS or whatever is appropriate.
 */
#if	defined(NGRPS) && ! defined(NGROUPS)
#define	NGROUPS	NGRPS					/* name change! */
#endif
#ifndef		NGROUPS
#define		NGROUPS		1			/* could be better */
#endif		NGROUPS

static char *gnameptr[NGROUPS];				/* point to names */
static int  ngroups = 0;				/* true if have gids */

getperms (io, sysflag, name)
struct io_f *io;
int     sysflag;					/* true for remote system */
char   *name;
{

    register int    i;
    FILE * acs, *fopen ();
    struct group   *gr,
                   *getgrgid ();
    char    fn[WDLEN];
    struct perm_f   entry;
    int     hisperms;					/* built up perms */
    int     given;					/* if assigned perms */

    if (sysflag == 0 && globuid == Notesuid)		/* "notes" omnipotent */
    {
	io -> access = READOK + RESPOK + WRITOK + DRCTOK;/* all */
/*
 *	should I just set it to -1 or something that turns on
 *	all the bits?  or leave it with the defined bits only?
 */
	return;
    }

    if (sysflag == 0 && ngroups == 0)
    {
	register int    i,
	                j;				/* temp loop stuff */
	int     gidset[NGROUPS];			/* hold gid list */

	ngroups = NGROUPS;				/* max allowed */
	/* 
	 * NOTE that the getgroups system call doesn't behave as 
	 * documented in the 4.2 manual.  The manual says to call it
	 * ret=getgroups(&ngroups,&gidset) where ngroups is value-result.
	 * and ret is 0 on success.  Actual implementation works as below.
	 */
	if ((ngroups = getgroups (ngroups, &gidset)) >= 0)/* worked */
	{
	    for (i = 0, j = 0; i < ngroups; i++)	/* get names */
	    {
		if ((gr = getgrgid (gidset[i])) == NULL)
		    continue;				/* bogus, skip it */
		gnameptr[j++] = strsave (gr -> gr_name);/* save it */
	    }
	    ngroups = j;				/* save count */
	}
    }
    io -> access = 0;					/* default null */
    hisperms = 0;					/* set up mask */
    given = 0;

    sprintf (fn, "%s/%s/%s", io -> basedir, io -> nf, ACCESS);
    x ((acs = fopen (fn, "r")) == NULL, "getperms: no list");
    while (fread (&entry, sizeof entry, 1, acs) == 1)
    {
	if ((sysflag != 0) && (entry.ptype != PERMSYSTEM))
	    continue;					/* looking for systems */
	if ((sysflag == 0) && (entry.ptype == PERMSYSTEM))
	    continue;					/* users != systems */

	if (strcmp (entry.name, "Other") == 0)
	{
	    if (!given)					/* he hasn't matched */
		hisperms = entry.perms;			/* give him these */
	    goto gotit;					/* and exit ... */
	}
	switch (entry.ptype)
	{
	    case PERMUSER: 
		if (strcmp (name, entry.name) == 0)
		{
		    hisperms = entry.perms;
		    goto gotit;
		}
		break;

	    case PERMGROUP: 				/* a group entry */
		for (i = 0; i < ngroups; i++)		/* check all */
		    if (strcmp (entry.name, gnameptr[i]) == 0)
		    {
			hisperms |= entry.perms;	/* OR them in */
			given++;			/* mark as such */
			break;
		    }
		break;

	    case PERMSYSTEM: 
		if (strcmp (name, entry.name) == 0)
		{
		    hisperms = entry.perms;
		    given++;
		    goto gotit;
		}
		break;

	    default: 					/* bad access list */
		x (1, "getperms: bad list");
	}
    }
gotit: 
    fclose (acs);					/* close the access file */
    io -> access = hisperms;				/* what we built */

    return;
}

/*
 *	Some compatibility routines to let us use the fancy 4.2 Bsd
 *	getgroups() system call while running 4.1, 4.1a or V7 kernels.
 *	I've undoubtedly missed some kernels.
 */


/*
 *	getgroups
 *
 *	Returns an integer and a set of groups.
 *	Emulates the 4.2 getgroups command under 4.1a Bsd
 *
 *	Stolen mostly from the 4.1a command "groups".
 *
 *	Ray Essick, January 1984
 *
 */

#if	defined (BSD41A)

getgroups (ngroups, gidset)
int     ngroups;
int    *gidset;
{
    register int    i;
    register int    maxback;				/* most to user */
    int     grps[NGRPS / (sizeof (int) * 8)];		/* NGRPS=NGROUPS */

    setgrp (0, grps);					/* get groups */
    maxback = ngroups;					/* save limit */
    ngroups = 0;					/* start empty */
    for (i = 0; i < NGRPS && ngroups < maxback; i++)	/* for each */
	if (grps[i / (sizeof (int) * 8)] & (1 << (i % (sizeof (int) * 8))))
	{
	    *gidset++ = i;				/* save the group */
	    ngroups++;					/* and count */
	}
    return (ngroups);
}
#endif	defined (BSD41A)

/*
 *	The V7 and 4.1 version of this system call. Also serves well 
 *	for the 2.8 Bsd kernels and probably for the more recent BTL
 *	kernels.
 *	This could be extended to read from /etc/groups to actually give
 *	the user all groups he is permitted.
 */

#if	defined (V7) || defined (BSD41) || defined (BSD2x) || defined (USG)
getgroups (ngroups, gidset)				/* simple V7 one */
int     ngroups;
int    *gidset;
{
    *gidset = getgid () & GIDMASK;
    ngroups = 1;
    return (1);
}
#endif	defined (V7) || defined (BSD41) || defined (BSD2x) || defined(USG)
