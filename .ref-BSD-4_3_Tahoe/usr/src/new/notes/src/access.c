#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: access.c,v 1.7 85/01/18 15:00:31 notes Rel $";
#endif	RCSIDENT

/*
 *	access - process access list editing
 *
 *	functions include:
 *	(1) display access lists
 *	(2) insert new entries
 *	(3) delete old entries
 *	(4) modify existing entries
 *
 *	Original Coding: Ray Essick	January 1982
 */
#include <pwd.h>
#include <grp.h>					/* /etc/passwd and /etc/group formats */

char   *kmap[] =					/* mapping for name types */
{
    "usr:", "grp:", "sys:"
};
char   *map[] =
{
     /* ----- */ "Null",
     /* ----r */ "Read Only",
     /* ---w- */ "(02)",
     /* ---wr */ "(03)",
     /* --d-- */ "(04)",				/* nonsense */
     /* --d-r */ "(05)",				/* nonsense */
     /* --dw- */ "(06)",				/* nonsense */
     /* --dwr */ "(07)",
     /* -a--- */ "Answer Only",				/* nonsense */
     /* -a--r */ "Read/Answer",
     /* -a-w- */ "Write only",
     /* -a-wr */ "Read/Write",
     /* -ad-- */ "(014)",				/* nonsense */
     /* -ad-r */ "(015)",				/* nonsense */
     /* -adw- */ "(016)",				/* nonsense */
     /* -adwr */ "Director/R/W"
};

struct perm_f   entry;
static int  length;					/* max on screen */

accessedit (io)
struct io_f *io;					/* notefile working with */
{
    struct passwd  *getpwnam ();
    struct group   *getgrnam ();			/* check validity of name/group */
    FILE * acs, *fopen ();				/* stream I/O */
    char    fn[WDLEN];
    struct auth_f   me;					/* for detecting suicidals */
    struct perm_f   alist[NPERMS];			/* hold the access list */

    int     items,
            base,
            i,
            which,
            changed;
    char    c;
    short   ptype;
    char    zname[NAMESZ + 1];				/* hold new user */


    sprintf (fn, "%s/%s/%s", io -> basedir, io -> nf, ACCESS);/* file name */
    x ((acs = fopen (fn, "r")) == NULL, "access: no access file");
    x ((items = fread (alist, sizeof entry, NPERMS, acs)) == 0, "access: empty file");
    fclose (acs);					/* and close the file */
    changed = 0;					/* no changes made to the list yet */
    length = nrows - 6;					/* max to show */
    base = 0;						/* which part are we displaying */
    erase ();
    plotit (alist, base, items);
    while (1)
    {
	at (-1, 1);
	printf ("Option: ");
	c = gchar ();					/* grab command */
	printf ("\b \b");				/* overwrite */
	switch (c)
	{
	    case '?': 
	    case 'h': 
		help (ACCHLP);				/* print the help page */
		goto redraw;				/* replot the screen */

	    case '!': 					/* fork a shell for him */
		gshell ();
		goto redraw;

#ifdef	K_KEY
	    case 'K': 					/* same as Q */
#endif	K_KEY
	    case 'Q': 
		return 0;				/* return to the caller */

	    case '\004': 				/* abort notefiles */
		return QUITFAST;

#ifdef	K_KEY
	    case 'k': 					/* same as q */
#endif	K_KEY
	    case 'q': 					/* update lists (if changed) and leave) */
		if (changed)
		{
		    acssort (alist, items);		/* order them */
		    x ((acs = fopen (fn, "w")) == NULL, "access: reopen");
		    x (fwrite (alist, sizeof entry, items, acs) != items, "access:update write");
		    fclose (acs);			/* and close the file */
		}
		return 0;

	    case '-': 					/* scroll display backwards */
		base -= length / 2 - 1;			/* back a half sreen */
		if (base < 0)
		    base = 0;				/* don't pass zero */
		goto redraw;

	    case '+': 					/* scroll display forwards */
		base += length / 2 - 1;			/* up half screen */
		if (base >= items - (length / 2))	/* try to keep full */
		{
		    base = items - (length - 3);	/* don't over-run */
/*
 *	Gotta subtract 3: one for OBOE and two for the "-more-" message
 *	that might be there.
 */
		    if (base < 0)
			base = 0;			/* careful */
		}
		goto redraw;

	    case 's': 					/* sort and replot the list */
		acssort (alist, items);			/* do the sort */
							/* and fall through to ... */
		break;

	    case 'r': 					/* replot the lists */
	    case '\014': 				/* everyone else uses ^L, might as well */
	redraw: 
		erase ();
		plotit (alist, base, items);
		break;					/* back to command sucker */

	    case 'i': 					/* enter a bunch of permissions */
		while (items < NPERMS)			/* not if all full */
		{
	    reget:  at (-4, 40);
		    printf ("Entry type:  \b");
		    if ((c = gchar ()) == '\n' || c == 'q' || c == 'k')
			break;				/* get out */
		    switch (c)
		    {
			case 'u': 
			    ptype = PERMUSER;
			    break;
			case 'g': 
			    ptype = PERMGROUP;
			    break;
			case 's': 
			    ptype = PERMSYSTEM;
			    break;
			default: 
			    printf ("\07  (u,g,s, q,k,<cr>)");
			    goto reget;
		    }
		    at (-3, 40);
		    printf ("Name:                \b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
		    if (gline (zname, NAMESZ) == 1)
			continue;			/* null name */
		    if (ptype == PERMUSER && strcmp ("Other", zname) != 0)
		    {
			if (getpwnam (zname) == NULL)
			{
			    at (-2, 40);
			    printf ("--No such user--");
			    continue;
			}
		    }
		    if (ptype == PERMGROUP && strcmp ("Other", zname) != 0)
		    {
			if (getgrnam (zname) == NULL)
			{
			    at (-2, 40);
			    printf ("--No such group--");
			    continue;
			}
		    }
/*
 *	make sure that it isn't already there.
 */

		    for (i = 0; i < items; i++)
			if (alist[i].ptype == ptype && strcmp (alist[i].name, zname) == 0)
			{
			    at (-2, 40);
			    printf ("%s entry exists", zname);
			    goto reget;
			}

		    alist[items].perms = DFLTPERMS;	/* give him default */
		    getmode (&alist[items].perms);
		    alist[items].ptype = ptype;
		    strmove (zname, alist[items].name);	/* copy things over */
		    items++;
		    changed = 1;			/* and set flags */
		    acssort (alist, items);
		    erase ();				/* clean screen */
		    plotit (alist, base, items);	/* show new list */
		}
		endpwent ();
		endgrent ();				/* close passwd and group files */
		break;


	    case 'd': 					/* delete some permissions */
		at (-1, 1);
		printf ("Delete entry #: ");
		if ((c = gchar ()) == '\n')
		    break;				/* null */
		printf ("\b");
		which = getnum (c);			/* grab number */
		if (which <= 0)
		    break;				/* don't update */
		if (which > items || c < '0' || c > '9')
		{
		    printf ("Bad entry");
		    break;
		}
		which--;				/* adjust to zero base */
		getname (&me, 0);			/* grab my name */
		if ((alist[which].ptype = PERMUSER) && strcmp (me.aname, alist[which].name) == 0)
		{
		    printf (" Can't Delete self");
		    break;
		}
		items--;				/* decrement count  and */
		for (i = which; i < items; i++)		/* tamp down list */
		{
		    alist[i].ptype = alist[i + 1].ptype;
		    strmove (alist[i + 1].name, alist[i].name);
		    alist[i].perms = alist[i + 1].perms;
		}
		changed = 1;				/* mark it as changed */
		goto redraw;				/* show updated screen */

	    case 'm': 					/* modify someones permission */
		at (-1, 1);				/* grab which slot */
		printf ("Modify entry #: ");
		if ((c = gchar ()) == '\n')
		    break;				/* null entry */
		printf ("\b");
		which = getnum (c);
		if (which <= 0)
		    break;
		if (which > items || c < '0' || c > '9')/* check its validity */
		{
		    at (-2, 1);
		    printf ("Bad entry");
		    break;
		}
		which--;				/* adjust to zero base */
		getmode (&alist[which].perms);
		changed = 1;				/* set changed flag */
		goto redraw;				/* repaint screen */

	    default: 					/* wrong key dummy */
		printf ("\07");
		break;
	}
    }
}

/*
 *	Grab a set of permissions
 */
getmode (zmode)
short  *zmode;
{							/* grab a mode from the tty */
    char    c;
    short   mode;					/* resulting mode */

    mode = *zmode;					/* set to what passed in */

    while (1)
    {
	at (-2, 40);
	printf ("%*s", 25, " ");
	at (-2, 40);
	printf (" Mode: %s", map[mode]);
	at (-1, 40);
	printf ("Mods: ");
	c = gchar ();
	switch (c)
	{
	    case 'a': 					/* toggle answer */
		if (mode & WRITOK)
		    break;				/* write supersedese */
		if (mode & RESPOK)
		    mode &= NOT RESPOK;
		else
		    mode |= RESPOK;
		break;

	    case 'r': 					/* toggle read */
		if (mode & DRCTOK)
		    break;				/* director supersedes */
		if (mode & READOK)
		    mode &= NOT READOK;
		else
		    mode |= READOK;
		break;

	    case 'w': 					/* toggle write */
		if (mode & DRCTOK)
		    break;				/* director supersedes */
		if (mode & WRITOK)
		    mode &= NOT WRITOK;
		else
		    mode |= WRITOK + RESPOK;
		break;

	    case 'd': 					/* toggle director */
		if (mode & DRCTOK)
		    mode &= NOT DRCTOK;
		else
		    mode |= DRCTOK + READOK + WRITOK + RESPOK;
		break;

	    case 'n': 					/* set to null */
		mode = 0;
		break;

	    case '\n': 					/* acceptable to him */
#ifdef	K_KEY
	    case 'k': 					/* same as q */
	    case 'K': 
#endif	K_KEY
	    case 'q': 
	    case 'Q': 
		return (*zmode = mode);			/* do both ways */

	    default: 
		printf ("\07  (d,r,w,a,n,q,k<cr>)");
		break;
	}
    }
}
plotit (alist, base, items)				/* plot the list */
struct perm_f   alist[];
int     base;
int     items;
{
    register int    atrow,
                    atcol,
                    i;

    atrow = 1;
    atcol = 1;
    if (base != 0)
    {
	at (atrow++, atcol);
	printf (" -- More -- ");
    }
    for (i = base; i < items && atrow < length; i++)
    {
	at (atrow++, atcol);
	printf ("%2d %s%-*s %s", i + 1, kmap[alist[i].ptype], NAMESZ,
		alist[i].name, map[alist[i].perms]);
    }
    if (i < items)					/* tell him more */
    {
	at (atrow++, atcol);
	printf (" -- More -- ");
    }
}
