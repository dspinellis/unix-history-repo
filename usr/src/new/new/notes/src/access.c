static char *sccsid = "@(#)access.c	1.2 2/1/83";

#include "parms.h"
#include "structs.h"
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
#include <grp.h>			/* /etc/passwd and /etc/group formats */

char   *kmap[] =			/* mapping for name types */
{
    "usr:", "grp:", "sys:"
};
char   *map[] =
{
     /* ---- */ "Null",
     /* ---r */ "Read Only",
     /* --w- */ "(02)",
     /* --wr */ "(03)",
     /* -d-- */ "(04)",					/* nonsense */
     /* -d-r */ "(05)",					/* nonsense */
     /* -dw- */ "(06)",					/* nonsense */
     /* -dwr */ "(07)",
     /* a--- */ "Answer Only",				/* nonsense */
     /* a--r */ "Read/Answer",
     /* a-w- */ "Write only",
     /* a-wr */ "Read/Write",
     /* ad-- */ "(014)",				/* nonsense */
     /* ad-r */ "(015)",				/* nonsense */
     /* adw- */ "(016)",				/* nonsense */
     /* adwr */ "Director/R/W"
};

struct perm_f   entry;
struct perm_f   alist[NPERMS];		/* hold the access list */
access (io)
struct io_f *io;			/* notefile working with */
{
    struct passwd  *getpwnam ();
    struct group   *getgrnam ();	/* check validity of name/group */
    FILE * acs, *fopen ();		/* stream I/O */
    char    fn[WDLEN];
    struct auth_f   me;			/* for detecting suicidals */

    int     items,
            base,
            i,
            which,
            changed;
    char    c;
    short ptype;
    char    zname[NAMESZ + 1];		/* hold new user */


    sprintf(fn, "%s/%s/%s", MSTDIR, io->nf, ACCESS); /* file name */
    x ((acs = fopen(fn, "r")) == NULL, "access: no access file");
    x ((items = fread(alist, sizeof entry, NPERMS, acs)) == 0, "access: empty file");
    fclose(acs);			/* and close the file */
    changed = 0;			/* no changes made to the list yet */
    base = 0;				/* which part are we displaying */
    erase();
    plotit(base, items);
    while(1) {
	cmdprompt();
	c = gchar();					/* grab command */
	switch(c) {
	    case '?': 
	    case 'h': 
		help(ACCHLP);			/* print the help page */
		goto redraw;			/* redraw the screen */

	    case '!': 				/* fork a shell for him */
		gshell();
		goto redraw;

	    case 'K':
	    case 'Q': 
		return(0);			/* return to the caller */

	    case '\004': 			/* abort notefiles */
		return(QUITFAST);

	    case 'k':
	    case 'q': 		/* update lists (if changed) and leave) */
		if (changed) {
		    acssort(items);			/* order them */
		    ignsigs++;
		    x ((acs = fopen(fn, "w")) == NULL, "access: reopen");
		    x (fwrite(alist, sizeof entry, items, acs) != items,
			"access:update write");
		    fclose(acs);		/* and close the file */
		    ignsigs--;
		}
		return(0);

	    case '-': 				/* scroll display backwards */
		base -= (nrows - 6);		/* back a half sreen */
		if (base < 0) {
		    base = 0;			/* don't pass zero */
		}
		goto redraw;

	    case '+': 				/* scroll display forwards */
		base += (nrows - 6);		/* up half screen */
		if (base >= items) {
		    base = items - (nrows - 6);		/* don't over-run */
		}
		goto redraw;

	    case 's': 				/* sort and redraw the list */
		acssort (items);		/* do the sort */
						/* and fall through to ... */
		break;

	    case 'r': 		/* redraw the lists */
	    case '\014': 	/* everyone else uses ^L, might as well */
	redraw: 
		erase();
		plotit(base, items);
		break;			/* back to command sucker */

	    case 'i': 			/* enter a bunch of permissions */
		while (items < NPERMS) {		/* not if all full */
	    reget:  at(-4, 40);
		    putstr("Entry type:  \b");
		    c = gchar();
		    if (c == '\n' || c == '\r' || c == 'q')
			break;				/* get out */
		    switch (c) {
			case 'u': 
			    ptype = PUSER;
			    break;

			case 'g': 
			    ptype = PGROUP;
			    break;
			     
			case 's': 
			    ptype = PSYSTEM;
			    break;

			default: 
			    putstr("\07  (u,g,s,q,<cr>)");
			    goto reget;
		    }
		    putch(c);
		    at(-3, 40);
		    putstr("Name: ");
		    clear_eol();
		    if (gline(zname, NAMESZ) == 1)
			continue;			/* null name */
		    if (ptype == PUSER && strcmp ("Other", zname) != 0)
		    {
			if (getpwnam (zname) == NULL)
			{
			    at (-2, 40);
			    putstr("--No such user-- ");
			    continue;
			}
		    }
		    if (ptype == PGROUP && strcmp ("Other", zname) != 0)
		    {
			if (getgrnam (zname) == NULL)
			{
			    at (-2, 40);
			    putstr("--No such group--");
			    continue;
			}
		    }

		    alist[items].perms = DFLTPERMS;     /* give him default */
		    getmode(&alist[items].perms);
		    alist[items].ptype = ptype;
		    strmove(zname, alist[items].name); /* copy things over */
		    items++;
		    changed = 1;			/* and set flags */
		    acssort(items);
		    erase();				/* clean screen */
		    plotit(base, items);		/* show new list */
		}
		endpwent();
		endgrent();				/* close passwd and group files */
		break;


	    case 'd': 					/* delete some permissions */
		prompt("Delete entry #: ");
		if ((c = gchar()) == '\n' || c == '\r')
		    break;				/* null */
		which = getnum(c);			/* grab number */
		if (which <= 0)
		    break;				/* don't update */
		if (which > items || c < '0' || c > '9')
		{
		    warn("Bad entry");
		    break;
		}
		which--;				/* adjust to zero base */
		getname(&me, 0);			/* grab my name */
		if ((alist[which].ptype = PUSER) && strcmp (me.aname, alist[which].name) == 0)
		{
		    warn("Can't Delete self");
		    break;
		}
		items--;			/* decrement count  and */
		for (i = which; i < items; i++) {	/* tamp down list */
		    alist[i].ptype = alist[i + 1].ptype;
		    strmove(alist[i + 1].name, alist[i].name);
		    alist[i].perms = alist[i + 1].perms;
		}
		changed = 1;			/* mark it as changed */
		goto redraw;			/* show updated screen */

	    case 'm': 				/* modify someones permission */
		prompt("Modify entry #: ");
		if ((c = gchar()) == '\n' || c == '\r') {
		    break;				/* null entry */
		}
		which = getnum(c);
		if (which <= 0) {
		    break;
		}
		/* check its validity */
		if (which > items || c < '0' || c > '9') {
		    warn("Bad entry");
		    break;
		}
		which--;			/* adjust to zero base */
		getmode(&alist[which].perms);
		changed = 1;			/* set changed flag */
		goto redraw;			/* repaint screen */

	    default: 				/* wrong key dummy */
		putch('\07');
		break;
	}
    }
}

acscmp(a, b)
struct perm_f  *a,
               *b;
{
		/* people before groups and systems */
    if (a->ptype < b->ptype) {
	return(-1);
    }
    if (a->ptype > b->ptype) {
	return(1);
    }
    if (strcmp ("Other", a->name) == 0) {
	if (strcmp ("Other", b->name) == 0) {
	    return(0);
	} else {
	    return(1);					/* put "Other" last */
	}
    }
    if (strcmp ("Other", b->name) == 0) {
	return - 1;					/* is correct */
    }
    return(strcmp(a->name, b->name));
}

acssort(items)					/* sort the access list */
{
    qsort(alist, items, sizeof entry, acscmp);
}

getmode(zmode)
short *zmode;
{						/* grab a mode from the tty */
    char    c;
    short mode;					/* resulting mode */
    char buf[80];

    mode = *zmode;				/* set to what passed in */

    while (1) {
	at(-2, 40);
	sprintf(buf, "Mode: %s", map[mode]);
	putstr(buf);
	clear_eol();
	at(-1, 40);
	putstr("Mods: ");
again:
	c = gchar();
	switch(c) {
	    case 'a': 				/* toggle answer */
		if (mode & WRITOK) {
		    break;			/* write supersedese */
		}
		if (mode & RESPOK) {
		    mode &= NOT RESPOK;
		} else {
		    mode |= RESPOK;
		}
		break;

	    case 'r': 				/* toggle read */
		if (mode & DRCTOK) {
		    break;			/* director supersedes */
		}
		if (mode & READOK) {
		    mode &= NOT READOK;
		} else {
		    mode |= READOK;
		}
		break;

	    case 'w': 				/* toggle write */
		if (mode & DRCTOK) {
		    break;			/* director supersedes */
		}
		if (mode & WRITOK) {
		    mode &= NOT WRITOK;
		} else {
		    mode |= WRITOK + RESPOK;
		}
		break;

	    case 'd': 				/* toggle director */
		if (mode & DRCTOK) {
		    mode &= NOT DRCTOK;
		} else {
		    mode |= DRCTOK + READOK + WRITOK + RESPOK;
		}
		break;

	    case 'n': 				/* set to null */
		mode = 0;
		break;

	    case '\n':				/* acceptable to him, return */
	    case 'q': 
		return(*zmode = mode);		/* do both ways */

	    default: 
		putstr("\07  (d,r,w,a,n,q,<cr>)");
		goto again;
	}
	putch(c);
    }
}

plotit (base, items)					/* plot the list */
{
    register int    atrow,
                    length,
                    atcol,
                    i;

    atrow = 1;
    atcol = 1;
    length = nrows - 6;					/* maximum in a col */
    for (i = base; i < items && i - base < 2 * length; i++) {
	at(atrow++, atcol);
	printf("%2d %s%-*s %s", i + 1, kmap[alist[i].ptype], NAMESZ,
		alist[i].name, map[alist[i].perms]);
	if (atrow > length) {
	    atrow = 1;
	    atcol += 40;
	}
    }
}
