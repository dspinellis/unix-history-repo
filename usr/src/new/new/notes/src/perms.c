static char *sccsid = "@(#)perms.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
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
#include <grp.h>
static char gname[NAMESZ];
static int  gnamegot = 0;				/* whether grabbed group id */

getperms (io, sysflag, name)
struct io_f *io;
/* sysflag true if we want remote system */
char   *name;
{

    FILE * acs, *fopen ();
    struct group   *gr,
                   *getgrgid ();
    char    fn[WDLEN];
    struct perm_f   entry;

    if (sysflag == 0 && gnamegot == 0) {
	if ((gr = getgrgid (getgid () & GIDMASK)) == NULL) {
	    strmove ("Other", gname);
	} else {
	    strmove(gr->gr_name, gname);
	    endgrent();				/* close group file */
	}
	gnamegot = 1;				/* mark it as gotten */
    }
    io->access = 0;				/* null default */

    sprintf (fn, "%s/%s/%s", MSTDIR, io->nf, ACCESS);

    x ((acs = fopen (fn, "r")) == NULL, "getperms: no list");

    while (fread (&entry, sizeof entry, 1, acs) == 1) {

	if ((sysflag) && (entry.ptype != PSYSTEM)) {
	    continue;				/* not a system entry */
	}

	if (strcmp (entry.name, "Other") == 0) {
	    goto gotit;
	}

	switch (entry.ptype) {

	    case PUSER: 

		if (strcmp (name, entry.name) == 0) {
		    goto gotit;
		} else {
		    break;
		}

	    case PGROUP: 				/* a groupd entry */

		if (strcmp (gname, entry.name) == 0) {
		    goto gotit;
		} else {
		    break;
		}

	    case PSYSTEM: 

		if (strcmp (name, entry.name) == 0) {
		    goto gotit;
		} else {
		    break;
		}

	    default: 			/* bad access list */

		x (1, "getperms: bad list");
	}
    }
    entry.perms = 0;			/* turn him off since was null */
gotit: 
    fclose (acs);			/* close the access file */
    io->access = entry.perms;

    return;
}
