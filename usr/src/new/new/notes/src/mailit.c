static char *sccsid = "@(#)mailit.c	1.2 2/2/83";

#include <sys/types.h>
#include <sys/stat.h>
#include "parms.h"
#include "structs.h"
/*
 *	mailit
 *
 *	takes the text record specified, along with the notefile name,
 *	the author name, (other things added later), and builds a 
 *	temp file. The user is prompted for a list of the people to
 *	send the file to. After an edit session, the mail is sent.
 *
 *	Returns:	-1 if no letter sent
 *			 0 if letter sent (or if it thinks so)
 *
 *	Original author: Ray Essick	June 1981.
 *	modified:	Ray Essick	December 1981.
 *	modified again:	Thanks to Malcolm Slaney of Purdue EE dept.
 *		added the SUPERMAILER processing. May 25, 1982
 *
 */

mailit (io, where, author, unique, date, nfname, toauth, wtext)
struct io_f *io;
struct daddr_f *where;
struct auth_f  *author;
struct id_f *unique;
struct when_f  *date;
char   *nfname;
/* toauth - true if mail to author of note */
/* wtext - true is mail with text of note */
{
    char    buf[CMDLEN];
    char    whoto[WDLEN + 1];			/* destination */
    char    fn[WDLEN];				/* hold scratch file name */
    int     f;
    FILE * txtfile;
    extern char *mymailer, *myeditor;

    if (toauth) {
	if (strcmp(author->aname, "Anonymous") == 0) {
	    warn("Can't send to Anonymous");
	    return(-1);
	}
    }

    if (toauth) {
	if (strcmp(SYSTEM, unique->sys) != 0) {
	    sprintf(whoto, "%s!%s", unique->sys, author->aname);
	} else {
	    sprintf(whoto, "%s", author->aname);
	}
    } else {
	prompt("Send to whom? ");
	if (gline(whoto, WDLEN) == 1)
	    return(-1);					/* no letter sent */

    }

    if (wtext) {				/* add text if specified */
	sprintf(fn, "/tmp/nfm%d", getpid ());
	x ((txtfile = fopen (fn, "w")) == NULL, "mailit: creat tmp");
	x (chmod(fn, 0666) < 0, "mailit: chmod tmp");
	preptxt(io, txtfile, nfname, author, unique, date, where);
	fclose(txtfile);
#ifndef BERKELEY
	/* don't need these 9 lines if our mailer can read input files */
	putchar('\n');
	f = dounix(1, 1, myeditor, fn, 0, 0, 0);
	if (f != 0) {
	    wfchar();
	    return(0);
	}
#endif BERKELEY

#ifdef BERKELEY
	sprintf(buf, "%s %s -F %s", ROUTER, whoto, fn);
#else
	sprintf(buf, "%s %s <%s", mymailer, whoto, fn);
#endif BERKELEY
    }

    prompt("mailing to %s", whoto);
    putchar('\n');
    fflush(stdout);
    if (wtext) {
	f = dounix(1, 1, "/bin/sh", "-c", buf, 0, 0);     /* do it */
    } else {
#ifdef BERKELEY
	f = dounix(1, 1, ROUTER, whoto, 0, 0, 0);     /* do it */
#else
	f = dounix(1, 1, mymailer, whoto, 0, 0, 0);     /* do it */
#endif BERKELEY
    }

#ifndef	SUPERMAILER				/* super saves it for us */
    if (f != 0)	{				/* everything work ok? */
     						/* nope, save the letter */
	printf("Well, couldn't mail that so I left it in %s\n", fn);
    } else
#endif SUPERMAILER			/* dumb mailer save stuff */
	unlink(fn);			/* don't worry if didn't get it */
    if (f != 0)
    	wfchar();
    return(0);
}
