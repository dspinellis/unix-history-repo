static char *sccsid = "@(#)gname.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"

/* 
 * 	get the user id (and his name from the password file)
 *	the easy way - with system calls.
 */
getname (who_me, anon)			/* anon=true for anonymous */
struct auth_f  *who_me;
{
    static char name[PASSWDLEN];	/* must contain the /etc/passwd entry */
    register    count;
    register char  *s,
                   *d;
    static int gotname = 0;		/* whether we have done a getpw */
    static int gotstat = 0;		/* status getpw returned */

    if ((gotname == 0) && (anon == 0)) {/* grab name if we will require it */
	gotstat = getpw(globuid, name);	/* grab it */
	gotname = 1;			/* set flag saying we have it */
    }
    if (gotstat || anon) {
	s = "Anonymous:";
	who_me->aid = ANONUID;
    } else {
	s = name;
	who_me->aid = globuid;
    }
    d = who_me->aname;
    count = NAMESZ;
    while (((*d++ = *s++) != ':') && --count);
    *--d = '\0';
}
