static char *sccsid = "@(#)talkto.c	1.1\t1/23/83";

#include <sys/types.h>
#include <sys/stat.h>
#include "parms.h"
#include "structs.h"
/*
 *	talkto(auth, uniq) struct auth_f *auth; struct id_d *uniq;
 *	see if the author is a local user (by checking the sys field of
 *	the note to see where it was written) and then grab his name from
 *	the auth structure. If he is not "Anonymous", and is local then
 *	we do a 'write name' command to talk with him.
 *
 *	Ray Essick	December 1981
 */

talkto (auth, uniq)
struct auth_f  *auth;
struct id_f *uniq;
{
    extern char *mywrite;
    int ret;

    if (strcmp("Anonymous", auth->aname) == 0) {
	warn("Anonymous author");
	return(0);
    }

    if (strcmp(SYSTEM, uniq->sys) != 0) {
	warn("Author on remote machine");
	return(0);
    }

    prompt("%s %s", mywrite, auth->aname);	/* let him know what doing */
    putchar('\n');
    fflush(stdout);

    ret = dounix(1, 1, mywrite, auth->aname, 0, 0, 0);
    if (ret != 0)
    	wfchar();
    return(1);
}
