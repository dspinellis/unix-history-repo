#ifdef	RCSIDENT
static char rcsid[] = "$Header: talkto.c,v 1.7.0.1 85/03/18 20:56:55 notes Rel $";
#endif	RCSIDENT

/*
 *	talkto(auth, uniq) struct auth_f *auth; struct id_d *uniq;
 *	see if the author is a local user (by checking the sys field of
 *	the note to see where it was written) and then grab his name from
 *	the auth structure. If he is not "Anonymous", and is local then
 *	we do a 'write name' command to talk with him.
 *
 *	Ray Essick	December 1981
 */
#include "parms.h"
#include "structs.h"

static char *command = NULL;

talkto (auth)
struct auth_f  *auth;
{
    char    cmdline[CMDLEN];				/* build the command in here */

    if (strcmp ("Anonymous", auth -> aname) == 0)
	return;
/*
 *	with 4.2 and later -- we might want to let this go on past
 *	since the talk(1) program works across machine boundaries.
 */
    if (strcmp (Authsystem, auth -> asystem) != 0)
	return;						/* hard to talk to other machine */
    if (command == NULL && (command = getenv ("WRITE")) == NULL)
	command = WRITE;				/* assign default */
    sprintf (cmdline, "%s %s", command, auth -> aname);
    printf ("%s\n", cmdline);				/* let him know what doing */

#ifndef	FASTFORK
    dounix (cmdline, 1, 1);				/* run the command */
#else
    dounix (1, 1, command, auth -> aname, 0, 0, 0);
#endif

    printf ("--Hit any key to continue--");
    gchar ();						/* grab character and drop it */
}
