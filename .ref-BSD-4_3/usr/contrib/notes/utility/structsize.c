#include "../src/parms.h"
#include "../src/structs.h"

/*
 *	structsize
 *
 *	Tells you the size of some of the structures in the
 *	data base.
 *
 *	This is useful when installing new versions of the
 *	notesfile code.  I have recently (Sept 6, '83) played
 *	with one of the structures and it resulted in a 
 *	non-obvious (but quickly figured out) change in the
 *	size of the data structures.  This broke extant notesfiles.
 *
 *	If things don't fly when you first use new code, I would
 *	suggest compiling and running this file with both the
 *	old and new structure files (it will work with both)
 *	and see what changed.  Then play with the filler space
 *	that I left in the structure definitions to make things
 *	work out.
 *
 *	Sorry that things like this have to happen. *sigh*
 *
 *	-- Ray Essick	September 6, 1983
 *
 * $Header: /mntb/3/srg/notes/work/utility/RCS/structsize.c,v 1.6 84/03/07 19:04:42 notes Exp $
 *
 */

main ()
{

    printf ("size of descr_f %d\n", sizeof (struct descr_f));
    printf ("size of note_f %d\n", sizeof (struct note_f));
    printf ("size of resp_f %d\n", sizeof (struct resp_f));
    exit (0);
}
