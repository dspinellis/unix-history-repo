/* open.c: to insulate <fcntl.h> from the rest of rc. */

#include <fcntl.h>
#include "rc.h"

/* prototype for open() follows. comment out if necessary */

/*extern int open(const char *, int,...);*/

/*
   Opens a file with the necessary flags. Assumes the following
   declaration for redirtype:

	enum redirtype {
		rFrom, rCreate, rAppend, rHeredoc, rHerestring
	};
*/

static const int mode_masks[] = {
	/* rFrom */	O_RDONLY,
	/* rCreate */	O_TRUNC | O_CREAT | O_WRONLY,
	/* rAppend */	O_APPEND | O_CREAT | O_WRONLY
};

extern int rc_open(const char *name, redirtype m) {
	if ((unsigned) m >= arraysize(mode_masks))
		panic("bad mode passed to rc_open");
	return open(name, mode_masks[m], 0666);
}
