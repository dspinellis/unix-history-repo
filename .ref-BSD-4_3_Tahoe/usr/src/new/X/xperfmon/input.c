#ifndef lint
static char *rcsid_input_c = "$Header: input.c,v 10.2 86/02/01 16:02:55 tony Rel $";
#endif	lint

/* input.c */

#include <X/Xlib.h>

extern KeyMapEntry StdMap[];

mapkey (keycode)
register int keycode;
{
	short c;

	c = StdMap[keycode & ValueMask][KeyState(keycode)];
	if ((keycode & ShiftLockMask) && c >= 'a' && c <= 'z')
		c += 'A' - 'a';
	return(c);
}
