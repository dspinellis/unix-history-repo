#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984	*/

/* tabs.c */

#ifndef lint
static char *rcsid_tabs_c = "$Header: tabs.c,v 10.7 86/02/01 16:07:10 tony Rel $";
#endif	lint

#include <X/Xlib.h>
#include "ptyx.h"
/*
 * This file presumes 32bits/word.  This is somewhat of a crock, and should
 * be fixed sometime.
 */

/*
 * places tabstops at only every 8 columns
 */
TabReset(tabs)
Tabs	tabs;
{
	register int i;

	for (i=0; i<TAB_ARRAY_SIZE; ++i)
		tabs[i] = 0;

	for (i=0; i<MAX_TABS; i+=8)
		TabSet(tabs, i);
}	


/*
 * places a tabstop at col
 */
TabSet(tabs, col)
Tabs	tabs;
{
	tabs[col >> 5] |= (1 << (col & 31));
}

/*
 * clears a tabstop at col
 */
TabClear(tabs, col)
Tabs	tabs;
{
	tabs[col >> 5] &= ~(1 << (col & 31));
}

/*
 * returns the column of the next tabstop
 * (or MAX_TABS - 1 if there are no more).
 * A tabstop at col is ignored.
 */
TabNext (tabs, col)
Tabs	tabs;
{
	for (++col; col<MAX_TABS; ++col)
		if (tabs[col >> 5] & (1 << (col & 31)))
			return (col);

	return (MAX_TABS - 1);
}

/*
 * clears all tabs
 */
TabZonk (tabs)
Tabs	tabs;
{
	register int i;

	for (i=0; i<TAB_ARRAY_SIZE; ++i)
		tabs[i] = 0;
}
