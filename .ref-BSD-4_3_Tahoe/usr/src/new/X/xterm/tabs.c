/*
 *	$Source: /u1/X/xterm/RCS/tabs.c,v $
 *	$Header: tabs.c,v 10.100 86/12/01 14:45:38 jg Rel $
 */

#ifndef lint
static char *rcsid_tabs_c = "$Header: tabs.c,v 10.100 86/12/01 14:45:38 jg Rel $";
#endif	lint

#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1984	*/

/* tabs.c */

#ifndef lint
/* @(#)tabs.c       X10/6.6B 12/26/86 */
#endif	lint

#include <X/Xlib.h>
#include "scrollbar.h"
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
	extern Terminal term;
	register Screen *screen = &term.screen;

	if(screen->curses && screen->do_wrap && (term.flags & WRAPAROUND)) {
		Index(screen, 1);
		col = screen->cur_col = screen->do_wrap = 0;
	}
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
