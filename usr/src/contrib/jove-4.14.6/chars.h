/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define CTL(c)		((c) & 037)
#define META(c)		((c) | 0200)
#define RUBOUT		'\177'
#define LF		CTL('J')
#define CR		CTL('M')
#define BS		CTL('H')
#define ESC		'\033'
