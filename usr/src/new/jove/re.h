/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define NALTS	10	/* number of alternate search strings */

extern char	searchstr[128],
		compbuf[128],		/* global default compbuf */
		rep_search[128],	/* replace search string */
		rep_str[128],		/* contains replacement string */
		*cur_compb,		/* usually points at compbuf */
		REbuf[LBSIZE],		/* points at line we're scanning */
		*alternates[NALTS];

extern int	REdirection,
		REeom,
		REbom,
		REalt_num;
