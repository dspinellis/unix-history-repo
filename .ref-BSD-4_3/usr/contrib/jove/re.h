/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

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
