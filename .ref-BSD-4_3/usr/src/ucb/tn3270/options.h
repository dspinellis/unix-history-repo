/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

/* the various options that run our life.  Very few of these are implemented
	as yet.
 */

int	OptHome;		/* where home should send us */

int	OptLeftMargin;		/* where new line should send us */

char	OptColTabs[80];		/* local tab stops */

int	OptAPLmode;

int	OptNullProcessing;	/* improved null processing */

int	OptZonesMode;		/* zones mode off */

int	OptEnterNL;		/* regular enter/new line keys */

int	OptColFieldTab;		/* regular column/field tab keys */

int	OptPacing;		/* do pacing */

int	OptAlphaInNumeric;	/* allow alpha in numeric fields */

int	OptHome;

int	OptLeftMargin;

int	OptWordWrap;
