/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#define NALTS		16	/* number of alternate search strings */
#define COMPSIZE	256

/* kinds of regular expression compiles */
#define NORM	0	/* nothing special */
#define OKAY_RE	1	/* allow regular expressions */
#define IN_CB	2	/* in curly brace; implies OKAY_RE */

struct RE_block {
	char	r_compbuf[COMPSIZE],
		*r_alternates[NALTS],
		r_lbuf[LBSIZE];
	int	r_nparens,
		r_firstc,
		r_anchored;
};

extern char	rep_search[128],	/* replace search string */
		rep_str[128];		/* contains replacement string */

extern int	REbom,		/* beginning and end columns of match */
		REeom,
		REdelta;	/* increase in line length due to last re_dosub */

extern bool	okay_wrap;	/* Do a wrap search ... not when we're
				   parsing errors ... */
extern char
	*getsearch proto((void));

extern bool
	re_lindex proto((struct line *line, int offset, int dir,
		struct RE_block *re_blk, int lbuf_okay, int crater)),
	LookingAt proto((char *pattern,char *buf,int offset)),
	look_at proto((char *expr));

extern Bufpos
	*docompiled proto((int dir, struct RE_block *re_blk)),
	*dosearch proto((char *pattern, int dir, bool re));

extern void
	REcompile proto((char *pattern, bool re, struct RE_block *re_blk)),
	find_tag proto((char *tag,int localp)),
	putmatch proto((int which,char *buf,size_t size)),
	re_dosub proto((struct RE_block *re_blk, char *tobuf, int delp)),
	setsearch proto((char *str)),
	RErecur proto((void));
