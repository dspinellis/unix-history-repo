.\" Copyright (c) 1980 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.roff%
.\"
.\"	@(#)win_st.c	6.3 (Berkeley) %G%
.\"
# define	WINDOW	struct _win_st

struct _win_st {
	short		_cury, _curx;
	short		_maxy, _maxx;
	short		_begy, _begx;
	short		_flags;
	short		_ch_off;
	bool		_clear;
	bool		_leave;
	bool		_scroll;
	char		**_y;
	short		*_firstch;
	short		*_lastch;
	struct _win_st	*_nextp, *_orig;
};

# define	_ENDLINE	001
# define	_FULLWIN	002
# define	_SCROLLWIN	004
# define	_FLUSH		010
# define	_FULLLINE	020
# define	_IDLINE		040
# define	_STANDOUT	0200
# define	_NOCHANGE	-1
