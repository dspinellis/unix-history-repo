/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

# define	WINDOW	struct _win_st

struct _win_st {
	short	_cury, _curx;
	short	_maxy, _maxx;
	short	_begy, _begx;
	short	_flags;
	bool	_clear;
	bool	_leave;
	bool	_scroll;
	char	**_y;
	short	*_firstch;
	short	*_lastch;
};

# define	_SUBWIN		01
# define	_ENDLINE	02
# define	_FULLWIN	04
# define	_SCROLLWIN	010
# define	_STANDOUT	0200
