/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tutor.h	5.1 (Berkeley) 5/29/85
 */

struct situatn  {
	int	brd[26];
	int	roll1;
	int	roll2;
	int	mp[4];
	int	mg[4];
	int	new1;
	int	new2;
	char	*(*com[8]);
};
