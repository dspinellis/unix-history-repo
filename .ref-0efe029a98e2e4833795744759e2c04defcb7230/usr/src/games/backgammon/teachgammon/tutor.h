/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tutor.h	5.4 (Berkeley) %G%
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
