/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)shield.c	5.4 (Berkeley) 6/18/88";
#endif /* not lint */

# include	"trek.h"
# include	"getpar.h"

/*
**  SHIELD AND CLOAKING DEVICE CONTROL
**
**	'f' is one for auto shield up (in case of Condition RED),
**	zero for shield control, and negative one for cloaking
**	device control.
**
**	Called with an 'up' or 'down' on the same line, it puts
**	the shields/cloak into the specified mode.  Otherwise it
**	reports to the user the current mode, and asks if she wishes
**	to change.
**
**	This is not a free move.  Hits that occur as a result of
**	this move appear as though the shields are half up/down,
**	so you get partial hits.
*/

struct cvntab Udtab[] =
{
	"u",		"p",			(int (*)())1,		0,
	"d",		"own",			0,		0,
	0
};

shield(f)
int	f;
{
	register int		i;
	char			c;
	struct cvntab		*r;
	char			s[100];
	char			*device, *dev2, *dev3;
	int			ind;
	char			*stat;

	if (f > 0 && (Ship.shldup || damaged(SRSCAN)))
		return;
	if (f < 0)
	{
		/* cloaking device */
		if (Ship.ship == QUEENE)
			return (printf("Ye Faire Queene does not have the cloaking device.\n"));
		device = "Cloaking device";
		dev2 = "is";
		ind = CLOAK;
		dev3 = "it";
		stat = &Ship.cloaked;
	}
	else
	{
		/* shields */
		device = "Shields";
		dev2 = "are";
		dev3 = "them";
		ind = SHIELD;
		stat = &Ship.shldup;
	}
	if (damaged(ind))
	{
		if (f <= 0)
			out(ind);
		return;
	}
	if (Ship.cond == DOCKED)
	{
		printf("%s %s down while docked\n", device, dev2);
		return;
	}
	if (f <= 0 && !testnl())
	{
		r = getcodpar("Up or down", Udtab);
		i = (int) r->value;
	}
	else
	{
		if (*stat)
			(void)sprintf(s, "%s %s up.  Do you want %s down", device, dev2, dev3);
		else
			(void)sprintf(s, "%s %s down.  Do you want %s up", device, dev2, dev3);
		if (!getynpar(s))
			return;
		i = !*stat;
	}
	if (*stat == i)
	{
		printf("%s already ", device);
		if (i)
			printf("up\n");
		else
			printf("down\n");
		return;
	}
	if (i)
		if (f >= 0)
			Ship.energy -= Param.shupengy;
		else
			Ship.cloakgood = 0;
	Move.free = 0;
	if (f >= 0)
		Move.shldchg = 1;
	*stat = i;
	return;
}
