/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)kill.c	5.1 (Berkeley) 1/29/86";
#endif not lint

# include	"trek.h"

/*
**  KILL KILL KILL !!!
**
**	This file handles the killing off of almost anything.
*/

/*
**  Handle a Klingon's death
**
**	The Klingon at the sector given by the parameters is killed
**	and removed from the Klingon list.  Notice that it is not
**	removed from the event list; this is done later, when the
**	the event is to be caught.  Also, the time left is recomputed,
**	and the game is won if that was the last klingon.
*/

killk(ix, iy)
int	ix, iy;
{
	register int		i, j;

	printf("   *** Klingon at %d,%d destroyed ***\n", ix, iy);

	/* remove the scoundrel */
	Now.klings -= 1;
	Sect[ix][iy] = EMPTY;
	Quad[Ship.quadx][Ship.quady].klings -= 1;
	/* %%% IS THIS SAFE???? %%% */
	Quad[Ship.quadx][Ship.quady].scanned -= 100;
	Game.killk += 1;

	/* find the Klingon in the Klingon list */
	for (i = 0; i < Etc.nkling; i++)
		if (ix == Etc.klingon[i].x && iy == Etc.klingon[i].y)
		{
			/* purge him from the list */
			Etc.nkling -= 1;
			for (; i < Etc.nkling; i++)
				bmove(&Etc.klingon[i+1], &Etc.klingon[i], sizeof Etc.klingon[i]);
			break;
		}

	/* find out if that was the last one */
	if (Now.klings <= 0)
		win();

	/* recompute time left */
	Now.time = Now.resource / Now.klings;
	return;
}


/*
**  handle a starbase's death
*/

killb(qx, qy)
int	qx, qy;
{
	register struct quad	*q;
	register struct xy	*b;

	q = &Quad[qx][qy];

	if (q->bases <= 0)
		return;
	if (!damaged(SSRADIO))
		/* then update starchart */
		if (q->scanned < 1000)
			q->scanned -= 10;
		else
			if (q->scanned > 1000)
				q->scanned = -1;
	q->bases = 0;
	Now.bases -= 1;
	for (b = Now.base; ; b++)
		if (qx == b->x && qy == b->y)
			break;
	bmove(&Now.base[Now.bases], b, sizeof *b);
	if (qx == Ship.quadx && qy == Ship.quady)
	{
		Sect[Etc.starbase.x][Etc.starbase.y] = EMPTY;
		if (Ship.cond == DOCKED)
			undock();
		printf("Starbase at %d,%d destroyed\n", Etc.starbase.x, Etc.starbase.y);
	}
	else
	{
		if (!damaged(SSRADIO))
		{
			printf("Uhura: Starfleet command reports that the starbase in\n");
			printf("   quadrant %d,%d has been destroyed\n", qx, qy);
		}
		else
			schedule(E_KATSB | E_GHOST, 1e50, qx, qy, 0);
	}
}


/**
 **	kill an inhabited starsystem
 **/

kills(x, y, f)
int	x, y;	/* quad coords if f == 0, else sector coords */
int	f;	/* f != 0 -- this quad;  f < 0 -- Enterprise's fault */
{
	register struct quad	*q;
	register struct event	*e;
	register char		*name;
	char			*systemname();

	if (f)
	{
		/* current quadrant */
		q = &Quad[Ship.quadx][Ship.quady];
		Sect[x][y] = EMPTY;
		name = systemname(q);
		if (name == 0)
			return;
		printf("Inhabited starsystem %s at %d,%d destroyed\n",
			name, x, y);
		if (f < 0)
			Game.killinhab += 1;
	}
	else
	{
		/* different quadrant */
		q = &Quad[x][y];
	}
	if (q->qsystemname & Q_DISTRESSED)
	{
		/* distressed starsystem */
		e = &Event[q->qsystemname & Q_SYSTEM];
		printf("Distress call for %s invalidated\n",
			Systemname[e->systemname]);
		unschedule(e);
	}
	q->qsystemname = 0;
	q->stars -= 1;
}


/**
 **	"kill" a distress call
 **/

killd(x, y, f)
int	x, y;		/* quadrant coordinates */
int	f;		/* set if user is to be informed */
{
	register struct event	*e;
	register int		i;
	register struct quad	*q;

	q = &Quad[x][y];
	for (i = 0; i < MAXEVENTS; i++)
	{
		e = &Event[i];
		if (e->x != x || e->y != y)
			continue;
		switch (e->evcode)
		{
		  case E_KDESB:
			if (f)
			{
				printf("Distress call for starbase in %d,%d nullified\n",
					x, y);
				unschedule(e);
			}
			break;

		  case E_ENSLV:
		  case E_REPRO:
			if (f)
			{
				printf("Distress call for %s in quadrant %d,%d nullified\n",
					Systemname[e->systemname], x, y);
				q->qsystemname = e->systemname;
				unschedule(e);
			}
			else
			{
				e->evcode |= E_GHOST;
			}
		}
	}
}
