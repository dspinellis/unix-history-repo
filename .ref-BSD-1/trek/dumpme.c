# include	"trek.h"

/*
**  Dump the starship somewhere in the galaxy
**
**	Parameter is zero if bounce off of negative energy barrier,
**	one if through a black hole
**
**	Note that the quadrant is NOT initialized here.  This must
**	be done from the calling routine.
**
**	Repair of devices must be deferred.
*/

dumpme(flag)
int	flag;
{
	register int		f;
	double			x;
	register struct event	*e;
	register int		i;

	f = flag;
	Ship.quadx = ranf(NQUADS);
	Ship.quady = ranf(NQUADS);
	Ship.sectx = ranf(NSECTS);
	Ship.secty = ranf(NSECTS);
	x =+ 1.5 * franf();
	Move.time =+ x;
	if (f)
	{
		printf("%s falls into a black hole.\n", Ship.shipname);
	}
	else
	{
		printf("Computer applies full reverse power to avoid hitting the\n");
		printf("   negative energy barrier.  A space warp was entered.\n");
	}
	/* bump repair dates forward */
	for (i = 0; i < MAXEVENTS; i++)
	{
		e = &Event[i];
		if (e->evcode != E_FIXDV)
			continue;
		reschedule(e, (e->date - Now.date) + x);
	}
	events(1);
	printf("You are now in quadrant %d,%d.  It is stardate %.2f\n",
		Ship.quadx, Ship.quady, Now.date);
	Move.time = 0;
}
