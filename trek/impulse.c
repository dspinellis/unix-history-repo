# include	"trek.h"

/**
 **	move under impulse power
 **/

impulse()
{
	int			course;
	register int		power;
	float			dist, time;
	register int		percent;
	extern float		move();

	if (Ship.cond == DOCKED)
		return (printf("Scotty: Sorry captain, but we are still docked.\n"));
	if (damaged(IMPULSE))
		return (out(IMPULSE));
	if (getcodi(&course, &dist))
		return;
	power = 20 + 100 * dist;
	percent = 100 * power / Ship.energy + 0.5;
	if (percent >= 85)
	{
		printf("Scotty: That would consume %d%% of our remaining energy.\n",
			percent);
		if (!getynpar("Are you sure that is wise"))
			return;
		printf("Aye aye, sir\n");
	}
	time = dist / 0.095;
	percent = 100 * time / Now.time + 0.5;
	if (percent >= 85)
	{
		printf("Spock: That would take %d%% of our remaining time.\n",
			percent);
		if (!getynpar("Are you sure that is wise"))
			return;
		printf("(He's finally gone mad)\n");
	}
	Move.time = move(0, course, time, 0.095);
	Ship.energy =- 20 + 100 * Move.time * 0.095;
}
