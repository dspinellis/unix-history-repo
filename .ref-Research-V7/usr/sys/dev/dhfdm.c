/*
 *	DM-BB fake driver
 */
#include "../h/param.h"
#include "../h/tty.h"
#include "../h/conf.h"

struct	tty	dh11[];

dmopen(dev)
{
	register struct tty *tp;

	tp = &dh11[minor(dev)];
	tp->t_state |= CARR_ON;
}

dmctl(dev, bits)
{
}
