#
/*
 */

/*
 *	DM-BB fake driver
 */
#include "../tty.h"
#include "../conf.h"

struct	tty	dh11[];

dmopen(dev)
{
	register struct tty *tp;

	tp = &dh11[dev.d_minor];
	tp->t_state =| CARR_ON;
}

dmclose(dev)
{
}
