/*	dhfdm.c	4.1	11/9/80	*/

#include "../conf/dh.h"
#if NDH11 > 0
/*
 * DM-BB fake driver
 */
#include "../h/param.h"
#include "../h/tty.h"
#include "../h/conf.h"

struct	tty	dh11[];

/*ARGSUSED*/
dmopen(dev)
{
	register struct tty *tp;

	tp = &dh11[minor(dev)];
	tp->t_state |= CARR_ON;
}

/*ARGSUSED*/
dmctl(dev, bits, how)
{

}
#endif
