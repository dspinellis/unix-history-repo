#
/*
 */

/*
 *	indirect driver for controlling tty.
 */
#include "../param.h"
#include "../conf.h"
#include "../user.h"
#include "../tty.h"
#include "../proc.h"

syopen(dev, flag)
{
	register *tp;

	if(tp = syttyp())
	(*cdevsw[tp->t_dev.d_major].d_open)(tp->t_dev, flag);
}

syread(dev)
{
	register *tp;

	if(tp = syttyp())
	(*cdevsw[tp->t_dev.d_major].d_read)(tp->t_dev);
}

sywrite(dev)
{
	register *tp;

	if(tp = syttyp())
	(*cdevsw[tp->t_dev.d_major].d_write)(tp->t_dev);
}

sysgtty(dev, flag)
{
	register *tp;

	if(tp = syttyp())
	(*cdevsw[tp->t_dev.d_major].d_sgtty)(tp->t_dev, flag);
}

syttyp()
{
	register tp;

	tp = u.u_procp->p_ttyp;
	if(tp == NULL)
		u.u_error = ENXIO;
	return(tp);
}
