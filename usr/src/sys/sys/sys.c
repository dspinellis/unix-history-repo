/*
 *	indirect driver for controlling tty.
 */
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/proc.h"

syopen(dev, flag)
{

	if(u.u_ttyp == NULL) {
		u.u_error = ENXIO;
		return;
	}
	(*cdevsw[major(u.u_ttyd)].d_open)(u.u_ttyd, flag);
}

syread(dev)
{

	(*cdevsw[major(u.u_ttyd)].d_read)(u.u_ttyd);
}

sywrite(dev)
{

	(*cdevsw[major(u.u_ttyd)].d_write)(u.u_ttyd);
}

sysioctl(dev, cmd, addr, flag)
caddr_t addr;
{

	(*cdevsw[major(u.u_ttyd)].d_ioctl)(u.u_ttyd, cmd, addr, flag);
}
