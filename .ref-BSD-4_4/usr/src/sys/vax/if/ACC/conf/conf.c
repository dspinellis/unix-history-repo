
If you have either the PI or the X.29 (DDA_RAWOPT or DDA_PADOPT) defined,
you will have to add new device driver entries into the file /sys/vax/conf.c.

The following example is for an Ultrix 3.0 system,  the similar changes are
necessary for Ultrix 2.n and 4.3bsd systems.  If you are not using DDA_RAWOPT
and not using DDA_PADOPT,  there is no need to modify conf.c.

Someplace after the definiton of nulldev() and nodev() enter the following
preamble:

#include "dda.h"
#if (NDDA > 0) && defined(DDA_PADOPT)
int xxopen(), xxclose(), xxread(), xxwrite();
int xxioctl(), xxreset(), xxstop(), xxselect();
struct tty xx_tty[];
#else
#define	xxopen	nodev			/* ACP 5250/6250 x29 interface */
#define	xxclose	nodev
#define xxread	nodev
#define	xxwrite	nodev
#define	xxioctl	nodev
#define	xxreset	nulldev
#define	xxstop	nodev
#define	xxselect nodev
#define	xx_tty	NULL
#endif

#if (NDDA > 0) && defined(DDA_RAWOPT)
int piopen(), piclose(), piioctl();
#else
#define	piopen	nodev			/* ACP 5250/6250 pi interface */
#define	piclose	nodev
#define	piioctl	nodev
#endif

At the end of the cdevsw table (character device dispatch table),  add the
following two entries:

	/* ACP5250/6250 x29 interface */
	xxopen,		xxclose,	xxread,		xxwrite,	/*??*/
	xxioctl,	xxstop,		xxreset,	xx_tty,
	xxselect,	nodev,		0,

	/* ACP5250/6250 programmers interface */
	piopen,		piclose,	nodev,		nodev,		/*??*/
	piioctl,	nodev,		nulldev,	NULL,
	nodev,		nodev,		0,

For the sake of sanity,  enter the major device numbers for these devices
in the "??" spaces after each entry.  For example (again, Ultrix 3.0), if
the last device is 59,  the X.29 interface is accessed via major # 60, and
the X.29 interface is accessed via major # 61.

You should also edit the file MAKEDEV.acc that accompanied this distribution
so that it will contain the proper major numbers for each of these interfaces.
