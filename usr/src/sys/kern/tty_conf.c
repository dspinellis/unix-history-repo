/*	tty_conf.c	4.1	82/10/17	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/tty.h"
#include "../h/conf.h"

int	nodev();
int	nulldev();

int	ttyopen(),ttyclose(),ttread(),ttwrite(),nullioctl(),ttstart();
int	ttyinput();

#include "bk.h"
#if NBK > 0
int	bkopen(),bkclose(),bkread(),bkinput(),bkioctl();
#endif

#include "tb.h"
#if NTB > 0
int	tbopen(),tbclose(),tbread(),tbinput(),tbioctl();
#endif

struct	linesw linesw[] =
{
	ttyopen, nodev, ttread, ttwrite, nullioctl,
	ttyinput, nodev, nulldev, ttstart, nulldev,
#if NBK > 0
	bkopen, bkclose, bkread, ttwrite, bkioctl,
	bkinput, nodev, nulldev, ttstart, nulldev,
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
#endif
	ttyopen, ttyclose, ttread, ttwrite, nullioctl,
	ttyinput, nodev, nulldev, ttstart, nulldev,
#if NTB > 0
	tbopen, tbclose, tbread, nodev, tbioctl,
	tbinput, nodev, nulldev, ttstart, nulldev,		/* 3 */
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
#endif
#if NTB > 0
	tbopen, tbclose, tbread, nodev, tbioctl,
	tbinput, nodev, nulldev, ttstart, nulldev,		/* 4 */
#else
	nodev, nodev, nodev, nodev, nodev,
	nodev, nodev, nodev, nodev, nodev,
#endif
	0		/* XXX */
};

int	nldisp = 5;

