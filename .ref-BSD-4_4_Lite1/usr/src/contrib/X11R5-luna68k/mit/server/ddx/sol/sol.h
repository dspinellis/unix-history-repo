/*
 *  sol.h --
 *
 *	remade by A.Fujita, DEC-16-1992
 */

#include <stdio.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/ioctl_compat.h>
#include <sys/mman.h>

#undef	_POSIX_SOURCE
#include <sys/signal.h>

#include <errno.h>

#include "X.h"
#include "Xmd.h"

#define	NEED_EVENTS
#include "Xproto.h"

#include "osdep.h"
#include "misc.h"

#include "scrnintstr.h"
#include "screenint.h"

#include "servermd.h"

#include "input.h"
#include "inputstr.h"

#include "mipointer.h"

#include "mfb.h"


/* dix */
extern void	NoopDDA();
extern int	AddScreen();
extern int	AllocateScreenPrivateIndex();
extern Bool	RegisterBlockAndWakeupHandlers();

/* os */
extern int	AddEnabledDevice();
extern int	RemoveEnabledDevice();

/* ddx/mi */
extern Bool	mieqInit();
extern int	mieqProcessInputEvents();
extern int	mieqEnqueue();
extern void	miRegisterPointerDevice();
extern Bool	miDCInitialize();

/* ddx/sol */
extern void	solEnqueueEvents();

extern void	solWakeupProc();



/* libc */
extern int	errno;

/* os */
extern long	EnabledDevices[];
extern long	LastSelectMask[];

/* ddx/sol */
extern int	solScreenIndex;	
extern int	monitorResolution;

extern int	lastEventTime;

extern CARD8 *solKeyModMap[];
extern KeySymsRec solKeySyms[];
