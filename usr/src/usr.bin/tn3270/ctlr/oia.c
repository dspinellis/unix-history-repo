/*
 * Routines to maintain the Operator Information Area.
 */

#include "oia.h"

OIA OperatorInformationArea;

static int
    Insert,
    SystemLocked,
    PWait,
    TWait,
    OnlineA,
    Ready3274,
    MyJob;

#define	X_AREA_SYSTEM	0xc6, "\xb2\xb8"
#define	X_AREA_CLOCK	0xc6, "\xf2\xf3"
#define	X_AREA_NULL	0xc6, ""

static void
SetXarea(x, xwhy)
int	x;
char	*xwhy;
{
    register int i;

    OperatorInformationArea.x = x;
    for (i = 0; i < sizeof OperatorInformationArea.xwhy; i++) {
	if (*xwhy) {
	    OperatorInformationArea.xwhy[i] = *xwhy++;
	} else {
	    OperatorInformationArea.xwhy[i] = 0;
	}
    }
}


SetInsert(onoff)
int	onoff;
{
    Insert = onoff;

    if (onoff) {
	OperatorInformationArea.insert = OIA_INSERT_ON;
    } else {
	OperatorInformationArea.insert = 0;
    }
}

SetSystemLocked(onoff)
int	onoff;
{
    SystemLocked = onoff;

    if ((PWait == 0) && (TWait == 0)) {
	SetXArea(X_AREA_SYSTEM);
    }
}

SetPWait(onoff)
int	onoff;
{
    PWait = onoff;

    if (onoff) {
	SetXArea(X_AREA_CLOCK);
    } else {
	if (SystemLocked) {
	    SetXArea(X_AREA_SYSTEM);
	} else {
	    SetXArea(X_AREA_NULL);
	}
    }
}

SetTWait(onoff)
int	onoff;
{
    TWait = onoff;

    SetPWait(onoff);
}

SetOnlineA(onoff)
int	onoff;
{
    OnlineA = onoff;

    if (onoff) {
	SetMyJob(1);
	OperatorInformationArea.online = OIA_ONLINE_A;
    } else {
	OperatorInformationArea.online = 0;
    }
}

SetReady3274(onoff)
int	onoff;
{
    Ready3274 = onoff;

    if (onoff) {
	OperatorInformationArea.ready = OIA_READY_3274;
    } else {
	OperatorInformationArea.ready = 0;
    }
}

SetMyJob(onoff)
int	onoff;
{
    MyJob = onoff;

    if (onoff) {
	OperatorInformationArea.ownership = OIA_OWNERSHIP_MYJOB;
    } else {
	OperatorInformationArea.ownership = 0;
    }
}
