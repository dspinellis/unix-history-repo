/*
 *  solKbd.c --	
 *
 *	remade by A.Fujita, DEC-16-1992
 */

#include "sol.h"
#include "solKbd.h"


static Bool solDevInit();
static void solDevGiveUp();

static void solBell();
static void solKbdCtrl();

static void solCrossScreen();
static void solWarpCursor();
static void solMouseControl();
static Bool solCursorOffScreen();

static solDevPrv private;

static Bool
solDevInit(prv)
solDevPrvPtr prv;
{
	static Bool initFlag = FALSE;
	int arg = 1;

#ifdef	DEBUG
	fprintf(stderr, "solDevInit\t[solKbd.c]\tStart\n");
#endif
	if (initFlag == FALSE) {
		if((prv->fd = open("/dev/kbd",O_RDWR,0)) < 0) {
			Error("Can't open /dev/kbd");
			return FALSE;
		}

		if ((prv->flags = fcntl(prv->fd, F_GETFL, NULL)) < 0) {
			Error("Keyboard fcntl F_GETFL fault.");
			return FALSE;
		}
		prv->flags |= O_NONBLOCK;
		if (fcntl(prv->fd, F_SETFL, prv->flags) < 0) {
			Error("Keyboard fcntl F_SETFL fault.");
			return FALSE;
		}

		initFlag = TRUE;
	}

	prv->kbd_type = 0;

	prv->num++;

#ifdef	DEBUG
	fprintf(stderr, "solDevInit\t[solKbd.c]\tprv->num = %d\n", prv->num);
	fprintf(stderr, "solDevInit\t[solKbd.c]\tEnd\n");
#endif

	return TRUE;
}

static void
solDevGiveUp(prv)
solDevPrvPtr prv;
{
#ifdef	DEBUG
	fprintf(stderr, "solDevGiveUp\t[solKbd.c]\tStart\n");
#endif

	prv->num--;

	if (prv->num == 0) {
		close(prv->fd);
	}

#ifdef	DEBUG
	fprintf(stderr, "solDevGiveUp\t[solKbd.c]\tEnd\n");
#endif
}

unsigned char	codebuf[MAXEVENTS];


int
solDevGetEvents(evp)
	register solDevEvtPtr evp;
{
        solDevPrvPtr prv = &private;
	register unsigned char *p = codebuf;
	register int size, numEvt = 0;
	static unsigned char	ms_queue[3];
	static int ms_byte = 0;

	size = read(prv->fd, codebuf, MAXEVENTS);

	if (size > 0) {
		while(p < (codebuf + size)) {
			switch (ms_byte) {
			case 0:
				if ((*p >= 0x80) && (*p <= 0x87)) {
					ms_queue[ms_byte] = *p++;
					ms_byte++;
				} else {
					evp->type     = EvtKey;
					evp->key_code = (*p++);
					numEvt++;
					evp++;
				}
				break;

			case 1:
				ms_queue[ms_byte] = *p++;
				ms_byte++;
				break;

			case 2:
				ms_queue[ms_byte] = *p++;
				evp->type       = EvtMouse;
				evp->ms_state   = (~ms_queue[0] & 0x7);
				evp->ms_x_delta = (char) ms_queue[1];
				evp->ms_y_delta = -((char) ms_queue[2]);
				numEvt++;
				evp++;
				ms_byte = 0;
				break;
			}
		}
	}

	return(numEvt);
}


int
solKbdProc(pKeyboard,what)
DevicePtr     pKeyboard;
int           what;
{
	static Bool initFlag = FALSE;
        solDevPrvPtr prv = &private;

#ifdef	DEBUG
	fprintf(stderr, "solKbdProc\t[solKbd.c]\tStart\n");
#endif
	switch(what) {

	case DEVICE_INIT:
#ifdef	DEBUG
		fprintf(stderr, "solKbdProc\t[solKbd.c]\tDEVICE_INIT\n");
#endif
		pKeyboard->devicePrivate = (pointer)prv;
		if (initFlag == FALSE) {
			prv->offset = 0;
			if (!solDevInit(prv))
				return (!Success);
			initFlag = TRUE;
		}
		prv->keybdCtrl = defaultKeyboardControl;
		prv->key_state = 0;
		InitKeyboardDeviceStruct(pKeyboard,
					 &(solKeySyms[prv->kbd_type]),
					 (solKeyModMap[prv->kbd_type]),
					 solBell, solKbdCtrl);
		break;

	case DEVICE_ON:
#ifdef	DEBUG
		fprintf(stderr, "solKbdProc\t[solKbd.c]\tDEVICE_ON\n");
#endif
		AddEnabledDevice(prv->fd);
		pKeyboard->on = TRUE;
		break;

	case DEVICE_OFF:
	case DEVICE_CLOSE:
#ifdef	DEBUG
		fprintf(stderr, "solKbdProc\t[solKbd.c]\tDEVICE_OFF or DEVICE_CLOSE\n");
#endif
		RemoveEnabledDevice(prv->fd);
		pKeyboard->on = FALSE;
		break;
	}

#ifdef	DEBUG
	fprintf(stderr, "solKbdProc\t[solKbd.c]\tEnd\n");
#endif
	return (Success);
}
		
void
solKbdGiveUp()
{
        solDevPrvPtr prv = &private;

#ifdef	DEBUG
	fprintf(stderr, "solKbdGiveUp\t[solKbd.c]\tStart\n");
#endif

	solDevGiveUp(prv);

#ifdef	DEBUG
	fprintf(stderr, "solKbdGiveUp\t[solKbd.c]\tEnd\n");
#endif
}
	
static void
solBell(loudness, pKeyboard)
int           loudness;
DevicePtr     pKeyboard;
{
#ifdef	DEBUG
	fprintf(stderr, "solBell\t[solKbd.c]\tStart\n");
	fprintf(stderr, "solBell\t[solKbd.c]\tEnd\n");
#endif
}

static void
solKbdCtrl(pKeyboard, ctrl)
DevicePtr     pKeyboard;
KeybdCtrl     *ctrl;
{
}

Bool
LegalModifier(key)
BYTE key;
{
#ifdef	DEBUG
	fprintf(stderr, "LegalModifier\t[solKbd.c]\tStart\n");
	fprintf(stderr, "LegalModifier\t[solKbd.c]\tEnd\n");
#endif
	return(TRUE);
}

void
solKbdEnqueueEvent(pKeyboard, evp)
DevicePtr     pKeyboard;
solDevEvtPtr	evp;
{
	xEvent xE;
	CARD8 keyModifiers;
	register unsigned char key = (evp->key_code & 0x7F);
	solDevPrvPtr prv = (solDevPrvPtr)(pKeyboard->devicePrivate);

	lastEventTime = GetTimeInMillis();
	xE.u.keyButtonPointer.time = lastEventTime;

	if ((evp->key_code & 0x80)) {
		xE.u.u.type = KeyRelease;
	} else {
		xE.u.u.type = KeyPress;
	}

	xE.u.u.detail = key;

	mieqEnqueue(&xE);
}


/*** for Mouse ***/

miPointerScreenFuncRec solPointerScreenFuncs = {
	solCursorOffScreen,
	solCrossScreen,
	solWarpCursor,
};

int
solMouseProc(pMouse,what)
DevicePtr 	pMouse;
int		what;
{
	static Bool initFlag = FALSE;
        solDevPrvPtr prv = &private;
	BYTE    map[5];
	int mouse_mode;

#ifdef	DEBUG
	fprintf(stderr, "solMouseProc\t[solKbd.c]\tStart\n");
#endif

	switch(what) {

	case DEVICE_INIT:
#ifdef	DEBUG
		fprintf(stderr, "solMouseProc\t[solKbd.c]\tDEVICE_INIT\n");
#endif
		pMouse->devicePrivate = (pointer)prv;
		if (initFlag == FALSE) {
			if (!solDevInit(prv))
				return (!Success);
			initFlag = TRUE;
		}
		map[1] = Button1;
		map[2] = Button2;
		map[3] = Button3;
		InitPointerDeviceStruct( pMouse,map, 3, miPointerGetMotionEvents,
				solMouseControl, miPointerGetMotionBufferSize());
		break;

	case DEVICE_ON:
#ifdef	DEBUG
		fprintf(stderr, "solMouseProc\t[solKbd.c]\tDEVICE_ON\n");
#endif
		mouse_mode = TRUE;
		if (ioctl(prv->fd, KIOCMOUSE, &mouse_mode) < 0) {
			Error("Mouse  ioctl KIOCMOUSE fault.");
			return (!Success);
		}
		AddEnabledDevice (prv->fd);
		pMouse->on = TRUE;
		break;

	case DEVICE_OFF:
	case DEVICE_CLOSE:
#ifdef	DEBUG
		fprintf(stderr, "solMouseProc\t[solKbd.c]\tDEVICE_OFF or DEVICE_CLOSE\n");
#endif
		RemoveEnabledDevice(prv->fd);
		pMouse->on = FALSE;
		mouse_mode = FALSE;
		if (ioctl(prv->fd, KIOCMOUSE, &mouse_mode) < 0) {
			Error("Mouse  ioctl KIOCMOUSE fault.");
			return (!Success);
		}
		break;
	}

#ifdef	DEBUG
	fprintf(stderr, "solMouseProc\t[solKbd.c]\tEnd\n");
#endif
	return (Success);
}

void
solMouseGiveUp()
{
        solDevPrvPtr prv = &private;

#ifdef	DEBUG
	fprintf(stderr, "solMouseGiveUp\t[solMouse.c]\tStart\n");
#endif

	solDevGiveUp(prv);

#ifdef	DEBUG
	fprintf(stderr, "solMouseGiveUp\t[solMouse.c]\tEnd\n");
#endif
}

static Bool
solCursorOffScreen(pScreen, x, y)
    ScreenPtr	*pScreen;
    int		*x, *y;
{
	return FALSE;
}

static void
solCrossScreen(pScreen,x,y)
ScreenPtr	pScreen;
int		x;
int		y;
{
}

static
solMouseAccelerate (pMouse, delta)
	DevicePtr pMouse;
	int	  delta;
{
	register PtrCtrl *p;
	register int  s;

	p = &((DeviceIntPtr)pMouse)->ptrfeed->ctrl;
  
	if(delta > 0) {
		s = 1;
	} else {
		s = -1;
		delta = -delta;
	}
	
	if (delta > p->threshold) {
		return ((s * (p->threshold +
			      ((delta - p->threshold) * p->num) / p->den)));
	} else {
		return ((s * delta));
	}
}

static void
solMouseControl()
{
}

static void
solWarpCursor (pScreen, x, y)
	ScreenPtr   pScreen;
	int         x, y;
{
	int oldmask;

	oldmask = sigblock (sigmask(SIGABRT));
	miPointerWarpCursor (pScreen, x, y);
	sigsetmask (oldmask);
}

void
solMouseEnqueueEvent(pMouse, evp)
DevicePtr	pMouse;
solDevEvtPtr	evp;
{
	solDevPrvPtr prv = (solDevPrvPtr) (pMouse->devicePrivate);
	xEvent xE;
	int delta_X,delta_Y;
	register int state, mask;

	state = prv->mouse_state ^ ((int) evp->ms_state);
	lastEventTime = GetTimeInMillis();
	xE.u.keyButtonPointer.time = lastEventTime;

	if (state != 0) {
		if ((mask = state & BUTTON_L)) {
			if ((int) evp->ms_state & mask)
				xE.u.u.type = ButtonPress;
			else
				xE.u.u.type = ButtonRelease;
			xE.u.u.detail = Button1;
			mieqEnqueue(&xE);
		}

		if ((mask = state & BUTTON_M)) {
			if ((int) evp->ms_state & mask)
				xE.u.u.type = ButtonPress;
			else
				xE.u.u.type = ButtonRelease;
			xE.u.u.detail = Button2;
			mieqEnqueue(&xE);
		}

		if ((mask = state & BUTTON_R)) {
			if ((int) evp->ms_state & mask)
				xE.u.u.type = ButtonPress;
			else
				xE.u.u.type = ButtonRelease;
			xE.u.u.detail = Button3;
			mieqEnqueue(&xE);
		}
	}

	xE.u.u.type = MotionNotify;
	delta_X = solMouseAccelerate (pMouse, (int) evp->ms_x_delta);
	delta_Y = solMouseAccelerate (pMouse, (int) evp->ms_y_delta);
	miPointerDeltaCursor (delta_X, delta_Y, lastEventTime);

	prv->mouse_state = (int) evp->ms_state;
}
