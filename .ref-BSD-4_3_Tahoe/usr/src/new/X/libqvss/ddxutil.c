/* Copyright 1985, Massachusetts Institute of Technology */
/* util.c		Various utilities
 *
 *	SoundBell	Generate audible bell
 *	SetKeyClick	Control key click
 *	SetAutoRepeat	Control auto repeat
 *	SetLockLED	Control Lock LED
 *	SetVideo	Disable/enable video
 *	QueryShape	Determine shapes
 *	ResolveColors	does nothing
 *	StoreColors	does nothing
 */

#include <sys/types.h>
#include <vaxuba/qvioctl.h>

#include "ddxqvss.h"

extern int vsdev;

#define LK_REPEAT_ON 0xe3	/* enable autorepeat across kbd */
#define LK_REPEAT_OFF 0xe1	/* diable autorepeat across kbd */
#define LK_ENABLE_CLICK 0x1b	/* enable keyclick / set volume	*/
#define LK_DISABLE_CLICK 0x99	/* disable keyclick entirely	*/
#define LK_ENABLE_BELL 0x23	/* enable bell / set volume 	*/
#ifndef LK_RING_BELL		/* can go away after 1.2 is out */
#define LK_RING_BELL 0xa7	/* command to ring a bell 	*/
#define LED_1 0x81		/* led 1 on the keyboard	*/
#define LED_2 0x82		/* led 2 on the keyboard	*/
#define LED_3 0x84		/* led 3 on the keyboard	*/
#define LED_4 0x88		/* led 4 on the keyboard	*/
#define LK_LED_ENABLE 0x13	/* turn on led			*/
#define LK_LED_DISABLE 0x11	/* turn off led			*/
#endif

/* Sound bell, volume between 0 (quiet) and 7 (loud) */

SoundBell (volume)
	int volume;
{
	struct qv_kpcmd ioc;
	volume = volume & 7;
	volume = 7 - volume;

	ioc.nbytes = 1;
	ioc.cmd = LK_ENABLE_BELL;
	ioc.par[0] = volume;
	ioctl(vsdev, QIOCKPCMD, &ioc);

	ioc.nbytes = 0;
	ioc.cmd = LK_RING_BELL;
	return(ioctl(vsdev, QIOCKPCMD, &ioc));
}

/* Set key click, volume between -1 (default), 0 (off) and 8 (loud) */

SetKeyClick (volume)
	int volume;
{
	struct qv_kpcmd ioc;
	int ret;
	if(volume < 0) volume = 6;
	if(volume > 0 ) {
		volume -= 1;
		if(volume > 7) volume = 7;
		volume = 7 - volume;

		ioc.nbytes = 1;
		ioc.cmd = LK_ENABLE_CLICK;
		ioc.par[0] = volume;
		ret = ioctl(vsdev, QIOCKPCMD, &ioc);
	}
	else if(volume == 0) {
		ioc.nbytes = 0;
		ioc.cmd = LK_DISABLE_CLICK;
		ret = ioctl(vsdev, QIOCKPCMD, &ioc);
	}
	return(ret);
}

/* Set autorepeat */

SetAutoRepeat (onoff)
	int onoff;
{
	struct qv_kpcmd ioc;
	register char *divsets;
	divsets = onoff ? (char *) AutoRepeatLKMode () : (char *) UpDownLKMode ();
	ioc.nbytes = 0;
	while (ioc.cmd = *divsets++)
		ioctl(vsdev, QIOCKPCMD, &ioc);
	ioc.cmd = ((onoff > 0) ? LK_REPEAT_ON : LK_REPEAT_OFF );
	return(ioctl(vsdev, QIOCKPCMD, &ioc));
}

int SetVideo(onoff)
	int onoff;
{
	return(onoff - 1);
}
QueryShape (shape, width, height)
	int shape;
	short *width, *height;
{
	switch (shape) {
	case CursorShape:	/* braindamaged qvss cursor.... */
	    if (*width > 16)
		*width = 16;
	    if (*height > 16)
		*height = 16;
	    break;
	case TileShape:
	    *width = *height = 16;
	    break;
	}
}

SetLockLED (onoff)
	int onoff;
{
	struct qv_kpcmd ioc;
	if (onoff)
		ioc.cmd = LK_LED_ENABLE;
	else
		ioc.cmd = LK_LED_DISABLE;
	ioc.par[0] = LED_3;
	ioc.par[1]  = 0;
	ioc.nbytes = 1;
	ioctl(vsdev, QIOCKPCMD, &ioc);
	return;
}

ResolveColor (red, green, blue)
	unsigned short *red, *green, *blue;
{
}

StoreColors (count, entries)
	int count;
	ColorDef *entries;

{
}
