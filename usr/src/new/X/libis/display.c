/*
 *	$Source: /u1/X/libis/RCS/display.c,v $
 *	$Header: display.c,v 1.1 86/11/17 14:33:42 swick Rel $
 */

#ifndef lint
static char *rcsid_display_c = "$Header: display.c,v 1.1 86/11/17 14:33:42 swick Rel $";
#endif	lint

#include "is-copyright.h"

/*	display.c - routines to open and close display
 *
 *	OpenDisplay
 *	InitDisplay
 *	DisplayDead
 *	AllocateSpace
 *
 * 	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"
#include <sys/file.h>
#include <errno.h>

extern int	errno;

DEVICE *CurrentDevice;

int	indev;		/* fd of the desktop */

extern int InputHandler();

/*
 *	OpenDisplay
 */
OpenDisplay(name)
char	*name;
{
    int ldisc = TWSDISC;
    short mmode = (VT_MOUSE_DOWN | VT_MOUSE_UP | VT_MOUSE_CONTINUOUS);
    char	tname[32];
    extern char *strcpy(), *strcat();

    strcpy(tname, "/dev/ttyw");
    strcat(tname, name);

#ifdef DEBUG
if (debug & D_Misc)
    printf("OpenDisplay(name=\"%s\")\n", tname);
#endif DEBUG

    indev = GIP_Init();
    setreuid(-1, -1);       /* don't need to be setuid root anymore */
    ioctl(indev, TIOCSETD, &ldisc);	/* SetLineDisc */
    ioctl(indev, TIOVSETMM, &mmode);	/* SetMouseMode */
    return (indev);
}

/*
 *	InitDisplay
 */
InitDisplay(dp)
register DEVICE	*dp;
{
    static vsCursor	vsmouse;
    static vsBox	vsmbox;
    static vsEventQueue	vsqueue;

    struct gconfig gc;

#ifdef DEBUG
if (debug & D_Misc)
    printf("InitDisplay(dp=0x%x)\n", dp);
#endif DEBUG

    CurrentDevice = dp;

    if (indev >= 0) {
	if (ioctl(indev, TIOVGETHW, &gc)) {	/* GetGraphicsConfig */
	    return (-1);
	}

	switch (gc.realcolors) {
	case 2:
	    dp->id = XDEV_ISIBW;	/* monochrome */
	    break;
	case 16:
	    dp->id = XDEV_ISICOLOR4;	/* 4 bit plane color */
	    break;
	default:
	    dp->id = -1;		/* no idea! */
	    break;
	}
	dp->width = ScreenPixmap.width;
	dp->height = ScreenPixmap.height;
	dp->planes = ScreenPixmap.kind & 0xf;
	if (dp->planes > 1)
	    dp->entries = gc.lutsize;
	else
	    dp->entries = 0;		/* no lut! */
	dp->mouse = &vsmouse;
	dp->mbox = &vsmbox;
	dp->queue = &vsqueue;


	Define_input_handler(InputHandler);

    } else {
	return (-1);
    }
    return (0);
}

/*
 *	DisplayDead
 */

DisplayDead()
{
#ifdef DEBUG
if (debug & D_Misc) {
    printf("DisplayDead()?\n");
    fflush(stdout);
}
#endif DEBUG

    GIP_Quit();

    return (0);
}

/*
 *	AllocateSpace
 *
 *	The presumption here is that only one AllocateSpace call is
 *	made/request
 */

#define BUFSIZE 3072	/* arbitrary size for buffer */

caddr_t AllocateSpace(size)
int	size;
{
    static char buf[BUFSIZE];

#ifdef DEBUG
if (debug & D_Misc)
    printf("AllocateSpace(size=%d)\n", size);
#endif DEBUG

    if (size < BUFSIZE) {
	return (buf);
    }
    errno = ENOMEM;
    return (NULL);
}
