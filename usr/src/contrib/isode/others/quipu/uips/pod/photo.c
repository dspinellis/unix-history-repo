#ifndef lint
static char *rcsid = "$Header: /f/osi/others/quipu/uips/pod/RCS/photo.c,v 7.2 91/02/22 09:31:38 mrose Interim $";
#endif

/*
 * $Header: /f/osi/others/quipu/uips/pod/RCS/photo.c,v 7.2 91/02/22 09:31:38 mrose Interim $
 */


#include "quipu/photo.h"
#include "pod.h"

extern Widget toplevel;
extern Widget PhotoWindow;

GC XCreateGC();
void make_photo_widget();

GC gc;
char photo_name[1024];
Pixmap photo_pixmap = 0;
Display *dpy;
Screen *scr;
int winX, winY, winW, winH;
extern int NUMLINES,PIC_LINESIZE;
extern unsigned position;
unsigned long XorVal;

static int passno = 1;
static int x, y;
int px, py, maxx;
int two_passes;

/*ARGSUSED*/
int photo_start(name)
     char *name;
{
  x = y = 0;
  if (passno == 1)
    maxx = 0, two_passes = 1;
  return 0;
}

int photo_end(name)
     char *name;
{
  if (passno == 1) {
    passno = 2;
    px = x = maxx;
    py = --y;
  
    make_photo_widget();

    dpy = XtDisplay(toplevel);
    scr = DefaultScreenOfDisplay(dpy);
    
    winW = x;
    winH = y;

    photo_pixmap = XCreatePixmap(dpy, XtWindow(PhotoWindow),
				 (Dimension) x, (Dimension) y, 
				 DefaultDepthOfScreen(scr));
    gc = XCreateGC(dpy, photo_pixmap, 0, NULL);

    XSetLineAttributes(dpy, gc, 0, LineSolid, CapButt, JoinBevel);
    XSetFunction(dpy, gc, GXclear);
    
    XSetBackground(dpy, gc, WhitePixelOfScreen(scr));
    XSetForeground(dpy, gc, BlackPixelOfScreen(scr));
    
    XFillRectangle(dpy, photo_pixmap, gc, 0, 0, winW,winH);

    XSetFunction(dpy, gc, GXcopy);

    XSetForeground(dpy, gc, BlackPixelOfScreen(scr));
    XSetBackground(dpy, gc, WhitePixelOfScreen(scr));

    XFillRectangle(dpy, photo_pixmap, gc, 0, 0, winW,winH);
    XSetFunction(dpy, gc, GXclear);

    return 0;
  }
  if (name && *name) (void) strcpy(photo_name, name);
  passno = 1;
  x = y = maxx = 0;
  return 0;
}

/*ARGSUSED*/
int photo_line_end(line)
     bit_string *line;
{
  /* the end of a line has been reached */
  /* A bit string is stored in line->dbuf_top */
  
  if (passno == 1 && x > maxx)
    maxx = x;
  x = 0, y++;
  
  return 0;
}

int photo_black(length)
     int length;
{
  if (passno == 1) {
    x += length;
    return 0;
  }
  /* draw a black line of 'length' pixels */
  return 0;
}

int photo_white(length)
     int length;
{
  if (passno == 1) {
    x += length;
    return 0;
  }
  
  /* draw a white line of 'length' pixels */
  XDrawLine (dpy, photo_pixmap, gc, position, 
	     NUMLINES, length+position-1, NUMLINES);
  
  return 0;
}

