/*
 * This neat little program, another in a continuing series of eye (and
 * screen) grabbers, will cause a disease to strike a random window on your 
 * screen.... Need I say more??
 */


#include <stdio.h>
#include <X/Xlib.h>
#include "twiddle.h"
#define TRUE 1
#define FALSE 0
#include <strings.h>

typedef struct
{
  int x,y;
  int falling;
} BadBit;

BadBit disease[100000];

int NoRotted;

Window Victim;
WindowInfo Info;

caddr_t *bitmap;

Display *dpy;

main(argc,argv)
int argc;
char **argv;
{
  
  Window foo, *targets;
  int ntargets;
  char display[256];
  int i;
  char *strind;

  display[0] = '\0';

  for (i=0; i < argc; i++) {
    strind = index (argv[i], ':');
    if(strind != NULL) {
      strncpy(display, argv[i], sizeof(display));
      continue;
    }
  }
  
  srandom (getpid());
  dpy = XOpenDisplay(display);

  XQueryTree(RootWindow, &foo, &ntargets, &targets);

  do
    {
      Victim = targets[rnd(ntargets)];
      XQueryWindow(Victim, &Info);
    }
  while (Info.mapped != IsMapped && Info.type != IsOpaque);

  printf("Info:\nWindow:%d  X:%d   Y:%d\n", Victim, Info.x, Info.y);

  NoRotted = 0;			/* Not for long.. */

  GrabBitmap();
  while(1) 
    {
/*      GrabBitmap(); */
      RotMoreBits();
      MoveBits();
      XFlush();
    }
}

GrabBitmap()
{
  XQueryWindow(Victim, &Info);
  bitmap = (caddr_t *)calloc( XYPixmapSize(Info.width, Info.height, dpy -> dplanes) ,1 );
  XPixmapGetXY(Victim, 0, 0, Info.width, Info.height, bitmap);
}

RotABit(x,y)
     register int x,y;
{
  register BadBit *p=disease;
  register int i;

  for(i=NoRotted; i; i--,p++)
    if (p->x==x && p->y==y) return;
  disease[NoRotted].x=x;
  disease[NoRotted].y=y;
  disease[NoRotted].falling = FALSE;
  ++NoRotted;
}
RotMoreBits()
{
  register int i, x, y, bxsz;
  bxsz = (Info.width+15) & ~0xf ;

  for (i=100; i; --i)
    {
      x=rnd(Info.width);
      y=rnd(Info.height);

      if (!fetch (bitmap, (x + y*bxsz))) { RotABit(x,y);}
    }
}

MoveBits()
{
  register int i, bxsz;
  register BadBit *p;

  bxsz = (Info.width+15) & ~0xf;

  for (i=NoRotted, p=disease; i; --i, ++p)
    {
      if (p->falling)
	{
	  if (!fetch(bitmap, p->x + (p->y*bxsz)) && 
	      fetch(bitmap, p->x + (p->y+1) * bxsz))
	    {
	      set(bitmap, p->x + p->y*bxsz);
	      SetBit(p->x, p->y);
	      (p->y)+=1;
	      reset(bitmap, p->x + p->y*bxsz);
	      ClearBit(p->x, p->y);
	    }
	  else {p->falling = FALSE; RotABit(p->x, p->y+1);}
	}
      else
	{
	  if (fetch(bitmap, p->x + (p -> y+1) *bxsz))
	    p->falling = TRUE;
	}
    }
}
SetBit(x,y)
     int x,y;
{
  XTileSet(Victim, x, y, 1, 1, WhitePixmap);
}

ClearBit(x,y)
     int x,y;
{
  XTileSet(Victim, x, y, 1, 1, BlackPixmap);
}

SplatBitmap()
{
  Pixmap map;
  
  map = XStorePixmapXY(Info.width, Info.height, bitmap);
  XPixmapPut(Victim, 0, 0, 0, 0,
	             Info.width, Info.height,
	     map, GXcopy, dpy -> dplanes);
  XFlush();
  XFreePixmap(map);
}

rnd(n)
     int n;

{
  long random();

  return (random() % n);
}
  
