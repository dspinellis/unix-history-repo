#ifndef lint
static char *rcsid_bitpix_c = "$Header: bitpix.c,v 10.1 86/11/29 13:50:35 jg Rel $";
#endif	lint
    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */

/* Routines to cache bitmaps and pixmaps in the frame buffer memory:
 *
 *	StoreBitmap	Creates a bitmap
 *	FreeBitmap	Frees the storage taken by a bitmap
 *	CharBitmap	Creates a bitmap from a font character
 *	StorePixmap	Creates a pixmap
 *	FreePixmap	Frees the storage taken by a pixmap
 *	MakePixmap	Create a pixmap from a bitmap
 *	PixmapSave	Save a region of the screen
 *	PixmapGet	Read a region of the screen
 *
 */

#include "Xapollo.h"
#include <errno.h>

extern int errno;
extern struct Scr Screen;
status_$t status;

char *Xalloc();
PIXMAP *MakePixmap();

BITMAP
*StoreBitmap (width, height, data)
	int width, height;
	char *data;
{
  register BITMAP *bm;
  int size;
  
  bm = make_bitmap(data, width, height, true);
  return (bm);
}

FreeBitmap (bitmap)
     register BITMAP *bitmap;
{

  if( bitmap->kind == (int)memory_bitmap )
    free ((caddr_t) bitmap->data);
  else
    if( bitmap->kind == (int)apollo_bitmap )
      gpr_$deallocate_bitmap((gpr_$bitmap_desc_t)bitmap->data, status);
  /* need to free attribute block as well... */
  free ((caddr_t) bitmap);
  
}

BITMAP
*CharBitmap (c, font)
     unsigned c;
     register FONT *font;
{
  int width;
  register BITMAP *bm;


  if (c < font->first || c > font->last) {
    fprintf(stderr, "CharBitmap: font: %s char: %x\n", font->name, c);
    errno = EINVAL;
    return (NULL);
  }
  
  if (font->fixed)
    width = font->avg_width;
  else
    /* this won't work for Apollo-style fonts */
    {
      FontPriv * fp = (FontPriv *)font->data;
      width = fp->widths[c];
    }
  if (width == 0) {
    errno = EINVAL;
    fprintf(stderr, "CharBitmap: font: %s char: %x\n", font->name, c);
    return (NULL);
  }
  bm = (BITMAP *) Xalloc (sizeof (BITMAP));
  bm->width = width;
  bm->height = font->height;
  bm->refcnt = 1;
  if ((bm->data = 
       (caddr_t) malloc (BitmapSize(width, bm->height))) == NULL) {
	 free ((caddr_t) bm);
	 errno = ENOMEM;
	 return (NULL);
       }
  
  CopyText ((caddr_t) &c, 1, font, bm);
  return (bm);
}

/*ARGSUSED*/
PIXMAP
*StorePixmap (width, height, format, data)
     int width, height, format;
     char *data;
{
  register PIXMAP *pm;

  if (Screen.depth == 1) {
    register BITMAP *bm;
    
    bm = (BITMAP *) StoreBitmap(width, height, data);
    if (bm == NULL)
      return (NULL);
    bm->refcnt = 0;
    if ((pm = MakePixmap(bm, 1, 0)) == NULL) {
      FreeBitmap(bm);
      return (NULL);
    }
  }
  else if (Screen.depth <= 8) {
    char *     newdata;
    int         size;

    pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
    pm->width = width;
    pm->height = height;
    pm->refcnt = 1;
    switch (format) {
    case XYFormat:
      size = XYPixmapSize(width, height, Screen.depth);
      pm->kind = XYColorPixmap;
      break;
    case ZFormat:
      size = BZPixmapSize(width, height);
      if (width&1)
	size += height;
      pm->kind = ZColorPixmap;
      break;
    }
    newdata = (char *) Xalloc(size);
    if (width&1) {
      register int i;
      register char * old = data, *new = newdata;

      for (i = 0; i < height; i++) { 
	bcopy(old, new, width);
	old += width;
	new += width + 1;
      }
    } else
      bcopy(data, newdata, size);
    pm->data = newdata;
  }

  return (pm);
}

FreePixmap (pixmap)
     register PIXMAP *pixmap;
{
  register BITMAP *bm;

  switch (pixmap->kind) {
  case BitmapPixmap:
    bm = PDATA(pixmap);
    if (--bm->refcnt == 0)
      FreeBitmap(bm);
    	break;
  case ZColorPixmap:
  case XYColorPixmap:
    free((caddr_t)pixmap->data);
    break;
  case ConstantPixmap:
    return;
  }
  free((caddr_t) pixmap);
}

PIXMAP constpix0 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) 0};
PIXMAP constpix1 = {1, 1, 1, 1, ConstantPixmap, (caddr_t) 1};

PIXMAP
*MakePixmap (xymask, fore, back)
     register BITMAP *xymask;
     int fore, back;
{
  register PIXMAP *pm = NULL;


  if (xymask == NULL) {
    if (Screen.depth == 1) {
      if (fore & 1)
	pm = &constpix1;
      else
	pm = &constpix0;
      pm->refcnt++;
    }
    else if (Screen.depth <= 8) {
      static PIXMAP *constpm[256];

      if (constpm[fore & 0xFF] == 0) {
	constpm[fore & 0xFF] = pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
	pm->width = 1;
	pm->height = 1;
	pm->refcnt = 1;
	pm->tile = CanBeTiled;
	pm->kind = ConstantPixmap;
	pm->data = (caddr_t) fore;
      }
      else {
	pm = constpm[fore & 0xFF];
	pm->refcnt++;
      }
    }
    return (pm);
  }

  pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
  pm->width = xymask->width;
  pm->height = xymask->height;
  pm->refcnt = 1;
  xymask->refcnt++;
  pm->kind = BitmapPixmap;
  pm->data = (caddr_t) xymask;
  pm->tile = CanBeTiled;
  /* save a bit to indicate if we have to invert the source */

  if ((Screen.depth == 1) && (back & 1))
    pm->kind |= InvertFlag;

  return (pm);
}

PIXMAP
*PixmapSave (srcx, srcy, width, height)
     int srcx, srcy, width, height;
{
  gpr_$position_t dest;
  gpr_$window_t sou;
  PIXMAP     *pm = NULL;

  if (Screen.depth == 1) 
    {
      register BITMAP *bm;

      bm = make_bitmap(NULL, width, height, false);
      /* should check for NULL */
      bm->refcnt = 0;

      CheckCursor(srcx, srcy, width, height);
      sou.x_coord = srcx;
      sou.y_coord = srcy;
      sou.x_size = width;
      sou.y_size = height;
      dest.x_coord = 0;
      dest.y_coord = 0;
      gpr_$set_bitmap((gpr_$bitmap_desc_t)bm->data, status);
      gpr_$pixel_blt(Screen.bm, sou, dest, status);
      gpr_$set_bitmap(Screen.bm, status);
      RestoreCursor();

      if ((pm = MakePixmap(bm, 1, 0)) == 0) {
	FreeBitmap(bm);
	return (NULL);
      }
    }
  else if (Screen.depth <= 8) {
    int sz = BZPixmapSize(width, height);
    
    if (width&1)
      sz += height;
    pm = (PIXMAP *) Xalloc(sizeof(PIXMAP));
    pm->width = width;
    pm->height = height;
    pm->refcnt = 1;
    pm->kind = ZColorPixmap;
    if ((pm->data =
	 (caddr_t) malloc(sz)) == NULL) {
	   free((caddr_t) pm);
	   return (NULL);
	 }               
  }
  
  return (pm);
}

/*ARGSUSED*/
PixmapGet (srcx, srcy, width, height, client, format, swapit)
	int srcx, srcy, width, height, client, format;
{
    PIXMAP     *pm;
    BITMAP     *bm;

dprintf( stderr, "pixmapget: %d,%d\n", srcx, srcy );
                                   
    /* !! this only works if PixmapSave returns a memory_bitmap */
    /* hence, it doesn't work now because all bitmaps are apollo bitmaps */
    pm = PixmapSave(srcx, srcy, width, height);
    switch (pm->kind) {
    case BitmapPixmap:{
	    int         size = BitmapSize(width, height);

	    bm = (BITMAP *) pm->data;
	    if (swapit)
		Swap_shorts((short *) bm->data, size >> 1);
	    InvertPixelOrder((short *) bm->data, size >> 1);
	    Write(client, bm->data, size);
	    /* Pad amount written to 32-bit boundary - Ahem! */
	    if (size%4) {
		Write(client, bm->data, 4 - (size%4));
	    }
	}
	break;
    case ZColorPixmap:{
	    switch (format) {
	    case XYFormat:{
		    caddr_t     newdata;
		    int size;

		    size = XYPixmapSize(width, height, Screen.depth);
		    newdata = (caddr_t) malloc(size);
		    if (newdata) {
			ZtoXY(width, height, Screen.depth, pm->data, newdata);
			free(pm->data);
			pm->data = newdata;
			if (swapit)
			    Swap_shorts((short *) pm->data, size >> 1);
			InvertPixelOrder((short *) pm->data, size >> 1);
		    }
		    Write(client, pm->data, size);
	            /* Pad amount written to 32-bit boundary - Ahem! */
	            if (size%4) {
		        Write(client, pm->data, 4 - (size%4));
	            }
		}
		break;
	    case ZFormat: {
		int size = BZPixmapSize(width, height);

		if (width&1) {
		    register int i;
		    register char *old = pm->data;

		    for (i = 0; i < height; i++) {
			Write(client, old, width);
			old += width + 1;
		    }
		} else {
		    Write(client, pm->data, size);
		}
	        /* Pad amount written to 32-bit boundary - Ahem! */
	        if (size%4) {
		    Write(client, pm->data, 4 - (size%4));
	        }
	    }
	    break;
	}
	break;
	}
    case XYColorPixmap:
#ifdef apollo_color
#else
	/*NOTREACHED*/
#endif apollo_color
	break;
    }
    FreePixmap(pm);

}

static ZtoXY(w, h, d, old, new)
int	w, h, d;
caddr_t	old;
u_char *new;
{
#ifdef apollo_color
    unsigned mask = 1;
    struct pixrect *New, *Old;

    Old = mem_point(w, h, d, old);
    while (d--) {
	register int y;

	New = mem_point(w, h, 1, new);
	new += BitmapSize(w, h);
	for (y = 0; y < h; y++) {
	    register int x;

	    for (x = 0; x < w; x++) {
		pr_put(New, x, y, (pr_get(Old, x, y) & mask));
	    }
	}
	pr_destroy(New);
	mask <<= 1;
    }
#endif apollo_color
}
