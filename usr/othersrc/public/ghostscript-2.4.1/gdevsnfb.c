/* Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gdevsnfb.c */
/* Sony News frame buffer driver for GhostScript */
#include "gdevprn.h"
/*#include <sys/types.h> problems with ushort! */
typedef	char *	caddr_t;
typedef	long	off_t;
#include <sys/uio.h>
#include <newsiop/framebuf.h>

/* The device descriptor */
private dev_proc_open_device(sonyfb_open);
private dev_proc_output_page(sonyfb_output_page);
private dev_proc_close_device(sonyfb_close);
private gx_device_procs sonyfb_procs =
  prn_procs(sonyfb_open, sonyfb_output_page, sonyfb_close);
gx_device_printer gs_sonyfb_device =
  prn_device(sonyfb_procs, "sonyfb",
	102.4,				/* width_10ths */
	103.2,				/* height_10ths */
	100,				/* x_dpi */
	100,				/* y_dpi */
	0,0,0,0,			/* margins */
	1, 0);

private int fb_file = -1;
sPrimRect prect;

private int
sonyfb_open(gx_device *dev)
{
  sScrType stype;

  if(fb_file < 0)
    if((fb_file = open("/dev/fb", 2)) < 0)
      perror("open failed");
    else
      if(ioctl(fb_file, FBIOCGETSCRTYPE, &stype) < 0)
	perror("ioctl failed");
      else
	prect.rect = stype.visiblerect;

  return gdev_prn_open(dev);
}

private int
sonyfb_close(gx_device *dev)
{
  if(fb_file >= 0)
    {
      close(fb_file);
      fb_file = -1;
    }
  return gdev_prn_close(dev);
}

#define FRAME_WIDTH	1024

/* Send the page to the printer. */
private int
sonyfb_output_page(gx_device *dev, int num_copies, int flush)
{
  int l, i, byte_width, height;
  unsigned char *bm, *fbs, *fb;

  byte_width = (dev->width + 7) / 8;
  height = dev->height;
  bm	 = (typeof(bm))prn_dev->mem.base;

  prect.refPoint.x = 0;
  prect.refPoint.y = 0;
  prect.ptnRect = prect.rect;
  
  prect.ptnBM.type  = BM_MEM;
  prect.ptnBM.depth = 1;
  prect.ptnBM.width = (byte_width + 1) / 2;
  prect.ptnBM.rect.origin.x = 0;
  prect.ptnBM.rect.origin.y = 0;
  prect.ptnBM.rect.extent.x = byte_width * 8; /* width in 16bit words */
  prect.ptnBM.rect.extent.y = height;
  prect.ptnBM.base = (typeof(prect.ptnBM.base))bm;
  
  prect.fore_color = 1;
  prect.aux_color = 0;
  prect.planemask = FB_PLANEALL;
  prect.transp = 0;
  prect.func = BF_S;
  prect.clip = prect.rect;
  prect.drawBM.type  = BM_FB;
  prect.drawBM.depth = 1;
  prect.drawBM.width = (prect.rect.extent.x + 15) / 16;
  prect.drawBM.rect = prect.rect;
  prect.drawBM.base = 0;
  
  if(ioctl(fb_file, FBIOCRECTANGLE, &prect) < 0)
    perror("rect ioctl failed");

  return 0;
}
