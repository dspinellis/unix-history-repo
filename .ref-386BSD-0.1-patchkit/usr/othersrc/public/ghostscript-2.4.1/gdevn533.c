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

/* gdevn533.c */
/* Sony NWP-533 driver for GhostScript */
#include "gdevprn.h"
#include <sys/file.h>
#include <sys/ioctl.h>
#include <newsiop/lbp.h>

/* The device descriptor */
private dev_proc_open_device(nwp533_open);
private dev_proc_output_page(nwp533_output_page);
private dev_proc_close_device(nwp533_close);
private gx_device_procs nwp533_procs =
  prn_procs(nwp533_open, nwp533_output_page, nwp533_close);
gx_device_printer gs_nwp533_device =
  prn_device(nwp533_procs, "nwp533",
	78.4,				/* width_10ths */
	112.9,				/* height_10ths */
	400,				/* x_dpi */
	400,				/* y_dpi */
	0,0,0,0,			/* margins */
	1, 0);

private int printer_file = -1;

/* return True if should retry - False if should quit */
private int
analyze_error()
{
  struct lbp_stat status;
  char message[80];
  char *detail, *old_detail;
  int waiting;
  int retry_after_return;

  if(ioctl(printer_file, LBIOCRESET, 0) < 0)
    return (0 == 1);
  if(ioctl(printer_file, LBIOCSTATUS, &status) < 0)
    return (0 == 1);
  sprintf(message, "printer status: 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x 0x%02x",
	  status.stat[0], status.stat[1], status.stat[2], 
	  status.stat[3], status.stat[4], status.stat[5]);
  perror(message);

  old_detail = detail = NULL;
  waiting = retry_after_return = (1 == 1); /* True */
  do
    {
      if(status.stat[0] & (ST0_CALL | ST0_REPRINT_REQ | ST0_WAIT | ST0_PAUSE))
	{
	  if(status.stat[1] & ST1_NO_CARTRIGE)/* mispelled? */
	    detail = "No cartridge - waiting";
	  else if(status.stat[1] & ST1_NO_PAPER)
	    detail = "Out of paper - waiting";
	  else if(status.stat[1] & ST1_JAM)
	    detail = "Paper jam - waiting";
	  else if(status.stat[1] & ST1_OPEN)
	    detail = "Door open - waiting";
	  else if(status.stat[1] & ST1_TEST)
	    detail = "Test printing - waiting";
	  else
	    {
	      retry_after_return = (1 == 0);
	      waiting = (1 == 0);
	      detail = "Please analyze status bytes";
	    }
	}
      else
	waiting = (0 == 1);
      if(detail != NULL && detail != old_detail)
	{
	  perror(detail);
	  old_detail = detail;
	}
      if(waiting)
	{
	  ioctl(1, LBIOCRESET, 0);
	  sleep(5);
	  ioctl(1, LBIOCSTATUS, &status);
	}
    }
  while(waiting);
  return retry_after_return;
}

private int
nwp533_open(gx_device *dev)
{
  fprintf(stderr, "in nwp533 open\n");

  if(printer_file < 0)
    if((printer_file = open("/dev/lbp", O_WRONLY)) < 0)
      return printer_file;

  return gdev_prn_open(dev);
}

private int
nwp533_close(gx_device *dev)
{
  fprintf(stderr, "in nwp533 close\n");

  if(printer_file >= 0)
    {
      close(printer_file);
      printer_file = -1;
    }

  return gdev_prn_close(dev);
}

/* Send the page to the printer. */
private int
nwp533_output_page(gx_device *dev, int num_copies, int flush)
{
  fprintf(stderr, "in nwp533 output page [%d, %d]\n",
	  dev->width, dev->height);

 restart:
  if(ioctl(printer_file, LBIOCSTOP, 0) < 0)
    {
      if(analyze_error())
	goto restart;
      perror("Waiting for device");
      return -1;
    }
  lseek(printer_file, 0, 0);

  if(write(printer_file, prn_dev->mem.base,
	   (dev->width * dev->height) / 8) !=
     (dev->width * dev->height) / 8)
    {
      perror("Writting to output");
      return -1;
    }
 retry:
  if(ioctl(printer_file, LBIOCSTART, 0) < 0)
    {
      if(analyze_error())
	goto retry;
      perror("Starting print");
      return -1;
    }

  return 0;
}
