/* Copyright (C) 1990, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gdevprn.c */
/* Generic printer support for Ghostscript */
#include "gdevprn.h"
#include "gp.h"
#include "gsprops.h"

/* Define the scratch file name prefix for mktemp */
#define SCRATCH_TEMPLATE gp_scratch_file_name_prefix

/* Internal routine for opening a scratch file */
private int
open_scratch(char *fname, FILE **pfile)
{	*pfile = gp_open_scratch_file(SCRATCH_TEMPLATE, fname, "w+b");
	if ( *pfile == NULL )
	   {	eprintf1("could not open the scratch file %s.\n", fname);
		return -1;
	   }
	return 0;
}

/* ------ Standard device procedures ------ */

/* Macros for casting the pdev argument */
#define ppdev ((gx_device_printer *)pdev)
#define pmemdev ((gx_device_memory *)pdev)
#define pcldev ((gx_device_clist *)pdev)

gx_device_procs prn_std_procs =
  prn_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close);

/* Generic initialization for the printer device. */
/* Specific devices may wish to extend this. */
int
gdev_prn_open(gx_device *pdev)
{	gx_device_memory *mdev =
	  gdev_mem_device_for_bits(pdev->color_info.depth);
	ulong mem_space;
	byte *base;
	if ( mdev == 0 ) return -1;
	memset(ppdev->skip, 0, sizeof(ppdev->skip));
	mem_space = gdev_mem_bitmap_size(pmemdev);
	if ( mem_space >= ppdev->max_bitmap ||
	     mem_space != (uint)mem_space ||	/* too big to allocate */
	     (base = (byte *)gs_malloc((uint)mem_space, 1, "printer buffer")) == 0	/* can't allocate */
	   )
	   {	/* Buffer the image in a command list. */
		uint space;
		for ( space = ppdev->use_buffer_space; ; )
		   {	base = (byte *)gs_malloc(space, 1,
						 "command list buffer");
			if ( base != 0 ) break;
			if ( (space >>= 1) < PRN_BUFFER_SPACE )
				return -1;	/* no hope */
		   }
		ppdev->buffer_space = space;
		pcldev->mdev = *mdev;
		ppdev->buf = base;
		pcldev->data = base;
		pcldev->data_size = space;
		if ( open_scratch(ppdev->ccfname, &ppdev->ccfile) < 0 )
			return -1;
		if ( open_scratch(ppdev->cbfname, &ppdev->cbfile) < 0 )
			return -1;
		pcldev->target = pdev;
		pcldev->cfile = ppdev->ccfile;
		pcldev->bfile = ppdev->cbfile;
		pcldev->bfile_end_pos = 0;
		ppdev->mod_procs = *gs_clist_device.procs;
	   }
	else
	   {	/* Render entirely in memory. */
		ppdev->buffer_space = 0;
		ppdev->ccfile = NULL;
		ppdev->cbfile = NULL;
		pmemdev->base = base;
		ppdev->mod_procs = *mdev->procs;
	   }
	/* Synthesize the procedure vector. */
	/* Rendering operations come from the memory or clist device, */
	/* non-rendering come from the printer device. */
	{ gx_device_procs *pprocs = pdev->procs;
	  ppdev->orig_procs = pprocs;
	  pdev->procs = &ppdev->mod_procs;
#define copy_proc(p) ppdev->mod_procs.p = pprocs->p
	  copy_proc(get_initial_matrix);
	  copy_proc(output_page);
	  copy_proc(close_device);
	  copy_proc(map_rgb_color);
	  copy_proc(map_color_rgb);
	  copy_proc(get_props);
	  copy_proc(put_props);
#undef copy_proc
	}
	ppdev->page_count = 0;
	ppdev->file = 0;
	return (*pdev->procs->open_device)(pdev);
}

/* Added properties for printers */

private gs_prop_item props_prn[] = {
	prop_def("BufferSpace", prt_int),
	prop_def("MaxBitmap", prt_int),
	prop_def("OutputFile", prt_string)
};

/* Get properties.  In addition to the standard properties, */
/* we supply the max bitmap size, buffer size, and output name. */
int
gdev_prn_get_props(gx_device *pdev, gs_prop_item *plist)
{	int start = gx_default_get_props(pdev, plist);
	if ( plist != 0 )
	   {	register gs_prop_item *pi = plist + start;
		memcpy(pi, props_prn, sizeof(props_prn));
		pi[0].value.i = ppdev->use_buffer_space;
		pi[1].value.i = ppdev->max_bitmap;
		pi[2].value.a.p.s = ppdev->fname;
		pi[2].value.a.size = -1;
	   }
	return start + sizeof(props_prn) / sizeof(gs_prop_item);
}

/* Put properties. */
int
gdev_prn_put_props(gx_device *pdev, gs_prop_item *plist, int count)
{	gs_prop_item *known[3];
	props_extract(plist, count, props_prn, 3, known, 0);
	gx_default_put_props(pdev, plist, count);
	if ( known[0] != 0 )
	   {	gs_prop_item *pi = known[0];
		if ( pi->value.i < 10000 )
			pi->status = pv_rangecheck;
		else
			ppdev->use_buffer_space = known[0]->value.i;
	   }
	if ( known[1] != 0 )
		ppdev->max_bitmap = known[1]->value.i;
	if ( known[2] != 0 )
	   {	gs_prop_item *pn = known[2];
		int size = pn->value.a.size;
		if ( size >= sizeof(ppdev->fname) )
			pn->status = pv_limitcheck;
		else
		   {	memcpy(ppdev->fname, pn->value.a.p.s, size);
			pn->value.a.p.s[size] = 0;
		   }
	   }
	return 0;
}

/* Generic routine to send the page to the printer. */
/****** Note that num_copies is currently ignored: this is wrong. ******/
int
gdev_prn_output_page(gx_device *pdev, int num_copies, int flush)
{	int code;

	ppdev->page_count++;
	code = gdev_prn_open_printer(pdev);
	if ( code < 0 ) return code;

	/* print the accumulated page description */
	code = (*ppdev->print_page)(ppdev, ppdev->file);
	if ( code < 0 ) return code;

	code = gdev_prn_close_printer(pdev);
	if ( code < 0 ) return code;

	if ( ppdev->buffer_space ) /* reinitialize clist for writing */
	  code = (*gs_clist_device.procs->output_page)(pdev, num_copies, flush);

	return code;
}

/* Generic closing for the printer device. */
/* Specific devices may wish to extend this. */
int
gdev_prn_close(gx_device *pdev)
{	if ( ppdev->ccfile != NULL )
	   {	fclose(ppdev->ccfile);
		fclose(ppdev->cbfile);
		ppdev->ccfile = NULL;
		unlink(ppdev->ccfname);
		unlink(ppdev->cbfname);
		gs_free((char *)ppdev->buf, (uint)ppdev->buffer_space, 1,
			"command list buffer");
	   }
	if ( ppdev->file != NULL )
	   {	gp_close_printer(ppdev->file, ppdev->fname);
		ppdev->file = NULL;
	   }
	pdev->procs = ppdev->orig_procs;
	return 0;
}

/* Map colors.  This is different from the default, because */
/* printers write black, not white. */

gx_color_index
gdev_prn_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	/* Map values >= 1/2 to 0, < 1/2 to 1. */
	return ((r | g | b) > gx_max_color_value / 2 ?
		(gx_color_index)0 : (gx_color_index)1);
}

int
gdev_prn_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	/* Map 0 to max_value, 1 to 0. */
	prgb[0] = prgb[1] = prgb[2] = -((gx_color_value)color ^ 1);
	return 0;
}

/* ------ Driver services ------ */

/* Open the current page for printing. */
int
gdev_prn_open_printer(gx_device *pdev)
{	char *fname = ppdev->fname;
	char pfname[sizeof(ppdev->fname) + 10];
	if ( strchr(fname, '%') )
	   {	sprintf(pfname, fname, ppdev->page_count);
		fname = pfname;
	   }
	if ( ppdev->file == NULL )
	   {	ppdev->file = gp_open_printer(fname);
		if ( ppdev->file == NULL ) return -1;
	   }
	return 0;
}

/* Get the size of a scan line for copying. */
uint
gdev_prn_bytes_per_scan_line(gx_device_printer *pdev)
{	return gx_device_bytes_per_scan_line((gx_device *)pdev, 0);
}

/* Copy scan lines from the buffer to the printer. */
int
gdev_prn_get_bits(gx_device_printer *pdev, int y, byte *str, uint size, int pad)
{	int swap = (*pdev->procs->get_bits)((gx_device *)pdev, y, str, size, pad);
	uint line_size = gdev_prn_bytes_per_scan_line(pdev);
	/* If monobit, trim off trailing garbage. */
	if ( pad )
	   {	int extra_bits = -(pdev->width * pdev->color_info.depth) & 31;
		if ( extra_bits )
		   {	ulong last_mask = 0xffffffffL << extra_bits;
			byte *line_end = str + line_size - 4;
			int n;
			if ( swap )	/* swap the mask */
			  memswab(&last_mask, &last_mask, sizeof(last_mask));
			for ( n = size; n >= line_size;
			      n -= line_size, line_end += line_size
			    )
				*(ulong *)line_end &= last_mask;
		   }
	   }
	else				/* know !swap */
	   {	byte last_mask =
		  0xff << (-(pdev->width * pdev->color_info.depth) & 7);
		if ( last_mask != 0xff )
		   {	byte *line_end = str + line_size - 1;
			int n;
			for ( n = size; n >= line_size;
			      n -= line_size, line_end += line_size
			    )
				*line_end &= last_mask;
		   }
	   }
	return swap;
}
/* Copy scan lines to a buffer.  Return the number of scan lines, */
/* or <0 if error. */
int
gdev_prn_copy_scan_lines(gx_device_printer *pdev, int y, byte *str, uint size)
{	int code;
	code = gdev_prn_get_bits(pdev, y, str, size, 0);
	if ( code < 0 ) return code;
	return size / gdev_prn_bytes_per_scan_line(pdev);
}

/* Close the current page. */
int
gdev_prn_close_printer(gx_device *pdev)
{	if ( strchr(ppdev->fname, '%') )	/* file per page */
	   {	gp_close_printer(ppdev->file, ppdev->fname);
		ppdev->file = NULL;
	   }
	return 0;
}
