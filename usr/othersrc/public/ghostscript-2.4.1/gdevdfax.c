/*
 *	Copyright 1992 DigiBoard, Inc. All rights reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted.
 * This software is provided "as is" without express or implied warranty.
 */

/* gdevdfax.c */
/* DigiBoard, Inc. DigiFAX driver for Ghostscript. */

#include "gdevprn.h"
#include "gdevdfg3.h"

#ifdef __PROTOTYPES__
#	define PROTO_ARGS(X)	X
#else
#	define PROTO_ARGS(X)	()
#endif

/*************************************************************************
 *	Format of page header in PC Research "group_3" FAX file
 *
 *	Note that all "shorts" are stored LSB in lowest byte address
 *************************************************************************/
typedef struct
{
	char	magic[24];
} FAXBREAK;

#define	FAXMAGIC	"\000PC Research, Inc\000\000\000\000\000\000"
	
typedef struct
{
/*24*/	short	pages;		/* Number of pages in file */
/*26*/	short	pagenum;
/*28*/	char	msbfirst;	/* Coding used in file for codewords */
				/* MUST be set to 1, meaning the codewords */
				/* are filled starting with the most */
				/* significant bit */
/*29*/	char	hires;		/* Set to 1 for Hi resolution, else 0 */
/*30*/	char	dirty;		/* File may contain T.4 errors if 1, else 0 */
				/* e.g. it was received by FAX, not created */
				/* by a program */
/*31*/	char	reservedc;
/*32*/	short	reserved[12];
} FAXINFO;

#define	OFFSET_PAGES	24

typedef struct
{
	char	jt0;
	char	jthires;	/* Set to 0x40 for Hi resolution, else 0 */
	char	jt2;
	char	jt3;
	char	jt4;
	char	jt5;
	char	jt6;
	char	jt7;
} JTINFO;

/*
 *	This appears before each page in a FAX file
 */
typedef struct
{
	FAXBREAK	id;
	FAXINFO		info;
	JTINFO		jtinfo;
} FAXHDR;

/**********************************************************************/
/*
 *	Generic fax output library
 */
/**********************************************************************/

typedef struct
{
	FILE		*fp;
	int		fax_byte;
	int		fax_weight;
	int		pages;
} FAXOUT;

private FAXOUT	*faxout_open PROTO_ARGS((char *));
private FAXOUT	*faxout_open_fp PROTO_ARGS((FILE *));
private int	faxout_begin_page PROTO_ARGS((FAXOUT *, int));
private int	faxout_eolcode PROTO_ARGS((FAXOUT *));
private int	faxout_end_page PROTO_ARGS((FAXOUT *));
private int	faxout_close PROTO_ARGS((FAXOUT *));
private unsigned short fax_ushort PROTO_ARGS((unsigned short));
private void	tofax();

private void putwhitespan();
private void putblackspan();
private void putcode();
private void puteol();
private void putbit();
private void flushbits();

/*************************************************************************
	Redefine the device descriptor
 *************************************************************************/
struct gx_device_dfax_s {
	gx_device_common;
	gx_prn_device_common;
	FAXOUT	*fax;
};
typedef struct gx_device_dfax_s gx_device_dfax;

/*	Too bad the prn_device() macro doesn't let you specify the
 *	size of the gx_device structure -- we have to repeat it here
 */
#define generic_prn_device(Struct, procs, dev_name, width_10ths, height_10ths, x_dpi, y_dpi, l_margin, b_margin, r_margin, t_margin, color_bits, print_page) {\
	sizeof(Struct),\
	&procs,\
	dev_name,\
	(int)((long)width_10ths * x_dpi / 10),	/* width */\
	(int)((long)height_10ths * y_dpi / 10),	/* height */\
	x_dpi,\
	y_dpi,\
	l_margin, b_margin, r_margin, t_margin,\
	   {	(color_bits > 1 ? 3 : 1),	/* num_components */\
		((color_bits > 1) & (color_bits < 8) ? 8 : color_bits),	/* depth */\
		(color_bits >= 8 ? 255 : 1),	/* max_gray */\
		(color_bits >= 8 ? 255 : color_bits > 1 ? 1 : 0),	/* max_rgb */\
		(color_bits >= 8 ? 5 : 2),	/* dither_gray */\
		(color_bits >= 8 ? 5 : color_bits > 1 ? 2 : 0),	/* dither_rgb */\
	   },\
	0,		/* not initialized yet */\
	  { 0 },	/* skip */\
	print_page,\
	PRN_MAX_BITMAP,\
	PRN_BUFFER_SPACE,\
	  { 0 }		/* fname */\
}

/* The device descriptor */
#define X_DPI 204
#define Y_DPI 196
#define LINE_SIZE ((X_DPI * 85 / 10 + 7) / 8)	/* bytes per line */

private dev_proc_open_device(dfax_prn_open);
private dev_proc_print_page(dfax_print_page);
private dev_proc_close_device(dfax_prn_close);

gx_device_procs dfax_std_procs =
	prn_procs(dfax_prn_open, gdev_prn_output_page, dfax_prn_close);

gx_device_dfax gs_dfaxhigh_device =
	generic_prn_device(
		gx_device_dfax,
		dfax_std_procs,
		"dfaxhigh",
		85,			/* width_10ths, 8.5" */
		110,			/* height_10ths, 11" */
		X_DPI, Y_DPI,
		0,0,0,0,		/* margins */
		1,
		dfax_print_page
	);

gx_device_dfax gs_dfaxlow_device =
	generic_prn_device(
		gx_device_dfax,
		dfax_std_procs,
		"dfaxlow",
		85,			/* width_10ths, 8.5" */
		110,			/* height_10ths, 11" */
		X_DPI, Y_DPI/2,
		0,0,0,0,		/* margins */
		1,
		dfax_print_page
	);

/*************************************************************************
	Driver entry points
 *************************************************************************/

private int
dfax_prn_open(gx_device *pdev)
{
	gx_device_dfax	*ddev = (gx_device_dfax *) pdev;
	int		rc;

	rc = gdev_prn_open(pdev);
	ddev->fax = faxout_open_fp(ddev->file);
	return (rc);
}

private int
dfax_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
	gx_device_dfax	*ddev = (gx_device_dfax *) pdev;
	char		data[LINE_SIZE + 4];
	int		lnum;
	int		line_size;
	FAXOUT		*fax = ddev->fax;
 
	/* For some odd reason, the file isn't open until now */
	fax->fp = prn_stream;	
	faxout_begin_page(fax, ddev->y_pixels_per_inch > 120);
	
	line_size = gdev_mem_bytes_per_scan_line( (gx_device *) pdev);
	for ( lnum = 0; lnum < pdev->height; lnum++ )
	{
		gdev_prn_copy_scan_lines(pdev, lnum, (byte *)data, line_size);
		tofax(fax, data, 1728);
	}
	faxout_end_page(fax);
	return 0;
}

private int
dfax_prn_close(gx_device *pdev)
{
	gx_device_dfax	*ddev = (gx_device_dfax *) pdev;
	int		rc;
	FAXOUT		*fax = ddev->fax;
	unsigned short	 p = fax_ushort(fax->pages);

	if (fax->fp)
	{
		fflush(fax->fp);
		if (fseek(fax->fp, (long) OFFSET_PAGES, 0) == 0)
			fwrite((char*)&p, sizeof(p), 1, fax->fp);
	}
	rc = gdev_prn_close(pdev);
	return(rc);
}

/*************************************************************************
	Internal routines
 *************************************************************************/

/************************Coding FAX Routines*************************/
/*
 *	faxp = faxout_open(filename);
 *	faxp = faxout_open_fp(fp);
 *	faxout_begin_page(faxp, resolution);
 *	for(;;) tofax(faxp, linebuf);
 *	faxout_end_page(faxp);
 *	faxout_close(faxp);
 */

private FAXOUT	*
faxout_open_fp(FILE *fp)
{
	register FAXOUT	*faxp;

	faxp = (FAXOUT *) malloc(sizeof(*faxp));

	faxp->fp = fp;
	faxp->fax_byte = 0;
	faxp->fax_weight = 0x80;
	faxp->pages = 0;

	return (faxp);
}

private FAXOUT	*
faxout_open(char *filename)
{
	register FILE	*fp;
	register FAXOUT	*faxp;

	if (filename)
		fp = fopen(filename, "w");
	else
		fp = stdout;
	if (!fp) return (NULL);

	return(faxout_open_fp(fp));
}

private int
faxout_begin_page(FAXOUT *faxp, int resolution)
{
	FAXHDR	hdr;

	memset(&hdr, 0, sizeof(hdr));
	memcpy(hdr.id.magic, FAXMAGIC, sizeof(hdr.id.magic));
	hdr.info.pagenum = fax_ushort(++faxp->pages);
	hdr.info.msbfirst = 1;
	hdr.info.hires = resolution;
	hdr.jtinfo.jthires = resolution ? 0x40 : 0;
	fwrite((char*)&hdr, sizeof(hdr), 1, faxp->fp);

	puteol(faxp);

	return (0);
}

private int
faxout_end_page(FAXOUT *faxp)
{
	flushbits(faxp);
	puteol(faxp);
	puteol(faxp);
	puteol(faxp);
	puteol(faxp);
	puteol(faxp);
	flushbits(faxp);
	return (0);
}

private int
faxout_close(FAXOUT *faxp)
{
	unsigned short p = fax_ushort(faxp->pages);

	fflush(faxp->fp);
	if (fseek(faxp->fp, (long) OFFSET_PAGES, 0) == 0)
		fwrite((char*)&p, sizeof(p), 1, faxp->fp);
	fclose(faxp->fp);
	free(faxp);

	return (0);
};

private unsigned short
fax_ushort(unsigned short v)
{
	static unsigned short	x = 0x1122;
	static unsigned short	*xp = &x;
	
	if ( *((unsigned char *)xp) == 0x22)
		return (v);
	else
		return ( ((v>>8)&255) + (v<<8) );
}

private unsigned char	b_run_tbl[8][256] =
{
  {	/* START BIT 0 */
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
  },
  {	/* START BIT 1 */
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
    0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 0, 0, 1, 2, 
  },
  {	/* START BIT 2 */
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
    0, 0, 0, 0, 1, 1, 2, 3, 0, 0, 0, 0, 1, 1, 2, 3, 
  },
  {	/* START BIT 3 */
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4, 
  },
  {	/* START BIT 4 */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 
  },
  {	/* START BIT 5 */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6, 
  },
  {	/* START BIT 6 */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 6, 7, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 6, 7, 
  },
  {	/* START BIT 7 */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
    4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 7, 8, 
  },
};

private unsigned char	w_run_tbl[8][256] =
{
  {	/* START BIT 0 */
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
    1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 
  },
  {	/* START BIT 1 */
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
    2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 2, 1, 0, 0, 
  },
  {	/* START BIT 2 */
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
    3, 2, 1, 1, 0, 0, 0, 0, 3, 2, 1, 1, 0, 0, 0, 0, 
  },
  {	/* START BIT 3 */
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
    4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
  },
  {	/* START BIT 4 */
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    5, 4, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  },
  {	/* START BIT 5 */
    6, 5, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    6, 5, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    6, 5, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    6, 5, 4, 4, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  },
  {	/* START BIT 6 */
    7, 6, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    7, 6, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  },
  {	/* START BIT 7 */
    8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
  },
};

/*
 *	Macros to use tables in findrun.c
 *	Input is *p, bit
 *	Output is rl, *p, bit
 */

#define	find_black_run() \
for (rl = 0;;) \
{ \
	if (run = b_run_tbl[bit][*p]) \
	{ \
		rl += run; \
		bit -= run; \
		if (bit < 0 && ++p < ep) { bit=7; continue;} \
	} \
	break; \
}

#define	find_white_run() \
for (rl = 0;;) \
{ \
	if (run = w_run_tbl[bit][*p]) \
	{ \
		rl += run; \
		bit -= run; \
		if (bit < 0 && ++p < ep) { bit=7; continue;} \
	} \
	break; \
}

private void
tofax(FAXOUT *faxp, unsigned char *p)
{
	unsigned char		*ep;
	register int		bit;
	register int		run;
	register int		rl;

	ep = p + 1728/8;
	bit = 7;
	for (;;)
	{
		find_white_run();
		putwhitespan(faxp, rl);
		if (p >= ep) break;

		find_black_run();
		putblackspan(faxp, rl);
		if (p >= ep) break;
	}
	puteol(faxp);
}


/*************************************************************************
	The rest of this file is a FAX encoding algorithm
	derived from pbmplus.  It is not the normal DigiFAX algorithm.
	The following copyright applies.
**
** Copyright (C) 1989 by Paul Haeberli <paul@manray.sgi.com>.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
 *************************************************************************/

private void
putwhitespan(register FAXOUT *faxp, int c)
{
    register int tpos;
    register tableentry* te;

    if(c>=64) {
	tpos = (c/64)-1;
	te = mwtable+tpos;
	c -= te->count;
	putcode(faxp, te);
    }
    tpos = c;
    te = twtable+tpos;
    putcode(faxp, te);
}

private void
putblackspan(register FAXOUT *faxp, int c)
{
    register int tpos;
    register tableentry* te;

    if(c>=64) {
	tpos = (c/64)-1;
	te = mbtable+tpos;
	c -= te->count;
	putcode(faxp, te);
    }
    tpos = c;
    te = tbtable+tpos;
    putcode(faxp, te);
}

private void
putcode(register FAXOUT *faxp, tableentry *te)
{
    register unsigned int mask;
    register int code;

    mask = 1<<(te->length-1);
    code = te->code;
    while(mask) {
 	if(code&mask)
	    putbit(faxp, 1);
	else
	    putbit(faxp, 0);
	mask >>= 1;
    }

}

private void
puteol(register FAXOUT *faxp)
{
    register int i;

    for(i=0; i<11; ++i)
	putbit(faxp, 0);
    putbit(faxp, 1);
}

private void
putbit(register FAXOUT *faxp, int d)
{
    if(d) 
	faxp->fax_byte = faxp->fax_byte|faxp->fax_weight;
    faxp->fax_weight = faxp->fax_weight>>1;
    if((faxp->fax_weight&0xff) == 0) {
	putc(faxp->fax_byte, faxp->fp);
	faxp->fax_byte = 0;
	faxp->fax_weight = 0x80;
    }
}

private void
flushbits(register FAXOUT *faxp)
{
    if (faxp->fax_weight != 0x80) {
	putc(faxp->fax_byte, faxp->fp);
	faxp->fax_byte = 0;
	faxp->fax_weight = 0x80;
    }
}
