/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* gdevgif.c */
/* GIF output device for Ghostscript. */
#include "gdevprn.h"
#include "gserrors.h"			/* REALLY? */
#include "gdevpccm.h"

/* Thanks to Phil Conrad for donating the original version */
/* of these drivers to Aladdin Enterprises. */

/* ------ The device descriptors ------ */

/*
 * Standard U.S. page width and height.  A4 paper is 8.4" x 11.7".
 */
#define WIDTH_10THS 85
#define HEIGHT_10THS 110

/*
 * Default X and Y resolution.
 */
#define X_DPI 72
#define Y_DPI 72

/* The same print_page routine currently serves for */
/* both monochrome and color. */
private dev_proc_print_page(gif_print_page);

/* Monochrome. */

gx_device_printer gs_gifmono_device =
  prn_device(prn_std_procs, "gifmono",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	1, gif_print_page);

/* Chunky 8-bit (SuperVGA-style) color. */
/* (Uses a fixed palette of 3,3,2 bits.) */

private gx_device_procs gif8_procs =
  prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
    pc_8bit_map_rgb_color, pc_8bit_map_color_rgb);
gx_device_printer gs_gif8_device =
  prn_device(gif8_procs, "gif8",
	WIDTH_10THS, HEIGHT_10THS,
	X_DPI, Y_DPI,
	0,0,0,0,			/* margins */
	8, gif_print_page);

/* ------ Private definitions ------ */

typedef struct gif_header_s {
	byte	signature[3];	/* magic number == 'GIF' */
	byte	version[3];	/* version # '87a' or '89a' */
	ushort	width;		/* screen width */
	ushort	height;		/* screen height */

/*	struct	{		/* bit structure of flags */
/*	unsigned globalcolor:1;	/* global color table flag - MSB*/
#define globalcolor_shift 7
/*	unsigned colorres:3;	/* bits/color */
#define colorres_shift 4
/*	unsigned sort:1;		/* color table sorted */
#define sort_shift 3
/*	unsigned colorsize:3;	/* 2^colorsize bytes in color table -LSB */
#define colorsize_shift 0
/*	} flags;		*/

	byte 	flags;
	byte	background;	/* background color index */
	byte	aspect;		/* pixel aspect ratio */
				/* ratio = (aspect + 15) / 64 */
} gif_header;

typedef struct image_descriptor_s {
/*	byte	separator;	/* image separator == 0x2c */
	ushort	left_pos;	/* image left pos (pixels) */
	ushort	top_pos;	/* image top  pos (pixels) */
	ushort	width;		/* image width    (pixels) */
	ushort	height;		/* image height   (pixels) */

/*	struct	{		*/
/*	unsigned localcolor:1;	/* local color table flag */
/*	unsigned interlace:1;	/* image interlaced  0=no */
/*	unsigned sort:1;	/* color table sorted 0=no*/
/*	unsigned resv:2;	*/
/*	unsigned localsize:3;	/* 2^localsize+1 = color table size */
/*	} flags;		*/

	byte	flags;
} image_descriptor;

/* State for reading bits from image. */
typedef struct pixel_reader_s {
	byte *next;
	int bits_left;
	uint bit_buffer;	/* low-order bits_left bits are valid */
} pixel_reader;

/********************************************************/
/* LZW routines are based on:				*/
/* Dr. Dobbs Journal --- Oct. 1989. 			*/
/* Article on LZW Data Compression by Mark R. Nelson 	*/
/********************************************************/

#define MAX_BITS 12		/* this is max for GIF. */

#define TABLE_SIZE 5021		/* this is max for 12-bit codes */

/* State of LZW encoder */
typedef struct code_entry_s {
	int	code_value;
	ushort	prefix_code;
	byte	append_character;
} code_entry;
typedef struct lzw_encoder_s {
	int	bits;
	ushort	Max_Code;
	ushort	Clear_code;
	ushort	next_code;
	FILE	*file;
	code_entry	*table;
	ushort	string_code;
	/* State of output buffer */
	byte	output_bit_buffer;
	int	output_bit_count;	/* # of valid low-order bits */
					/* (between 0 and 7) in buffer */
	uint	byte_count;
	byte	gif_buffer[260];
} lzw_encoder;

/* Initialize LZW encoder */
private void lzw_set_bits(P2(register lzw_encoder _ss *, int));
private void lzw_reset(P1(register lzw_encoder _ss *));
private int
lzw_init(register lzw_encoder _ss *pe, int bits, FILE *file)
{	lzw_set_bits(pe, bits);
	pe->Clear_code = (1 << bits);
	pe->file = file;
	pe->byte_count = 1;
	pe->output_bit_count = 0;
	pe->output_bit_buffer = 0;
	pe->table = (code_entry *)gs_malloc(TABLE_SIZE, sizeof(code_entry), "GIF code table");

	if ( pe->table == 0 )
		return gs_error_VMerror;	/* can't allocate buffers */

	lzw_reset(pe);
	pe->string_code = 0;
	return 0;
}
/* Establish the width of the code in bits */
private void
lzw_set_bits(register lzw_encoder _ss *pe, int bits)
{	pe->bits = bits;
	pe->Max_Code = (1 << (bits+1)) - 1;
}
/* Reset the encoding table */
private void
lzw_reset(register lzw_encoder _ss *pe)
{	int index;
	for ( index = 0; index < TABLE_SIZE; index++ )
		pe->table[index].code_value = -1;
	pe->next_code = pe->Clear_code + 2;
}

/* Put out (data) of length (bits) to GIF buffer */
private void
lzw_putc(register lzw_encoder _ss *pe, uint data)
{	int bits = pe->bits + 1;	/* output width */
	ulong buffer = pe->output_bit_buffer | ((ulong)data << pe->output_bit_count);
	pe->output_bit_count += bits;
	while ( pe->output_bit_count >= 8 )
	{	/* putc(output_bit_buffer >> 24, file); */
		pe->gif_buffer[pe->byte_count] = (byte)buffer;  /* low byte */
		buffer >>= 8;
		pe->output_bit_count -= 8;
		pe->byte_count++;
		if ( pe->byte_count == 256 )
		{	pe->byte_count = 1;
			pe->gif_buffer[0] = 255;  /* byte count for block */
			fwrite(pe->gif_buffer, 1, 256, pe->file);
		}
	}
	pe->output_bit_buffer = (byte)buffer;
}

/* Finish encoding, and flush the buffers. */
private void
lzw_finish(register lzw_encoder _ss *pe)
{	lzw_putc(pe, pe->string_code);	/* output last code */
	lzw_putc(pe, pe->Clear_code+1);	/* output eof code */
	lzw_putc(pe, 0);	/* force out last code */
	if ( pe->byte_count != 1 )
	{	pe->gif_buffer[0] = pe->byte_count;
		fwrite(pe->gif_buffer, 1, pe->byte_count+1, pe->file);
	}
}

/* Terminate LZW encoder. */
private void
lzw_exit(register lzw_encoder _ss *pe)
{	gs_free((char *)pe->table, TABLE_SIZE, sizeof(code_entry), "GIF code table");
}

/* Get the next (depth) bits from the pixel buffer. */
private uint
lzw_getc(pixel_reader _ss *pr, uint depth)
{	if ( pr->bits_left < depth )
	{	pr->bit_buffer <<= 8; 
		pr->bit_buffer |= *(pr->next++);
		pr->bits_left += 8;
	}
	pr->bits_left -= depth;
	return (pr->bit_buffer >> pr->bits_left) & ((1 << depth) - 1);
}

/* Output 1 row of data in GIF (LZW) format. */
private void
lzw(byte *from, byte *end, register lzw_encoder _ss *pe, byte depth)
{	pixel_reader reader;
	/* Set up the reader. */
	reader.next = from;
	reader.bits_left = 0;

	if ( pe->next_code == (pe->Clear_code + 2))	/* first time through */
	{	pe->string_code = lzw_getc(&reader, depth);
	}

	while ( reader.next < end || reader.bits_left >= depth )
	{	uint	data = lzw_getc(&reader, depth);  /* actually only a byte */

		/* Hash to find a match for the prefix+char */
		/* string in the string table */

		ushort	hash_prefix = pe->string_code;
		int	index = (data << 4) ^ hash_prefix;
		int	hash_offset;
		register code_entry *pce;

		if ( index == 0 )
			hash_offset = 1;
		else
			hash_offset = TABLE_SIZE - index;

		while ( 1 )
		{	pce = &pe->table[index];
			if ( pce->code_value == -1 )
				break;
			if ( pce->prefix_code == hash_prefix && 
			     pce->append_character == data )
				break;
			index -= hash_offset;
			if ( index < 0 )
				index += TABLE_SIZE;
		}
		if ( pce->code_value != -1 )
			pe->string_code = pce->code_value;
		else
		{	/* Make a new entry */
			pce->code_value = pe->next_code++;
			pce->prefix_code = pe->string_code;
			pce->append_character = data;

			lzw_putc(pe, pe->string_code);

			if ( pe->next_code > (pe->Max_Code + 1) )
			{	/* Increment the width of the code */
				if ( pe->bits+1 >= MAX_BITS )
				{	/* output clear code first*/
					lzw_putc(pe, pe->Clear_code);
					pe->bits = (depth == 1 ? 2 : depth);
					lzw_reset(pe);
				}
				else
					pe->bits++;
				lzw_set_bits(pe, pe->bits);
			}
			pe->string_code = data;
		}
	}
}


/* Write a page to a file in GIF format. */
int
gif_print_page(gx_device_printer *pdev, FILE *file)
{	int raster = gdev_prn_bytes_per_scan_line(pdev);
	int height = pdev->height;
	int depth = pdev->color_info.depth;
	byte *row = (byte *)gs_malloc(raster * 2, 1, "gif file buffer");
	byte *end = row + raster;
	gif_header header;
	image_descriptor header_desc;
	lzw_encoder encoder;
	int y;
	int code = 0;			/* return code */

	if ( row == 0 )			/* can't allocate row buffer */
		return gs_error_VMerror;
	code = lzw_init(&encoder, (depth == 1 ? 2 : depth), file);
	if ( code < 0 )
		return code;

	/* Set up the header. */

	memcpy(header.signature, "GIF", 3);
	memcpy(header.version, "87a", 3);
	header.width = raster * (8/depth);	/*pdev->width;*/
						/* for most decoders the */
						/* width must be on byte */
						/* boundry		 */
	header.height = height;
/*	header.flags.globalcolor = TRUE;	*/
/*	header.flags.colorres = depth-1;	*/
/*	header.flags.sort = FALSE;		*/
/*	header.flags.colorsize = depth-1;	*/
	header.flags =
		(1 << globalcolor_shift) +
		((depth - 1) << colorres_shift) +
		(0 << sort_shift) +
		((depth - 1) << colorsize_shift);
	header.background = 0;
	header.aspect = 0;
	
	/* Write the header. */

	if ( fwrite(&header, 1, 13, file) < 13 )
	   {	code = gs_error_ioerror;
		goto gif_done;
	   }

	/* Write the header global color palette. */

	if ( pc_write_palette((gx_device *)pdev, 1 << depth, file) < 0 )
	   {	code = gs_error_ioerror;
		goto gif_done;
	   }

	header_desc.left_pos = 0;
	header_desc.top_pos = 0;
	header_desc.width = raster * (8/depth);	/*pdev->width;*/
	header_desc.height = height;
/*	header_desc.flags.localcolor = FALSE;	*/
/*	header_desc.flags.interlace = FALSE;	*/
/*	header_desc.flags.sort = FALSE;		*/
/*	header_desc.flags.localsize = 0;	*/

	header_desc.flags = 0;

	/* Write the header image descriptor. */

	fputc(0x2c,file);		/* start with separator */
	if ( fwrite(&header_desc, 1, 9, file) < 9 )
	   {	code = gs_error_ioerror;
		goto gif_done;
	   }

	fflush(file);

	printf("The Graphics Interchange Format(c) is \n");
	printf("the Copyright Property of CompuServe Incorporated.\n");
	printf("GIF(sm) is a Service Mark property of CompuServe Incorporated\n");

	fputc(encoder.bits, file);		/* start with code size */

	lzw_putc(&encoder, encoder.Clear_code);	/* output clear code first*/

	/* Dump the contents of the image. */
	for ( y = 0; y < height; y++ ) 
	   {	gdev_prn_copy_scan_lines(pdev, y, row, raster);
		lzw(row, end, &encoder, depth);
	   }

	lzw_finish(&encoder);
        fputc(0, file);
	fputc(0x3b, file);	/* EOF indicator */

gif_done:
	lzw_exit(&encoder);
	gs_free((char *)row, raster * 2, 1, "gif file buffer");
	return code;
}
