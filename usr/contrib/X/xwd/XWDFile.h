#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */

/* $Header: XWDFile.h,v 10.3 86/02/01 16:07:50 tony Rel $ */
/*
 * XWDFile.h	MIT Project Athena, X Window system window raster
 *		image dumper, dump file format header file.
 *
 *  Author:	Tony Della Fera, DEC
 *		27-Jun-85
 */

#define XWD_FILE_VERSION 5

typedef struct _xwd_file_header {
	int header_size;	/* Size of the entire file header (bytes). */
	int file_version;	/* XWD_FILE_VERSION */
	int display_type;	/* Display type. */
	int display_planes;	/* Number of display planes. */
	int pixmap_format;	/* Pixmap format. */
	int pixmap_width;	/* Pixmap width. */
	int pixmap_height;	/* Pixmap height. */
	short window_width;	/* Window width. */
	short window_height;	/* Window height. */
	short window_x;		/* Window upper left X coordinate. */
	short window_y;		/* Window upper left Y coordinate. */
	short window_bdrwidth;	/* Window border width. */
	short padding;		/* Long word alignment padding. */
} XWDFileHeader;

