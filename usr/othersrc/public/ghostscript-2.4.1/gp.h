/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gp.h */
/* Interface to platform-specific routines for Ghostscript */

/*
 * This file defines ***ALL*** the routines that are Ghostscript- and
 * platform-specific.  The routines are implemented in a gp_*.c file
 * specific to each platform.  We try very hard to keep this list short!
 */

/* ------ Initialization/termination ------ */

/*
 * This routine is called early in the Ghostscript initialization.
 * It should do as little as possible.  In particular, it should not
 * do things like open display connections: that is the responsibility
 * of the display device driver.
 */
extern void gp_init(P0());

/*
 * This routine is called just before Ghostscript exits (normally or
 * abnormally).  It too should do as little as possible.
 */
extern void gp_exit(P0());

/* ------ Date and time ------ */

/*
 * Read the current date (in days since Jan. 1, 1980) into pdt[0],
 * and time (in milliseconds since midnight) into pdt[1].
 */
extern void gp_get_clock(P1(long *pdt));

/* ------ Screen management ------ */

/*
 * These routines are only relevant in a single-window environment
 * such as a PC; on platforms with window systems, the 'make current'
 * routines do nothing.
 */

struct gx_device_s;

/* Write a string to the console. */
extern void gp_console_puts(P2(const char *, uint));

/* Make the console current on the screen. */
extern int gp_make_console_current(P1(struct gx_device_s *));

/* Make the graphics current on the screen. */
extern int gp_make_graphics_current(P1(struct gx_device_s *));

/* ------ Printer accessing ------ */

/*
 * Open a connection to a printer.  A null file name means use the
 * standard printer connected to the machine, if any.
 * If possible, support "|command" for opening an output pipe.
 * Return NULL if the connection could not be opened.
 */
extern FILE *gp_open_printer(P1(char *fname));

/* Close the connection to the printer. */
extern void gp_close_printer(P2(FILE *pfile, const char *fname));

/* ------ File names ------ */

/* Define the character used for separating file names in a list. */
extern const char gp_file_name_list_separator;

/* Define the default scratch file name prefix. */
extern const char gp_scratch_file_name_prefix[];

/* Define whether case is insignificant in file names. */
extern const int gp_file_names_ignore_case;

/* Create and open a scratch file with a given name prefix. */
/* Write the actual file name at fname. */
extern FILE *gp_open_scratch_file(P3(const char *prefix, char *fname,
				     const char *mode));

/* Answer whether a file name contains a directory/device specification, */
/* i.e. is absolute (not directory- or device-relative). */
extern int gp_file_name_is_absolute(P2(const char *fname, uint len));

/* Answer the string to be used for combining a directory/device prefix */
/* with a base file name.  The file name is known to not be absolute. */
extern char *gp_file_name_concat_string(P4(const char *prefix, uint plen,
					   const char *fname, uint len));

/* ------ File operations ------ */

/* If the file given by fname exists, fill in its status and return 1; */
/* otherwise return 0. */
typedef struct file_status_s {
	long size_pages;
	long size_bytes;
	long time_referenced;
	long time_created;
} file_status;
extern int gp_file_status(P2(const char *fname, file_status *pstatus));

/* ------ File enumeration ------ */

struct file_enum_s;	/* opaque to client, defined by implementor */
typedef struct file_enum_s file_enum;

/*
 * Begin an enumeration.  pat is a C string that may contain *s or ?s.
 * The implementor should copy the string to a safe place.
 * If the operating system doesn't support correct, arbitrarily placed
 * *s and ?s, the implementation should modify the string so that it
 * will return a conservative superset of the request.  E.g., if the OS
 * doesn't implement ? (single-character wild card), any consecutive
 * string of ?s should be interpreted as *.  Note that \ can appear in
 * the pattern also, as a quoting character.
 */
extern file_enum *gp_enumerate_files_init(P4(const char *pat, uint patlen,
					     proc_alloc_t palloc,
					     proc_free_t pfree));

/*
 * Return the next file name in the enumeration.  The client passes in
 * a scratch string and a max length.  If the name of the next file fits,
 * the procedure returns the length.  If it doesn't fit, the procedure
 * returns max length +1.  If there are no more files, the procedure
 * returns -1.
 */
extern uint gp_enumerate_files_next(P3(file_enum *pfen, char *ptr, uint maxlen));

/*
 * Clean up a file enumeration.  This is only called to abandon
 * an enumeration partway through: ...next should do it if there are
 * no more files to enumerate.  This should deallocate the file_enum
 * structure and any subsidiary structures, strings, buffers, etc.
 */
extern void gp_enumerate_files_close(P1(file_enum *pfen));
