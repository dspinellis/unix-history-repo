/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gp_vms.c */
/* VAX/VMS specific routines for Ghostscript */
#include "gs.h"
#include "gp.h"
#include <stat.h>

#define MAX_VMS_FILENAME_LEN 255

/* VMS string descriptor structure */
#define DSC$K_DTYPE_T 14
#define DSC$K_CLASS_S  1
struct dsc$descriptor_s {
	unsigned short	dsc$w_length;
	unsigned char	dsc$b_dtype;
	unsigned char	dsc$b_class;
	char		*dsc$a_pointer;
};
typedef struct dsc$descriptor_s descrip;

/* VMS RMS constants */
#define RMS$_NMF    99018
#define RMS$_NORMAL 65537

struct file_enum_s {
  uint context, length;
  descrip *pattern;
};

extern uint
  LIB$FIND_FILE(descrip *, descrip *, uint *, descrip *, descrip *,
		uint *, uint *),
  LIB$FIND_FILE_END(uint *),
  SYS$FILESCAN (descrip *, uint *, uint *);

private uint
strlength(char *str, uint maxlen, char term)
{	uint i = 0;
	while ( i < maxlen && str[i] != term ) i++;
	return i;
}

/* Do platform-dependent initialization. */
void
gp_init()
{
}

/* Do platform-dependent cleanup. */
void
gp_exit()
{
}

/* ------ Date and time ------ */

/* Read the current date (in days since Jan. 1, 1980) */
/* and time (in milliseconds since midnight). */
void
gp_get_clock(long *pdt)
{	struct {uint _l0, _l1;} binary_date;
	long lib$day(), sys$bintim();
	long days, days0, seconds;
	char *jan_1_1980 = "1-JAN-1980";
	char *midnight   = "00:00:00.00";
	descrip str_desc;

	/* Get days from system zero date (November 17, 1858) to present. */
	(void) lib$day (&days0);

	/* For those interested, Wednesday, November 17, 1858 is the base
	   of the Modified Julian Day system adopted by the Smithsonian
	   Astrophysical Observatory in 1957 for satellite tracking.  (The
	   year 1858 preceded the oldest star catalog in use at the
	   observatory.)  VMS uses quadword time stamps which are offsets
	   in 100 nanosecond units from November 17, 1858.  With a 63-bit
	   absolute time representation (sign bit must be clear), VMS will
	   have no trouble with time until 31-JUL-31086 02:48:05.47. */

	/* Convert January 1, 1980 into a binary absolute time */
	str_desc.dsc$w_length  = strlen(jan_1_1980);
	str_desc.dsc$a_pointer = jan_1_1980;
	(void) sys$bintim (&str_desc, &binary_date);

	/* Now get days from system zero date to January 1, 1980 */
	(void) lib$day (&days, &binary_date);

	/* Now compute number of days since January 1, 1980 */
	pdt[0] = 1 + days0 - days;

	/* Convert midnight into a binary delta time */
	str_desc.dsc$w_length  = strlen(midnight);
	str_desc.dsc$a_pointer = midnight;
	(void)  sys$bintim (&str_desc, &binary_date);

	/* Now get number 10 millisecond time units since midnight */
	(void) lib$day (&days, &binary_date, &seconds);
	pdt[1] = 10 * seconds;
}

/* ------ Screen management ------ */

/* Write a string to the console. */
void
gp_console_puts(const char *str, uint size)
{	fwrite(str, 1, size, stdout);
}

/* Make the console current on the screen. */
int
gp_make_console_current(struct gx_device_s *dev)
{	return 0;
}

/* Make the graphics current on the screen. */
int
gp_make_graphics_current(struct gx_device_s *dev)
{	return 0;
}

/* ------ Printer accessing ------ */

/* Open a connection to a printer.  A null file name means use the */
/* standard printer connected to the machine, if any. */
/* Return NULL if the connection could not be opened. */
FILE *
gp_open_printer(char *fname)
{	return
	  (strlen(fname) == 0 ?
	   gp_open_scratch_file(gp_scratch_file_name_prefix, fname, "wb") :
	   fopen(fname, "wb"));
}

/* Close the connection to the printer. */
void
gp_close_printer(FILE *pfile, const char *fname)
{	fclose(pfile);
}

/* ------ File names ------ */

/* Define the character used for separating file names in a list. */
const char gp_file_name_list_separator = ',';

/* Define the default scratch file name prefix. */
const char gp_scratch_file_name_prefix[] = "_temp_";

/* Define whether case is insignificant in file names. */
const int gp_file_names_ignore_case = 1;

/* Create and open a scratch file with a given name prefix. */
/* Write the actual file name at fname. */
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{	strcpy(fname, prefix);
	strcat(fname, "XXXXXX");
	mktemp(fname);
	return fopen(fname, mode);
}
/*  Answer whether a file name contains a directory/device specification, i.e.,
 *  is absolute (not directory- or device-relative).  Since for VMS, the concept
 *  of an "absolute" file reference has no meaning.  As Ghostscript is here
 *  merely checking to see if it will make sense to paste a path to the front
 *  of the file name, we use the VMS system service SYS$FILESCAN to check that
 *  the file name has no node, device, root, or directory specification: if all
 *  four of these items are missing from the file name then it is considered to
 *  a relative file name to which a path may be prefixed. (Roots are associated
 *  with rooted logical names.)
 */

int
gp_file_name_is_absolute(const char *fname, uint len)
{
	descrip str_desc;
	struct { unsigned fscn$v_node : 1;
		 unsigned fscn$v_device : 1;
		 unsigned fscn$v_root : 1;
		 unsigned fscn$v_directory : 1;
		 unsigned fscn$v_name : 1;
		 unsigned fscn$v_type : 1;
		 unsigned fscn$v_version : 1;
		 unsigned fscn$v_fill_23 : 1;} flags;

	str_desc.dsc$w_length  = len;
	str_desc.dsc$a_pointer = fname;
	SYS$FILESCAN (&str_desc, &0L,  &flags);
	if ( flags.fscn$v_directory || flags.fscn$v_root ||
	     flags.fscn$v_device    || flags.fscn$v_node) return 1;
	else return 0;
}

/* Answer the string to be used for combining a directory/device prefix */
/* with a base file name.  The file name is known to not be absolute. */
char *
gp_file_name_concat_string(const char *prefix, uint plen,
			   const char *fname, uint len)
{
	/*  Full VAX/VMS paths are of the form:
	 *
	 *    device:[root.][directory.subdirectory]filename.extension;version
	 *    logical:filename.extension;version
	 *
	 *  Roots are fairly rare and associated typically with rooted logical
	 *  names.
	 *
	 *  Examples:
	 *
	 *    DUA1:[GHOSTSCRIPT]GHOST.PS;1
	 *    THOR_DEC:[DOOF.A.B.C.D]FILE.DAT;-3
	 *    LOG:GHOST.PS  (LOG is a logical defined as DUA1:[GHOSTSCRIPT])
	 *    LOG:DOOF.DAT  (LOG is defined as DUA1, current directory is
	 *                   is used as the directory spec.)
	 *
	 */
	if ( plen > 0 )
	  switch ( prefix[plen - 1] )
	   {	case ':': case ']': return "";
	   };
	return ":";
}

/* ------ File operations ------ */

/* If the file given by fname exists, fill in its status and return 1; */
/* otherwise return 0. */
int
gp_file_status(const char *fname, file_status *pstatus)
{	struct stat sbuf;
	if ( stat(fname, &sbuf) < 0 ) return 0;
	pstatus->size_pages = (sbuf.st_size + 1023) >> 10;	/* no st_blocks */
	pstatus->size_bytes = sbuf.st_size;
	pstatus->time_referenced = sbuf.st_mtime;
	pstatus->time_created = sbuf.st_ctime;
	return 1;
}

/* ------ Wild card file search procedures ------ */

private void
gp_free_enumeration(file_enum *pfen)
{
	if (pfen) {
	  LIB$FIND_FILE_END(&pfen->context);
	  gs_free(pfen->pattern->dsc$a_pointer, pfen->length, 1,
		  "GP_ENUM(pattern)");
	  gs_free((char *)pfen->pattern, sizeof(descrip), 1,
		  "GP_ENUM(descriptor)");
	  gs_free((char *)pfen, sizeof(file_enum), 1,
		  "GP_ENUM(file_enum)");
	}
}

/* Begin an enumeration.  pat is a C string that may contain *s or ?s. */
/* The implementor should copy the string to a safe place. */
/* If the operating system doesn't support correct, arbitrarily placed */
/* *s and ?s, the implementation should modify the string so that it */
/* will return a conservative superset of the request.  E.g., if the OS */
/* doesn't implement ? (single-character wild card), any consecutive */
/* string of ?s should be interpreted as *.  Note that \ can appear in */
/* the pattern also, as a quoting character. */

file_enum *
gp_enumerate_files_init(const char *pat, uint patlen)
{
	file_enum *pfen;
	uint i, len;
	char *c, *newpat;

	pfen = (file_enum *)gs_malloc(sizeof (file_enum), 1,
				      "GP_ENUM(file_enum)");
	pfen->pattern = (descrip *)gs_malloc(sizeof (descrip), 1,
					     "GP_ENUM(descriptor)");
	newpat = (char *)gs_malloc(patlen, 1, "GP_ENUM(pattern)");

	/*  Copy the pattern removing backslash quoting characters and
	 *  transforming unquoted question marks, '?', to percent signs, '%'.
	 *  (VAX/VMS uses the wildcard '%' to represent exactly one character
	 *  and '*' to represent zero or more characters.  Any combination and
	 *  number of interspersed wildcards is permitted.)
	 */
	c = newpat;
	for ( i = 0; i < patlen; pat++, i++ )
	  switch (*pat) {
	    case '?'  :
		*c++ = '%'; break;
	    case '\\' :
		i++;
		if (i < patlen) *c++ = *++pat;
		break;
	    default   :
		*c++ = *pat; break;
	  }
	len = c - newpat;

	/* Pattern may not exceed 255 characters */
	if (len > 255) {
	  gs_free(newpat, patlen, 1, "GP_ENUM(pattern)");
	  gs_free((char *)pfen->pattern, sizeof (descrip), 1,
		  "GP_ENUM(descriptor)");
	  gs_free((char *)pfen, sizeof (file_enum), 1, "GP_ENUM(file_enum)");
	  return (file_enum *)0;
	}

	pfen->context = 0;
	pfen->length = patlen;
	pfen->pattern->dsc$w_length  = len;
	pfen->pattern->dsc$b_dtype   = DSC$K_DTYPE_T;
	pfen->pattern->dsc$b_class   = DSC$K_CLASS_S;
	pfen->pattern->dsc$a_pointer = newpat;

	return pfen;
}

/* Return the next file name in the enumeration.  The client passes in */
/* a scratch string and a max length.  If the name of the next file fits, */
/* the procedure returns the length.  If it doesn't fit, the procedure */
/* returns max length +1.  If there are no more files, the procedure */
/* returns -1. */

uint
gp_enumerate_files_next(file_enum *pfen, char *ptr, uint maxlen)
{
	char *c, filnam[MAX_VMS_FILENAME_LEN];
	descrip result = {MAX_VMS_FILENAME_LEN, DSC$K_DTYPE_T,
			  DSC$K_CLASS_S, filnam};
	uint i, len;

	/* Find the next file which matches the pattern */
	i = LIB$FIND_FILE(pfen->pattern, &result, &pfen->context,
			  (descrip *)0, (descrip *)0, (uint *)0, (uint *)0);

	/* Check the return status */
	if (i == RMS$_NMF) {
	  gp_free_enumeration (pfen);
	  return (uint)-1;
	}
	else if (i != RMS$_NORMAL) return 0;
	else if ((len = strlength (filnam, MAX_VMS_FILENAME_LEN, ' ')) > maxlen)
	  return maxlen+1;

	/* Copy the returned filename over to the input string ptr */
	c = ptr;
	for (i = 0; i < len; i++) *c++ = filnam[i];

	return len;
}

/* Clean up a file enumeration.  This is only called to abandon */
/* an enumeration partway through: ...next should do it if there are */
/* no more files to enumerate.  This should deallocate the file_enum */
/* structure and any subsidiary structures, strings, buffers, etc. */

void
gp_enumerate_files_close(file_enum *pfen)
{	gp_free_enumeration (pfen);
}
