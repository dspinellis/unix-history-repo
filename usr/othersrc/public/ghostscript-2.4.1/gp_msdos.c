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

/* gp_msdos.c */
/* Common platform-specific routines for MS-DOS (any compiler) */
#include <stdio.h>
#include <fcntl.h>
#include "dos_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"

/* ------ Date and time ------ */

/* Read the current date (in days since Jan. 1, 1980) */
/* and time (in milliseconds since midnight). */
void
gp_get_clock(long *pdt)
{	union REGS osdate, ostime;
	long idate;
	static int mstart[12] =
	   { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };
	osdate.h.ah = 0x2a;		/* get date */
	intdos(&osdate, &osdate);
#define da_year rshort.cx
#define da_mon h.dh
#define da_day h.dl
	ostime.h.ah = 0x2c;		/* get time */
	intdos(&ostime, &ostime);
#define ti_hour h.ch
#define ti_min h.cl
#define ti_sec h.dh
#define ti_hund h.dl
	idate = (long)osdate.da_year * 365 +
		(osdate.da_year / 4 + 1 +	/* account for leap years */
		 mstart[osdate.da_mon - 1] +	/* month is 1-origin */
		 osdate.da_day - 1);		/* day of month is 1-origin */
	if ( osdate.da_mon <= 2 && osdate.da_year % 4 == 0 )		/* Jan. or Feb. of leap year */
		idate--;
	pdt[0] = idate;
	pdt[1] =
		(ostime.ti_hour * 60 + ostime.ti_min) * 60000L +
		(ostime.ti_sec * 100 + ostime.ti_hund) * 10L;
}

/* ------ Printer accessing ------ */

/* Put the printer into binary mode.  This is not a standard gp procedure, */
/* but all MS-DOS configurations need it. */
void
gp_set_printer_binary(int prnfno)
{	union REGS regs;
	regs.h.ah = 0x44;	/* ioctl */
	regs.h.al = 0;		/* get device info */
	regs.rshort.bx = prnfno;
	intdos(&regs, &regs);
	regs.h.dl |= 0x20;	/* binary (no ^Z intervention) */
	regs.h.dh = 0;
	regs.h.ah = 0x44;	/* ioctl */
	regs.h.al = 1;		/* set device info */
	intdos(&regs, &regs);
}

/* ------ File names ------ */

/* Define the character used for separating file names in a list. */
const char gp_file_name_list_separator = ';';

/* Define the default scratch file name prefix. */
const char gp_scratch_file_name_prefix[] = "_temp_";

/* Define whether case is insignificant in file names. */
const int gp_file_names_ignore_case = 1;

/* Answer whether a file name contains a directory/device specification, */
/* i.e. is absolute (not directory- or device-relative). */
int
gp_file_name_is_absolute(const char *fname, uint len)
{	/* A file name is absolute if it contains a drive specification */
	/* (second character is a :) or if it start with / or \. */
	return ( len >= 1 && (*fname == '/' || *fname == '\\' ||
		(len >= 2 && fname[1] == ':')) );
}

/* Answer the string to be used for combining a directory/device prefix */
/* with a base file name.  The file name is known to not be absolute. */
char *
gp_file_name_concat_string(const char *prefix, uint plen,
  const char *fname, uint len)
{	if ( plen > 0 )
	  switch ( prefix[plen - 1] )
	   {	case ':': case '/': case '\\': return "";
	   };
	return "\\";
}

/* ------ File enumeration ------ */

struct file_enum_s {
	ff_struct_t ffblk;
	char *pattern;
	int patlen;			/* allocated length of pattern */
	int head_size;			/* pattern length through last */
					/* : or \ */
	int first_time;
	gs_memory_procs mprocs;
};

/* Initialize an enumeration.  Note that * and ? in a directory */
/* don't work, and \* and \? don't work. */
file_enum *
gp_enumerate_files_init(const char *pat, uint patlen,
  proc_alloc_t palloc, proc_free_t pfree)
{	file_enum *pfen = (file_enum *)(*palloc)(1, sizeof(file_enum), "gp_enumerate_files");
	char *pattern;
	char *p;
	int hsize = 0;
	int i;
	if ( pfen == 0 ) return 0;
	pattern = (*palloc)(patlen + 1, 1,
			    "gp_enumerate_files(pattern)");
	if ( pattern == 0 ) return 0;
	p = pattern;
	for ( i = 0; i < patlen; i++ )
	   {	switch ( pat[i] )
		 {
		case '*':
		   /* Skip to . or end of string so DOS can do it. */
		   *p++ = '*';
		   while ( i < patlen && pat[i] != '.' ) i++;
		   i--;
		   continue;
		case '\\':
		   i++;
		   if ( pat[i] != '\\' && pat[i] != '/' && pat[i] != ':' )
			break;
		   /* falls through */
		case ':':
		case '/':
		   hsize = p + 1 - pattern;
		 }
		*p++ = pat[i];
	   }
	*p = 0;
	pfen->pattern = pattern;
	pfen->patlen = patlen;
	pfen->head_size = hsize;
	pfen->mprocs.alloc = palloc;
	pfen->mprocs.free = pfree;
	pfen->first_time = 1;
	return pfen;
}

/* Enumerate the next file. */
uint
gp_enumerate_files_next(file_enum *pfen, char *ptr, uint maxlen)
{	int code;
	char *p, *q;
	if ( pfen->first_time )
	   {	code = dos_findfirst(pfen->pattern, &pfen->ffblk);
		pfen->first_time = 0;
	   }
	else
		code = dos_findnext(&pfen->ffblk);
	if ( code != 0 )
	   {	/* All done, clean up. */
		gp_enumerate_files_close(pfen);
		return ~(uint)0;
	   }
	if ( maxlen < 13 + pfen->head_size ) return maxlen + 1;	/* cop out! */
	memcpy(ptr, pfen->pattern, pfen->head_size);
	for ( p = &pfen->ffblk.ff_name[0], q = ptr + pfen->head_size; *p; p++ )
	  if ( *p != ' ' ) *q++ = *p;
	return q - ptr;
}

/* Clean up the file enumeration. */
void
gp_enumerate_files_close(file_enum *pfen)
{	proc_free_t pfree = pfen->mprocs.free;
	(*pfree)(pfen->pattern, pfen->patlen + 1, 1,
		 "gp_enumerate_files_close(pattern)");
	(*pfree)((char *)pfen, 1, sizeof(file_enum), "gp_enumerate_files_close");
}
