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

/* gp_unix.c */
/* Unix-specific routines for Ghostscript */
#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"
#include "stat_.h"
#include "time_.h"

/* popen isn't POSIX-standard, so we declare it here. */
extern FILE *popen();
extern int pclose();

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
{	long secs_since_1980;
	struct timeval tp;
	struct timezone tzp;
	time_t tsec;
	struct tm *tm, *localtime();

	if ( gettimeofday(&tp, &tzp) == -1 )
	   {	perror("Ghostscript: gettimeofday failed:");
		exit(-1);
	   }

	/* tp.tv_sec is #secs since Jan 1, 1970 */

	/* subtract off number of seconds in 10 years */
	/* leap seconds are not accounted for */
	secs_since_1980 = tp.tv_sec - (long)(60 * 60 * 24 * 365.25 * 10);
 
	/* adjust for timezone */
	secs_since_1980 -= (tzp.tz_minuteswest * 60);

	/* adjust for daylight savings time - assume dst offset is 1 hour */
	tsec = tp.tv_sec;
	tm = localtime(&tsec);
	if ( tm->tm_isdst )
		secs_since_1980 += (60 * 60);

	/* divide secs by #secs/day to get #days (integer division truncates) */
	pdt[0] = secs_since_1980 / (60 * 60 * 24);
	/* modulo + microsecs/1000 gives number of millisecs since midnight */
	pdt[1] = (secs_since_1980 % (60 * 60 * 24)) * 1000 + tp.tv_usec / 1000;
#ifdef DEBUG_CLOCK
	printf("tp.tv_sec = %d  tp.tv_usec = %d  pdt[0] = %ld  pdt[1] = %ld\n",
		tp.tv_sec, tp.tv_usec, pdt[0], pdt[1]);
#endif
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
/* "|command" opens an output pipe. */
/* Return NULL if the connection could not be opened. */
FILE *
gp_open_printer(char *fname)
{	return
	  (strlen(fname) == 0 ?
	   gp_open_scratch_file(gp_scratch_file_name_prefix, fname, "wb") :
	   fname[0] == '|' ?
	   popen(fname + 1, "wb") :
	   fopen(fname, "wb"));
}

/* Close the connection to the printer. */
void
gp_close_printer(FILE *pfile, const char *fname)
{	if ( fname[0] == '|' )
		pclose(pfile);
	else
		fclose(pfile);
}

/* ------ File names ------ */

/* Define the character used for separating file names in a list. */
const char gp_file_name_list_separator = ':';

/* Define the default scratch file name prefix. */
const char gp_scratch_file_name_prefix[] = "/tmp/gs_";

/* Define whether case is insignificant in file names. */
const int gp_file_names_ignore_case = 0;

/* Create and open a scratch file with a given name prefix. */
/* Write the actual file name at fname. */
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{	strcpy(fname, prefix);
	strcat(fname, "XXXXXX");
	mktemp(fname);
	return fopen(fname, mode);
}

/* Answer whether a file name contains a directory/device specification, */
/* i.e. is absolute (not directory- or device-relative). */
int
gp_file_name_is_absolute(const char *fname, uint len)
{	/* A file name is absolute if it starts with a /. */
	return ( len >= 1 && *fname == '/' );
}

/* Answer the string to be used for combining a directory/device prefix */
/* with a base file name.  The file name is known to not be absolute. */
char *
gp_file_name_concat_string(const char *prefix, uint plen,
			   const char *fname, uint len)
{	if ( plen > 0 && prefix[plen - 1] == '/' )
		return "";
	return "/";
}

/* ------ File operations ------ */

/* If the file given by fname exists, fill in its status and return 1; */
/* otherwise return 0. */
int
gp_file_status(const char *fname, file_status *pstatus)
{	struct stat sbuf;
	/* The RS/6000 prototype for stat doesn't include const, */
	/* so we have to explicitly remove the const modifier. */
	if ( stat((char *)fname, &sbuf) < 0 ) return 0;
	pstatus->size_pages = stat_blocks(&sbuf);	/* st_blocks is */
					/* missing on some systems, */
					/* see stat_.h */
	pstatus->size_bytes = sbuf.st_size;
	pstatus->time_referenced = sbuf.st_mtime;
	pstatus->time_created = sbuf.st_ctime;
	return 1;
}

/* ------ File enumeration ------ */

/****** THIS IS NOT SUPPORTED ON UNIX SYSTEMS. ******/
/* Amazingly enough, there is no standard Unix library routine */
/* for enumerating the files matching a pattern, */
/* or even for enumerating (conveniently) the files in a directory. */

struct file_enum_s {
	char *pattern;
	int first_time;
	gs_memory_procs mprocs;
};

/* Initialize an enumeration.  NEEDS WORK ON HANDLING * ? \. */
file_enum *
gp_enumerate_files_init(const char *pat, uint patlen,
  proc_alloc_t palloc, proc_free_t pfree)
{	file_enum *pfen = (file_enum *)(*palloc)(1, sizeof(file_enum), "gp_enumerate_files");
	char *pattern;
	if ( pfen == 0 ) return 0;
	pattern = (*palloc)(patlen + 1, 1,
			    "gp_enumerate_files(pattern)");
	if ( pattern == 0 ) return 0;
	memcpy(pattern, pat, patlen);
	pattern[patlen] = 0;
	pfen->pattern = pattern;
	pfen->mprocs.alloc = palloc;
	pfen->mprocs.free = pfree;
	pfen->first_time = 1;
	return pfen;
}

/* Enumerate the next file. */
uint
gp_enumerate_files_next(file_enum *pfen, char *ptr, uint maxlen)
{	if ( pfen->first_time )
	   {	pfen->first_time = 0;
	   }
	return -1;
}

/* Clean up the file enumeration. */
void
gp_enumerate_files_close(file_enum *pfen)
{	proc_free_t pfree = pfen->mprocs.free;
	(*pfree)(pfen->pattern, strlen(pfen->pattern) + 1, 1,
		 "gp_enumerate_files_close(pattern)");
	(*pfree)((char *)pfen, 1, sizeof(file_enum), "gp_enumerate_files_close");
}
