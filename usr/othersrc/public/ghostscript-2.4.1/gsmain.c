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

/* gsmain.c */
/* Framework for Ghostscript drivers */
#include "string_.h"
#include "memory_.h"
#include "gx.h"
#include "gp.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"
#include "gxdevmem.h"

/*
 * This routine provides the following standard services for parsing
 * a command line:
 *	- setting debug flags (-A, -e, -E switches; -Z switch if debugging);
 *	- passing other arguments and switches back to the caller.
 *
 * Calling convention:
 *	gs_main(argc, argv, switch_proc, arg_proc)
 * Calls
 *	switch_proc(&switch, rest_of_arg) for switches,
 *	arg_proc(&arg, index) for non-switch args.
 * If switch_proc returns a negative value, gs_main prints an
 *   "unknown switch" error message and aborts.
 * gs_main returns the number of non-switch args handed to arg_proc.
 */

/* Imported data */
extern int gs_alloc_debug;
extern int gs_log_errors;
extern gx_device *gx_device_list[];

void gs_exit(P1(int));

int
gs_main(int argc, char *argv[],
    int (*switch_proc)(P2(char **, char *)),
    void (*arg_proc)(P2(char **, int)))
{	int argi = 1;
	int arg_count = 0;
	gs_log_errors = 0;
	gs_out = stdout;
#ifdef DEBUG
	gs_debug_out = stdout;
	/* Reset debugging flags */
	memset(gs_debug, 0, 128);
#endif
	/* If debugging is enabled, trace the device calls. */
#ifdef DEBUG
	   {	extern gx_device *gs_trace_device(P1(gx_device *));
		extern gx_device_memory
			mem_mono_device, mem_mapped8_color_device,
			mem_true24_color_device, mem_true32_color_device;
		static gx_device_memory *mdevs[5] =
		   {	&mem_mono_device, &mem_mapped8_color_device,
			&mem_true24_color_device, &mem_true32_color_device,
			0
		   };
		extern gx_device gs_clist_device;	/* gx_clist_device */
		static gx_device *cldevs[2] =
		   {	&gs_clist_device,
			0
		   };
		gx_device **pdevs[4];
		gx_device ***ppdev;
		gx_device **pdev;
		pdevs[0] = gx_device_list;
		pdevs[1] = (gx_device **)mdevs;
		pdevs[2] = cldevs;
		pdevs[3] = 0;
/******
		for ( ppdev = pdevs; *ppdev != 0; ppdev++ )
		 for ( pdev = *ppdev; *pdev != 0; pdev++ )
		   {	gx_device *tdev = gs_trace_device(*pdev);
			if ( tdev == 0 )
			   {	lprintf("Can't allocate traced device!\n");
				gs_exit(1);
			   }
			*pdev = tdev;
		   }
 ******/
	   }
#endif
	for ( ; argi < argc; argi++ )
	   {	char **argp = &argv[argi];
		char *arg = *argp;
		if ( *arg == '-' )
		   {	switch ( *++arg )
			   {
			default:
				if ( (*switch_proc)(argp, arg + 1) < 0 )
					printf("Unknown switch %s - ignoring\n", arg - 1);
				break;
			case 'A':
				gs_alloc_debug = 1; break;
			case 'e':
				gs_log_errors = 1; break;
			case 'E':
				gs_log_errors = 2; break;
			case 'Z':
#ifdef DEBUG
				if ( !arg[1] )
				   {	/* No options, set all flags */
					memset(gs_debug, 0xff, 128);
				   }
				else
				   {	while ( *++arg )
						gs_debug[*arg & 127] = 0xff;
				   }
#else
				printf("Not a debugging configuration, -Z switch ignored\n");
#endif
				break;
			   }
		   }
		else
			(*arg_proc)(argp, arg_count++);
	   }
	return arg_count;
}

/* Free all resources and exit. */
void gs_malloc_release(P0());
void file_close_all(P0());
int gs_closedevice(P1(gx_device *));
void gp_exit(P0());
void
gs_exit(int code)
{	gx_device **pdev = gx_device_list;
	if ( code != 0 ) fflush(stderr);	/* in case of error exit */
	for ( ; *pdev != 0; pdev++ )
	  gs_closedevice(*pdev);
	/* Close all open files. */
	file_close_all();
	/* Release all memory acquired with malloc. */
	gs_malloc_release();
	/* Do platform-specific cleanup. */
	gp_exit();
	exit(code);
}

/* ------ Debugging routines ------ */

/* Log an error return */
int
gs_log_error(int err, const char _ds *file, int line)
{	if ( gs_log_errors )
	  { if ( file == NULL )
	      dprintf1("Returning error %d\n", err);
	    else
	      dprintf3("%s(%d): returning error %d\n",
		       (char *)file, line, err);
	  }
	return err;
}
