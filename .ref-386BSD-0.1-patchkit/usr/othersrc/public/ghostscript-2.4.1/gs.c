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

/* gs.c */
/* Driver program for Ghostscript */
#include <stdio.h>
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "alloc.h"
#include "estack.h"
#include "ostack.h"
#include "store.h"
#include "stream.h"
  
#ifndef GS_LIB
#  define GS_LIB "GS_LIB"
#endif

#ifndef PROGRAM_NAME
#  define PROGRAM_NAME "Ghostscript"
#endif

/* Library routines not declared in a standard header */
extern char *getenv(P1(const char *));

/* Exported data */
uint memory_chunk_size = 20000;
/* File name search paths */
const char **gs_lib_paths;
private int gs_lib_count;
private char *gs_lib_env_path;

/* Configuration information imported from gconfig.c. */
extern const char *gs_lib_default_path;
extern const char *gs_init_file;
extern ref gs_init_file_array[];

/* Device procedures imported from gsdevice.c. */
typedef struct gx_device_s gx_device;
extern gx_device *gs_getdevice(P1(int));
extern char *gs_devicename(P1(gx_device *));

/* Help string */
private char *gs_help1 = "\
Usage: gs [switches] [file1.ps file2.ps ...]\n\
  or : gs [switches] [file1.ps ...] -- filen.ps arg1 arg2 ...\n\
The latter passes arg1 ... to the program in filen.ps.\n\
Available devices:\n   ";
/* We have to break help2 up into two parts, because the Watcom compiler */
/* has a limit of 510 characters for a single token. */
private char *gs_help2a = "\n\
Switches: (you can use # in place of =)\n\
    -d<name>[=<token>]   define name as token, or null if no token given\n\
    -g<width>x<height>   set width and height (`geometry') for device\n\
    -I<prefix>           add prefix to search path\n\
    -q                   `quiet' mode, suppress most messages\n\
    -r<res>              set resolution for initial device\n";
private char *gs_help2b = "\
    -r<xres>x<yres>      set device X and Y resolution separately\n\
    -s<name>=<string>    define name as string\n\
    -sDEVICE=<devname>   select initial device\n\
    -sOUTPUTFILE=<file>  select output file, embed %d for page #,\n\
                           |command to pipe\n\
`-' alone as a file name means read from stdin non-interactively.\n\
For more information, please read the use.doc file.\n";

/* Forward references */
private int esc_strlen(P1(const char *));
private void esc_strcat(P2(char *, const char *));
private void runarg(P4(char **, char *, char *, int));
private void run_string(P1(char *));
private void init1(), init2();
private void set_lib_paths();
private void run_file(P2(const char *file_name, int user_errors));
private void debug_dump_stack(P1(int code));

/* Parameters set by swproc */
private int user_errors;
private int quiet;
private int batch;

/* Static versions of argc and argv (for -- only) */
private int static_argc;
private char **static_argv;

main(int argc, char *argv[])
{	int num_files;
	int swproc(P2(char **, char *));
	void argproc(P2(char **, int));
	static_argc = argc;
	static_argv = argv;
	/* Do platform-dependent initialization. */
	/* We have to do this as the very first thing, */
	/* because it detects attempts to run 80N86 executables (N>0) */
	/* on incompatible processors. */
	gp_init();
	/* Initialize the file search paths */
	gs_lib_env_path = 0;
	   {	char *lib = getenv(GS_LIB);
		if ( lib != 0 ) 
		   {	int len = strlen(lib);
			gs_lib_env_path = gs_malloc(len + 1, 1, "GS_LIB");
			strcpy(gs_lib_env_path, lib);
		   }
	   }
	gs_lib_paths =
		(const char **)gs_malloc(argc + 2, sizeof(char *), "-I array");
	gs_lib_count = 0;
	set_lib_paths();
	/* Execute files named in the command line, */
	/* processing options along the way. */
	/* Wait until the first file name (or the end */
	/* of the line) to finish initialization. */
	batch = 0;
	quiet = 0;
	user_errors = 1;
	num_files = gs_main(argc, argv, swproc, argproc);
	if ( num_files == 0 )
	   {	init2();
	   }
	if ( !batch ) run_string("start");
	gs_exit(0);
}

/* Process switches */
int
swproc(char **swp, char *arg)
{	char sw = (*swp)[1];
	switch ( sw )
	   {
	default:
		return -1;
	case 0:				/* read stdin as a file */
		batch = 1;
		/* Set NOPAUSE so showpage won't try to read from stdin. */
		   {	char *d = "-dNOPAUSE";
			swproc(&d, d);
		   }
		init2();		/* Finish initialization */
		run_string("(%stdin) (r) file cvx execute");
		break;
	case '-':			/* run with command line args */
	case '+':			/* ditto */
	   {	int nstrs = static_argv + static_argc - (swp + 2);
		if ( nstrs < 0 )	/* no file to run! */
		   {	printf("Usage: gs ... -- file.ps arg1 ... argn\n");
			gs_exit(1);
		   }
		runarg(swp + 1, "{userdict /ARGUMENTS [", "] put (", nstrs);
	   }
		gs_exit(0);
	case 'h':			/* print help */
	case '?':
		fputs(gs_help1, stdout);
		   {	int i;
			gx_device *pdev;
			for ( i = 0; (pdev = gs_getdevice(i)) != 0; i++ )
				printf(" %s", gs_devicename(pdev));
		   }
		fputs(gs_help2a, stdout);
		fputs(gs_help2b, stdout);
		gs_exit(0);
	case 'I':			/* specify search path */
		gs_lib_paths[gs_lib_count] = arg;
		gs_lib_count++;
		set_lib_paths();
		break;
	case 'q':			/* quiet startup */
	   {	ref vnull;
		quiet = 1;
		init1();
		make_null(&vnull);
		initial_enter_name("QUIET", &vnull);
	   }	break;
	case 'D':			/* define name */
	case 'd':
	case 'S':			/* define name as string */
	case 's':
	   {	char *eqp = strchr(arg, '=');
		int isd = (sw == 'D' || sw == 'd');
		ref value;
		if ( eqp == NULL ) eqp = strchr(arg, '#');
		/* Initialize the object memory, scanner, and */
		/* name table now if needed. */
		init1();
		if ( eqp == arg )
		   {	printf("Usage: -dname, -dname=token, -sname=string\n");
			gs_exit(1);
		   }
		if ( eqp == NULL )
		   {	if ( isd ) make_null(&value);
			else make_tasv(&value, t_string, a_read+a_execute,
				       0, bytes, (byte *)"");
		   }
		else
		   {	int code;
			*eqp++ = 0;	/* delimit name */
			if ( isd )
			   {	stream astream;
				sread_string(&astream,
					     (byte *)eqp, strlen(eqp));
				code = scan_token(&astream, 0, &value);
				if ( code )
				   {	printf("-dname= must be followed by a valid token\n");
					gs_exit(1);
				   }
			   }
			else
			   {	int len = strlen(eqp);
				char *str = gs_malloc((uint)len, 1, "-s");
				if ( str == 0 )
				   {	lprintf("Out of memory!\n");
					gs_exit(1);
				   }
				memcpy(str, eqp, len);
				make_tasv(&value, t_string, a_read+a_execute,
					  len, bytes, (byte *)str);
			   }
		   }
		/* Enter the name in systemdict */
		initial_enter_name(arg, &value);
		break;
	   }
	case 'g':			/* define device geometry */
	   {	long width, height;
		ref value;
		init1();
		if ( sscanf(arg, "%ldx%ld", &width, &height) != 2 )
		   {	printf("-g must be followed by <width>x<height>\n");
			gs_exit(1);
		   }
		make_int(&value, width);
		initial_enter_name("DEVICEWIDTH", &value);
		make_int(&value, height);
		initial_enter_name("DEVICEHEIGHT", &value);
		break;
	   }
	case 'M':			/* set memory allocation increment */
	   {	unsigned msize = 0;
		sscanf(arg, "%d", &msize);
		if ( msize <= 0 || msize >= 64 )
		   {	printf("-M must be between 1 and 64\n");
			gs_exit(1);
		   }
		memory_chunk_size = msize << 10;
	   }
		break;
	case 'r':			/* define device resolution */
	   {	long xres, yres;
		ref value;
		init1();
		switch ( sscanf(arg, "%ldx%ld", &xres, &yres) )
		   {
		default:
			printf("-r must be followed by <res> or <xres>x<yres>\n");
			gs_exit(1);
		case 1:			/* -r<res> */
			yres = xres;
		case 2:			/* -r<xres>x<yres> */
			make_int(&value, xres);
			initial_enter_name("DEVICEXRESOLUTION", &value);
			make_int(&value, yres);
			initial_enter_name("DEVICEYRESOLUTION", &value);
		   }
		break;
	   }
	   }
	return 0;
}

/* Define versions of strlen and strcat that insert \ escapes */
/* before \, (, and ). */
#define needs_esc(ch) ((ch) == '(' || (ch) == ')' || (ch) == '\\')
private int
esc_strlen(const char *str)
{	int n = strlen(str);
	const char *p;
	for ( p = str; *p; p++ ) if ( needs_esc(*p) ) n++;
	return n;
}
private void
esc_strcat(char *dest, const char *src)
{	char *d = dest + strlen(dest);
	const char *p;
	for ( p = src; *p; p++ )
	   {	if ( needs_esc(*p) ) *d++ = '\\';
		*d++ = *p;
	   }
	*d = 0;
}

/* Process file names */
void
argproc(char **argp, int index)
{	runarg(argp, "{", "(", 0);
}
private void
runarg(char **argp, char *pre, char *post, int nstrs)
{	char *arg = *argp;
	static char *pex = ")run}execute";
	int len = strlen(pre) + esc_strlen(arg) + strlen(post) + strlen(pex) + 1;
	char *line;
	int i;
	for ( i = 1; i <= nstrs; i++ )
		len += esc_strlen(argp[i]) + 2;
	init2();	/* Finish initialization */
	line = gs_malloc(len, 1, "argproc");
	if ( line == 0 )
	   {	lprintf("Out of memory!\n");
		gs_exit(1);
	   }
	strcpy(line, pre);
	for ( i = 1; i <= nstrs; i++ )
	   {	strcat(line, "(");
		esc_strcat(line, argp[i]);
		strcat(line, ")");
	   }
	strcat(line, post);
	esc_strcat(line, arg);
	strcat(line, pex);
	run_string(line);
}
private void
run_string(char *str)
{	int code;
	ref stref;
	make_tasv(&stref, t_string, a_executable + a_read + a_execute,
		  strlen(str), bytes, (byte *)str);
	code = gs_interpret(&stref, user_errors);
	zflush((ref *)0);	/* flush stdout */
	zflushpage((ref *)0); /* force display update */
	if ( code ) debug_dump_stack(code), gs_exit(2);
}

private int init1_done = 0, init2_done = 0;
private void
init1()
{	if ( !init1_done )
	   {	alloc_init(gs_malloc, gs_free, memory_chunk_size);
		name_init();
		obj_init();		/* requires name_init */
		scan_init();		/* ditto */
		init1_done = 1;
	   }
}
private void
init2()
{	init1();
	if ( !init2_done )
	   {	gs_init();
		zop_init();
		interp_init(1);		/* requires obj_init */
		op_init();		/* requires obj_init, scan_init */
		/* Set up the array of additional initialization files. */
		{	ref *ifp = gs_init_file_array;
			ref ifa;
			for ( ; ifp->value.bytes != 0; ifp++ )
			  r_set_size(ifp, strlen((const char *)ifp->value.bytes));
			make_tasv(&ifa, t_array, a_read + a_execute,
				  ifp - gs_init_file_array, refs,
				  gs_init_file_array);
			initial_enter_name("INITFILES", &ifa);
		}
		/* Execute the standard initialization file. */
		run_file(gs_init_file, user_errors);
		init2_done = 1;
	   }
   }

/* Complete the list of library search paths. */
private void
set_lib_paths()
{	const char **ppath = &gs_lib_paths[gs_lib_count];
	if ( gs_lib_env_path != 0 ) *ppath++ = gs_lib_env_path;
	if ( gs_lib_default_path != 0 ) *ppath++ = gs_lib_default_path;
	*ppath = 0;
}

/* Open and execute a file */
private int
run_open(const char *file_name, ref *pfile)
{
#define maxfn 200
	byte fn[maxfn];
	uint len;
	return lib_file_open(file_name, strlen(file_name), fn, maxfn,
			     &len, pfile);
}
private void
run_file(const char *file_name, int user_errors)
{	ref initial_file;
	int code;
	if ( run_open(file_name, &initial_file) < 0 )
	   {	eprintf1("Can't find initialization file %s\n", file_name);
		gs_exit(1);
	   }
	r_set_attrs(&initial_file, a_execute + a_executable);
	code = gs_interpret(&initial_file, user_errors);
	if ( code < 0 )
		debug_dump_stack(code), gs_exit(1);
}

/* Debugging code */
extern void debug_print_ref(P1(ref *));
extern void debug_dump_refs(P3(ref *, uint, char *));
extern ref error_object;

/* Dump the stacks after interpretation */
private void
debug_dump_stack(int code)
{	zflush(osp);	/* force out buffered output */
	dprintf1("\nUnexpected interpreter error %d!\nError object: ", code);
	debug_print_ref(&error_object);
	dputc('\n');
	debug_dump_refs(osbot, osp + 1 - osbot, "Operand stack");
	debug_dump_refs(esbot, esp + 1 - esbot, "Execution stack");
}
