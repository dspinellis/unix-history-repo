/*
 * config.h -- configuration definitions for gawk.
 *
 * For VMS (assumes V4.6 or later; tested on V5.3 and V5.4)
 */

/* 
 * Copyright (C) 1991, 1992 the Free Software Foundation, Inc.
 * 
 * This file is part of GAWK, the GNU implementation of the
 * AWK Progamming Language.
 * 
 * GAWK is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 * 
 * GAWK is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with GAWK; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/**************************/
/* Miscellanious features */
/**************************/

/*
 * BLKSIZE_MISSING
 *	VMS: missing--not applicable
 * Check your /usr/include/sys/stat.h file.  If the stat structure
 * does not have a member named st_blksize, define this.  (This will
 * most likely be the case on most System V systems prior to V.4.)
 */
#define BLKSIZE_MISSING	1

/*
 * SIGTYPE
 *	VMS: either should work; void is 'correct'
 * The return type of the routines passed to the signal function.
 * Modern systems use `void', older systems use `int'.
 * If left undefined, it will default to void.
 */
#define SIGTYPE	void

/*
 * SIZE_T_MISSING
 *	VMS: <stddef.h> via <stdlib.h> (VAX C V2.3 & up); <sys/types.h> (GNU C)
 * If your system has no typedef for size_t, define this to get a default
 */
/* #define	SIZE_T_MISSING	1 */

/*
 * CHAR_UNSIGNED
 *	VMS: well behaved, either signed or unsigned (signed by default)
 * If your machine uses unsigned characters (IBM RT and RS/6000 and others)
 * then define this for use in regex.c
 */
/* #define CHAR_UNSIGNED	1 */

/*
 * HAVE_UNDERSCORE_SETJMP
 *	VMS: not present
 * Check in your /usr/include/setjmp.h file.  If there are routines
 * there named _setjmp and _longjmp, then you should define this.
 * Typically only systems derived from Berkeley Unix have this.
 */
/* #define HAVE_UNDERSCORE_SETJMP	1 */

/***********************************************/
/* Missing library subroutines or system calls */
/***********************************************/

/*
 * MEMCMP_MISSING
 * MEMCPY_MISSING
 * MEMSET_MISSING
 *	VMS: <string.h> (introduced V4.6)
 * These three routines are for manipulating blocks of memory. Most
 * likely they will either all three be present or all three be missing,
 * so they're grouped together.
 */
/* #define MEMCMP_MISSING	1 */
/* #define MEMCPY_MISSING	1 */
/* #define MEMSET_MISSING	1 */

/*
 * RANDOM_MISSING
 *	VMS: missing (as of V5.4)
 * Your system does not have the random(3) suite of random number
 * generating routines.  These are different than the old rand(3)
 * routines!
 */
#define RANDOM_MISSING	1

/*
 * STRCASE_MISSING
 *	VMS: missing
 * Your system does not have the strcasemp() and strncasecmp()
 * routines that originated in Berkeley Unix.
 */
#define STRCASE_MISSING	1

/*
 * STRCHR_MISSING
 *	VMS: <string.h>
 * Your system does not have the strchr() and strrchr() functions.
 */
/* #define STRCHR_MISSING	1 */

/*
 * STRERROR_MISSING
 *	VMS: <stdlib.h> (introduced V4.6)
 * Your system lacks the ANSI C strerror() routine for returning the
 * strings associated with errno values.
 */
/* #define STRERROR_MISSING	1 */

/*
 * STRTOD_MISSING
 *	VMS: <stdlib.h> (introduced V4.6)
 * Your system does not have the strtod() routine for converting
 * strings to double precision floating point values.
 */
/* #define STRTOD_MISSING	1 */

/*
 * STRFTIME_MISSING
 *	VMS: missing (as of V5.4) [see below; do not change STRFTIME_MISSING]
 * Your system lacks the ANSI C strftime() routine for formatting
 * broken down time values.
 */
#define STRFTIME_MISSING	1

/*
 * TZSET_MISSING
 *	VMS: missing, but can't use missing/tzset.c  [no timezone support]
 * If you have a 4.2 BSD vintage system, then the strftime() routine
 * supplied in the missing directory won't be enough, because it relies on the
 * tzset() routine from System V / Posix.  Fortunately, there is an
 * emulation for tzset() too that should do the trick.  If you don't
 * have tzset(), define this.
 */
/* #define TZSET_MISSING	1 */

/*
 * TZNAME_MISSING
 *
 * Some systems do not support the external variables tzname and daylight.
 * If this is the case *and* strftime() is missing, define this.
 */
/* #define TZNAME_MISSING	1 */

/*
 * STDC_HEADERS
 *	VMS: close enough (as of V4.6, VAX C V2.3) [GCC, see below]
 * If your system does have ANSI compliant header files that
 * provide prototypes for library routines, then define this.
 */
#define STDC_HEADERS	1

/*
 * NO_TOKEN_PASTING
 *	VMS: compiler specific--see below
 * If your compiler define's __STDC__ but does not support token
 * pasting (tok##tok), then define this.
 */
/* #define NO_TOKEN_PASTING	1 */

/*****************************************************************/
/* Stuff related to the Standard I/O Library.			 */
/*****************************************************************/
/* Much of this is (still, unfortunately) black magic in nature. */
/* You may have to use some or all of these together to get gawk */
/* to work correctly.						 */
/*****************************************************************/

/*
 * NON_STD_SPRINTF
 *	VMS: ok
 * Look in your /usr/include/stdio.h file.  If the return type of the
 * sprintf() function is NOT `int', define this.
 */
/* #define NON_STD_SPRINTF	1 */

/*
 * VPRINTF_MISSING
 *	VMS: ok (introduced V4.6)
 * Define this if your system lacks vprintf() and the other routines
 * that go with it.  This will trigger an attempt to use _doprnt().
 * If you don't have that, this attempt will fail and you are on your own.
 */
/* #define VPRINTF_MISSING	1 */

/*
 * Casts from size_t to int and back.  These will become unnecessary
 * at some point in the future, but for now are required where the
 * two types are a different representation.
 */
/* #define SZTC */
/* #define INTC */

/*
 * SYSTEM_MISSING
 *	VMS: ok (introduced V4.6)
 * Define this if your library does not provide a system function
 * or you are not entirely happy with it and would rather use
 * a provided replacement (atari only).
 */
/* #define SYSTEM_MISSING   1 */

/*
 * FMOD_MISSING
 *	VMS: ok (introduced V4.6)
 * Define this if your system lacks the fmod() function and modf() will
 * be used instead.
 */
/* #define FMOD_MISSING	1 */


/*******************************/
/* Gawk configuration options. */
/*******************************/

/*
 * DEFPATH
 *	VMS: "/AWK_LIBRARY" => "AWK_LIBRARY:"
 * The default search path for the -f option of gawk.  It is used
 * if the AWKPATH environment variable is undefined.
 *
 * Note: OK even if no AWK_LIBRARY logical name has been defined.
 */

#define DEFPATH	".,/AWK_LIBRARY"
#define ENVSEP	','

/*
 * alloca already has a prototype defined - don't redefine it
 */
/* #define ALLOCA_PROTO	1 */

/*
 * srandom already has a prototype defined - don't redefine it
 */
/* #define SRANDOM_PROTO	1 */

/*
 * Extended source file access.
 */
#define DEFAULT_FILETYPE ".awk"

/*
 * Pipe handling.
 */
#define PIPES_SIMULATED	1

/*
 * %g format in VAXCRTL is broken (chooses %e format when should use %f).
 */
#define GFMT_WORKAROUND	1

/*
 * VAX C
 *
 * As of V3.2, VAX C is not yet ANSI-compliant.  But it's close enough
 * for GAWK's purposes.  Comment this out for VAX C V2.4 and earlier.
 * Value of 0 should mean "not ANSI-C", but GAWK uses def/not-def tests.
 * YYDEBUG definition is needed for combination of VAX C V2.x and Bison.
 */
#if defined(VAXC) && !defined(__STDC__)
#define __STDC__	0
#define NO_TOKEN_PASTING
#ifndef __DECC	/* DEC C does not support #pragma builtins even in VAXC mode */
#define VAXC_BUILTINS
#endif
/* #define YYDEBUG 0 */
#endif

/*
 * DEC C
 *
 * Digital's ANSI complier.
 */
#ifdef __DECC
 /* nothing special at the moment */
#endif

/*
 * GNU C
 *
 * Versions of GCC (actually GAS) earlier than 1.38 don't produce the
 * right code for ``extern const'' constructs, and other usages of
 * const might not be right either.  The old set of include files from
 * the gcc-vms distribution did not contain prototypes, and this could
 * provoke some const-related compiler warnings.  If you've got an old
 * version of gcc for VMS, define 'const' out of existance, and by all
 * means obtain the most recent version!
 *
 * Note: old versions of GCC should also avoid defining STDC_HEADERS,
 *       because most of the ANSI-C required header files are missing.
 */
#ifdef __GNUC__
/* #define const */
/* #undef STDC_HEADERS */
#ifndef STDC_HEADERS
#define alloca __builtin_alloca
#define environ $$PsectAttributes_NOSHR$$environ	/* awful GAS kludge */
#endif
#endif

#ifdef STRFTIME_MISSING
/*
 * Always use the version of strftime() in missing/strftime.c instead of
 * the [as yet undocumented/unsupported] one in VAXCRTL.  Renaming it here
 * guarantees that it won't clash with the library routine.
 */
#define strftime gnu_strftime
#endif
