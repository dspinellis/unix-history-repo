/* config.h -- Configuration file for bash. */

/* Copyright (C) 1987,1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef _CONFIG_
#define _CONFIG_

#ifndef VOID
#ifdef NO_VOID
#define VOID char
#else
#define VOID void
#endif
#endif

#if defined (__GNUC__)
#  if !defined (HAVE_ALLOCA)
#    define HAVE_ALLOCA
#  endif /* HAVE_ALLOCA */
#  if !defined (BUILDING_MAKEFILE)
#    define alloca __builtin_alloca
#  endif
#else
#  if defined (HAVE_ALLOCA_H)
#    if !defined (HAVE_ALLOCA)
#      define HAVE_ALLOCA
#    endif /* HAVE_ALLOCA */
#    if !defined (BUILDING_MAKEFILE)
#      include <alloca.h>
#    endif
#  endif /* HAVE_ALLOCA_H */
#endif /* __GNUC__ */

#if defined (HPUX) || defined (UNIXPC) || defined (Xenix)
#  if !defined (USG)
#    define USG
#  endif
#endif

#if defined (HAVE_UNISTD_H) && !defined (BUILDING_MAKEFILE)
#include <unistd.h>
#endif

/* Define JOB_CONTROL if your operating system supports
   BSD-like job control. */
#define JOB_CONTROL

/* Note that vanilla System V machines don't support BSD job control,
   although some do support Posix job control. */
#if defined (USG) && !defined (_POSIX_JOB_CONTROL)
#undef JOB_CONTROL
#endif /* USG */

/* Define ALIAS if you want the alias features. */
#define ALIAS

/* Define PUSHD_AND_POPD if you want those commands to be compiled in.
   (Also the `dirs' commands.) */
#define PUSHD_AND_POPD

/* Define READLINE to get the nifty/glitzy editing features.
   This is on by default.  You can turn it off interactively
   with the -nolineediting flag. */
#define READLINE

/* If READLINE is defined, right now we assume that you have the full
   source code.  If you simply have the library and header files installed,
   then undefine HAVE_READLINE_SOURCE. */
#if defined (READLINE)
#  define HAVE_READLINE_SOURCE
#endif /* READLINE */

/* The default value of the PATH variable. */
#define DEFAULT_PATH_VALUE \
  ":/usr/gnu/bin:/usr/local/bin:/usr/ucb:/bin:/usr/bin:/etc:/usr/etc"

/* The value for PATH when invoking `command -p'.  This is only used when
   the Posix.2 confstr () function, or CS_PATH define are not present. */
#define STANDARD_UTILS_PATH \
  "/bin:/usr/bin:/usr/ucb:/usr/sbin:/etc:/usr/etc"

/* Define V9_ECHO if you want to give the echo builtin backslash-escape
   interpretation using the -e option, in the style of the Bell Labs 9th
   Edition version of echo. */
#define V9_ECHO

/* Define CONTINUE_AFTER_KILL_ERROR if you want the kill command to
   continue processing arguments after one of them fails. */
#define CONTINUE_AFTER_KILL_ERROR

/* Define BREAK_COMPLAINS if you want the non-standard, but useful
   error messages about `break' and `continue' out of context. */
#define BREAK_COMPLAINS

/* Define GETOPTS_BUILTIN if you want the Posix.2 `getopts' shell builtin
   compiled into the shell. */
#define GETOPTS_BUILTIN

/* When ALLOW_RIGID_POSIX_COMPLIANCE is defined, you can turn on strictly
   Posix compliant behaviour by setting the environment variable
   POSIX_PEDANTIC. */
#define ALLOW_RIGID_POSIX_COMPLIANCE

/* Define DISABLED_BUILTINS if you want "builtin foo" to always run the
   shell builtin "foo", even if it has been disabled with "enable -n foo". */
/* #define DISABLED_BUILTINS */

#endif	/* _CONFIG_ */
