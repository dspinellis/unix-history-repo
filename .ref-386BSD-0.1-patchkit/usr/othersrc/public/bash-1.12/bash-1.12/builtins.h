/* builtins.h -- What a builtin looks like, and where to find them. */

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

#include "config.h"
#include "general.h"

#if defined (ALIAS)
#include "alias.h"
#endif

/* The thing that we build the array of builtins out of. */
struct builtin {
  char *name;
  Function *function;
  int enabled;
  char **long_doc;		/* NULL terminated array of strings. */
  char *short_doc;
};

/* Found in builtins.c, created by builtins/mkbuiltins. */
extern struct builtin shell_builtins[];
extern int num_shell_builtins;


