/* Definitions for Intel 386 running system Vr4.
   Copyright (C) 1988 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by James Van Artsdalen, Dell Computer Corporation.
   james@bigtex.cactus.org */

#include "tm-i386v.h"

#undef STANDARD_STARTFILE_PREFIX
#define STANDARD_STARTFILE_PREFIX "/usr/ccs/lib/"

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} %{pg:gcrti.o%s}%{!pg:%{p:mcrti.o%s}%{!p:crti.o%s}} values-Xt.o%s"

#undef LIB_SPEC
#define LIB_SPEC "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} -Y P,/usr/ccs/lib:/usr/lib -Qy -lc crtn.o%s"

/* Brain-damaged v4 include files won't work right if __STDC__ != 0 */

#define STDC_VALUE 0

/* We do not want to output SDB debugging information.  */

#undef SDB_DEBUGGING_INFO

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
  do {								\
    char *p = (char *) strrchr(main_input_filename, '/');	\
    if (!p++)							\
      p = main_input_filename;					\
    fprintf ((FILE), "\t.file\t\"%s\"\n", p);			\
  } while (0)
