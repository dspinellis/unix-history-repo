/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 * [and modified yet again -wfj 1992]
 *
 *	@(#)tm-i386b.h	6.2 (Berkeley) 5/8/91
 */

/* Definitions for 386BSD ; derived from tm-seq386.h.
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

#include "tm-i386.h"

/* Use the BSD assembler syntax.  */

#include "tm-bsd386.h"

/* By default, target has a 80387.  */

#define TARGET_DEFAULT 1

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-Dunix -Di386 -D__386BSD__"

/* Don't permit / as a comment start character.  */

#undef COMMENT_BEGIN
#define COMMENT_BEGIN "#"

#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) fprintf (FILE, "#NO_APP\n");

/* We want to output DBX debugging information.  */

#define DBX_DEBUGGING_INFO
#undef DBX_NO_XREFS
#undef DBX_CONTIN_LENGTH

/* Floating-point return values come in the FP register.  */

#define VALUE_REGNO(MODE) \
  (((MODE)==SFmode || (MODE)==DFmode) ? FIRST_FLOAT_REG : 0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N)== FIRST_FLOAT_REG)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry. */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tmovl $LP%d,%%eax\n\tcall mcount\n", (LABELNO));
