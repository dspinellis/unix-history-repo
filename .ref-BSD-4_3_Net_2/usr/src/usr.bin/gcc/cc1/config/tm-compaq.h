/* Definitions for Compaq as target machine.  NOT TESTED!
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

/* Use the ATT assembler syntax.  */

#include "tm-att386.h"

/* By default, target has a 80387.  */

#define TARGET_DEFAULT 1

#define ASM_SPEC ""

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Di386 -Di80386 -Dunix"


#include "tm-i386.h"
#include "tm-att386.h"
