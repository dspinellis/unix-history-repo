/* BFD back-end for HP 9000/300 (68000-based) machines running BSD Unix.
   Copyright 1992 Free Software Foundation, Inc.

This file is part of BFD, the Binary File Descriptor library.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define TARGET_IS_BIG_ENDIAN_P
#define N_HEADER_IN_TEXT(x) 0
#define BYTES_IN_WORD 4
#define ARCH 32
#define ENTRY_CAN_BE_ZERO
#define N_SHARED_LIB(x) 0 /* Avoids warning */
#define TEXT_START_ADDR 0
#define PAGE_SIZE 4096
#define SEGMENT_SIZE PAGE_SIZE
#define DEFAULT_ARCH bfd_arch_m68k

#define MY(OP) CAT(luna68k_,OP)
#define TARGETNAME "a.out-luna68k"

#include "bfd.h"
#include "sysdep.h"
#include "libbfd.h"
#include "libaout.h"

#include "aout-target.h"
