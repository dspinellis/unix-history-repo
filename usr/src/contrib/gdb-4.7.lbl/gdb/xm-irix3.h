/* Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of GDB.

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

/* This is for the iris. */

#define HAVE_TERMIO

#include "xm-bigmips.h"

/* Override register locations in upage for SGI machines */
#undef REGISTER_U_ADDR
#define REGISTER_U_ADDR(addr, blockend, regno) 		\
  if (regno < PC_REGNUM)				\
      addr = regno;					\
  else							\
      addr = regno + NSIG_HNDLRS; /* Skip over signal handlers */

