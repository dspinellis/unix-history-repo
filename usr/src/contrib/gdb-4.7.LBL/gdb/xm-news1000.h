/* Parameters for a Sony/NEWS series 1000 with News-OS version 3,
   for GDB, the GNU debugger.
   Copyright (C) 1990 Free Software Foundation, Inc.

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

/* This is required by Sony include files like <sys/user.h> so we
   get the right offset into the u area.  Relying on the compiler
   to define this only works for cc, not gcc.  */
#undef mc68030
#define mc68030
#include "xm-news.h"
