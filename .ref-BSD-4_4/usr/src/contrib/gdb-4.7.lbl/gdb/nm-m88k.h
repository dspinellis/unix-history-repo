/* Native support macros for m88k, for GDB.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991, 1992
   Free Software Foundation, Inc.

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

#define FETCH_INFERIOR_REGISTERS

#define REGISTER_U_ADDR(addr, blockend, regno) \
        (addr) = m88k_register_u_addr ((blockend),(regno));
