/* Declarations of BFD back end for traditional Unix core files 
   Copyright (C) 1990-1991 Free Software Foundation, Inc.
   Written by Cygnus Support.  Mostly John Gilmore's fault.

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

#include "ansidecl.h"

/* forward declaration */
PROTO (bfd_target *, trad_unix_core_file_p, (bfd *abfd));
PROTO (char *, 	     trad_unix_core_file_failing_command, (bfd *abfd));
PROTO (int,	     trad_unix_core_file_failing_signal, (bfd *abfd));
PROTO (boolean,      trad_unix_core_file_matches_executable_p,
			 (bfd *core_bfd, bfd *exec_bfd));
