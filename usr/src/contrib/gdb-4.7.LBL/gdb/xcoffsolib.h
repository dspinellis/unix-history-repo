/* Data structures for RS/6000 shared libraries, for GDB.
   Copyright 1991, 1992 Free Software Foundation, Inc.

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

/*
   the vmap struct is used to describe the virtual address space of
   the target we are manipulating.  The first entry is always the "exec"
   file.  Subsequent entries correspond to other objects that are
   mapped into the address space of a process created from the "exec" file.
   These are either in response to exec()ing the file, in which case all
   shared libraries are loaded, or a "load" system call, followed by the
   user's issuance of a "load" command. */

struct vmap {
	struct vmap *nxt;	/* ^ to next in chain			*/
	bfd *bfd;		/* BFD for mappable object library	*/
	char *name;		/* ^ to object file name		*/
	char *member;		/* ^ to member name			*/
	CORE_ADDR tstart;	/* virtual addr where member is mapped	*/
	CORE_ADDR tend;		/* virtual upper bound of member	*/
	CORE_ADDR tadj;		/* heuristically derived adjustment	*/
	CORE_ADDR dstart;	/* virtual address of data start	*/
	CORE_ADDR dend;		/* vitrual address of data end		*/

	CORE_ADDR ostart;	/* objext start ???			*/
	unsigned  loaded:1;	/* True if symbols are loaded		*/
	unsigned  reloced:1;	/* True, if symbols relocated		*/
	unsigned  padding:14;
};


struct vmap_and_bfd {
  bfd *pbfd;
  struct vmap *pvmap;
};

extern struct vmap *vmap;
