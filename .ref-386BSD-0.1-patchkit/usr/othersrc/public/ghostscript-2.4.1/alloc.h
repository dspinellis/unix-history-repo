/* Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* alloc.h */
/* Interface to Ghostscript memory allocator */

/* Ordinary allocator interface */
void	alloc_init(P3(proc_alloc_t, proc_free_t, uint));
char	*alloc(P3(uint num_elts, uint elt_size, const char *client_name));
void	alloc_free(P4(char *data, uint num_elts, uint elt_size, const char *client_name));
void	alloc_status(P2(long *, long *));

/* Dynamic allocation */
/* Note that these procedures use byte * rather than char * */
byte	*alloc_grow(P5(byte *data, uint old_num, uint new_num, uint elt_size, const char *client_name));
byte	*alloc_shrink(P5(byte *data, uint old_num, uint new_num, uint elt_size, const char *client_name));
