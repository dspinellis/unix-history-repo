/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gconfig.c */
/* Installed device table for Ghostscript */
#include "ghost.h"
/*
 * Since we only declare variables of type gx_device *,
 * it should be sufficient to define struct gx_device_s as
 * an abstract (undefined) structure.  However, the VAX VMS compiler
 * isn't happy with this, so we have to include the full definition.
 */
#include "gxdevice.h"
/*
 * We generate an array of strings as a static structure:
 */
#define ref_(t) struct { struct tas_s tas; t value; }
#define string_v(n,s)\
 { {(t_string<<r_type_shift)+a_read+a_execute, n}, s }

/*
 * The Ghostscript makefile generates the file gconfig.h, which consists of
 * lines of the form
 *	device_(gs_xxx_device)
 * for each installed device,
 *	oper_(zxxx_op_defs)
 * for each operator option, and
 *	psfile_("gs_xxxx.ps")
 * for each optional initialization file.
 *
 * We include this file multiple times to generate various different
 * source structures.  (It's a hack, but we haven't come up with anything
 * more satisfactory.)
 */

/* Here is where the library search path and the name of the */
/* initialization file are defined.  We supply defaults just in case */
/* someone compiles the file without the proper command line flags. */
#ifndef GS_LIB_DEFAULT
#  define GS_LIB_DEFAULT ""
#endif
#ifndef GS_INIT
#  define GS_INIT "gs_init.ps"
#endif
const char *gs_lib_default_path = GS_LIB_DEFAULT;
const char *gs_init_file = GS_INIT;

/* Optional operators are handled in iinit.c. */
#define oper_(defs)

#define psfile_(fns)

/* Declare the devices as extern. */
#define device_(dev) extern gx_device dev;
#include "gconfig.h"
#undef device_

/* Set up the device table. */
#define device_(dev) &dev,
gx_device *gx_device_list[] = {
#include "gconfig.h"
	0
};
#undef device_
#define device_(dev)

/* Set up the .ps file name string array. */
/* We fill in the lengths at initialization time. */
#define ref_(t) struct { struct tas_s tas; t value; }
#define string_(s)\
 { {(t_string<<r_type_shift)+a_read+a_execute, 0}, s },
#undef psfile_
#define psfile_(fns) string_(fns)
ref_(const char *) gs_init_file_array[] = {
#include "gconfig.h"
	string_(0)
};

/* Some C compilers insist on executable code here, so.... */
void
gconfig_dummy()
{
}
