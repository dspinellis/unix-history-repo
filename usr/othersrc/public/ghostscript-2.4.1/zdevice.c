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

/* zdevice.c */
/* Device-related operators for GhostScript */
#include "ghost.h"
#include "alloc.h"
#include "errors.h"
#include "oper.h"
#include "state.h"
#include "gsmatrix.h"
#include "gsstate.h"
#include "gxdevice.h"
#include "store.h"

/* copy for devices -- called from zcopy in zgeneric.c. */
int
zcopy_device(register os_ptr op)
{	gx_device *new_dev;
	int code = gs_copydevice(&new_dev, op->value.pdevice, alloc);
	if ( code < 0 ) return code;
	make_tv(op, t_device, pdevice, new_dev);
	return 0;
}

/* copyscanlines */
int
zcopyscanlines(register os_ptr op)
{	os_ptr op1 = op - 1;
	os_ptr op2 = op - 2;
	gx_device *dev;
	int code;
	uint bytes_copied;
	check_type(*op2, t_device);
	dev = op2->value.pdevice;
	check_type(*op1, t_integer);
	if ( op1->value.intval < 0 || op1->value.intval > dev->height )
		return e_rangecheck;
	check_write_type(*op, t_string);
	code = gs_copyscanlines(dev, (int)op1->value.intval,
		op->value.bytes, r_size(op), NULL, &bytes_copied);
	if ( code < 0 ) return e_typecheck;	/* not a memory device */
	*op2 = *op;
	r_set_size(op2, bytes_copied);
	pop(2);
	return 0;
}

/* currentdevice */
int
zcurrentdevice(register os_ptr op)
{	gx_device *dev = gs_currentdevice(igs);
	push(1);
	make_tv(op, t_device, pdevice, dev);
	return 0;
}

/* devicename */
int
zdevicename(register os_ptr op)
{	const char *dname;
	int code;
	check_type(*op, t_device);
	dname = gs_devicename(op->value.pdevice);
	code = string_to_ref(dname, op, "devicename");
	if ( code < 0 ) return code;
	return 0;
}

/* deviceinitialmatrix */
int
zdeviceinitialmatrix(register os_ptr op)
{	int code = write_matrix(op);
	if ( code < 0 ) return code;
	check_type(op[-1], t_device);
	gs_deviceinitialmatrix(op[-1].value.pdevice, (gs_matrix *)op->value.refs);
	op[-1] = *op;
	pop(1);
	return 0;
}

/* flushpage */
int
zflushpage(register os_ptr op)
{	return gs_flushpage(igs);
}

/* getdevice */
int
zgetdevice(register os_ptr op)
{	gx_device *dev;
	check_type(*op, t_integer);
	if ( op->value.intval != (int)(op->value.intval) )
		return e_rangecheck;	/* won't fit in an int */
	dev = gs_getdevice((int)(op->value.intval));
	if ( dev == 0 ) return e_rangecheck;	/* index out of range */
	make_tv(op, t_device, pdevice, dev);
	return 0;
}

/* makeimagedevice */
int
zmakeimagedevice(register os_ptr op)
{	gs_matrix imat;
	gx_device *new_dev;
	byte *colors;
	int num_colors;
	int code;
	check_type(op[-2], t_integer);	/* width */
	check_type(op[-1], t_integer);	/* height */
	if ( r_has_type(op, t_null) )	/* true color */
	   {	colors = 0;
		num_colors = -24;	/* 24-bit true color */
	   }
	else
	   {	check_type(*op, t_string);	/* palette */
		if ( r_size(op) > 3*256 ) return e_rangecheck;
		colors = op->value.bytes;
		num_colors = r_size(op);
	   }
	if (	(ulong)(op[-2].value.intval) > max_uint >> 1 ||
		(ulong)(op[-1].value.intval) > max_uint >> 1
	   ) return e_rangecheck;
	if ( (code = read_matrix(op - 3, &imat)) < 0 ) return code;
	/* Everything OK, create device */
	code = gs_makeimagedevice(&new_dev, &imat,
				  (int)op[-2].value.intval,
				  (int)op[-1].value.intval,
				  colors, num_colors, alloc);
	if ( code == 0 )
	   {	make_tv(op - 3, t_device, pdevice, new_dev);
		pop(3);
	   }
	return code;
}

/* nulldevice */
int
znulldevice(register os_ptr op)
{	gs_nulldevice(igs);
	return 0;
}

/* .outputpage */
int
zoutputpage(register os_ptr op)
{	int code;
	check_type(op[-1], t_integer);
	check_type(*op, t_boolean);
	code = gs_output_page(igs, (int)op[-1].value.intval, op->value.index);
	if ( code < 0 ) return code;
	pop(2);
	return 0;
}

/* setdevice */
int
zsetdevice(register os_ptr op)
{	int code;
	check_type(*op, t_device);
	code = gs_setdevice(igs, op->value.pdevice);
	if ( code == 0 ) pop(1);
	return code;
}

/* ------ Initialization procedure ------ */

op_def zdevice_op_defs[] = {
	{"3copyscanlines", zcopyscanlines},
	{"0currentdevice", zcurrentdevice},
	{"2deviceinitialmatrix", zdeviceinitialmatrix},
	{"1devicename", zdevicename},
	{"0flushpage", zflushpage},
	{"1getdevice", zgetdevice},
	{"4makeimagedevice", zmakeimagedevice},
	{"0nulldevice", znulldevice},
	{"2.outputpage", zoutputpage},
	{"1setdevice", zsetdevice},
	op_def_end(0)
};
