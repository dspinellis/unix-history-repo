/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gsprops.h */
/* "Property list" definitions for Ghostscript */

/*
 * Several interfaces in Ghostscript use the idea of a "property list",
 * essentially a dictionary with a fixed set of key names and known
 * value types.  A property list is represented by an array of structures
 * with four components:
 *
 *	- The name of the property;
 *
 *	- The type of value provided (or expected);
 *
 *	- The value itself;
 *
 *	- An indication of whether the value was supplied, and if so,
 * whether it was acceptable.
 *
 * Currently, property lists are only used to communicate parameter values
 * to devices.
 */

/* Define the types of values. */
typedef enum {
	prt_int,			/* (long) */
	prt_float,
	prt_bool,
	prt_string,
	prt_int_array,			/* array of prt_int item */
	prt_float_array,		/* array of prt_float item */
	prt_null
} gs_prop_type;
typedef int p_bool;
/* Arrays and strings must represent their size explicitly. */
typedef union gs_prop_value_s gs_prop_value;

/* Define the type for property list items. */
#ifndef gs_prop_item_DEFINED
#  define gs_prop_item_DEFINED
typedef struct gs_prop_item_s gs_prop_item;
#endif

/* Define the union of all possible value types. */
union gs_prop_value_s {
	long i;
	float f;
	p_bool b;
	struct {
		ushort size;
		union {
			char *s;
			gs_prop_item *v;
		} p;
	} a;
};

/* Define the status of an entry in a property list. */
typedef enum {
	pv_unspecified,			/* no value specified */
	pv_set,				/* other explicit value */
		/* Recipients return these codes */
	pv_OK,
	pv_unknown,			/* unknown key */
	pv_typecheck,
	pv_rangecheck,
	pv_limitcheck
} gs_prop_status;

/* Finally, define the structure of a property item. */
struct gs_prop_item_s {
	const char *pname;
	int name_size;			/* -1 means use strlen */
	gs_prop_type type;
	gs_prop_status status;
	gs_prop_value value;
};
#define gs_prop_item_s_DEFINED

/*
 * It is often convenient to construct a property list template statically,
 * and just copy it and fill in the values.  Here are some macros useful
 * for doing this.  The format is
 *	prop_item xyz_props[] = {
 *		prop_def("name1", prt1),
 *		prop_def("name2", prt2),
 *		...
 *	};
 * or
 *	typedef struct {
 *		prop_item name1, name2, ...;
 *	xyz_props_struct;
 *	xyz_props_struct xyz_props = {
 *		prop_def("name1", prt1),
 *		prop_def("name2", prt2),
 *		...
 *	};
 * For slots that form part of an array, use prop_slot instead of prop_def.
 */
#define prop_def(name, type) { name, -1, type, pv_set }
#define prop_int { 0, -1, prt_int, pv_set }
#define prop_float { 0, -1, prt_float, pv_set }
