/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this source code without charge, but are not authorized
 * to license or distribute it to anyone else except as part of a product or
 * program developed by the user.
 *
 * THIS PROGRAM CONTAINS SOURCE CODE COPYRIGHTED BY SUN MICROSYSTEMS, INC.
 * SUN MICROSYSTEMS, INC., MAKES NO REPRESENTATIONS ABOUT THE SUITABLITY
 * OF SUCH SOURCE CODE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT
 * EXPRESS OR IMPLIED WARRANTY OF ANY KIND.  SUN MICROSYSTEMS, INC. DISCLAIMS
 * ALL WARRANTIES WITH REGARD TO SUCH SOURCE CODE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN
 * NO EVENT SHALL SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY SPECIAL, INDIRECT,
 * INCIDENTAL, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
 * FROM USE OF SUCH SOURCE CODE, REGARDLESS OF THE THEORY OF LIABILITY.
 * 
 * This source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction, 
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS
 * SOURCE CODE OR ANY PART THEREOF.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California 94043
 */

/*	@(#)link.h 1.6 88/08/19 SMI	*/

/*
 * Copyright (c) 1987, 1991 by Sun Microsystems, Inc.
 */

/*
 * Link editor public definitions.
 */

#ifndef _link_h
#define _link_h

/*
 * Structure describing logical name and requirements on an object
 * which is to be loaded dynamically.
 */
struct old_link_object {
	char	*lo_name;		/* name of object */
	int	lo_library : 1,		/* searched for by library rules */
		lo_unused : 31;
	short	lo_major;		/* major version number */
	short	lo_minor;		/* minor version number */
};

struct link_object {
	long	lo_name;		/* name (often relative) */
	int	lo_library : 1,		/* searched for by library rules */
		lo_unused : 31;
	short	lo_major;		/* major version number */
	short	lo_minor;		/* minor version number */
	long	lo_next;		/* next one (often relative) */
};

/*
 * Structure describing name and placement of dynamically loaded
 * objects in a process' address space.
 */
struct link_map {
	caddr_t	lm_addr;		/* address at which object mapped */
	char 	*lm_name;		/* full name of loaded object */
	struct	link_map *lm_next;	/* next object in map */
	struct	link_object *lm_lop;	/* link object that got us here */
	caddr_t lm_lob;			/* base address for said link object */
	int	lm_rwt : 1;		/* text is read/write */
	struct	link_dynamic *lm_ld;	/* dynamic structure */
	caddr_t	lm_lpd;			/* loader private data */
};

/*
 * Version 1 of dynamic linking information.  With the exception of
 * ld_loaded (determined at execution time) and ld_stab_hash (a special
 * case of relocation handled at execution time), the values in this
 * structure reflect offsets from the containing link_dynamic structure.
 */
struct link_dynamic_1 {
	struct	link_map *ld_loaded;	/* list of loaded objects */
	long	ld_need;		/* list of needed objects */
	long	ld_rules;		/* search rules for library objects */
	long	ld_got;			/* global offset table */
	long	ld_plt;			/* procedure linkage table */
	long	ld_rel;			/* relocation table */
	long	ld_hash;		/* symbol hash table */
	long	ld_stab;		/* symbol table itself */
	long	(*ld_stab_hash)();	/* "pointer" to symbol hash function */
	long	ld_buckets;		/* number of hash buckets */
	long	ld_symbols;		/* symbol strings */
	long	ld_symb_size;		/* size of symbol strings */
	long	ld_text;		/* size of text area */
};

struct link_dynamic_2 {
	struct	link_map *ld_loaded;	/* list of loaded objects */
	long	ld_need;		/* list of needed objects */
	long	ld_rules;		/* search rules for library objects */
	long	ld_got;			/* global offset table */
	long	ld_plt;			/* procedure linkage table */
	long	ld_rel;			/* relocation table */
	long	ld_hash;		/* symbol hash table */
	long	ld_stab;		/* symbol table itself */
	long	(*ld_stab_hash)();	/* "pointer" to symbol hash function */
	long	ld_buckets;		/* number of hash buckets */
	long	ld_symbols;		/* symbol strings */
	long	ld_symb_size;		/* size of symbol strings */
	long	ld_text;		/* size of text area */
	long	ld_plt_sz;		/* size of procedure linkage table */
};

/*
 * Structure pointing to run time allocated common symbols and
 * its string.
 */
struct rtc_symb {
	struct	nlist *rtc_sp;		/* symbol for common */
	struct	rtc_symb *rtc_next;	/* next common */
};

/*
 * Debugger interface structure.
 */
struct 	ld_debug {
	int	ldd_version;		/* version # of interface */
	int	ldd_in_debugger;	/* a debugger is running us */
	int	ldd_sym_loaded;		/* we loaded some symbols */
	char    *ldd_bp_addr;		/* place for ld-generated bpt */
	int	ldd_bp_inst;		/* instruction which was there */
	struct rtc_symb *ldd_cp;	/* commons we built */
};

/*
 * Structure associated with each object which may be or which requires
 * execution-time link editing.  Used by the run-time linkage editor to
 * identify needed objects and symbol definitions and references.
 */
struct 	old_link_dynamic {
	int	ld_version;		/* version # of this structure */
	union {
		struct link_dynamic_1 ld_1;
	} ld_un;

	int	in_debugging;
	int	sym_loaded;
	char    *bp_addr;
	int	bp_inst;
	struct rtc_symb *cp; 		/* pointer to an array of runtime */
					/* allocated common symbols. */
};

struct	link_dynamic {
	int	ld_version;		/* version # of this structure */
	struct 	ld_debug *ldd;
	union {
		struct link_dynamic_1 *ld_1;
		struct link_dynamic_2 *ld_2;
	} ld_un;
};

#define v2 ld_un.ld_2
#define v1 ld_un.ld_1

/*
 * get size of relocations
 */
#define GETGOTSZ(x) (x->ld_version < 2 ? ((struct old_link_dynamic *) x)->v1.ld_plt - ((struct old_link_dynamic *) x)->v1.ld_got : (x)->v2->ld_plt - (x)->v2->ld_got)

#define GETPLTSZ(x) (x->ld_version < 2 ? ((struct old_link_dynamic *) x)->v1.ld_rel - ((struct old_link_dynamic *) x)->v1.ld_plt : (x)->v2->ld_rel - (x)->v2->ld_plt)

#define GETRELSZ(x) (x->ld_version < 2 ? ((struct old_link_dynamic *) x)->v1.ld_hash - ((struct old_link_dynamic *) x)->v1.ld_rel : (x)->v2->ld_hash - (x)->v2->ld_rel)

#define GETHASHSZ(x) (x->ld_version < 2 ? ((struct old_link_dynamic *) x)->v1.ld_stab - ((struct old_link_dynamic *) x)->v1.ld_hash : (x)->v2->ld_stab - (x)->v2->ld_hash)

#define GETSTABSZ(x) (x->ld_version < 2 ? ((struct old_link_dynamic *) x)->v1.ld_symbols - ((struct old_link_dynamic *) x)->v1.ld_stab : (x)->v2->ld_symbols - (x)->v2->ld_stab)

#undef v2
#undef v1

#endif /*!_link_h*/
