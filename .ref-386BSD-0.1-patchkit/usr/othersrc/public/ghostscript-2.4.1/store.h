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

/* store.h */
/* Assignment macros */

/*
 * Macros for storing a ref.  We use macros for storing into objects,
 * since the storage manager needs to be able to track stores for
 * save/restore and also for global/local checking.
 * We also use macros for other ref assignments, because (as it happens)
 * Turbo C generates pretty awful code for doing this.
 *
 * There are three cases that we need to distinguish:
 *	- Storing to a stack (no special action);
 *	- Storing into a newly created object (set l_new);
 *	- Storing into a slot of an existing object (check l_new in
 *	    old value, set in new value).
 * The macros are called
 *	<make/store><new_type><case>(place_to_store, new_value)
 * where <case> is nothing for storing to the stack, _new for storing into
 * a new object, and _old for storing into an existing object.
 * (The _old macros also take a client name for tracing and debugging.)
 * <new_type> and new_value are chosen from the following alternatives:
 *	ref_assign		POINTER TO arbitrary ref
 *	make_t		type (only for null and mark)
 *	make_tv		type, value field name, value
 *			  (only for scalars, which don't have attributes)
 *	make_tav	type, attributes, value field name, value
 *	make_tasv	type, attributes, size, value field name, value
 * There are also specialized make_ macros for specific types:
 *	make_int, make_real, make_bool, make_false, make_true,
 *	make_mark, make_null, make_oper.
 * Not all of the specialized make_ macros have _new and _old variants.
 *
 * For _tav and _tasv, we must store the value first, because sometimes
 * it depends on the contents of the place being stored into.
 */

/*
 * Define the most efficient ref assignment macro for the platform.
 */
#ifdef __TURBOC__
	/* Move the data in two 32-bit chunks, because */
	/* otherwise the compiler calls SCOPY@. */
	/* The cast to void is to discourage the compiler from */
	/* wanting to deliver the value of the expression. */
#  define ref_assign(pto,pfrom)\
	(void)((pto)->value = (pfrom)->value,\
	       (pto)->tas = (pfrom)->tas)
#else
	/* Trust the compiler and hope for the best. */
	/* The MIPS compiler doesn't like the cast to void. */
#  define ref_assign(pto,pfrom)\
	(*(pto) = *(pfrom))
#endif

/******
 ****** NOTE: the declarations of alloc_save_*_mask, alloc_save_change,
 ****** and alloc_refs are duplicated from save.h.
 ******/
extern int alloc_save_new_mask;		/* l_new if in save, 0 if not */
extern int alloc_save_test_mask;	/* 0 if in save, -1 if not */
extern int alloc_save_change(P2(ref *ptr, const char *client_name));
#define ref_save(pto,cname)\
  (void)((r_type_attrs(pto) & l_new) == alloc_save_test_mask ?\
	 alloc_save_change(pto, cname) : 0)
#define ref_mark_new(pto) ((pto)->tas.type_attrs |= alloc_save_new_mask)
#define ref_assign_new(pto,pfrom)\
  (void)(ref_assign(pto,pfrom), ref_mark_new(pto))
#define ref_assign_old(pto,pfrom,cname)\
  (ref_save(pto,cname), ref_assign_new(pto,pfrom))
extern ref *alloc_refs(P2(uint num_refs, const char *client_name));

#define make_t(pref,newtype) r_set_type(pref, newtype)
#define make_t_new(pref,newtype)\
  r_set_type_attrs(pref, newtype, alloc_save_new_mask)
#define make_t_old(pref,newtype,cname)\
  (ref_save(pref,cname), make_t_new(pref,newtype))

#define make_tav(pref,newtype,newattrs,valfield,newvalue)\
  ((pref)->value.valfield = (newvalue),\
   r_set_type_attrs(pref, newtype, newattrs))
#define make_tav_new(pref,t,a,vf,v)\
  make_tav(pref,t,(a)|alloc_save_new_mask,vf,v)
#define make_tav_old(pref,t,a,vf,v,cname)\
  (ref_save(pref,cname), make_tav_new(pref,t,a,vf,v))

#define make_tv(pref,newtype,valfield,newvalue)\
  make_tav(pref,newtype,0,valfield,newvalue)
#define make_tv_new(pref,t,vf,v) make_tav_new(pref,t,0,vf,v)
#define make_tv_old(pref,t,vf,v,cname) make_tav_old(pref,t,0,vf,v,cname)

#define make_tasv(pref,newtype,newattrs,newsize,valfield,newvalue)\
  (make_tav(pref,newtype,newattrs,valfield,newvalue),\
   r_set_size(pref, newsize))
#define make_tasv_new(pref,t,a,s,vf,v)\
  (make_tav_new(pref,t,a,vf,v), r_set_size(pref,s))
#define make_tasv_old(pref,t,a,s,vf,v,cname)\
  (make_tav_old(pref,t,a,vf,v,cname), r_set_size(pref,s))

/* Type-specific constructor macros */

#define make_bool(pref,bval) make_tv(pref, t_boolean, index, bval)
#define make_false(pref) make_bool(pref, 0)
#define make_true(pref) make_bool(pref, 1)

#define make_int(pref,ival) make_tv(pref, t_integer, intval, ival)
#define make_int_new(pref,ival) make_tv_new(pref, t_integer, intval, ival)

#define make_mark(pref) make_t(pref, t_mark)

#define make_null(pref) make_t(pref, t_null)
#define make_null_new(pref) make_t_new(pref, t_null)
#define make_null_old(pref,cname) make_t_old(pref, t_null, cname)

#define make_oper(pref,opidx,proc)\
  make_tasv(pref, t_operator, a_executable, opidx, opproc, proc)

#define make_real(pref,rval) make_tv(pref, t_real, realval, rval)
#define make_real_new(pref,rval) make_tv_new(pref, t_real, realval, rval)
