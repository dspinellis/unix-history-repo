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

/* zcontrol.c */
/* Control operators for Ghostscript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "estack.h"
#include "iutil.h"
#include "store.h"

/* Export the index of the 'for' operator */
/* for the transfer function mapper in zcolor.c. */
int i_zfor;

/* Check for updating the currentfile cache. */
#define esfile_check(ep)\
  if ( r_has_type_attrs(ep, t_file, a_executable) ) esfile = 0

/* Forward references */
private es_ptr find_stopped(P0());

/* exec */
int
zexec(register os_ptr op)
{	check_op(1);
	check_estack(1);
	++esp;
	ref_assign(esp, op);
	esfile_check(esp);
	pop(1);
	return o_push_estack;
}

/* if */
int
zif(register os_ptr op)
{	check_type(op[-1], t_boolean);
	if ( op[-1].value.index )		/* true */
	   {	check_estack(1);
		++esp;
		ref_assign(esp, op);
		esfile_check(esp);
	   }
	pop(2);
	return o_push_estack;
}

/* ifelse */
int
zifelse(register os_ptr op)
{	check_type(op[-2], t_boolean);
	check_estack(1);
	++esp;
	if ( op[-2].value.index )
	   {	ref_assign(esp, op - 1);
	   }
	else
	   {	ref_assign(esp, op);
	   }
	esfile_check(esp);
	pop(3);
	return o_push_estack;
}

/* for */
private int
  for_pos_int_continue(P1(os_ptr)),
  i_for_pos_int_continue,
  for_neg_int_continue(P1(os_ptr)),
  i_for_neg_int_continue,
  for_real_continue(P1(os_ptr)),
  i_for_real_continue;
int
zfor(register os_ptr op)
{	int code;
	float params[3];
	register es_ptr ep;
	check_proc(*op);
	if ( r_has_type(op - 1, t_integer) &&
	     r_has_type(op - 2, t_integer) &&
	     r_has_type(op - 3, t_integer)
	   )
		code = 7;
	else if ( (code = num_params(op - 1, 3, params)) < 0 )
		return code;
	check_estack(7);
	/* Push a mark, the control variable, the initial value, */
	/* the increment, the limit, and the procedure, */
	/* and invoke the continuation operator. */
	mark_estack(es_for);
	ep = esp += 5;
	if ( (code & 3) == 3 )		/* initial & increment are ints */
	   {	ep[-4] = op[-3];
		ep[-3] = op[-2];
		if ( code == 7 )
			ep[-2] = op[-1];
		else
			make_int(ep - 2, (long)params[2]);
		if ( ep[-3].value.intval >= 0 )
			make_op_estack(ep, for_pos_int_continue, i_for_pos_int_continue);
		else
			make_op_estack(ep, for_neg_int_continue, i_for_neg_int_continue);
	   }
	else
	   {	make_real(ep - 4, params[0]);
		make_real(ep - 3, params[1]);
		make_real(ep - 2, params[2]);
		make_op_estack(ep, for_real_continue, i_for_real_continue);
	   }
	ep[-1] = *op;
	pop(4);
	return o_push_estack;
}
/* Continuation operators for for, separate for positive integer, */
/* negative integer, and real. */
/* Execution stack contains mark, control variable, increment, */
/* limit, and procedure (procedure is topmost.) */
/* The continuation operator is just above the top of the e-stack. */
/* Continuation operator for positive integers. */
private int
for_pos_int_continue(register os_ptr op)
{	register es_ptr ep = esp;
	long var = ep[-3].value.intval;
	if ( var > ep[-1].value.intval )
	   {	esp -= 5;	/* pop everything */
		return o_pop_estack;
	   }
	push(1);
	make_int(op, var);
	ep[-3].value.intval = var + ep[-2].value.intval;
	ref_assign(ep + 2, ep);		/* saved proc */
	esp = ep + 2;
	return o_push_estack;
}
/* Continuation operator for negative integers. */
private int
for_neg_int_continue(register os_ptr op)
{	register es_ptr ep = esp;
	long var = ep[-3].value.intval;
	if ( var < ep[-1].value.intval )
	   {	esp -= 5;	/* pop everything */
		return o_pop_estack;
	   }
	push(1);
	make_int(op, var);
	ep[-3].value.intval = var + ep[-2].value.intval;
	ref_assign(ep + 2, ep);		/* saved proc */
	esp = ep + 2;
	return o_push_estack;
}
/* Continuation operator for reals. */
private int
for_real_continue(register os_ptr op)
{	es_ptr ep = esp;
	float var = ep[-3].value.realval;
	float incr = ep[-2].value.realval;
	if ( incr >= 0 ? (var > ep[-1].value.realval) :
		(var < ep[-1].value.realval) )
		   {	esp -= 5;	/* pop everything */
			return o_pop_estack;
		   }
	push(1);
	ref_assign(op, ep - 3);
	ep[-3].value.realval = var + incr;
	esp = ep + 2;
	ref_assign(ep + 2, ep);		/* saved proc */
	return o_push_estack;
}

/* repeat */
private int repeat_continue(P1(os_ptr));
private int i_repeat_continue;
int
zrepeat(register os_ptr op)
{	check_type(op[-1], t_integer);
	check_proc(*op);
	if ( op[-1].value.intval < 0 ) return e_rangecheck;
	check_estack(5);
	/* Push a mark, the count, and the procedure, and invoke */
	/* the continuation operator. */
	mark_estack(es_for);
	*++esp = op[-1];
	*++esp = *op;
	pop(2);
	return repeat_continue(op - 2);
}
/* Continuation operator for repeat */
private int
repeat_continue(register os_ptr op)
{	es_ptr ep = esp;		/* saved proc */
	if ( --(ep[-1].value.intval) >= 0 )	/* continue */
	   {	push_op_estack(repeat_continue, i_repeat_continue);	/* push continuation */
		++esp;
		ref_assign(esp, ep);
		return o_push_estack;
	   }
	else				/* done */
	   {	esp -= 3;		/* pop mark, count, proc */
		return o_pop_estack;
	   }
}

/* loop */
private int loop_continue(P1(os_ptr));
private int i_loop_continue;
int
zloop(register os_ptr op)
{	check_proc(*op);
	check_estack(4);
	/* Push a mark and the procedure, and invoke */
	/* the continuation operator. */
	mark_estack(es_for);
	*++esp = *op;
	pop(1);
	return loop_continue(op - 1);
}
/* Continuation operator for loop */
private int
loop_continue(register os_ptr op)
{	register es_ptr ep = esp;		/* saved proc */
	make_op_estack(ep + 1, loop_continue, i_loop_continue);	/* push continuation */
	ref_assign(ep + 2, ep);
	esp = ep + 2;
	return o_push_estack;
}

/* exit */
int
zexit(register os_ptr op)
{	es_ptr ep = esp;
	esfile = 0;		/* be lazy, just clear the cache */
	while ( ep >= esbot )
	   {	if ( r_has_type(ep, t_null) )	/* control mark */
			switch ( (ep--)->value.index )
			   {
			case es_for: esp = ep; return o_pop_estack;
			case es_stopped: return e_invalidexit;	/* not a loop */
			   }
		else
			ep--;
	   }
	/* Return e_invalidexit if there is no mark at all. */
	/* This is different from PostScript, which aborts. */
	/* It shouldn't matter in practice. */
	return e_invalidexit;
}

/* stop */
int
zstop(register os_ptr op)
{	es_ptr ep = find_stopped();
	esfile = 0;		/* be lazy, just clear the cache */
	if ( ep )
	   {	esp = ep - 1;
		push(1);
		make_bool(op, 1);
		return o_pop_estack;
	   }
	/* Return e_invalidexit if there is no mark at all. */
	/* This is different from PostScript, which aborts. */
	/* It shouldn't matter in practice. */
	return e_invalidexit;
}

/* stopped */
int
zstopped(register os_ptr op)
{	check_op(1);
	/* Mark the execution stack, and push a false in case */
	/* control returns normally. */
	check_estack(3);
	mark_estack(es_stopped);
	++esp; make_false(esp);
	*++esp = *op;			/* execute the operand */
	esfile_check(esp);
	pop(1);
	return o_push_estack;
}

/* .instopped */
int
zinstopped(register os_ptr op)
{	push(1);
	make_bool(op, find_stopped() != 0);
	return 0;
}

/* countexecstack */
int
zcountexecstack(register os_ptr op)
{	push(1);
	make_int(op, esp - esbot + 1);
	return 0;
}

/* execstack */
private int execstack_continue(P1(os_ptr));
private int i_execstack_continue;
int
zexecstack(register os_ptr op)
{	/* We can't do this directly, because the interpreter */
	/* might have cached some state.  To force the interpreter */
	/* to update the stored state, we push a continuation on */
	/* the exec stack; the continuation is executed immediately, */
	/* and does the actual transfer. */
	int depth = esp - esbot + 1;
	check_write_type(*op, t_array);
	if ( depth > r_size(op) ) return e_rangecheck;
	check_estack(1);
	r_set_size(op, depth);
	push_op_estack(execstack_continue, i_execstack_continue);
	return o_push_estack;
}
/* Continuation operator to do the actual transfer */
private int
execstack_continue(register os_ptr op)
{	int depth = r_size(op);		/* was set above */
	refcpy_to_old(op->value.refs, esbot, depth, "execstack");
	return 0;
}

/* .quit */
int
zquit(register os_ptr op)
{	check_type(*op, t_integer);
	gs_exit((int)op->value.intval);
	/* gs_exit doesn't return, but just in case a miracle happens.... */
	pop(1);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zcontrol_op_defs[] = {
	{"0countexecstack", zcountexecstack},
	{"1exec", zexec},
	{"0execstack", zexecstack},
	{"0exit", zexit},
	{"2if", zif},
	{"3ifelse", zifelse},
	{"0.instopped", zinstopped},
	{"4for", zfor, &i_zfor},
	{"1loop", zloop},
	{"1.quit", zquit},
	{"2repeat", zrepeat},
	{"0stop", zstop},
	{"1stopped", zstopped},
		/* Internal operators */
	{"0%execstack_continue", execstack_continue, &i_execstack_continue},
	{"0%for_pos_int_continue", for_pos_int_continue, &i_for_pos_int_continue},
	{"0%for_neg_int_continue", for_neg_int_continue, &i_for_neg_int_continue},
	{"0%for_real_continue", for_real_continue, &i_for_real_continue},
	{"0%loop_continue", loop_continue, &i_loop_continue},
	{"0%repeat_continue", repeat_continue, &i_repeat_continue},
	op_def_end(0)
};

/* Internal routines */

/* Find a `stopped' mark on the e-stack. */
/* Return the e-stack pointer or 0. */
private es_ptr
find_stopped()
{	register es_ptr ep;
	for ( ep = esp; ep >= esbot; --ep )
	  if ( r_has_type(ep, t_null) && ep->value.index == es_stopped )
	    return ep;
	return 0;
}
