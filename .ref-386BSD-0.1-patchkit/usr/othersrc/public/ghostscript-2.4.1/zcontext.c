/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

/* zcontext.c */
/* Display PostScript context operators */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "dict.h"
#include "estack.h"
#include "state.h"
#include "store.h"

/****** THIS FILE IS NOT IN GOOD ENOUGH SHAPE TO USE YET. ******/

/* Procedure hooks in interp.c */
extern int (*gs_interp_reschedule_proc)(P0());
extern int (*gs_interp_time_slice_proc)(P0());

/* Context structure */
typedef enum {
	cs_invalid,
	cs_active,
	cs_done
} ctx_status;
typedef struct gs_context_s {
	ctx_status status;
	long index;
	int detach;			/* true if a detach has been */
					/* executed for this context */
	struct gs_context_s *next;	/* next context with same status */
					/* (active, waiting on same lock, */
					/* waiting on same condition) */
	struct gs_context_s *joiner;	/* context waiting on a join */
					/* for this one */
	struct gs_context_s *table_next;	/* hash table chain */
		/* Externally visible context state */
	ref *stacks;
#define default_stacksize 50
	uint stacksize;
	uint ossize;
	uint essize;
	uint dssize;
	gs_state *pgs;
	int_gstate istate;
	/****** MORE STUFF HERE ******/
} gs_context;

/* Context list structure */
typedef struct ctx_list_s {
	gs_context *head;
	gs_context *tail;
} ctx_list;

/* Condition structure */
typedef struct gs_condition_s {
	ctx_list waiting;	/* contexts waiting on this condition */
} gs_condition;

/* Lock structure */
typedef struct gs_lock_s {
	ctx_list waiting;	/* contexts waiting for this lock */
	gs_context *holder;		/* context holding the lock, if any */
} gs_lock;

/* Global state */
private gs_context *ctx_current;
private ctx_list active;
#define ctx_table_size 19
private gs_context *ctx_table[ctx_table_size];
private long ctx_next_index;

/* Forward references */
private int context_create(P2(uint, gs_context **));
private int context_param(P2(os_ptr, gs_context **));
#define check_context(op, vpc)\
  if ( (code = context_param(op, &vpc)) < 0 ) return code
private void context_destroy(P1(gs_context *));
private int lock_acquire(P1(os_ptr));
private int lock_release(P1(os_ptr));

/* List manipulation macros */
#define add_last(pl,pc)\
  (((pl)->head == 0 ? ((pl)->head = pc) : ((pl)->tail->next = pc)),\
   (pl)->tail = pc, (pc)->next = 0)
#define add_last_all(pl,pcl)		/* pcl->head != 0 */\
  (((pl)->head == 0 ? ((pl)->head = (pcl)->head) :\
    ((pl)->tail->next = (pcl)->head)),\
   (pl)->tail = (pcl)->tail, (pcl)->head = 0)

/* ------ Initialization ------ */

private int ctx_reschedule(P0());
private int ctx_time_slice(P0());
private void
zcontext_init()
{	ctx_current = 0;
	active.head = 0;
	memset(ctx_table, 0, sizeof(ctx_table));
	ctx_next_index = 1;
	/* Create an initial context. */
	context_create(default_stacksize, &ctx_current);
	/* Hook into the interpreter. */
	gs_interp_reschedule_proc = ctx_reschedule;
	gs_interp_time_slice_proc = ctx_time_slice;
}

/* ------ Interpreter interface to scheduler ------ */

/* When an operator decides it is time to run a new context, */
/* it returns o_reschedule.  The interpreter saves all its state in */
/* memory, calls ctx_reschedule, and then loads the state from memory. */
private int
ctx_reschedule()
{	register gs_context *pctx;
	ref *stkp;
	uint ossize, essize, dssize;
	/* Save the state of the current context in ctx_current, */
	/* if any context is current at all. */
	pctx = ctx_current;
	if ( pctx != 0 )
	   {	uint stackneed;
		ref *newstacks;
		ossize = osp - osbot + 1;
		essize = esp - esbot + 1;
		dssize = dsp - dstack + 1;
		stackneed = ossize + essize + dssize;
		if ( stackneed > pctx->stacksize )
		   {	alloc_free_refs(pctx->stacks, pctx->stacksize,
					"ctx_reschedule");
			newstacks = alloc_refs(stackneed, "ctx_reschedule");
			if ( newstacks == 0 )
			   {	/* Punt. */
				lprintf("Can't allocate stacks!");
				gs_exit(1);
			   }
			pctx->stacksize = stackneed;
			pctx->stacks = newstacks;
		   }
		stkp = pctx->stacks;
#define save_stack(sbot, ssize)\
  memcpy(stkp, sbot, ssize * sizeof(ref));\
  pctx->ssize = ssize;\
  stkp += ssize
		save_stack(osbot, ossize);
		save_stack(esbot, essize);
		save_stack(dstack, dssize);
#undef save_stack
		pctx->pgs = igs;
		pctx->istate = istate;
		/****** MORE TO DO HERE ******/
	   }
	/* Run the first ready context. */
	if ( active.head == 0 )
	   {	lprintf("No context to run!");
		gs_exit(1);
	   }
	ctx_current = active.head;
	active.head = active.head->next;
	/* Load the state of the new current context. */
	pctx = ctx_current;
	stkp = pctx->stacks;
#define reload_stack(sbot, ssize, sp)\
  ssize = pctx->ssize;\
  memcpy(sbot, stkp, ssize * sizeof(ref));\
  sp = sbot + (ssize - 1);\
  stkp += ssize
	reload_stack(osbot, ossize, osp);
	reload_stack(esbot, essize, esp);
	esfile = 0;
	reload_stack(dstack, dssize, dsp);
#undef reload_stack
	igs = pctx->pgs;
	istate = pctx->istate;
	/****** MORE TO DO HERE ******/
	return 0;
}

/* If the interpreter wants to time-slice, it saves its state, */
/* calls ctx_time_slice, and reloads its state. */
private int
ctx_time_slice()
{	if ( active.head == 0 ) return 0;
	add_last(&active, ctx_current);
	return ctx_reschedule();
}

/* ------ Context operators ------ */

private int
  fork_done(P1(os_ptr)),
  i_fork_done;

/* currentcontext */
int
zcurrentcontext(register os_ptr op)
{	push(1);
	make_int(op, ctx_current->index);
	return 0;
}

/* detach */
int
zdetach(register os_ptr op)
{	gs_context *pctx;
	int code;
	check_context(op, pctx);
	if ( pctx->joiner != 0 || pctx->detach )
		return e_invalidcontext;
	pop(1);
	switch ( pctx->status )
	   {
	case cs_active:
		pctx->detach = 1;
		break;
	case cs_done:
		context_destroy(pctx);
		if ( pctx == ctx_current )
		   {	ctx_current = 0;
			return o_reschedule;
		   }
	   }
	return 0;
}

/* fork */
int
zfork(register os_ptr op)
{	os_ptr mp = op - 1;
	gs_context *pctx;
	uint ossize, essize, dssize, stacksize;
	int code;
	ref *stkp;
	check_proc(*op);
	while ( !r_has_type(mp, t_mark) )
	   {	if ( mp <= osbot ) return e_unmatchedmark;
		mp--;
	   }
	ossize = op - mp - 1;
	essize = 2;
	dssize = dsp - dstack + 1;
	stacksize = ossize + essize + dssize + 10;
	code = context_create(stacksize, &pctx);
	if ( code < 0 ) return code;
	stkp = pctx->stacks;
	pctx->ossize = ossize;
	memcpy(stkp, mp + 1, ossize * sizeof(ref));
	stkp += ossize;
	pctx->essize = essize;
	make_oper(stkp, i_fork_done, fork_done);
	stkp++;
	*stkp = *op;
	stkp++;
	pctx->dssize = dssize;
	memcpy(stkp, dstack, dssize * sizeof(ref));
	pctx->pgs = igs;		/* ****** WRONG, MUST COPY ****** */
	pctx->istate = istate;
	/****** MORE INIT HERE? ******/
	add_last(&active, pctx);
	osp = mp;
	make_int(mp, pctx->index);
	return 0;
}
/* This gets executed when a context terminates normally. */
/****** HOW TO GET IT EXECUTED ON ERROR TERMINATION? ******/
private int
fork_done(os_ptr op)
{	if ( ctx_current->detach )
	   {	context_destroy(ctx_current);
		ctx_current = 0;
	   }
	else
	   {	gs_context *pctx = ctx_current->joiner;
		ctx_current->status = cs_done;
		/* Schedule the context waiting to join this one, if any. */
		if ( pctx != 0 ) add_last(&active, pctx);
	   }
	return o_reschedule;
}

/* join */
int
zjoin(register os_ptr op)
{	gs_context *pctx;
	int code;
	check_context(op, pctx);
	if ( pctx->joiner != 0 || pctx == ctx_current || pctx->detach )
		return e_invalidcontext;
	switch ( pctx->status )
	   {
	case cs_active:
		pctx->joiner = ctx_current;
		return o_reschedule;
	case cs_done:
	   {	uint count = pctx->ossize;
		os_ptr mp = op;
		push(count);
		make_mark(mp);
		memcpy(++mp, pctx->stacks, count * sizeof(ref));
		context_destroy(pctx);
	   }
	   }
	return 0;
}

/* yield */
int
zyield(register os_ptr op)
{	if ( active.head == 0 ) return 0;
	add_last(&active, ctx_current);
	return o_reschedule;
}

/* ------ Condition and lock operators ------ */

private int
  i_monitor,
  monitor_release(P1(os_ptr)),
  i_monitor_release,
  await_lock(P1(os_ptr)),
  i_await_lock;

/* condition */
int
zcondition(register os_ptr op)
{	gs_condition *pcond =
		(gs_condition *)alloc(1, sizeof(gs_condition), "zcondition");
	if ( pcond == 0 ) return e_VMerror;
	pcond->waiting.head = 0;
	push(1);
	make_tv(op, t_condition, pcond, pcond);
	return 0;
}

/* lock */
int
zlock(register os_ptr op)
{	gs_lock *plock = (gs_lock *)alloc(1, sizeof(gs_lock), "zlock");
	if ( plock == 0 ) return e_VMerror;
	plock->holder = 0;
	plock->waiting.head = 0;
	push(1);
	make_tv(op, t_lock, plock, plock);
	return 0;
}

/* monitor */
int
zmonitor(register os_ptr op)
{	gs_lock *plock;
	int code;
	check_type(op[-1], t_lock);
	check_proc(*op);
	plock = op[-1].value.plock;
	check_estack(2);
	if ( plock->holder == ctx_current ) return e_invalidcontext;
	code = lock_acquire(op - 1);
	/****** HOW TO GUARANTEE RELEASE IF CONTEXT DIES? ******/
	push_op_estack(monitor_release, i_monitor_release);
	*++esp = op[-1];
	pop(2);
	return code;
}
/* Release the monitor lock when the procedure completes. */
private int
monitor_release(os_ptr op)
{	es_ptr ep = esp--;
	return lock_release(ep);
}

/* notify */
int
znotify(register os_ptr op)
{	gs_condition *pcond;
	check_type(*op, t_condition);
	pcond = op->value.pcond;
	pop(1); op--;
	if ( pcond->waiting.head == 0 ) return 0;	/* nothing to do */
	add_last_all(&active, &pcond->waiting);
	return zyield(op);
}

/* wait */
int
zwait(register os_ptr op)
{	gs_condition *pcond;
	check_type(op[-1], t_lock);
	check_type(*op, t_condition);
	pcond = op->value.pcond;
	check_estack(1);
	lock_release(op - 1);
	add_last(&pcond->waiting, ctx_current);
	push_op_estack(await_lock, i_await_lock);
	return o_reschedule;
}
/* When the condition is signaled, wait for acquiring the lock. */
private int
await_lock(os_ptr op)
{	int code = lock_acquire(op - 1);
	pop(2);
	return code;
}

/* ------ Internal routines ------ */

/* Create a context. */
private int
context_create(uint stacksize, gs_context **ppctx)
{	gs_context *pctx;
	ref *stkp;
	long ctx_index;
	gs_context **pte;
	pctx = (gs_context *)alloc(1, sizeof(gs_context), "context");
	if ( stacksize < default_stacksize ) stacksize = default_stacksize;
	stkp = alloc_refs(stacksize, "context(stacks)");
	if ( pctx == 0 || stkp == 0 ) return e_VMerror;
	ctx_index = ctx_next_index++;
	pctx->stacks = stkp;
	pctx->stacksize = stacksize;
	pctx->status = cs_active;
	pctx->index = ctx_index;
	pctx->detach = 0;
	pctx->next = 0;
	pctx->joiner = 0;
	pte = &ctx_table[ctx_index % ctx_table_size];
	pctx->table_next = *pte;
	*pte = pctx;
	*ppctx = pctx;
	return 0;
}

/* Check a context ID.  Note that we do not check for context validity. */
private int
context_param(os_ptr op, gs_context **ppctx)
{	gs_context *pctx;
	long index;
	check_type(*op, t_integer);
	index = op->value.intval;
	if ( index < 0 ) return e_invalidcontext;
	pctx = ctx_table[index % ctx_table_size];
	for ( ; ; pctx = pctx->table_next )
	   {	if ( pctx == 0 ) return e_invalidcontext;
		if ( pctx->index == index ) break;
	   }
	*ppctx = pctx;
	return 0;
}

/* Destroy a context. */
private void
context_destroy(gs_context *pctx)
{	gs_context **ppctx = &ctx_table[pctx->index % ctx_table_size];
	while ( *ppctx != pctx )
		ppctx = &(*ppctx)->table_next;
	*ppctx = (*ppctx)->table_next;
	alloc_free_refs(pctx->stacks, pctx->stacksize, "context_destroy");
	alloc_free((char *)pctx, 1, sizeof(gs_context), "context_destroy");
}

/* Acquire a lock.  Return 0 if acquired, o_reschedule if not. */
private int
lock_acquire(os_ptr op)
{	gs_lock *plock = op->value.plock;
	if ( plock->holder == 0 )
	   {	plock->holder = ctx_current;
		return 0;
	   }
	add_last(&plock->waiting, ctx_current);
	return o_reschedule;
}

/* Release a lock.  Return 0 if OK, e_invalidcontext if not. */
private int
lock_release(os_ptr op)
{	gs_lock *plock = op->value.plock;
	if ( plock->holder == ctx_current )
	   {	plock->holder = 0;
		add_last_all(&active, &plock->waiting);
		return 0;
	   }
	return e_invalidcontext;
}

/* ------ Initialization procedure ------ */

op_def zcontext_op_defs[] = {
	{"0condition", zcondition},
	{"0currentcontext", zcurrentcontext},
	{"1detach", zdetach},
	{"2fork", zfork},
	{"1join", zjoin},
	{"0lock", zlock},
	{"2monitor", zmonitor, &i_monitor},
	{"1notify", znotify},
	{"2wait", zwait},
	{"0yield", zyield},
		/* Internal operators */
	{"0%fork_done", fork_done, &i_fork_done},
	{"2%monitor_release", monitor_release, &i_monitor_release},
	{"2%await_lock", await_lock, &i_await_lock},
	op_def_end(zcontext_init)
};
