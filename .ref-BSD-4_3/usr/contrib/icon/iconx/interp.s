#include "../h/config.h"
/*
 * Icon interpreter.
 */

Global(_c_exit)		/* Exit */
Global(_current)	/* Descriptor for current coexpression block */
Global(_file)		/* Source file name */
Global(_globals)	/* Pointer to first global variable */
Global(_ident)
Global(_line)		/* Source line number */
Global(_k_pos)		/* &pos */
Global(_k_subject)	/* &subject */
Global(_statics)	/* Pointer to first static variable */
Global(_syserr)		/* Internal system error */
Global(_interp)		/* Main interpreter loop */
#ifdef VAX
/*
 * Some defines for the interpreter.
 */
#define Op		r1
#define GetOp		movl	(ipc)+,Op
#define PushOp		pushl	Op
#define PushNull	clrq	-(sp)
#define Push_R(a)	pushl	a
#define Push_S(a)	pushl	a
#define Push_K(a)	pushl	$a
#define PushOpSum_R(a)	addl3	Op,a,-(sp)
#define PushOpSum_S(a)	addl3	Op,a,-(sp)
#define NextInst	jmp	_interp
#define CallN(n)	pushl	$n;\
			calls	$((n*2)+1),*optab(r0)
#define CallNameN(n,f)	pushl	$n;\
			calls	$((n*2)+1),f
#define BitClear(m)	bicl2	$~m,Op
#define Jump(lab)	jbr	lab
#define LongJump(lab)	jmp	lab
#define Label(lab)	lab:
/*
 * Globals for various routines.
 */
#define Glob4(a,b,c,d) Global(a); Global(b); Global(c); Global(d)
Glob4(_asgn, _bang, _bscan, _cat)
Glob4(_coact, _cofail, _compl, _coret)
Glob4(_create, _diff, _div, _eqv)
Glob4(_escan, _esusp, _field, _inter)
Glob4(_invoke, _keywd, _lconcat, _lexeq)
Glob4(_lexge, _lexgt, _lexle, _lexlt)
Glob4(_lexne, _limit, _llist, _lsusp)
Glob4(_minus, _mod, _mult, _neg)
Glob4(_neqv, _nonnull, _null, _number)
Glob4(_numeq, _numge, _numgt, _numle)
Glob4(_numlt, _numne, _pfail, _plus)
Glob4(_power, _pret, _psusp, _random)
Glob4(_rasgn, _refresh, _rswap, _sect)
Glob4(_size, _subsc, _swap, _tabmat)
Global(_toby); Global(_unioncs); Global(_value)

Label(jumptab)
/*
 * The jump table, the interpreter branches to the nth label
 *  to execute opcode n.
 */
/*   0 */ .long quit,		op_asgn,	op_bang,	op_cat
/*   4 */ .long op_compl,	op_diff,	op_div,		op_eqv
/*   8 */ .long op_inter,	op_lconcat,	op_lexeq,	op_lexge
/*  12 */ .long op_lexgt,	op_lexle,	op_lexlt,	op_lexne
/*  16 */ .long op_minus,	op_mod,		op_mult,	op_neg
/*  20 */ .long op_neqv,	op_nonnull,	op_null,	op_number
/*  24 */ .long op_numeq,	op_numge,	op_numgt,	op_numle
/*  28 */ .long op_numlt,	op_numne,	op_plus,	op_power
/*  32 */ .long op_random,	op_rasgn,	op_refresh,	op_rswap
/*  36 */ .long op_sect,	op_size,	op_subsc,	op_swap
/*  40 */ .long op_tabmat,	op_toby,	op_unioncs,	op_value
/*  44 */ .long op_bscan,	op_ccase,	op_chfail,	op_coact
/*  48 */ .long op_cofail,	op_coret,	op_create,	op_cset
/*  52 */ .long op_dup,		op_efail,	op_eret,	op_escan
/*  56 */ .long op_esusp,	op_field,	op_file,	op_goto
/*  60 */ .long op_incres,	op_init,	op_int,		op_invoke
/*  64 */ .long op_keywd,	err,		op_limit,	op_line
/*  68 */ .long op_llist,	op_lsusp,	op_mark,	op_pfail
/*  72 */ .long op_pnull,	op_pop,		op_pret,	op_psusp
/*  76 */ .long op_push1,	op_pushn1,	op_real,	op_sdup
/*  80 */ .long op_str,		op_unmark,	err,		err
/*  84 */ .long op_local,	err,		err,		err
/*  88 */ .long err,		err,		err,		err
/*  92 */ .long op_global,	op_arg,		op_static,	op_mark0
/*  96 */ .long err,		err,		err,		err
/* 100 */ .long err,		err,		err,		err
/* 104 */ .long err,		err,		err,		err
/* 108 */ .long err,		err,		err,		err
/* 112 */ .long op_globx,	op_globx,	op_globx,	op_globx
/* 116 */ .long op_globx,	op_globx,	op_globx,	op_globx
/* 120 */ .long op_globx,	op_globx,	op_globx,	op_globx
/* 124 */ .long op_globx,	op_globx,	op_globx,	op_globx
/* 128 */ .long op_locx,	op_locx,	op_locx,	op_locx
/* 132 */ .long op_locx,	op_locx,	op_locx,	op_locx
/* 136 */ .long op_locx,	op_locx,	op_locx,	op_locx
/* 140 */ .long op_locx,	op_locx,	op_locx,	op_locx
/* 144 */ .long op_intx,	op_intx,	op_intx,	op_intx
/* 148 */ .long op_intx,	op_intx,	op_intx,	op_intx
/* 152 */ .long op_intx,	op_intx,	op_intx,	op_intx
/* 156 */ .long op_intx,	op_intx,	op_intx,	op_intx
/* 160 */ .long op_statx,	op_statx,	op_statx,	op_statx
/* 164 */ .long op_statx,	op_statx,	op_statx,	op_statx
/* 168 */ .long op_argx,	op_argx,	op_argx,	op_argx
/* 172 */ .long op_argx,	op_argx,	op_argx,	op_argx
/* 176 */ .long op_unmk0,	op_unmk1,	op_unmk2,	op_unmk3
/* 180 */ .long op_unmk4,	op_unmk5,	op_unmk6,	op_unmk7
/* 184 */ .long op_invkx,	op_invkx,	op_invkx,	op_invkx
/* 188 */ .long op_invkx,	op_invkx,	op_invkx,	op_invkx
/* 192 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 196 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 200 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 204 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 208 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 212 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 216 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 220 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 224 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 228 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 232 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 236 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 240 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 244 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 248 */ .long op_linex,	op_linex,	op_linex,	op_linex
/* 252 */ .long op_linex,	op_linex,	op_linex,	op_linex
Label(optab)
/*
 * When an opcode n has a subroutine call associated with it, the
 *  nth word here is the routine to call.
 */
/*   0 */ .long	err,		_asgn,		_bang,		_cat
/*   4 */ .long	_compl,		_diff,		_div,		_eqv
/*   8 */ .long	_inter,		_lconcat,	_lexeq,		_lexge
/*  12 */ .long	_lexgt,		_lexle,		_lexlt,		_lexne
/*  16 */ .long	_minus,		_mod,		_mult,		_neg
/*  20 */ .long	_neqv,		_nonnull,	_null,		_number
/*  24 */ .long	_numeq,		_numge,		_numgt,		_numle
/*  28 */ .long	_numlt,		_numne,		_plus,		_power
/*  32 */ .long	_random,	_rasgn,		_refresh,	_rswap
/*  36 */ .long	_sect,		_size,		_subsc,		_swap
/*  40 */ .long	_tabmat,	_toby,		_unioncs,	_value
/*  44 */ .long	_bscan,		err,		err,		_coact
/*  48 */ .long	_cofail,	_coret,		_create,	err
/*  52 */ .long	err,		err,		err,		_escan
/*  56 */ .long	_esusp,		_field,		err,		err
/*  60 */ .long	err,		err,		err,		_invoke
/*  64 */ .long	_keywd,		err,		_limit,		err
/*  68 */ .long	_llist,		_lsusp,		err,		_pfail
/*  72 */ .long	err,		err,		_pret,		_psusp

 .text

			
/*
 * Interpreter main loop.
 */
Label(_interp)
	movzbl	(ipc)+,r0
	movl	r0,Op
	ashl	$2,r0,r0
	jmp	*jumptab(r0)

/*
 * Ternary operators.
 */

Label(op_toby)				/* e1 to e2 by e3 */
Label(op_escan)				/* escan */
	CallN(3)
	NextInst	
Label(op_sect)				/* e1[e2:e3] */
	PushNull
	CallN(4)
	NextInst

/*
 * Binary operators.
 */
Label(op_asgn)				/* e1 := e2 */
Label(op_cat)				/* e1 || e2 */
Label(op_diff)				/* e1 -- e2 */
Label(op_div)				/* e1 / e2 */
Label(op_eqv)				/* e1 === e2 */
Label(op_inter)				/* e1 ** e2 */
Label(op_lconcat)			/* e1 ||| e2 */
Label(op_lexeq)				/* e1 == e2 */
Label(op_lexge)				/* e1 >>= e2 */
Label(op_lexgt)				/* e1 >> e2 */
Label(op_lexle)				/* e1 <<= e2 */
Label(op_lexlt)				/* e1 << e2 */
Label(op_lexne)				/* e1 ~== e2 */
Label(op_minus)				/* e1 - e2 */
Label(op_mod)				/* e1 % e2 */
Label(op_mult)				/* e1 * e2 */
Label(op_neqv)				/* e1 ~==== e2 */
Label(op_numeq)				/* e1 = e2 */
Label(op_numge)				/* e1 >= e2 */
Label(op_numgt)				/* e1 > e2 */
Label(op_numle)				/* e1 <= e2 */
Label(op_numlt)				/* e1 < e2 */
Label(op_numne)				/* e1 ~= e2 */
Label(op_plus)				/* e1 + e2 */
Label(op_power)				/* e1 ^ e2 */
Label(op_rasgn)				/* e1 <- e2 */
Label(op_unioncs)			/* e1 ++ e2 */
	CallN(2)
	NextInst

Label(op_rswap)				/* e1 <-> e2 */
Label(op_subsc)				/* e1[e2] */
Label(op_swap)				/* e1 :=: e2 */
	PushNull
	CallN(3)
	NextInst
	
/*
 * Unary operators.
 */
Label(op_compl)				/* ~e */
Label(op_neg)				/* -e */
Label(op_nonnull)			/* \e */
Label(op_null)				/* /e */
Label(op_number)			/* +e */
Label(op_refresh)			/* ^e */
Label(op_size)				/* *e */
Label(op_value)				/* .e */
Label(op_coact)				/* @e */
Label(op_esusp)				/* esusp */
Label(op_pret)				/* pret */
	CallN(1)
	NextInst
	
Label(op_bang)				/* !e */
Label(op_random)			/* ?e */
Label(op_tabmat)			/* =e */
	PushNull
	CallN(2)
	NextInst
/*
 * Instructions.
 */
Label(op_bscan)				/* bscan */
	movq	_k_subject,-(sp)
	Push_S(_k_pos)
	Push_K(D_INTEGER)
	CallN(0)
	NextInst

Label(op_ccase)				/* ccase */
	PushNull
	movq	4(efp),-(sp)
	NextInst

Label(op_chfail)			/* chfail */
	GetOp
	addl3	ipc,Op,-8(efp)
	NextInst
		
Label(op_efail)				/* efail */
	LongJump(_efail)
	
Label(op_pfail)				/* pfail */
	LongJump(_pfail)

Label(op_cofail)			/* cofail */
Label(op_coret)				/* coret */
Label(op_limit)				/* limit */
Label(op_lsusp)				/* lsusp */
Label(op_psusp)				/* psusp */
	CallN(0)
	NextInst

Label(op_create)			/* create */
	GetOp
	PushOpSum_R(ipc)
	Push_K(D_INTEGER)
	CallNameN(0,_create)
	NextInst

Label(op_cset)				/* cset */
	GetOp
	PushOpSum_R(ipc)
	Push_K(D_CSET)
	NextInst
	
Label(op_dup)				/* dup */
	PushNull
	movq	8(sp),-(sp)
	NextInst

Label(op_field)				/* field */
	GetOp
	PushOp
	Push_K(D_INTEGER)
	CallNameN(2,_field)
	NextInst
	
Label(op_eret)				/* eret */
	movq	(sp)+,r0
	movl	-4(efp),gfp
	movl	efp,sp
	movl	(sp)+,efp
	movq	r0,-(sp)
	NextInst
	
Label(op_file)				/* file */
	GetOp
	addl3	Op,_ident,_file
	NextInst
	
Label(op_goto)				/* goto */
	GetOp
	addl2	Op,ipc
	NextInst
	
Label(op_incres)			/* incres */
	movl	_current+4,r0
	incl	28(r0)
	NextInst

Label(op_init)				/* init */
	movb	$59,-(ipc)
	addl2	$5,ipc		/* Watch out here, that 5 comes from
				   # bytes in op + operand, not to 
				   mention that the 59 is OP_GOTO */
	NextInst

Label(op_invoke)			/* invoke */
	GetOp
	Jump(invkjmp)
Label(op_invkx)
	BitClear(7)
Label(invkjmp)
	PushOp
	ashl	$1,Op,Op
	incl	Op
	calls	Op,_invoke
	NextInst	

Label(op_int)				/* int */
	movl	(ipc)+,Op	/* Special case here, integers come
				    out as a WORDSIZE value */
	Jump(intjmp)
Label(op_intx)
	BitClear(15)
Label(intjmp)
	PushOp
	Push_K(D_INTEGER)
	NextInst

Label(op_keywd)				/* keywd */
	GetOp
	PushOp
	Push_K(D_INTEGER)
	CallNameN(0,_keywd)
	NextInst
	
Label(op_line)				/* line */
	GetOp
	Jump(linejmp)
Label(op_linex)
	BitClear(63)
Label(linejmp)
	movl	Op,_line
	NextInst

Label(op_llist)				/* llist */
	GetOp
	PushOp
	movl	Op,r8		/* Do a workaround to allow for more */
	calls	$0,_llist	/*  than 256 words of arg list. */
	ashl	$1,r8,r8	/* This assumes that we don't "reenter" */
	incl	r8		/*  this piece of code. */
	ashl	$2,r8,r8
	addl2	r8,sp
	NextInst

Label(op_mark)				/* mark */
	GetOp
	addl2	ipc,Op
	Push_R(efp)
	movl	sp,efp
	Push_R(gfp)
	clrl	gfp
	PushOp
	NextInst
	
Label(op_mark0)				/* mark0 */
	Push_R(efp)
	movl	sp,efp
	Push_R(gfp)
	clrl	gfp
	Push_K(0)
	NextInst
	
Label(op_pnull)				/* pnull */
	PushNull
	NextInst

Label(op_pop)				/* pop */
	tstl	(sp)+
	tstl	(sp)+
	NextInst

Label(op_push1)				/* push1 */
	Push_K(1)
	Push_K(D_INTEGER)
	NextInst

Label(op_pushn1)			/* pushn1 */
	Push_K(-1)
	Push_K(D_INTEGER)
	NextInst

Label(op_real)				/* real */
	GetOp
	PushOpSum_R(ipc)
	Push_K(D_REAL)
	NextInst

Label(op_sdup)				/* sdup */
	movq	(sp),-(sp)
	NextInst

Label(op_str)				/* str */
	GetOp
	PushOpSum_S(_ident)
	GetOp
	PushOp
	NextInst

Label(op_unmark)			/* unmark */
	GetOp
Label(unmkjmp)
	movl	(efp),efp
	sobgtr	Op,unmkjmp
	movl	-4(efp),gfp
	movl	efp,sp
	movl	(sp)+,efp
	NextInst
Label(op_unmk7)
	movl	(efp),efp
Label(op_unmk6)
	movl	(efp),efp
Label(op_unmk5)
	movl	(efp),efp
Label(op_unmk4)
	movl	(efp),efp
Label(op_unmk3)
	movl	(efp),efp
Label(op_unmk2)
	movl	(efp),efp
Label(op_unmk1)
	movl	-4(efp),gfp
	movl	efp,sp
	movl	(sp)+,efp
Label(op_unmk0)
	NextInst

Label(op_global)			/* global */
	GetOp
	Jump(globjmp)
Label(op_globx)
	BitClear(15)
Label(globjmp)
	ashl	$3,Op,Op
	PushOpSum_S(_globals)
	Push_K(D_VAR)
	NextInst

Label(op_static)			/* static */
	GetOp
	Jump(statjmp)
Label(op_statx)
	BitClear(7)
Label(statjmp)
	ashl	$3,Op,Op
	PushOpSum_S(_statics)
	Push_K(D_VAR)
	NextInst

Label(op_local)				/* local */
	GetOp
	Jump(locjmp)
Label(op_locx)
	BitClear(15)
Label(locjmp)
	mnegl	Op,Op
	movaq	-16(fp)[Op],-(sp)
	Push_K(D_VAR)
	NextInst

Label(op_arg)				/* arg */
	GetOp
	Jump(argjmp)
Label(op_argx)
	BitClear(7)
Label(argjmp)
	pushaq	8(ap)[Op]
	Push_K(D_VAR)
	NextInst

Label(quit)				/* quit */
	Push_K(0)
	calls	$1,_c_exit

Label(err)				/* err */
	subl3	_code,ipc,-(sp)
	ashl	$-2,r0,-(sp)
	Push_K(unrecog)
	Push_K(message)
	calls	$3,_sprintf
	Push_K(message)
	calls	$0,_syserr
 .data
Label(message)
	.space	30
Label(unrecog)
	.asciz	"Unknown opcode %d, pc = %d\n"
	halt
#endif VAX
#ifdef PORT
/* Copy the code for the VAX in here and work on it */
DummyFcn(_interp)
#endif PORT

#ifdef PDP11
/ Icon interpreter

 .globl	_c_exit
 .globl	_current
 .globl	_file
 .globl	_globals
 .globl	_ident
 .globl	_line
 .globl	_k_pos
 .globl	_k_subject
 .globl	_statics
 .globl	_syserr

 .globl	_interp

_interp:
	movb	(r2)+,r0
	bic	$!377,r0
	mov	r0,r1
	asl	r0
	jmp	*jumptab(r0)

 .data
jumptab:
        quit;       op_asgn;    op_bang;    op_cat
        op_compl;   op_diff;    op_div;     op_eqv
        op_inter;   op_lconcat; op_lexeq;   op_lexge
        op_lexgt;   op_lexle;   op_lexlt;   op_lexne
        op_minus;   op_mod;     op_mult;    op_neg
        op_neqv;    op_nonnull; op_null;    op_number
        op_numeq;   op_numge;   op_numgt;   op_numle
        op_numlt;   op_numne;   op_plus;    op_power
        op_random;  op_rasgn;   op_refresh; op_rswap
        op_sect;    op_size;    op_subsc;   op_swap
        op_tabmat;  op_toby;    op_unioncs; op_value
        op_bscan;   op_ccase;   op_chfail;  op_coact
        op_cofail;  op_coret;   op_create;  op_cset
        op_dup;     op_efail;   op_eret;    op_escan
        op_esusp;   op_field;   op_file;    op_goto
        op_incres;  op_init;    op_int;     op_invoke
        op_keywd;   err;        op_limit;   op_line
        op_llist;   op_lsusp;   op_mark;    op_pfail
        op_pnull;   op_pop;     op_pret;    op_psusp
        op_push1;   op_pushn1;  op_real;    op_sdup
        op_str;     op_unmark;  err;        err
        op_local;   op_long;    err;        err
        err;        err;        err;        err
        op_global;  op_arg;     op_static;  op_mark0
        err;        err;        err;        err
        err;        err;        err;        err
        err;        err;        err;        err
        err;        err;        err;        err
        op_globx;   op_globx;   op_globx;   op_globx
        op_globx;   op_globx;   op_globx;   op_globx
        op_globx;   op_globx;   op_globx;   op_globx
        op_globx;   op_globx;   op_globx;   op_globx
        op_locx;    op_locx;    op_locx;    op_locx
        op_locx;    op_locx;    op_locx;    op_locx
        op_locx;    op_locx;    op_locx;    op_locx
        op_locx;    op_locx;    op_locx;    op_locx
        op_intx;    op_intx;    op_intx;    op_intx
        op_intx;    op_intx;    op_intx;    op_intx
        op_intx;    op_intx;    op_intx;    op_intx
        op_intx;    op_intx;    op_intx;    op_intx
        op_statx;   op_statx;   op_statx;   op_statx
        op_statx;   op_statx;   op_statx;   op_statx
        op_argx;    op_argx;    op_argx;    op_argx
        op_argx;    op_argx;    op_argx;    op_argx
        op_unmk0;   op_unmk1;   op_unmk2;   op_unmk3
        op_unmk4;   op_unmk5;   op_unmk6;   op_unmk7
        op_invkx;   op_invkx;   op_invkx;   op_invkx
        op_invkx;   op_invkx;   op_invkx;   op_invkx
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex
        op_linex;   op_linex;   op_linex;   op_linex

 .globl  _asgn,      _bang,      _cat
 .globl  _compl,     _diff,    	_div,       _eqv
 .globl  _inter,     _lconcat, 	_lexeq,     _lexge
 .globl  _lexgt,     _lexle,   	_lexlt,     _lexne
 .globl  _minus,     _mod,     	_mult,      _neg
 .globl  _neqv,      _nonnull, 	_null,      _number
 .globl  _numeq,     _numge,   	_numgt,     _numle
 .globl  _numlt,     _numne,   	_plus,      _power
 .globl  _random,    _rasgn,   	_refresh,   _rswap
 .globl  _sect,      _size,    	_subsc,     _swap
 .globl  _tabmat,    _toby,    	_unioncs,   _value
 .globl  _bscan
 .globl  _coact,     _cofail,  	_coret,     _create
 .globl  _efail
 .globl  _escan,     _esusp,   	_field
 .globl  _invoke,    _keywd
 .globl  _limit,     _llist,   	_lsusp
 .globl  _pfail
 .globl  _pret,      _psusp

optab:
        err;        _asgn;      _bang;      _cat
        _compl;     _diff;    	_div;       _eqv
        _inter;     _lconcat; 	_lexeq;     _lexge
        _lexgt;     _lexle;   	_lexlt;     _lexne
        _minus;     _mod;     	_mult;      _neg
        _neqv;      _nonnull; 	_null;      _number
        _numeq;     _numge;   	_numgt;     _numle
        _numlt;     _numne;   	_plus;      _power
        _random;    _rasgn;   	_refresh;   _rswap
        _sect;      _size;    	_subsc;     _swap
        _tabmat;    _toby;    	_unioncs;   _value
        _bscan;     err;        err;        _coact
        _cofail;    _coret;     _create;    err
        err;        _efail;     err;        _escan
        _esusp;     _field;     err;        err
        err;        err;        err;        _invoke
        _keywd;     err;        _limit;     err
        _llist;     _lsusp;     err;        _pfail
        err;        err;        _pret;      _psusp
 .text

/ ternary operators

op_toby:
op_escan:
	mov	$3,-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp
op_sect:
	clr	-(sp)
	clr	-(sp)
	mov	$4,-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp

/ binary operators

op_asgn:
op_cat:
op_diff:
op_div:
op_eqv:
op_inter:
op_lconcat:
op_lexeq:
op_lexge:
op_lexgt:
op_lexle:
op_lexlt:
op_lexne:
op_minus:
op_mod:
op_mult:
op_neqv:
op_numeq:
op_numge:
op_numgt:
op_numle:
op_numlt:
op_numne:
op_plus:
op_power:
op_rasgn:
op_unioncs:
	mov	$2,-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp
op_rswap:
op_subsc:
op_swap:
	clr	-(sp)
	clr	-(sp)
	mov	$3,-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp

/ unary operators

op_compl:
op_neg:
op_nonnull:
op_null:
op_number:
op_refresh:
op_size:
op_value:
op_coact:
op_esusp:
op_pret:
	mov	$1,-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp
op_bang:	
op_random:
op_tabmat:
	clr	-(sp)
	clr	-(sp)
	mov	$2,-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp

/ instructions

op_bscan:
	mov	_k_subject+2,-(sp)
	mov	_k_subject,-(sp)
	mov	_k_pos,-(sp)
	mov	$D_INTEGER,-(sp)
	clr	-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp
op_ccase:
	clr	-(sp)
	clr	-(sp)
	mov	4(r4),-(sp)
	mov	2(r4),-(sp)
	jbr	_interp
op_chfail:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r2,r1
	mov	r1,-4(r4)
	jbr	_interp
op_cofail:
op_coret:
op_efail:
op_limit:
op_lsusp:
op_pfail:
op_psusp:
	clr	-(sp)
	jsr	pc,*optab(r0)
	jbr	_interp
op_create:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r2,r1
	mov	r1,-(sp)
	mov	$D_INTEGER,-(sp)
	clr	-(sp)
	jsr	pc,_create
	jbr	_interp
op_cset:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r2,r1
	mov	r1,-(sp)
	mov	$D_CSET,-(sp)
	jbr	_interp
op_dup:
	clr	-(sp)
	clr	-(sp)
	mov	6(sp),-(sp)
	mov	6(sp),-(sp)
        jbr	_interp
op_eret:
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	-2(r4),r3
	mov	r4,sp
	mov	(sp)+,r4
	mov	r1,-(sp)
	mov	r0,-(sp)
	jbr	_interp
op_field:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	mov	r1,-(sp)
	mov	$D_INTEGER,-(sp)
	mov	$2,-(sp)
	jsr	pc,_field
	jbr	_interp
op_file:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	_ident,r1
	mov	r1,_file
	jbr	_interp
op_goto:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r1,r2
	jbr	_interp
op_incres:
	mov	_current+2,r0
	inc	14.(r0)
	jbr	_interp
op_init:
	movb	$59.,-(r2)	/ change the INIT to a GOTO for next time
	add	$3,r2
	jbr	_interp
op_int:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_intx:
	bic	$!017,r1
1:	mov	r1,-(sp)
	mov	$D_INTEGER,-(sp)
	jbr	_interp
op_invoke:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_invkx:
	bic	$!07,r1
1:	mov	r1,-(sp)
	jsr	pc,_invoke
	jbr	_interp
op_keywd:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	mov	r1,-(sp)
	mov	$D_INTEGER,-(sp)
	clr	-(sp)
	jsr	pc,_keywd
	jbr	_interp
op_line:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_linex:
	bic	$!077,r1
1:	mov	r1,_line
	jbr	_interp
op_llist:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	mov	r1,-(sp)
	jsr	pc,_llist
	jbr	_interp
op_long:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r2,r1
	mov	r1,-(sp)
	mov	$D_LONGINT,-(sp)
	jbr	_interp
op_mark:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r2,r1
	mov	r4,-(sp)
	mov	sp,r4
	mov	r3,-(sp)
	clr	r3
	mov	r1,-(sp)
	jbr	_interp
op_mark0:
	mov	r4,-(sp)
	mov	sp,r4
	mov	r3,-(sp)
	clr	r3
	clr	-(sp)
	jbr	_interp
op_pnull:
	clr	-(sp)
	clr	-(sp)
	jbr	_interp
op_pop:
	cmp	(sp)+,(sp)+
	jbr	_interp
op_push1:
	mov	$1,-(sp)
	mov	$D_INTEGER,-(sp)
	jbr	_interp
op_pushn1:
	mov	$-1,-(sp)
	mov	$D_INTEGER,-(sp)
	jbr	_interp
op_real:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	r2,r1
	mov	r1,-(sp)
	mov	$D_REAL,-(sp)
	jbr	_interp
op_sdup:
	mov	2(sp),-(sp)
	mov	2(sp),-(sp)
        jbr	_interp
op_str:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	add	_ident,r1
	mov	r1,-(sp)
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	mov	r1,-(sp)
	jbr	_interp
op_unmark:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	dec	r1
1:	mov	(r4),r4
	sob	r1,1b
	mov	-2(r4),r3
	mov	r4,sp
	mov	(sp)+,r4
	jbr	_interp
op_unmk7:
	mov	(r4),r4
op_unmk6:
	mov	(r4),r4
op_unmk5:
	mov	(r4),r4
op_unmk4:
	mov	(r4),r4
op_unmk3:
	mov	(r4),r4
op_unmk2:
	mov	(r4),r4
op_unmk1:
	mov	-2(r4),r3
	mov	r4,sp
	mov	(sp)+,r4
op_unmk0:
	jbr	_interp
op_global:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_globx:
	bic	$!017,r1
1:	asl	r1
	asl	r1
	add	_globals,r1
	mov	r1,-(sp)
	mov	$D_VAR,-(sp)
	jbr	_interp
op_static:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_statx:
	bic	$!07,r1
1:	asl	r1
	asl	r1
	add	_statics,r1
	mov	r1,-(sp)
	mov	$D_VAR,-(sp)
	jbr	_interp
op_local:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_locx:
	bic	$!017,r1
1:	asl	r1
	asl	r1
	add	$14.,r1
	neg	r1
	add	r5,r1
	mov	r1,-(sp)
	mov	$D_VAR,-(sp)
	jbr	_interp
op_arg:
	movb	(r2)+,r0
	movb	(r2)+,r1
	bic	$!0377,r0
	ash	$8.,r1
	bis	r0,r1
	br	1f
op_argx:
	bic	$!07,r1
1:	asl	r1
	asl	r1
	add	$6,r1
	add	r5,r1
	mov	r1,-(sp)
	mov	$D_VAR,-(sp)
	jbr	_interp
quit:
	clr	-(sp)
	jsr	pc,*$_c_exit
err:
	mov	$9f,-(sp)
	jsr	pc,_syserr
 .data
9:	<unrecognized ucode instruction\0>
 .even
#endif PDP11
