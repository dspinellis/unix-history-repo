#include "../h/config.h"
#ifdef VAX
/*
 * Icon interpreter
 */

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

.data
jumptab:
.long   quit,       op_asgn,    op_bang,    op_cat
.long   op_compl,   op_diff,    op_div,     op_eqv
.long   op_inter,   op_lconcat, op_lexeq,   op_lexge
.long   op_lexgt,   op_lexle,   op_lexlt,   op_lexne
.long   op_minus,   op_mod,     op_mult,    op_neg
.long   op_neqv,    op_nonnull, op_null,    op_number
.long   op_numeq,   op_numge,   op_numgt,   op_numle
.long   op_numlt,   op_numne,   op_plus,    op_power
.long   op_random,  op_rasgn,   op_refresh, op_rswap
.long   op_sect,    op_size,    op_subsc,   op_swap
.long   op_tabmat,  op_toby,    op_unioncs, op_value
.long   op_bscan,   op_ccase,   op_chfail,  op_coact
.long   op_cofail,  op_coret,   op_create,  op_cset
.long   op_dup,     op_efail,   op_eret,    op_escan
.long   op_esusp,   op_field,   op_file,    op_goto
.long   op_incres,  op_init,    op_int,     op_invoke
.long   op_keywd,   err,        op_limit,   op_line
.long   op_llist,   op_lsusp,   op_mark,    op_pfail
.long   op_pnull,   op_pop,     op_pret,    op_psusp
.long   op_push1,   op_pushn1,  op_real,    op_sdup
.long   op_str,     op_unmark,  err,        err
.long   op_local,   err,        err,        err
.long   err,        err,        err,        err
.long   op_global,  op_arg,     op_static,  op_mark0
.long   err,        err,        err,        err
.long   err,        err,        err,        err
.long   err,        err,        err,        err
.long   err,        err,        err,        err
.long   op_globx,   op_globx,   op_globx,   op_globx
.long   op_globx,   op_globx,   op_globx,   op_globx
.long   op_globx,   op_globx,   op_globx,   op_globx
.long   op_globx,   op_globx,   op_globx,   op_globx
.long   op_locx,    op_locx,    op_locx,    op_locx
.long   op_locx,    op_locx,    op_locx,    op_locx
.long   op_locx,    op_locx,    op_locx,    op_locx
.long   op_locx,    op_locx,    op_locx,    op_locx
.long   op_intx,    op_intx,    op_intx,    op_intx
.long   op_intx,    op_intx,    op_intx,    op_intx
.long   op_intx,    op_intx,    op_intx,    op_intx
.long   op_intx,    op_intx,    op_intx,    op_intx
.long   op_statx,   op_statx,   op_statx,   op_statx
.long   op_statx,   op_statx,   op_statx,   op_statx
.long   op_argx,    op_argx,    op_argx,    op_argx
.long   op_argx,    op_argx,    op_argx,    op_argx
.long   op_unmk0,   op_unmk1,   op_unmk2,   op_unmk3
.long   op_unmk4,   op_unmk5,   op_unmk6,   op_unmk7
.long   op_invkx,   op_invkx,   op_invkx,   op_invkx
.long   op_invkx,   op_invkx,   op_invkx,   op_invkx
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex
.long   op_linex,   op_linex,   op_linex,   op_linex

.globl  _asgn;	.globl  _bang;	  .globl  _cat
.globl  _compl;	  .globl   _diff;	  .globl   _div;	  .globl   _eqv
.globl  _inter;	  .globl   _lconcat;	  .globl  _lexeq;	  .globl   _lexge
.globl  _lexgt;	  .globl   _lexle;	  .globl  _lexlt;	  .globl   _lexne
.globl  _minus;	  .globl   _mod;	  .globl  _mult;	  .globl  _neg
.globl  _neqv;	  .globl  _nonnull;	  .globl  _null;	  .globl  _number
.globl  _numeq;	  .globl   _numge;	  .globl  _numgt;	  .globl   _numle
.globl  _numlt;	  .globl   _numne;	  .globl  _plus;	  .globl  _power
.globl  _random;	  .globl  _rasgn;	  .globl  _refresh;	  .globl _rswap
.globl  _sect;	  .globl  _size;	  .globl   _subsc;	  .globl   _swap
.globl  _tabmat;	  .globl  _toby;	  .globl   _unioncs;	  .globl _value
.globl  _bscan
.globl  _coact;	  .globl   _cofail;	  .globl _coret;
.globl   _create
.globl  _efail_unwind
.globl  _escan;	  .globl   _esusp;	  .globl  _field
.globl  _invoke;	  .globl  _keywd
.globl  _limit;	  .globl   _llist
.globl  _lsusp
.globl  _pfail
.globl  _pret;	  .globl  _psusp

optab:
.long   err,        _asgn,      _bang,      _cat
.long   _compl,     _diff,    	_div,       _eqv
.long   _inter,     _lconcat, 	_lexeq,     _lexge
.long   _lexgt,     _lexle,   	_lexlt,     _lexne
.long   _minus,     _mod,     	_mult,      _neg
.long   _neqv,      _nonnull, 	_null,      _number
.long   _numeq,     _numge,   	_numgt,     _numle
.long   _numlt,     _numne,   	_plus,      _power
.long   _random,    _rasgn,   	_refresh,   _rswap
.long   _sect,      _size,    	_subsc,     _swap
.long   _tabmat,    _toby,    	_unioncs,   _value
.long   _bscan,     err,        err,        _coact
.long   _cofail,    _coret,     _create,    err
.long   err,        _efail_unwind,     err,        _escan
.long   _esusp,     _field,     err,        err
.long   err,        err,        err,        _invoke
.long   _keywd,     err,        _limit,     err
.long   _llist,     _lsusp,     err,        _pfail
.long   err,        err,        _pret,      _psusp

.text
/*
 * interpreter main loop
 */
_interp:
	movzbl	(r9)+,r0
	movl	r0,r1
	ashl	$2,r0,r0
	jmp	*jumptab(r0)

/*
 * Ternary operators
 */

op_toby:
op_escan:
	pushl	$3
	calls	$7,*optab(r0)
	jmp	_interp	
op_sect:
	clrq	-(sp)
	pushl	$4
	calls	$9,*optab(r0)
	jmp	_interp

/*
 * Binary operators
 */
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
	pushl	$2
	calls	$5,*optab(r0)
	jmp	_interp

op_rswap:
op_subsc:
op_swap:
	clrq	-(sp)
	pushl	$3
	calls	$7,*optab(r0)
	jmp	_interp
	
/*
 * Unary operators
 */
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
	pushl	$1
	calls	$3,*optab(r0)
	jmp	_interp
	
op_bang:	
op_random:
op_tabmat:
	clrq	-(sp)
	pushl	$2
	calls	$5,*optab(r0)
	jmp	_interp
/*
 * Instructions
 */
op_bscan:
	movq	_k_subject,-(sp)
	pushl	_k_pos
	pushl	$D_INTEGER
	clrl	-(sp)
	calls	$1,*optab(r0)
	jmp	_interp

op_ccase:
	clrq	-(sp)
	movq	4(r11),-(sp)
	jmp	_interp

op_chfail:
	cvtwl	(r9)+,r1 /* cvt? */
	addl3	r9,r1,-8(r11)
	jmp	_interp
		
op_cofail:
	op_coret:
op_efail:
op_limit:
op_lsusp:
op_pfail:
op_psusp:
	clrl	-(sp)
	calls	$1,*optab(r0)			# arg count? -- whm
	jmp	_interp

op_create:
	cvtwl	(r9)+,r1
	addl3	r9,r1,-(sp)
	pushl	$D_INTEGER
	clrl	-(sp)
	calls	$1,_create
	jmp	_interp

op_cset:
	cvtwl	(r9)+,r1
	addl3	r9,r1,-(sp)
	pushl	$D_CSET
	jmp	_interp
	
op_dup:
	clrq	-(sp)
	movq	8(sp),-(sp)
	jmp	_interp

op_field:
	cvtwl	(r9)+,-(sp)
	pushl	$D_INTEGER
	pushl	$2
	calls	$5,_field
	jmp	_interp
	
op_eret:
	movq	(sp)+,r0
	movl	-4(r11),r10
	movl	r11,sp
	movl	(sp)+,r11
	movq	r0,-(sp)
	jmp	_interp
	
op_file:
	cvtwl	(r9)+,r1
	addl3	r1,_ident,_file
	jmp	_interp
	
op_goto:
	cvtwl	(r9)+,r1
	addl2	r1,r9
	jmp	_interp
	
op_incres:
	movl	_current+4,r0
	incl	28(r0)
	jmp	_interp

op_init:
	movb	$59,-(r9)
	addl2	$3,r9
	jmp	_interp

op_int:
	movl	(r9)+,r1	# Note that ints are 32 bits long
	jbr	1f
op_intx:
	bicl2	$0!0xf,r1
1:	pushl	r1
	pushl	$D_INTEGER
	jmp	_interp

op_invoke:
	cvtwl	(r9)+,r1
	jbr	1f
op_invkx:
	bicl2	$0!0x7,r1
1:	pushl	r1
	ashl	$1,r1,r1
	incl	r1
	calls	r1,_invoke
	jmp	_interp	

op_keywd:
	cvtwl	(r9)+,-(sp)
	pushl	$D_INTEGER
	clrl	-(sp)
	calls	$1,_keywd
	jmp	_interp
	
/*
 * Should line and linex be independent?
 */
op_line:
	cvtwl	(r9)+,r1
	jbr	1f

op_linex:
	bicl2	$0!0x3f,r1
1:	movl	r1,_line
	jmp	_interp

/*
 * Not quite sure about this
 */	
op_llist:
	cvtwl	(r9)+,r1
	pushl	r1
	ashl	$1,r1,r1
	incl	r1
	calls	r1,_llist
	jmp	_interp

op_mark:
	cvtwl	(r9)+,r1
	addl2	r9,r1
	pushl	r11
	movl	sp,r11
	pushl	r10
	clrl	r10
	pushl	r1
	jmp	_interp
	
op_mark0:
	pushl	r11
	movl	sp,r11
	pushl	r10
	clrl	r10
	clrl	-(sp)
	jmp	_interp
	
op_pnull:
	clrq	-(sp)
	jmp	_interp

op_pop:
	tstl	(sp)+
	tstl	(sp)+
	jmp	_interp

op_push1:
	pushl	$1
	pushl	$D_INTEGER
	jmp	_interp

op_pushn1:
	pushl	$-1
	pushl	$D_INTEGER
	jmp	_interp

op_real:
	cvtwl	(r9)+,r1
	addl3	r9,r1,-(sp)
	pushl	$D_REAL
	jmp	_interp

op_sdup:
	movq	(sp),-(sp)
	jmp	_interp

op_str:
	cvtwl	(r9)+,r1
	addl3	_ident,r1,-(sp)
	cvtwl	(r9)+,-(sp)
	jmp	_interp

op_unmark:
	cvtwl	(r9)+,r1
	decl	r1
1:	movl	(r11),r11
	sobgeq	r1,1b
	movl	-4(r11),r10
	movl	r11,sp
	movl	(sp)+,r11
	jmp	_interp
op_unmk7:
	movl	(r11),r11
op_unmk6:
	movl	(r11),r11
op_unmk5:
	movl	(r11),r11
op_unmk4:
	movl	(r11),r11
op_unmk3:
	movl	(r11),r11
op_unmk2:
	movl	(r11),r11
op_unmk1:
	movl	-4(r11),r10
	movl	r11,sp
	movl	(sp)+,r11
op_unmk0:
	jmp	_interp

op_global:
	cvtwl	(r9)+,r1
	jbr	1f
op_globx:
	bicl2	$0!0xf,r1
1:	ashl	$3,r1,r1
	addl3	_globals,r1,-(sp)
	pushl	$D_VAR
	jmp	_interp

op_static:
	cvtwl	(r9)+,r1
	jbr	1f
op_statx:
	bicl2	$0!0x07,r1
1:	ashl	$3,r1,r1
	addl3	_statics,r1,-(sp)
	pushl	$D_VAR
	jmp	_interp
		
op_local:
	cvtwl	(r9)+,r1
	jbr	1f
op_locx:
	bicl2	$0!0xf,r1
1:	mnegl	r1,r1
	movaq	-16(fp)[r1],-(sp)
	pushl	$D_VAR
	jmp	_interp

op_arg:
	cvtwl	(r9)+,r1
	jbr	1f
op_argx:
	bicl2	$0!0x07,r1
1:	pushaq	8(ap)[r1]
	pushl	$D_VAR
	jmp	_interp
quit:
	pushl	$0
	calls	$1,_c_exit
err:
	subl3	_code,r9,-(sp)
	ashl	$-2,r0,-(sp)
	pushl	$unrecog
	pushl	$1f
	calls	$3,_sprintf
	pushl	$1f
	calls	$0,_syserr
.data
1:	.space	30
unrecog:
	.asciz	"Unknown opcode %d, pc = %d\n"
	halt
#endif VAX

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
