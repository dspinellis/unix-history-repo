#
#	This module contains the stack push routine and the fltused
#	symbol definition.  Push() doesn't have to be machine coded
#	but it does get called alot.  The fltused definition will
#	prevent the loader from adding some printf-associated modules.
#
#	If APL is to be loaded from a library, then some undefined
#	symbol must be present in the module which contains the
#	fltused definition.  In that sense, it is necessary to
#	have _push (or something) in machine code.
#
#					Ross Harvey 18-May-78
#
.text
.globl	_push
_push:	.word	0
	movl	_sp, r0
#	movl	4(ap),(r0)
#	acbl	$4,_sp,_staktop,ok
	movl	4(ap),(r0)+
	cmpl	r0, _staktop
	bgtr	toobig
	movl	r0, _sp
	ret
toobig:	movl	r0,_sp
	calls	$0,_newstak
	ret
