/calls	$[0-9]*,_stack/d
/calls	$0,_unstack/s//movl	(sp)+,r0/
/calls	$0,_sp/s//movl	sp,r0/
/calls	$1,_inewint/s//movl (sp)+,r5\
	jsb	_qnewint/
/calls	$0,_newdot/s//jsb	_qnewdot/
/calls	$1,_popname/s//jsb	_qpopname/
/\*_np\([	, ]\)/s//(r6)\1/g
/\*_lbot\([	, ]\)/s//(r7)\1/g
/_np\([	, ]\)/s//r6\1/g
/_lbot\([	, ]\)/s//r7\1/g
/\*_np$/s//(r6)/g
/\*_lbot$/s//(r7)/g
/_np$/s//r6/g
/_lbot$/s//r7/g
