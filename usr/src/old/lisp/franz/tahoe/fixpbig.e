/callf	$[0-9]*,_stack/d
/callf	$4,_unstack/s//movl	(sp)+,r0/
/callf	$4,_sp/s//moval	(sp),r0/
/callf	$4,_nargs/s//movl	-2(fp),r0\
	cvtwl	r0,r0\
	subl2	$4,r0\
	shrl	$2,r0,r0/
/\*_np\([	, ]\)/s//(r6)\1/g
/\*_lbot\([	, ]\)/s//(r7)\1/g
/_np\([	, ]\)/s//r6\1/g
/_lbot\([	, ]\)/s//r7\1/g
/\*_np$/s//(r6)/g
/\*_lbot$/s//(r7)/g
/_np$/s//r6/g
/_lbot$/s//r7/g
