
	.asciz	"@(#)qfuncl.s	34.1	10/3/80"
#  opus 30 compiler call to ??? interface routines
	.globl	__qf0
__qf0:
	subl3	$4,r6,r7
	jbr 	__qfuncl

	.globl	__qf1
__qf1:
	subl3	$8,r6,r7
	jbr 	__qfuncl

	.globl	__qf2
__qf2:
	subl3	$12,r6,r7
	jbr 	__qfuncl

	.globl	__qf3
__qf3:
	subl3	$16,r6,r7
	jbr 	__qfuncl

	.globl	__qf4
__qf4:
	subl3	$20,r6,r7
	jbr 	__qfuncl

	.data
qfunbuf: .long	0
qlinbuf: .long	0
	.text
	.globl	__qfuncl
__qfuncl:				# quick function call
#	movab	qfunbuf,r0		# profiling
#	jsb	mcount			# profiling
	cmpl	r6,_nplim		# make sure stack ok
	blss	on1
	calls	$0,_namerr
on1:	movl	(r7),r0			# bring in addr of atom
	addl2	$4,r7			# inc lbot by one nament
	pushl	r0			# stack addr of atom of fcn to call
	movl	8(r0),r0		# bring in fcn binding addr
	jleq	nonexf			# jump if fcn non existant
 	tstl	_rsetsw			# see if in *rset mode
 	jeql	norset			# if not, call function
 	tstl	_bcdtrsw		# if (*rset t) & (sstatus bcdtrace t)
 	jneq	hackit			# then have Lfuncal do the work
norset:	
	ashl	$-9,r0,r1		# see if bcd
	cmpb	$5,_typetable+1[r1]	# we are calling
	jeql	gotbcd
hackit:
	calls	$1,_Lfuncal		# call lisp stuff
	movab	-4(r7),r6		# restore np to top
	rsb				# return to callee
gotbcd:
	calls	$1,*(r0)		# call code
	movab	-4(r7),r6		# restore np to top
	rsb	 			# return

nonexf: # non existant function, call c function to take care of it,
	# we could process it here but wish to minimize assembly language
	# code.
	# we should never return from this call
	# the addr of the atom is already stacked

#	addl2	$4,r7			# inc lbot by one nament for evalframe
	calls	$1,_Undeff		# call handler
	clrl	r0			# return nil to compiled code
	rsb				# if ever should return here



# transfer  table linkage routine 
#
	.globl	_qlinker
_qlinker:
	.word 	0xfc0			# save all possible registers 
#	movab	qlinbuf,r0		# profiling
#	jsb	mcount			# profiling
	tstl	_exception	        # any pending exceptions
	jeql	noexc
	tstl	_sigintcnt		# is it because of SIGINT
	jeql	noexc			# if not, just leave
	pushl	$2			# else push SIGINT
	calls	$1,_sigcall
noexc:
	movl	16(fp),r0		# get return pc
	addl2	-4(r0),r0		# get pointer to table
	movl	4(r0),r1		# get atom pointer
retry:					# come here after undef func error
	movl	8(r1),r2		# get function binding
	jleq	nonex			# if none, leave
	tstl	_stattab+2*4		# see if linking possible (Strans)
	jeql	nolink			# no, it isn't
	ashl	$-9,r2,r3		# check type of function
	cmpb	$5,_typetable+1[r3]	
	jeql	linkin			# bcd, link it in!
nolink:
	pushl	r1			# non, bcd, call interpreter
	calls	$1,_Lfuncal
	ret

linkin:	
	ashl	$-9,4(r2),r3		# check type of function discipline
	cmpb	$0,_typetable+1[r3]	# is it string?
	jeql	nolink			# yes, it is a c call, so dont link in
	movl	(r2),r2			# get function addr
	movl	r2,(r0)			# put fcn addr in table
	jmp	2(r2)			# enter fcn after mask

nonex:	pushl	r1			# non existant fcn
	calls	$1,_Undeff		# call processor
	movl	r0,r1			# back in r1
	jbr	retry			# for the retry.


	.globl	__erthrow		# errmessage for uncaught throws
__erthrow: 
	.byte	'U,'n,'c,'a,'u,'g,'h,'t,' ,'t,'h,'r,'o,'w
	.byte	' ,'f,'r,'o,'m,' ,'c,'o,'m,'p,'i,'l,'e,'d
	.byte	' ,'c,'o,'d,'e,0

	.globl _tynames
_tynames:
	.long	0				# nothing here
	.long	_lispsys+20*4	# str_name
	.long	_lispsys+21*4	# atom_name
	.long	_lispsys+19*4	# int_name
	.long	_lispsys+23*4	# dtpr_name
	.long	_lispsys+22*4	# doub_name
	.long	_lispsys+58*4	# funct_name
	.long	_lispsys+83*4	# port_name
	.long	_lispsys+47*4	# array_name
	.long	0				# nothing here
	.long	_lispsys+50*4	# sdot_name
	.long	_lispsys+53*4	# val_nam
#
#	Quickly allocate small fixnums
#
	.globl	_qnewint
_qnewint:
	cmpl	r5,$1024
	jgeq	alloc
	cmpl	r5,$-1024
	jlss	alloc
	moval	Fixzero[r5],r0
	rsb
alloc:
	movl	_int_str,r0	# move next cell addr to r0
	jlss	callnewi	# if no space, allocate
	incl	*_lispsys+24*4	# inc count of ints
	movl	(r0),_int_str	# advance free list
	movl	r5,(r0)		# put baby to bed.
	rsb
callnewi:
	pushl	r5
	calls	$0,_newint
	movl	(sp)+,(r0)
	rsb
	.globl	_qcons

# quick cons call, the car and cdr are stacked on the namestack
# and this function is jsb'ed to.

_qcons:
	movl	_dtpr_str,r0	# move next cell addr to r0
	jlss	getnew		# if ran out of space jump
	incl	*_lispsys+28*4	# inc count of dtprs
	movl	(r0),_dtpr_str	# advance free list
storit:	movl	-(r6),(r0)	# store in cdr
	movl	-(r6),4(r0)	# store in car
	rsb

getnew:	calls	$0,_newdot	# must gc to get one
	jbr	storit		# now initialize it.

#
# Fast equivalent of newdot, entered by jsb
#
	.globl	_qnewdot
_qnewdot:
	movl	_dtpr_str,r0	# mov next cell addr t0 r0
	jlss	mustallo	# if ran out of space
	incl	*_lispsys+28*4	# inc count of dtprs
	movl	(r0),_dtpr_str	# advance free list
	clrq	(r0)
	rsb
mustallo:
	calls	$0,_newdot
	rsb
	.globl	_qpopnames
_qpopnames:			# equivalent of C-code popnames, entered by jsb.
	movl	(sp)+,r0	# return address
	movl	(sp)+,r1	# Lower limit
	movl	_bnp,r2		# pointer to bind stack entry
qploop:
	subl2	$8,r2		# for(; (--r2) > r1;) {
	cmpl	r2,r1		# test for done
	jlss	qpdone		
	movl	(r2),*4(r2)	# r2->atm->a.clb = r2 -> val;
	brb	qploop		# }
qpdone:
	movl	r1,_bnp		# restore bnp
	jmp	(r0)		# return

# _qget : fast get subroutine
#  (get 'atom 'ind)
# called with -8(r6) equal to the atom
#	      -4(r6) equal to the indicator
# no assumption is made about r7
# unfortunately, the atom may not in fact be an atom, it may
# be a list or nil, which are special cases.
# For nil, we grab the nil property list (stored in a special place)
# and for lists we punt and call the C routine since it is  most likely
# and error and we havent put in error checks yet.
#
	.data
qgtbf:	.word	0		# for profiling
	.text

	.globl	_qget
_qget:	
#	movab	qgtbf,r0	# these instructions are for profiling
#	jsb	mcount
	movl	-4(r6),r1	# put indicator in r1
	movl	-8(r6),r0	# and atom into r0
	jeql	nilpli		# jump if atom is nil
	ashl	$-9,r0,r2	# check type
	cmpb	_typetable+1[r2],$1 # is it a symbol??
	jneq	notsymb		# nope
	movl	4(r0),r0	# yes, put prop list in r1 to begin scan
	jeql	fail		# if no prop list, we lose right away
lp:	cmpl	r1,4(r0)	# is car of list eq to indicator?
	jeql	good		# jump if so
	movl	*(r0),r0	# else cddr down list
	jneq	lp		# and jump if more list to go.

fail:	subl2	$8,r6		# unstack args
	rsb			# return with r0 eq to nil

good:	movl	(r0),r0		# return cadr of list
	movl	4(r0),r0
	subl2	$8,r6		#unstack args
	rsb

nilpli:	movl	_lispsys+64*4,r0 # want nil prop list, get it specially
	jneq	lp		# and process if anything there
	subl2	$8,r6		#unstack args
	rsb			# else fail
	
notsymb:
	movab	-8(r6),r7	# must set up r7 before calling
	calls	$0,_Lget	# not a symbol, call C routine to error check
	subl2	$8,r6		#unstack args
	rsb			# and return what it returned.

