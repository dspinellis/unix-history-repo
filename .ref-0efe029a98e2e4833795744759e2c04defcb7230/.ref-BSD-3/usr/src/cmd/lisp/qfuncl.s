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

	.globl	__qfuncl
__qfuncl:				# quick function call
	cmpl	r6,_nplim		# make sure stack ok
	blss	on1
	calls	$0,_namerr
on1:	movl	(r7),r0			# bring in addr of atom
	pushl	r0			# stack addr of atom of fcn to call
	movl	8(r0),r0		# bring in fcn binding addr
	jleq	nonexf			# jump if fcn non existant
	ashl	$-9,r0,r1		# see if bcd
	cmpb	$5,_typetable+1[r1]	# we are calling
	jeql	gotbcd
hackit:
	calls	$1,_Lfuncal		# call lisp stuff
	movl	r7,r6			# restore np to top
	rsb				# return to callee
gotbcd:
	addl2	$4,r7			# inc lbot by one nament
	calls	$1,*(r0)		# call code
	movab	-4(r7),r6		# restore np to top
	rsb	 			# return

nonexf: # non existant function, call c function to take care of it,
	# we could process it here but wish to minimize assembly language
	# code.
	# we should never return from this call
	# the addr of the atom is already stacked

	calls	$1,_Undeff		# call handler
	clrl	r0			# return nil to compiled code
	rsb				# if ever should return here

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






