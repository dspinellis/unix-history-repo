/*	uttoggle.s	4.2	83/02/21	*/

/*
 * Prototype toggle in bootstrap code for tm type tapes.
 * If on anything but a 780 with the drive on uba0
 * this will have to be repaired by patching uba and umem.
 */
begin:
	movl	uba,r1
	movl	$0x80200000,0x800(r1)
	clrl	0x804(r1)
	movl	umem,r2
	bisl2	$0172440,r2
	mnegw	$512,06(r2)
	mnegw	$256,02(r2)
	clrw	04(r2)
	movw	$071,(r2)
	halt
	.align	2
uba:	.long	0x20006000
umem:	.long	0x2013e000
