#include "../h/config.h"
/*
 * suspend suspends from a built-in procedure (a C function).
 *  The function calling suspend is suspending and the value to
 *  suspend is contained in arg0 of its argument list.  The generator
 *  or expression frame immediately containing the frame of the
 *  suspending procedure is duplicated.
 *
 * suspend returns through the duplicated procedure frame and the
 *  arg0 descriptor of the suspending function is left on the
 *  top of the stack.  When an alternative is needed, efail causes
 *  a return through the original procedure frame which was created
 *  by the original call to the built-in procedure.
 */ 
Global(_boundary)	/* Icon/C boundary address */
Global(_file)		/* Current file name */
Global(_k_level)	/* Value of &level */
Global(_line)		/* Current line number */

Global(_suspend)
#ifdef VAX
_suspend:
/*
 * Construct the generator frame
 */
	Mask	0x0fc0		# Start new generator frame by saving
				#  registers upon entry to suspend.
	movl	_boundary,r6	# Establish new boundary value and
	pushl	r6		#  save it in the new generator frame.
	movl	sp,gfp		# Point gfp at boundary word in new frame
	pushl	_k_level	# Push &level,
	pushl	_line		#  line number,
	pushl	_file		#   and file name to complete the frame.
/*
 * Determine region to be duplicated and copy it.  This is just
 *  like it's done in psusp.
 */
	movl	12(fp),r7	# Low word of region to copy is the
				#  low word of procedure frame of suspending
				#  procedure.
	movl	8(fp),r2	# Get ap of suspending procedure in r2
	movl	-8(r2),r4	# Get gfp from procedure frame of suspending
				#  procedure.
	bneq	f1		# If it is zero,
	movl	-4(r2),r4	#  get saved efp and
	subl2	$8,r4		#  use efp - 8.
	jmp	f2
f1:				# gfp is not zero,
	subl2	$12,r4		#  use gfp - 12.

/*
 * Copy region to be duplicated to top of stack.
 */
				# r7 points at the low word of the region
				#  to be copied.  r4 points at the high end
				#  of the region.  (i.e. r4 is the first
				#  word not_ to copy.)
f2:
	subl2   r7,r4		# r4 = r4 - r7, giving r4 number of bytes
				#  in region.
	subl2   r4,sp		# Move stack pointer down to make space
				#  for region.
	movc3	r4,(r7),(sp)	# Copy the region by moving r4 bytes starting
				#  at r7 to the top of the stack.
/*
 * Return from suspending function; resumption will return from suspend.
 */
	subl3	12(fp),8(fp),r0	# Calculate distance between fp and ap
				#  in suspender's frame, specifically,
				#  r0 = ap - fp
	addl2	sp,r0		# sp points at the first word of the
				#  duplicated procedure frame on the
				#  stack.  By adding it to r0, r0 points
				#  at nwords word in argument list of
				#  duplicated frame.  That is, r0 is
				#  serving as a pseudo ap.
	subl2	$8,r0		# Point r0 at location of saved gfp
				#  in duplicated frame.
	movl	gfp,(r0)	# Replace saved gfp with new gfp value

	movl	sp,fp		# Point fp at duplicated procedure frame
				#  in preparation for return through it.
	clrl	_boundary	# Clear the boundary since control is
				#  going back into Icon code.
	ret			# Return through duplicated frame.  This
				#  looks like a return from the original
				#  call to the built-in function.

#endif VAX

#ifdef PORT
DummyFcn(_suspend)
#endif PORT

#ifdef PDP11
/ suspend - Suspend from a (C) function.
/ Duplicates the most recent generator frame outside the
/ current boundary.  Suspend does not return directly.
/ The caller is reactivated when an alternative is needed;
/ the return actually comes from efail.

/ Register usage:
/   r0:    pointer to top of stack region to be copied,
/	     which is just above the procedure descriptor (arg0) of the
/	     suspending procedure
/   r2:    suspending procedure frame pointer
/   r3:    new generator frame pointer
/   r4:	   old generator frame pointer, indexed down to r0 during copy
/   r5:    current procedure frame pointer

_suspend:
	mov	r5,-(sp)	/ create new procedure frame
	mov	sp,r5
	mov	r4,-(sp)    	/ save registers
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	_boundary,r2	/ r2 <- pointer to suspending procedure frame
	mov	r2,-(sp)	/ save Icon/C boundary address

/ Calculate addresses of new generator frame.

	mov	sp,r3		/ r3 <- pointer to new generator frame
	mov	_k_level,-(sp)	/ save &level
	mov	_line,-(sp)	/ save current line number
	mov	_file,-(sp)	/   and file name
	mov	4(r2),r0	/ r0 <- pointer to top of region to be copied
	asl	r0		/	(= r2 + 10 + 4*nargs)
	asl	r0
	add	r2,r0
	add	$10.,r0
	mov	-4(r2),r4	/ r4 <- generator frame pointer from caller
	bne	1f		/   use saved r3 (gfp) - 6 if non-zero,
	mov	-2(r2),r4	/   else use saved r4 (efp) - 4
	cmp	-(r4),-(r4)
	br	2f
1:
	sub	$6,r4
	br	2f

/ Copy surrounding expression frame.

1:
	mov	-(r4),-(sp)
2:
	cmp	r4,r0		/ stop at end of frame
	bhi	1b

/ Copy return value of suspending function.

	mov	-(r4),-(sp)
	mov	-(r4),-(sp)

/ Return from suspending function; reactivation will return from suspend.

	mov	2(r2),r1	/ r1 <- return pc
	mov	(r2),r5		/ restore old registers
	mov	-(r2),r4
	tst	-(r2)		/   except generator frame pointer
	mov	-(r2),r2
	clr	_boundary	/ returning to Icon code
	jmp	(r1)		/ this really suspends
#endif PDP11
