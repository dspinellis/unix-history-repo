#include "../h/config.h"
Global(_boundary)	/* Icon/C boundary */
Global(_setbound)	/* Set the Icon/C boundary */
Global(_clrbound)	/* Clear the Icon/C boundary */

#ifdef VAX
/*
 * setbound sets the boundary to the value of the fp of the
 *  calling procedure.
 */
_setbound:
	Mask	0x0		# Don't need to save any registers
	tstl	_boundary	# If the boundary is already set
	jneq	f1		#  just leave it alone
	movl	12(fp),_boundary # Otherwise, set boundary to fp of
				#  calling procedure.  Must use saved
				#  fp because current fp points to
				#  frame made upon entry to setbound.
f1:	ret			# Return
/*
 * clrbound clears the Icon/C boundary if returning to Icon
 *  code from C code.
 */
_clrbound:
	Mask	0x0		# Don't need to save any registers
	cmpl	12(fp),_boundary # If the fp of the caller is equal
				#  to the boundary, the return is
				#  from C to Icon.
	jneq	f11		# If that is the case,
	clrl	_boundary	#  clear the boundary, otherwise,
f11:	ret			#  just return.
#endif VAX

#ifdef PORT
DummyFcn(_setbound)
DummyFcn(_clrbound)
#endif PORT

#ifdef PDP11
DummyFcn(_setbound)
DummyFcn(_clrbound)
#endif PDP11
