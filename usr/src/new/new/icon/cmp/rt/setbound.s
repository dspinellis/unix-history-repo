#include "../h/config.h"
#ifdef VAX
  /* setbound() - a kludge to set the Icon/C boundary to point to
   *  the procedure frame of the C routine called from Icon.
   *  on the 70, this is accomplished in the routine csv().
   *  since the VAX has replaced csv with a single instruction,
   *  setbound() must be the first executable instruction in any
   *  C procedure that could conceivably be called directly from
   *  Icon.
   */

.globl	_boundary		# Icon/C boundary

.globl  _setbound		# set the Icon/C boundary

_setbound:
	.word  0x0	        # uses no registers

	tstl	_boundary
	jneq	1f		# do nothing if boundary already set

	movl	12(fp),_boundary # boundary is fp of calling procedure.
1:
	ret

  /* clrbound() - a kludge to clear the Icon/C boundary if returning
   *  to Icon code from C code.
   *  on the 70, this is accomplished in the routine cret().
   *  since the VAX has replaced cret with a single instruction,
   *  clrbound() must be the last instruction executed in any
   *  C procedure that could conceivably be called directly from
   *  Icon.
   */

.globl  _clrbound		# clear the Icon/C boundary

_clrbound:
	.word  0x0	        # uses no registers

	cmpl    12(fp),_boundary
	jneq	1f		# do nothing if not returning to Icon

	clrl	_boundary       # clear the boundary
1:
	ret
#endif VAX

#ifdef PDP11
#endif PDP11
