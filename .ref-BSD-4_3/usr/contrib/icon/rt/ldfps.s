/*
 *  This file is only of use in the PDP-11 implementation where the
 *   ldfps function is used to control the floating-point processor.
 */
#ifdef VAX
Global(_ldfps)	# prevent null object module
#endif VAX

#ifdef PORT
Global(_ldfps)	# prevent null object module
#endif PORT

#ifdef PDP11
/ Load floating-point processor status register
Global(_ldfps)
_ldfps:
	ldfps	2(sp)
	rts	pc
#endif PDP11
