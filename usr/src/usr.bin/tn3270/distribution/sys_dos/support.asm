;
; Copyright (c) 1988 Regents of the University of California.
; All rights reserved.
;
; Redistribution and use in source and binary forms are permitted
; provided that this notice is preserved and that due credit is given
; to the University of California at Berkeley. The name of the University
; may not be used to endorse or promote products derived from this
; software without specific prior written permission. This software
; is provided ``as is'' without express or implied warranty.
;
;	@(#)support.asm	3.2 (Berkeley) %G%

_TEXT	segment	byte public 'CODE'
_TEXT	ends

_DATA	segment	word public 'DATA'
_DATA	ends

CONST	segment	word public 'CONST'
CONST	ends

_BSS	segment word public 'BSS'
_BSS	ends

DGROUP	group	CONST, _BSS, _DATA

	assume	cs:_TEXT, ds:DGROUP, ss:DGROUP, es:DGROUP

_TEXT	segment
	public	_iret_subr

_iret_subr	proc	far
	iret
_iret_subr	endp

_TEXT	ends

	end
