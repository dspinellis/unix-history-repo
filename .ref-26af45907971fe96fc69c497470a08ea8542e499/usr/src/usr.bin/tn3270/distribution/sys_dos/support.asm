; Copyright (c) 1988 The Regents of the University of California.
; All rights reserved.
;
; %sccs.include.redist.c%
;
;	@(#)support.asm	4.2 (Berkeley) %G%
;

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
