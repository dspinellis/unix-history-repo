;
; Copyright (c) 1988 Regents of the University of California.
; All rights reserved.
;
; Redistribution and use in source and binary forms are permitted
; provided that the above copyright notice and this paragraph are
; duplicated in all such forms and that any documentation,
; advertising materials, and other materials related to such
; distribution and use acknowledge that the software was developed
; by the University of California, Berkeley.  The name of the
; University may not be used to endorse or promote products derived
; from this software without specific prior written permission.
; THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
; WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
;
;	@(#)support.asm	4.1 (Berkeley) 12/4/88
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
