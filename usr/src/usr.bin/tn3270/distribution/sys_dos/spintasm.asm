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

;
; int_spawn gets control on an interrupt.  It switches the stack
; and does a 'return' from start_spawn.
;
	public	_int_spawn

_int_spawn	proc	near
	push	bp
	mov	bp,sp

	mov	sp,bp
	pop	bp
	ret
_int_spawn	endp

;
; start_spawn issues the dos interrupt after setting up the passed
; registers.  When control returns to it, it sets spawn->done to non-zero.
;
	public	_start_spawn

_start_spawn	proc	near
	push	bp
	mov	bp,sp

	mov	sp,bp
	pop	bp
	ret
_start_spawn	endp

;
; After int_spawn has faked a return from start_spawn, we come here to
; return to the interrupt issuer.
;
	public	_continue_spawn

_continue_spawn	proc	near
	push	bp
	mov	bp,sp

	mov	sp,bp
	pop	bp
	ret
_continue_spawn	endp

_TEXT	ends

	end
