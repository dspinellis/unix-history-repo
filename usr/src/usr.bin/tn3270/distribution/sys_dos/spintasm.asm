;
; The code in this file complete the spawn_int calls
;

spawn	struc
; union REGS
spawn_ax	dw	1
spawn_bx	dw	1
spawn_cx	dw	1
spawn_dx	dw	1
spawn_si	dw	1
spawn_di	dw	1
spawn_cflag	dw	1
; struct SREGS
spawn_es	dw	1
spawn_cs	dw	1
spawn_ss	dw	1
spawn_ds	dw	1
; int intno
spawn_intno	dw	1
; int done
spawn_done	dw	1
; int rc
spawn_rc	dw	1
;
spawn	ends


ENTER	MACRO
	; Begin enter
	push	bp
	mov	bp,sp

	push	ax
	push	bx
	push	cx
	push	dx
	push	bp
	push	di
	push	si
	push	ds
	push	es
	pushf

	mov	cs:save_sp, sp
	mov	cs:save_ss, ss
	; End enter
	ENDM

LEAVE	MACRO
	; Begin leave
	mov	sp, cs:save_sp
	mov	ss, cs:save_ss

	popf
	pop	es
	pop	ds
	pop	si
	pop	di
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	mov	sp,bp
	pop	bp
	ret
	; End leave
	ENDM

GETREGS	MACRO	wherefrom
	mov	si, wherefrom
	mov	spawn_segment, ds
	mov	spawn_offset, si

	mov	ax, spawn_ax[si]
	mov	bx, spawn_bx[si]
	mov	cx, spawn_cx[si]
	mov	dx, spawn_dx[si]
	; XXX mov	si, spawn_si[si]
	mov	di, spawn_di[si]
	mov	es, spawn_es[si]
	; Now, need to do DS, SI
	push	spawn_ds[si]
	mov	si, spawn_si[si]
	pop	ds
	ENDM


SETREGS	MACRO
	mov	cs:old_si, si
	mov	cs:old_ds, ds

	mov	ds, cs:spawn_segment
	mov	si, cs:spawn_offset

	mov	spawn_ax[si], ax
	mov	spawn_bx[si], bx
	mov	spawn_cx[si], cx
	mov	spawn_dx[si], dx

	mov	spawn_si[si], si
	mov	spawn_di[si], di

	mov	spawn_cs[si], cs
	mov	spawn_ds[si], ds
	mov	spawn_es[si], es
	mov	spawn_ss[si], ss
	; now, need to do SI, DS
	mov	ax, old_si
	mov	spawn_si[si], ax
	mov	ax, old_ds
	mov	spawn_ds[si], ax
	ENDM


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

save_sp		dw	1		; For use in our 'longjmp'
save_ss		dw	1		; For use in our 'longjmp'

spawn_segment	dw	1		; Segment of spawn control block
spawn_offset	dw	1		; Offset of spawn control block

old_si		dw	1		; SI of interrupt issuer (temporary)
old_ds		dw	1		; DS of interrupt issuer (temporary)

issuer_sp	dw	1		; sp of person who called us (permanent)

;
; int_spawn gets control on an interrupt.  It switches the stack
; and does a 'return' from start_spawn.
;
	public	_int_spawn

_int_spawn	proc	near
	mov	cs:issuer_sp, sp

	SETREGS

	LEAVE
_int_spawn	endp

;
; start_spawn issues the dos interrupt after setting up the passed
; registers.  When control returns to it, it sets spawn->done to non-zero.
;
	public	_start_spawn

_start_spawn	proc	near
	ENTER

	GETREGS	4[bp]

	int	21H		; Issue DOS interrupt

	SETREGS

	mov	ds, cs:spawn_segment
	mov	si, cs:spawn_offset
	mov	spawn_done[si], 1	; We are done

	LEAVE
_start_spawn	endp

;
; After int_spawn has faked a return from start_spawn, we come here to
; return to the interrupt issuer.
;
	public	_continue_spawn

_continue_spawn	proc	near
	ENTER

	GETREGS	4[bp]

	mov	sp, cs:issuer_sp		; Restore SP

	iret
_continue_spawn	endp

_TEXT	ends

	end
