;    Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
;    Distributed by Free Software Foundation, Inc.
;
; This file is part of Ghostscript.
;
; Ghostscript is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
; to anyone for the consequences of using it or for whether it serves any
; particular purpose or works at all, unless he says so in writing.  Refer
; to the Ghostscript General Public License for full details.
;
; Everyone is granted permission to copy, modify and redistribute
; Ghostscript, but only under the conditions described in the Ghostscript
; General Public License.  A copy of this license is supposed to have been
; given to you along with Ghostscript so you can know your rights and
; responsibilities.  It should be in a file named COPYING.  Among other
; things, the copyright notice and this notice must be preserved on all
; copies.

; iutilasm.asm
; Assembly code for Ghostscript interpreter on MS-DOS systems

	ifdef	FOR80386

	.286c

	endif

utilasm_TEXT	SEGMENT	WORD PUBLIC 'CODE'
	ASSUME	CS:utilasm_TEXT


	ifdef	FOR80386

; Macro for 32-bit operand prefix.
OP32	macro
	db	66h
	endm

	endif					; FOR80386


	ifdef	FOR80386

; Replace the multiply and divide routines in the Turbo C library
; if we are running on an 80386.

; Macro to swap the halves of a 32-bit register.
; Unfortunately, masm won't allow a shift instruction with a count of 16,
; so we have to code it in hex.
swap	macro	regno
	  OP32
	db	0c1h,0c0h+regno,16		; rol regno,16
	endm
regax	equ	0
regcx	equ	1
regdx	equ	2
regbx	equ	3

; Multiply (dx,ax) by (cx,bx) to (dx,ax).
	PUBLIC	LXMUL@
	PUBLIC	F_LXMUL@
F_LXMUL@ proc	far
LXMUL@	proc	far
	swap	regdx
	mov	dx,ax
	swap	regcx
	mov	cx,bx
	  OP32
	db	0fh,0afh,0d1h			; imul dx,cx
	  OP32
	mov	ax,dx
	swap	regdx
	ret
LXMUL@	endp
F_LXMUL@ endp

; Divide two stack operands, leave the result in (dx,ax).
setup32	macro
	mov	bx,sp
	  OP32
	mov	ax,ss:[bx+4]			; dividend
	endm
	PUBLIC	LDIV@, LUDIV@, LMOD@, LUMOD@
	PUBLIC	F_LDIV@, F_LUDIV@, F_LMOD@, F_LUMOD@
F_LDIV@	proc	far
LDIV@	proc	far
	setup32
	  OP32
	cwd
	  OP32
	idiv	word ptr ss:[bx+8]		; divisor
	  OP32
	mov	dx,ax
	swap	regdx
	ret	8
LDIV@	endp
F_LDIV@	endp
F_LUDIV@ proc	far
LUDIV@	proc	far
	setup32
	  OP32
	xor	dx,dx
	  OP32
	div	word ptr ss:[bx+8]		; divisor
	  OP32
	mov	dx,ax
	swap	regdx
	ret	8
LUDIV@	endp
F_LUDIV@ endp
F_LMOD@	proc	far
LMOD@	proc	far
	setup32
	  OP32
	cwd
	  OP32
	idiv	word ptr ss:[bx+8]		; divisor
	  OP32
	mov	ax,dx
	swap	regdx
	ret	8
LMOD@	endp
F_LMOD@	endp
F_LUMOD@ proc	far
LUMOD@	proc	far
	setup32
	  OP32
	xor	dx,dx
	  OP32
	div	word ptr ss:[bx+8]		; divisor
	  OP32
	mov	ax,dx
	swap	regdx
	ret	8
LUMOD@	endp
F_LUMOD@ endp

	else					; !FOR80386

; Replace the unsigned divide routines in the Turbo C library,
; which do the division one bit at a time (!).  (We should replace
; the signed divide routines as well, but it's too much work.)

	PUBLIC	LUDIV@, LUMOD@
	PUBLIC	F_LUDIV@, F_LUMOD@

; Divide two unsigned longs on the stack.
; Leave either the quotient or the remainder in (dx,ax).

; We use an offset in bx distinguish div from mod.
F_LUMOD@ proc	far
LUMOD@	proc	far
	mov	bx,2
	jmp	udiv
LUMOD@	endp
F_LUMOD@ endp
F_LUDIV@ proc	far
LUDIV@	proc	far
	xor	bx,bx
udiv:	push	bp
	push	bx				; 0 = div, 2 = mod
	mov	bp,sp
nlo	equ	8
nhi	equ	10
dlo	equ	12
dhi	equ	14
	mov	ax,[bp+nlo]
	mov	dx,[bp+nhi]
	mov	bx,[bp+dlo]
	mov	cx,[bp+dhi]
; Now we are dividing dx:ax by cx:bx.
; Check to see whether this is really a 32/16 division.
	or	cx,cx
	jnz	div2
; 32/16, check for 16- vs. 32-bit quotient
	cmp	dx,bx
	jae	div1
; 32/16 with 16-bit quotient, just do it.
	div	bx				; ax = quo, dx = rem
	pop	bx
	pop	bp
	jmp	cs:xx1[bx]
	even
xx1	dw	divx1
	dw	modx1
divx1:	xor	dx,dx
	ret	8
modx1:	mov	ax,dx
	xor	dx,dx
	ret	8
; 32/16 with 32-bit quotient, do in 2 parts.
div1:	mov	cx,ax				; save lo num
	mov	ax,dx
	xor	dx,dx
	div	bx				; ax = hi quo
	xchg	cx,ax				; save hi quo, get lo num
	div	bx				; ax = lo quo, dx = rem
	pop	bx
	pop	bp
	jmp	cs:xx1a[bx]
	even
xx1a	dw	offset divx1a
	dw	offset modx1
divx1a:	mov	dx,cx				; hi quo
	ret	8
; This is really a 32/32 bit division.
; (Note that the quotient cannot exceed 16 bits.)
; The following algorithm is taken from pp. 235-240 of Knuth, vol. 2
; (first edition).
; Start by normalizing the numerator and denominator.
div2:	or	ch,ch
	jz	div21				; ch == 0, but cl != 0
; Do 8 steps all at once.
	mov	bl,bh
	mov	bh,cl
	mov	cl,ch
	xor	ch,ch
	mov	al,ah
	mov	ah,dl
	mov	dl,dh
	xor	dh,dh
	rol	bx,1				; cancel following rcr
div2a:	rcr	bx,1				; finish previous shift
div21:	shr	dx,1
	rcr	ax,1
	shr	cx,1
	jnz	div2a
	rcr	bx,1
; Now we can do a 32/16 divide.
div2x:	div	bx				; ax = quo, dx = rem
; Multiply by the denominator, and correct the result.
	mov	cx,ax				; save quotient
	mul	word ptr [bp+dhi]
	mov	bx,ax				; save lo part of hi product
	mov	ax,cx
	mul	word ptr [bp+dlo]
	add	dx,bx
; Now cx = trial quotient, (dx,ax) = cx * denominator.
	not	dx
	neg	ax
	cmc
	adc	dx,0				; double-precision neg
	jc	divz				; zero quotient
						; requires special handling
	add	ax,[bp+nlo]
	adc	dx,[bp+nhi]
	jc	divx
; Quotient is too large, adjust it.
div3:	dec	cx
	add	ax,[bp+dlo]
	adc	dx,[bp+dhi]
	jnc	div3
; All done.  (dx,ax) = remainder, cx = lo quotient.
divx:	pop	bx
	pop	bp
	jmp	cs:xx3[bx]
	even
xx3	dw	offset divx3
	dw	offset modx3
divx3:	mov	ax,cx
	xor	dx,dx
modx3:	ret	8
; Handle zero quotient specially.
divz:	pop	bx
	jmp	cs:xxz[bx]
	even
xxz	dw	offset divxz
	dw	offset modxz
divxz:	pop	bp
	ret	8
modxz:	mov	ax,[bp+nlo-2]			; adjust for popf
	mov	dx,[bp+nhi-2]
	pop	bp
	ret	8
LUDIV@	endp
F_LUDIV@ endp

	endif					; FOR80386

; Swap even and odd bytes from src to dest.
; See gsmisc.c for the C definition.
	PUBLIC	_memswab
_memswab proc	far
	mov	bx,sp
	push	ds
	push	si
	mov	dx,ss:[bx+12]			; count
	shr	dx,1
	shr	dx,1
	les	si,ss:[bx+4]			; src
	lds	bx,ss:[bx+8]			; dest
	jnc	sw3
	add	si,2
	sub	bx,2
	jmp	sw2
sw1:	mov	ax,es:[si]
	add	si,4
	xchg	ah,al
	mov	[bx],ax
sw2:	mov	ax,es:[si-2]
	add	bx,4
	xchg	ah,al
	mov	[bx-2],ax
sw3:	dec	dx
	jge	sw1
	pop	si
	pop	ds
	ret
_memswab ENDP

; Transpose an 8x8 bit matrix.  See gsmisc.c for the algorithm in C.
	PUBLIC	_memflip8x8
_memflip8x8 proc far
	push	ds
	push	si
	push	di
		; After pushing, the offsets of the parameters are:
		; byte *inp=10, int line_size=14, byte *outp=16, int dist=20.
	mov	si,sp
	mov	di,ss:[si+14]			; line_size
	lds	si,ss:[si+10]			; inp
		; We assign variables to registers as follows:
		; ax = AE, bx = BF, cx (or di) = CG, dx = DH.
		; Load the input data.  Initially we assign
		; ax = AB, bx = EF, cx (or di) = CD, dx = GH.
	mov	ah,[si]
iload	macro	reg
	add	si,di
	mov	reg,[si]
	endm
	iload	al
	iload	ch
	iload	cl
	iload	bh
	iload	bl
	iload	dh
	iload	dl
		; Transposition macro, see C code for explanation.
trans	macro	reg1,reg2,shift,mask
	mov	si,reg1
	shr	si,shift
	xor	si,reg2
	and	si,mask
	xor	reg2,si
	shl	si,shift
	xor	reg1,si
	endm
		; Do 4x4 transpositions
	mov	di,cx			; we need cl for the shift count
	mov	cl,4
	trans	bx,ax,cl,0f0fh
	trans	dx,di,cl,0f0fh
		; Swap B/E, D/G
	xchg	al,bh
	mov	cx,di
	xchg	cl,dh
		; Do 2x2 transpositions
	mov	di,cx				; need cl again
	mov	cl,2
	trans	di,ax,cl,3333h
	trans	dx,bx,cl,3333h
	mov	cx,di				; done shifting >1
		; Do 1x1 transpositions
	trans	bx,ax,1,5555h
	trans	dx,cx,1,5555h
		; Store result
	mov	si,sp
	mov	di,ss:[si+20]			; dist
	lds	si,ss:[si+16]			; outp
	mov	[si],ah
istore	macro	reg
	add	si,di
	mov	[si],reg
	endm
	istore	bh
	istore	ch
	istore	dh
	istore	al
	istore	bl
	istore	cl
	istore	dl
		; All done
	pop	di
	pop	si
	pop	ds
	ret
_memflip8x8 ENDP


utilasm_TEXT ENDS
	END
