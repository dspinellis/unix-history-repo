;    Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
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

; gdevegaasm.asm
; Assembly code for Ghostscript PC frame buffer driver

gdevegaasm_TEXT	SEGMENT	BYTE PUBLIC 'CODE'
	ASSUME	CS:gdevegaasm_TEXT

; Note: Turbo C uses si and di for register variables, so
; we have to preserve them.

; Normal entry and exit.  Arguments are relative to bp.
enterp	macro
	push	bp
	mov	bp,sp
	x = 6				; offset of arguments,
					; large code model
	endm
leavep	macro
	pop	bp
	endm
; Fast entry and exit, for procedures that don't use bx until
; they've fetched all their arguments.  Arguments are relative to ss:bx.
enterf	macro
	mov	bx,sp
	x = 4				; offset of arguments,
					; large code model
	endm
leavef	macro
	endm

; Fast call to VESA set-page routine.
; void vesa_call_set_page(void (*set_page_proc)(int), int page_no, int win_no)
	PUBLIC	_vesa_call_set_page
_vesa_call_set_page proc far
	enterf
	mov ax,4f05h
	mov dx,ss:[bx+x+4]			; page_no
	push ss:[bx+x+2]			; set_page_proc
	push ss:[bx+x]
	mov bx,ss:[bx+x+6]			; win_no
	ret
_vesa_call_set_page endp

; Structure for operation parameters.
; Note that this structure is shared with C code.
; Not all parameters are used for every operation.
; typedef struct rop_params_s {
p_dest	equ	0	; fb_ptr dest;	/* pointer to frame buffer */
p_draster equ	4	; int draster;	/* raster of frame buffer */
p_src	equ	6	; byte far *src; /* pointer to source data */
p_sraster equ	10	; int sraster;	/* source raster */
p_width	equ	12	; int width;	/* width in bytes */
p_height equ	14	; int height;	/* height in scan lines */
p_shift equ	16	; int shift;	/* amount to right shift source */
p_invert equ	18	; int invert;	/* 0 or -1 to invert source */
p_data	equ	20	; int data;	/* data for fill */
; } rop_params;

; void memsetcol(rop_params _ss *rop)
; {	byte far *addr = rop->dest;
;	int yc = rop->height;
;	while ( yc-- )
;	 { byte discard = *addr;
;	   *addr = rop->data;
;	   addr += rop->draster;
;	 }
; }
	PUBLIC	_memsetcol
_memsetcol proc	far
	enterf
	push	ds
	mov	ax,ss
	mov	ds,ax
	mov	bx,[bx+x]			; rop
	mov	cx,[bx].p_height
	jcxz	msc0				; height == 0
	mov	ax,[bx].p_data
	mov	dx,[bx].p_draster
	lds	bx,[bx].p_dest
; Unroll the loop -- two copies.
	inc	cx		;round up to nearest word.  cx>=2 now.
	shr	cx,1		;make byte count into word count.
	jnc	msc2		;if it had been odd, do a half word first.
msc1:	mov	ah,[bx]
	mov	[bx],al
	add	bx,dx
msc2:	mov	ah,[bx]
	mov	[bx],al
	add	bx,dx
	loop	msc1
	pop	ds
msc0:	leavef
	ret
_memsetcol ENDP

; void memsetrect(rop_params _ss *rop)
; {	byte far *addr = rop->dest;
;	int yc = rop->height;
;	while ( yc-- )
;	 { int cnt = rop->width;
;	   while ( cnt-- ) *addr++ = rop->data;
;	   addr += rop->drast - rop->width;
;	 }
; }
	PUBLIC	_memsetrect
_memsetrect proc	far
	enterf
	push	ds
	mov	ax,ss
	mov	ds,ax
	mov	bx,[bx+x]			; rop
	mov	cx,[bx].p_height
	jcxz	msr0				; height == 0
	push	si
	push	di
	mov	ax,[bx].p_data
	les	di,[bx].p_dest
	cld
	mov	dx,[bx].p_draster
	mov	si,cx				; si = height
	mov	cx,[bx].p_width
	sub	dx,cx
	cmp	cx,10
	ja	msrl				; large count, use fast loop
; Small count, rep stosb is faster.
msrs:	mov	cx,[bx].p_width
	rep	stosb
	add	di,dx
	dec	si				; count reps
	jnz	msrs
	pop	di
	pop	si
msr0:	pop	ds
	leavef
	ret
; Large count, loop by words rather than bytes.
msrl:	mov	ah,al			;we may be storing words...
msr1:	mov	cx,[bx].p_width
	test	di,1			;test for an even address
	je	msr2			;if even, we can store words.
	stosb				;otherwise we need to even it out.
	dec	cx			;(cx is at least one here)
msr2:	shr	cx,1			;convert byte count into word count
	rep	stosw			;store them puppies as fast as we can.
	jnc	msr3			;if an odd number, store it, too.
	stosb				;(no need to dec cx here).
msr3:	add	di,dx
	dec	si			; count reps
	jnz	msr1
	pop	di
	pop	si
	pop	ds
	leavef
	ret
_memsetrect ENDP

; void memrwcol(rop_params _ss *rop)
; {	byte far *dp = rop->dest, *sp = rop->src;
;	int yc = rop->height;
;	int shift = rop->shift;
;	while ( yc-- )
;	 { byte discard = *dp;
;	   *dp = ((*sp >> shift) + (*sp << (8 - shift))) ^ rop->invert;
;	   dp += rop->draster, sp += rop->sraster;
;	 }
; }
	PUBLIC	_memrwcol
_memrwcol proc far
	enterp
	push	ds
	mov	ax,ss
	mov	ds,ax
	mov	bx,[bp+x]			; rop
	cmp	word ptr [bx].p_height,0
	jz	short mrw0
	push	si
	push	di
; Register usage:
;   ds:si = sp, es:di = dp, bx = sraster, dx = draster, cl = shift,
;   ch = invert, ah = low byte of yc.
	push	[bx].p_height
	mov	dx,[bx].p_draster
	mov	ax,[bx].p_sraster
	mov	cl,[bx].p_shift
	mov	ch,[bx].p_invert
	les	di,[bx].p_dest
	lds	si,[bx].p_src
	mov	bx,ax
	mov	ah,[bp-8]			; low byte of yc
	test	ah,ah
	jz	mrw2
mrw1:	mov	al,[si]
	ror	al,cl
	xor	al,ch
	xchg	es:[di],al
	add	si,bx
	add	di,dx
	dec	ah
	jnz	mrw1
mrw2:	dec	byte ptr [bp-7]			; high byte of yc
	jge	mrw1
	add	sp,2				; pop yc
	pop	di
	pop	si
mrw0:	pop	ds
	leavep
	ret
_memrwcol ENDP

; void memrwcol2(rop_params _ss *rop)
; {	byte far *dp = rop->dest, *sp = rop->src;
;	int yc = rop->height;
;	int shift = rop->shift;
;	while ( yc-- )
;	 { byte discard = *dp;
;	   *dp = ((sp[1] >> shift) + (*sp << (8 - shift))) ^ rop->invert;
;	   dp += rop->draster, sp += rop->sraster;
;	 }
; }
	PUBLIC	_memrwcol2
_memrwcol2 proc far
	enterp
	push	ds
	mov	ax,ss
	mov	ds,ax
	mov	bx,[bp+x]			; rop
	cmp	word ptr [bx].p_height,0
	jz	short mrw20
	push	si
	push	di
; Register usage:
;   ds:si = sp, es:di = dp, bx = sraster, dx = draster, cl = shift,
;   ch = invert.
	push	[bx].p_height
	mov	dx,[bx].p_draster
	mov	ax,[bx].p_sraster
	mov	cl,[bx].p_shift
	mov	ch,[bx].p_invert
	les	di,[bx].p_dest
	lds	si,[bx].p_src
	mov	bx,ax
mrw21:	mov	ax,[si]				; bytes are in wrong order...
	ror	ax,cl
	xor	ah,ch				; ... so result is in ah
	xchg	es:[di],ah
	add	si,bx
	add	di,dx
	dec	word ptr [bp-8]			; yc
	jg	mrw21
	add	sp,2				; pop yc
	pop	di
	pop	si
mrw20:	pop	ds
	leavep
	ret
_memrwcol2 ENDP

gdevegaasm_TEXT	ENDS
	END
