; $Header: /lapis_d/minshall/src/misc/minshall/src/netsys/RCS/support.asm,v 1.5 87/01/14 11:29:19 minshall Exp $


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

	public	dirconio

dirconio	proc	near
	push	bp
	mov	bp,sp

	mov	ah,01H		; test input
	int	16H
	mov	ax,0ffffH	; nothing read
	jz	dirconiodone
	mov	ah,00H		; consume the input
	int	16H

dirconiodone:

	mov	sp,bp
	pop	bp
	ret
dirconio	endp

_TEXT	ends

	end
