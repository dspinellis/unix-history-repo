/
/ Funny entry points for the
/ insidious output routines
/
.text
.globl _putchar, _outchar, _pline, _setlist, _setnorm, _setnumb
.globl _normchar, _termchar, _normline, _numbline
.globl _listchar
.globl _Putchar, _Outchar, _Pline

.globl _putNFL, _putnl, _flush
_putNFL:
	jsr	pc,_putnl
	jmp	_flush
_putchar:
	jmp	*_Putchar
_outchar:
	jmp	*_Outchar
_pline:
	jmp	*_Pline
_setlist:
	mov	_Putchar,r0
	mov	$_listchar,_Putchar
	rts	pc
_setnorm:
	mov	_Putchar,r0
	mov	$_normchar,_Putchar
	rts	pc
_setnumb:
	mov	$_numbline,_Pline
	rts	pc
.globl _setnoaddr, _eol, _setcount, _newline
.globl _setCNL, _setNAEOL
_setCNL:
	jsr	pc,_setcount
	jmp	_newline
_setNAEOL:
	jsr	pc,_setnoaddr
	jmp	_eol
.globl _setoutt
_setoutt:
	mov	$_termchar,_Outchar
	rts	pc
.data
_Putchar:	_normchar
_Outchar:	_termchar
_Pline:		_normline
