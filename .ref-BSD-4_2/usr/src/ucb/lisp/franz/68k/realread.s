| /* Copyright (c) 1982, Regents, University of California */
			.globl	__read
			.globl	__write
			.globl	_vfork
__read:                 movw    #0x3,d0
                        movl    a7@(4),a0
                        movl    a7@(8),d1
                        movl    a7@(12),a1
                        jmp     call

__write:                movw    #0x4,d0
			movl    a7@(4),a0
			movl    a7@(8),d1
			movl    a7@(12),a1
			jmp     call
_vfork:
			jmp	_fork
