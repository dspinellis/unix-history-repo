| this file was produced via adb on an a.out.
| It thus contains no information proprietary to Sun Microsystems, Inc.
| We hereby mark it Copyright (C) 1983, Regents, university of California
| in order that we may contribute it to the public domain.
.globl	__read
.globl	__write
.globl _vadvise
bad:                        jmp     cerror
__read:              pea     3:w
                        trap    #0
                        bcss   bad
                        rts
                        jmp     cerror

__write:                 pea     4:w
                        trap    #0
                        bcss    __read+0xa
_vadvise:
                        rts

