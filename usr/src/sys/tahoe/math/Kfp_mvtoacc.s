#include	"fp.h"

	.globl	_mvtofacc	# mvtofacc(value, acc_addr)
_mvtofacc:
	.word	0x0000

    #
    # move value to floating point accumulator
    #
	movl	4(fp),*12(fp)
	ret

	.globl	_mvtodacc	# mvtodacc(value_hi, value_lo, acc_addr)
_mvtodacc:
	.word	0x0000

    #
    # move value to double precision accumulator
    #
	movl	12(fp),r0	# address of accumulator
	movl	4(fp),(r0)	# most significant longword
	movl	8(fp),4(r0)	# least significant longword
	ret
