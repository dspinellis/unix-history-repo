/
/  Sendmail
/  Copyright (c) 1983  Eric P. Allman
/  Berkeley, California
/
/  Copyright (c) 1983 Regents of the University of California.
/  All rights reserved.  The Berkeley software License Agreement
/  specifies the terms and conditions for redistribution.
/
/	@(#)bmove.11.s	4.2 (Berkeley) 6/7/85
/
/
/  BMOVE -- block move
/
/
/	This is a highly optimized version of the old C-language
/	bmove routine; it's function (should be) identical.
/	It uses a fancy algorithm to move words instead of bytes
/	whenever possible.
/
/	In C the routine is:
/		char *bmove(a, b, l)
/		char	*a, *b;
/		int	l;
/		{
/			register int	n;
/			register char	*p, *q;
/			p = a;
/			q = b;
/			n = l;
/			while (n--)
/				*q++ = *p++;
/			return (q);
/		}
/
/	Parameters:
/		a [4(sp)] -- source area
/		b [6(sp)] -- target area
/		l [10(sp)] -- byte count
/
/	Returns:
/		Pointer to end of target area.
/
/	History:
/		3/14/79 [rse] -- added odd to odd case
/		2/9/78 [bob] -- converted from "C"
/
/

.globl	_bmove

_bmove:
	mov	r2,-(sp)	/ save r2
	mov	4(sp),r1	/ get src address
	mov	6(sp),r0	/ get dst address

	/ determine whether to use word or byte move
	mov	r0,r2		/ r2 will reflect the three cases
	bic	$177776,r2	/ keep only last bit of dst
	ror	4(sp)		/ get least significant bit of src
	adc	r2		/ add it in.
	beq	wordm		/ both on even boundary
	dec	r2		/ check for odd case
	bgt	wordodd		/ both on odd boundary

	mov	10(sp),r2	/ get count
	beq	done
bytem:
	movb	(r1)+,(r0)+	/ copy next byte
	sob	r2,bytem	/ branch until done
	br	done

wordm:
	mov	10(sp),r2	/ get count
wordt:
	beq	done
	asr	r2		/ get word count
	bcs	odd		/ count was odd
even:
	mov	(r1)+,(r0)+	/ copy word
	sob	r2,even		/ more to do if non-zero
	br	done

wordodd:
	mov	10(sp),r2	/ get count
	beq	done
	movb	(r1)+,(r0)+	/ copy byte
	dec	r2		/ dec count
	br	wordt		/ now treat as an even word move

odd:
	beq	odd2		/ special case of count = 1
odd1:
	mov	(r1)+,(r0)+	/ copy word
	sob	r2,odd1		/ continue
odd2:
	movb	(r1)+,(r0)+	/ count was odd. do last one

done:
	mov	(sp)+,r2	/ restore r2
	rts	pc		/ return
