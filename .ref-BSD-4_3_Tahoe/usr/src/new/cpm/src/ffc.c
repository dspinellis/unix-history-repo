/*	ffc.c	1.4	83/05/13	*/

/*
 * If used on a vax (i.e., VAX is defined), this routine simply executes
 * the VAX find-first-set instruction.
 * Usage:
 *	int startbit, field_lth, field, result;
 *	result = ffc(startbit,field_lth,field);
 *
 * startbit is the bit number of the field to start the search,
 *		the rightmost bit is number one.
 * field_lth is the length of the field in bits.
 * field is the actual field
 * result is the resulting bit number,
 *		e.g. the number of the first clear bit in the field.
 * hs 10/29/82
 */

ffc(start, len, field)

#ifndef VAX
	register long field;
	int start, len;
{
	register int i;

	for (i=start; i<len; i++) {
		if ((field&1) == 0)
			break;
		field >>= 1;
	}
	return (i);
}
#else

	int start, len, field;
{
	asm("ffc	4(ap),8(ap),12(ap),r0");
	return;
}
#endif
