/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)qsort_.c	5.1	%G%
 */

/*
 * quick sort interface
 *
 * calling sequence:
 *	external compar
 *	call qsort (array, len, isize, compar)
 *	----
 *	integer*2 function compar (obj1, obj2)
 * where:
 *	array contains the elements to be sorted
 *	len is the number of elements in the array
 *	isize is the size of an element, typically -
 *		4 for integer, float
 *		8 for double precision
 *		(length of character object) for character arrays
 *	compar is the name of an integer*2 function that will return -
 *		<0 if object 1 is logically less than object 2
 *		=0 if object 1 is logically equal to object 2
 *		>0 if object 1 is logically greater than object 2
 */

qsort_(array, len, isize, compar)
long *len, *isize;
long *array;
int (*compar)(); /* may be problematical */
{
	qsort(array, (int)*len, (int)*isize, compar);
}
