/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)bit.h	5.2 (Berkeley) %G%
 */

/*
 * bit.h --
 *
 *	Definition of macros for setting and clearing bits in an array
 *	of integers.
 *
 *	It is assumed that "int" is 32 bits wide.
 */

#ifndef _BIT
#define _BIT

#include "sprite.h"

#define BIT_NUM_BITS_PER_INT	32
#define BIT_NUM_BITS_PER_BYTE	8

#define Bit_NumInts(numBits)	\
	(((numBits)+BIT_NUM_BITS_PER_INT -1)/BIT_NUM_BITS_PER_INT)

#define Bit_NumBytes(numBits)	\
	(Bit_NumInts(numBits) * sizeof(int))

#define Bit_Alloc(numBits, bitArrayPtr)  	\
        bitArrayPtr = (int *)malloc((unsigned)Bit_NumBytes(numBits)); \
        Bit_Zero((numBits), (bitArrayPtr))

#define Bit_Free(bitArrayPtr)	\
        free((char *)bitArrayPtr)

#define Bit_Set(numBits, bitArrayPtr) \
	((bitArrayPtr)[(numBits)/BIT_NUM_BITS_PER_INT] |= \
				(1 << ((numBits) % BIT_NUM_BITS_PER_INT)))

#define Bit_IsSet(numBits, bitArrayPtr) \
	((bitArrayPtr)[(numBits)/BIT_NUM_BITS_PER_INT] & \
				(1 << ((numBits) % BIT_NUM_BITS_PER_INT)))

#define Bit_Clear(numBits, bitArrayPtr) \
	((bitArrayPtr)[(numBits)/BIT_NUM_BITS_PER_INT] &= \
				~(1 << ((numBits) % BIT_NUM_BITS_PER_INT)))

#define Bit_IsClear(numBits, bitArrayPtr) \
	(!(Bit_IsSet((numBits), (bitArrayPtr))))

#define Bit_Copy(numBits, srcArrayPtr, destArrayPtr) \
	bcopy((char *)(srcArrayPtr), (char *)(destArrayPtr), \
		Bit_NumBytes(numBits))

#define Bit_Zero(numBits, bitArrayPtr) \
	bzero((char *)(bitArrayPtr), Bit_NumBytes(numBits))

extern int	  Bit_FindFirstSet();
extern int	  Bit_FindFirstClear();
extern Boolean	  Bit_Intersect();
extern Boolean 	  Bit_Union();
extern Boolean 	  Bit_AnySet();
extern int  	  *Bit_Expand();
	 
#endif _BIT
