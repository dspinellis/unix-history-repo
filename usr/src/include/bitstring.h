/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Paul Vixie.
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
 *	@(#)bitstring.h	5.1 (Berkeley) %G%
 */

typedef	unsigned char bitstr_t;

/* internal macros */
				/* byte of the bitstring bit is in */
#define	_bit_byte(bit) \
	((bit) >> 3)

				/* mask for the bit within its byte */
#define	_bit_mask(bit) \
	(1 << ((bit)&0x7))

/* external macros */
				/* bytes in a bitstring of nbits bits */
#define	bitstr_size(nbits) \
	((((nbits) - 1) >> 3) + 1)

				/* allocate a bitstring */
#define	bit_alloc(nbits) \
	(bitstr_t *)malloc(1, \
	    (unsigned int)_bitstr_size(nbits) * sizeof(bitstr_t))

				/* allocate a bitstring on the stack */
#define	bit_decl(name, nbits) \
	(name)[bitstr_size(nbits)]

				/* is bit N of bitstring name set? */
#define	bit_test(name, bit) \
	((name)[_bit_byte(bit)] & _bit_mask(bit))

				/* set bit N of bitstring name */
#define	bit_set(name, bit) \
	(name)[_bit_byte(bit)] |= _bit_mask(bit)

				/* clear bit N of bitstring name */
#define	bit_clear(name, bit) \
	(name)[_bit_byte(bit)] &= ~_bit_mask(bit)

				/* clear bits 0 ... N in bitstring name */
#define	bit_nclear(name, start, stop) { \
	register int _startbyte, _stopbyte; \
	_startbyte = _bit_byte(start); \
	(name)[_startbyte] &= 0xff >> (8 - ((start)&0x7)); \
	for (_stopbyte = _bit_byte(stop); ++_startbyte < _stopbyte; \
	    (name)[_startbyte] = 0); \
	(name)[_stopbyte] &= 0xff << (((stop)&0x7) + 1); \
}

				/* set bits 0 ... N in string name */
#define	bit_nset(name, start, stop) { \
	register int _startbyte, _stopbyte; \
	_startbyte = _bit_byte(start); \
	(name)[_startbyte] |= 0xff << ((start)&0x7); \
	for (_stopbyte = _bit_byte(stop); ++_startbyte < _stopbyte; \
	    (name)[_startbyte] = 0xff); \
	(name)[_stopbyte] |= 0xff >> (7 - ((stop)&0x7)); \
}

				/* find first bit clear in name */
#define	bit_ffc(name, nbits, value) { \
	register int _byte, _stopbyte; \
	for ((value) = -1, _byte = 0, _stopbyte = _bit_byte(nbits); \
	    _byte <= _stopbyte; _byte++) \
		if ((name)[_byte] != 0xff) { \
			(value) = _byte << 3; \
			for (_stopbyte = (name)[_byte]; (_stopbyte&0x1); \
			    ++(value), _stopbyte >>= 1); \
			break; \
		} \
}

				/* find first bit set in name */
#define	bit_ffs(name, nbits, value) { \
	register int _byte, _stopbyte; \
	for ((value) = -1, _byte = 0, _stopbyte = _bit_byte(nbits); \
	    _byte <= _stopbyte; _byte++) \
		if ((name)[_byte]) { \
			(value) = _byte << 3; \
			for (_stopbyte = (name)[_byte]; !(_stopbyte&0x1); \
			    ++(value), _stopbyte >>= 1); \
			break; \
		} \
}
