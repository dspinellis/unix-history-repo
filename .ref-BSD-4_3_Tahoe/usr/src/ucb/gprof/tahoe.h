/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)tahoe.h	1.3 (Berkeley) 6/29/88
 */

    /*
     *	opcode of the `callf' instruction
     */
#define	CALLF	0xfe

    /*
     *	offset (in bytes) of the code from the entry address of a routine.
     *	(see asgnsamples for use and explanation.)
     */
#define OFFSET_OF_CODE	2
#define	UNITS_TO_CODE	(OFFSET_OF_CODE / sizeof(UNIT))

    /*
     *	register for pc relative addressing
     */
#define	PC	0xf

enum opermodes {
    literal, indexed, reg, regdef, autodec, autoinc, autoincdef, 
    bytedisp, bytedispdef, worddisp, worddispdef, longdisp, longdispdef,
    immediate, absolute, byterel, bytereldef, wordrel, wordreldef,
    longrel, longreldef
};
typedef enum opermodes	operandenum;
