/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/***********************************************************************
 *  file: hpBlock.h
 *
 *  Header file for blockHandler and wakeupHandler routines
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X11 to HP9000
 *		Steve Hiebert -- MTS
 *
 *
 */

/* don't move the four fields - each device will have its own
 * RegisterState struct which will add the necessary registers
 * after the save and restore routines.
*/
typedef struct
	{
	void (*save)();
	void (*restore)();
	void (*claim)();
	void (*surrender)();
	void (*cursorOff)();
	} HpRegisterState;

typedef HpRegisterState *HpRegisterStatePtr;
