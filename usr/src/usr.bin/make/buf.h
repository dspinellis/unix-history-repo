/*
 * Copyright (c) 1988, 1989, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)buf.h	8.2 (Berkeley) %G%
 */

/*-
 * buf.h --
 *	Header for users of the buf library.
 */

#ifndef _BUF_H
#define _BUF_H

#include    "sprite.h"

typedef char Byte;

typedef struct Buffer {
    int	    size; 	/* Current size of the buffer */
    int     left;	/* Space left (== size - (inPtr - buffer)) */
    Byte    *buffer;	/* The buffer itself */
    Byte    *inPtr;	/* Place to write to */
    Byte    *outPtr;	/* Place to read from */
} *Buffer;

/* Buf_AddByte adds a single byte to a buffer. */
#define	Buf_AddByte(bp, byte) \
	(void) (--(bp)->left <= 0 ? Buf_OvAddByte(bp, byte), 1 : \
		(*(bp)->inPtr++ = (byte), *(bp)->inPtr = 0), 1)

#define BUF_ERROR 256

void Buf_OvAddByte __P((Buffer, int));
void Buf_AddBytes __P((Buffer, int, Byte *));
void Buf_UngetByte __P((Buffer, int));
void Buf_UngetBytes __P((Buffer, int, Byte *));
int Buf_GetByte __P((Buffer));
int Buf_GetBytes __P((Buffer, int, Byte *));
Byte *Buf_GetAll __P((Buffer, int *));
void Buf_Discard __P((Buffer, int));
int Buf_Size __P((Buffer));
Buffer Buf_Init __P((int));
void Buf_Destroy __P((Buffer, Boolean));

#endif /* _BUF_H */
