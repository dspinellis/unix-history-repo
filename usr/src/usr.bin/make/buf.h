/*
 * Copyright (c) 1988, 1989, 1990 The Regents of the University of California.
 * Copyright (c) 1988, 1989 by Adam de Boor
 * Copyright (c) 1989 by Berkeley Softworks
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Adam de Boor.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)buf.h	5.5 (Berkeley) %G%
 */

/*-
 * buf.h --
 *	Header for users of the buf library.
 */

#ifndef _BUF_H
#define _BUF_H

#include    "sprite.h"

typedef unsigned char Byte;

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

void	 Buf_AddBytes __P((Buffer, int, Byte *));
void	 Buf_Destroy __P((Buffer, Boolean));
void	 Buf_Discard __P((Buffer, int));
Byte	*Buf_GetAll __P((Buffer, int *));
int	 Buf_GetByte __P((Buffer));
int	 Buf_GetBytes __P((Buffer, int, Byte *));
Buffer	 Buf_Init __P((int));
void	 Buf_OvAddByte __P((Buffer, int));
int	 Buf_Size __P((Buffer));
void	 Buf_UngetByte __P((Buffer, int));
void	 Buf_UngetBytes __P((Buffer, int, Byte *));

#endif /* _BUF_H */
