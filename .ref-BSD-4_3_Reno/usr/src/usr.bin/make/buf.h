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
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)buf.h	5.3 (Berkeley) 6/1/90
 */

/*-
 * buf.h --
 *	Header for users of the buf library.
 */

#ifndef _BUF_H
#define _BUF_H

#include    "sprite.h"

typedef struct Buffer *Buffer;
typedef unsigned char Byte;

Buffer	    	  Buf_Init();	    /* Initialize a buffer */
void	    	  Buf_Destroy();    /* Destroy a buffer */
void	    	  Buf_AddByte();    /* Add a single byte to a buffer */
void	    	  Buf_AddBytes();   /* Add a range of bytes to a buffer */
int	    	  Buf_GetByte();    /* Get a byte from a buffer */
int	    	  Buf_GetBytes();   /* Get multiple bytes */
void		  Buf_UngetByte();  /* Push a byte back into the buffer */
void		  Buf_UngetBytes(); /* Push many bytes back into the buf */
Byte	    	  *Buf_GetAll();    /* Get them all */
void	    	  Buf_Discard();    /* Throw away some of the bytes */
int	    	  Buf_Size();	    /* See how many are there */

#define BUF_ERROR 256

#endif _BUF_H
