/*-
 * buf.h --
 *	Header for users of the buf library.
 *
 * Copyright (c) 1987 by the Regents of the University of California
 *
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 *
 *	"$Id: buf.h,v 2.1 89/07/03 15:49:36 adam Exp $ SPRITE (Berkeley)"
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
