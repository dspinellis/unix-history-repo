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
 */

#ifndef lint
static char sccsid[] = "@(#)buf.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

/*-
 * buf.c --
 *	Functions for automatically-expanded buffers.
 */

#include    "sprite.h"
#include    "buf.h"

typedef struct {
    int	    size; 	/* Current size of the buffer */
    Byte    *buffer;	/* The buffer itself */
    Byte    *inPtr;	/* Place to write to */
    Byte    *outPtr;	/* Place to read from */
} Buf, *BufPtr;

#ifndef max
#define max(a,b)  ((a) > (b) ? (a) : (b))
#endif

/*
 * BufExpand --
 * 	Expand the given buffer to hold the given number of additional
 *	bytes.
 *	Makes sure there's room for an extra NULL byte at the end of the
 *	buffer in case it holds a string.
 */
#define BufExpand(bp,nb) \
 	if (((bp)->size - ((bp)->inPtr - (bp)->buffer)) < (nb)+1) {\
	    int newSize = (bp)->size + max((nb)+1,BUF_ADD_INC); \
	    Byte  *newBuf = (Byte *) realloc((bp)->buffer, newSize); \
	    \
	    (bp)->inPtr = newBuf + ((bp)->inPtr - (bp)->buffer); \
	    (bp)->outPtr = newBuf + ((bp)->outPtr - (bp)->buffer);\
	    (bp)->buffer = newBuf;\
	    (bp)->size = newSize;\
	}

#define BUF_DEF_SIZE	256 	/* Default buffer size */
#define BUF_ADD_INC	256 	/* Expansion increment when Adding */
#define BUF_UNGET_INC	16  	/* Expansion increment when Ungetting */

/*-
 *-----------------------------------------------------------------------
 * Buf_AddByte --
 *	Add a single byte to the buffer.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The buffer may be expanded.
 *
 *-----------------------------------------------------------------------
 */
void
Buf_AddByte (buf, byte)
    Buffer  buf;
    Byte    byte;
{
    register BufPtr  bp = (BufPtr) buf;

    BufExpand (bp, 1);

    *bp->inPtr = byte;
    bp->inPtr += 1;

    /*
     * Null-terminate
     */
    *bp->inPtr = 0;
}

/*-
 *-----------------------------------------------------------------------
 * Buf_AddBytes --
 *	Add a number of bytes to the buffer.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	Guess what?
 *
 *-----------------------------------------------------------------------
 */
void
Buf_AddBytes (buf, numBytes, bytesPtr)
    Buffer  buf;
    int	    numBytes;
    Byte    *bytesPtr;
{
    register BufPtr  bp = (BufPtr) buf;

    BufExpand (bp, numBytes);

    bcopy (bytesPtr, bp->inPtr, numBytes);
    bp->inPtr += numBytes;

    /*
     * Null-terminate
     */
    *bp->inPtr = 0;
}

/*-
 *-----------------------------------------------------------------------
 * Buf_UngetByte --
 *	Place the byte back at the beginning of the buffer.
 *
 * Results:
 *	SUCCESS if the byte was added ok. FAILURE if not.
 *
 * Side Effects:
 *	The byte is stuffed in the buffer and outPtr is decremented.
 *
 *-----------------------------------------------------------------------
 */
void
Buf_UngetByte (buf, byte)
    Buffer  buf;
    Byte    byte;
{
    register BufPtr	bp = (BufPtr) buf;

    if (bp->outPtr != bp->buffer) {
	bp->outPtr -= 1;
	*bp->outPtr = byte;
    } else if (bp->outPtr == bp->inPtr) {
	*bp->inPtr = byte;
	bp->inPtr += 1;
	*bp->inPtr = 0;
    } else {
	/*
	 * Yech. have to expand the buffer to stuff this thing in.
	 * We use a different expansion constant because people don't
	 * usually push back many bytes when they're doing it a byte at
	 * a time...
	 */
	int 	  numBytes = bp->inPtr - bp->outPtr;
	Byte	  *newBuf;

	newBuf = (Byte *)emalloc(bp->size + BUF_UNGET_INC);
	bcopy ((Address)bp->outPtr,
			(Address)(newBuf+BUF_UNGET_INC), numBytes+1);
	bp->outPtr = newBuf + BUF_UNGET_INC;
	bp->inPtr = bp->outPtr + numBytes;
	free ((Address)bp->buffer);
	bp->buffer = newBuf;
	bp->size += BUF_UNGET_INC;
	bp->outPtr -= 1;
	*bp->outPtr = byte;
    }
}

/*-
 *-----------------------------------------------------------------------
 * Buf_UngetBytes --
 *	Push back a series of bytes at the beginning of the buffer.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	outPtr is decremented and the bytes copied into the buffer.
 *
 *-----------------------------------------------------------------------
 */
void
Buf_UngetBytes (buf, numBytes, bytesPtr)
    Buffer  buf;
    int	    numBytes;
    Byte    *bytesPtr;
{
    register BufPtr	bp = (BufPtr) buf;

    if (bp->outPtr - bp->buffer >= numBytes) {
	bp->outPtr -= numBytes;
	bcopy (bytesPtr, bp->outPtr, numBytes);
    } else if (bp->outPtr == bp->inPtr) {
	Buf_AddBytes (buf, numBytes, bytesPtr);
    } else {
	int 	  curNumBytes = bp->inPtr - bp->outPtr;
	Byte	  *newBuf;
	int 	  newBytes = max(numBytes,BUF_UNGET_INC);

	newBuf = (Byte *)emalloc (bp->size + newBytes);
	bcopy((Address)bp->outPtr, (Address)(newBuf+newBytes), curNumBytes+1);
	bp->outPtr = newBuf + newBytes;
	bp->inPtr = bp->outPtr + curNumBytes;
	free ((Address)bp->buffer);
	bp->buffer = newBuf;
	bp->size += newBytes;
	bp->outPtr -= numBytes;
	bcopy ((Address)bytesPtr, (Address)bp->outPtr, numBytes);
    }
}

/*-
 *-----------------------------------------------------------------------
 * Buf_GetByte --
 *	Return the next byte from the buffer. Actually returns an integer.
 *
 * Results:
 *	Returns BUF_ERROR if there's no byte in the buffer, or the byte
 *	itself if there is one.
 *
 * Side Effects:
 *	outPtr is incremented and both outPtr and inPtr will be reset if
 *	the buffer is emptied.
 *
 *-----------------------------------------------------------------------
 */
int
Buf_GetByte (buf)
    Buffer  buf;
{
    BufPtr  bp = (BufPtr) buf;
    int	    res;

    if (bp->inPtr == bp->outPtr) {
	return (BUF_ERROR);
    } else {
	res = (int) *bp->outPtr;
	bp->outPtr += 1;
	if (bp->outPtr == bp->inPtr) {
	    bp->outPtr = bp->inPtr = bp->buffer;
	    bp->inPtr = 0;
	}
	return (res);
    }
}

/*-
 *-----------------------------------------------------------------------
 * Buf_GetBytes --
 *	Extract a number of bytes from the buffer.
 *
 * Results:
 *	The number of bytes gotten.
 *
 * Side Effects:
 *	The passed array is overwritten.
 *
 *-----------------------------------------------------------------------
 */
int
Buf_GetBytes (buf, numBytes, bytesPtr)
    Buffer  buf;
    int	    numBytes;
    Byte    *bytesPtr;
{
    BufPtr  bp = (BufPtr) buf;
    
    if (bp->inPtr - bp->outPtr < numBytes) {
	numBytes = bp->inPtr - bp->outPtr;
    }
    bcopy (bp->outPtr, bytesPtr, numBytes);
    bp->outPtr += numBytes;

    if (bp->outPtr == bp->inPtr) {
	bp->outPtr = bp->inPtr = bp->buffer;
	*bp->inPtr = 0;
    }
    return (numBytes);
}

/*-
 *-----------------------------------------------------------------------
 * Buf_GetAll --
 *	Get all the available data at once.
 *
 * Results:
 *	A pointer to the data and the number of bytes available.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
Byte *
Buf_GetAll (buf, numBytesPtr)
    Buffer  buf;
    int	    *numBytesPtr;
{
    BufPtr  bp = (BufPtr)buf;

    if (numBytesPtr != (int *)NULL) {
	*numBytesPtr = bp->inPtr - bp->outPtr;
    }
    
    return (bp->outPtr);
}

/*-
 *-----------------------------------------------------------------------
 * Buf_Discard --
 *	Throw away bytes in a buffer.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The bytes are discarded. 
 *
 *-----------------------------------------------------------------------
 */
void
Buf_Discard (buf, numBytes)
    Buffer  buf;
    int	    numBytes;
{
    register BufPtr	bp = (BufPtr) buf;

    if (bp->inPtr - bp->outPtr <= numBytes) {
	bp->inPtr = bp->outPtr = bp->buffer;
	*bp->inPtr = 0;
    } else {
	bp->outPtr += numBytes;
    }
}

/*-
 *-----------------------------------------------------------------------
 * Buf_Size --
 *	Returns the number of bytes in the given buffer. Doesn't include
 *	the null-terminating byte.
 *
 * Results:
 *	The number of bytes.
 *
 * Side Effects:
 *	None.
 *
 *-----------------------------------------------------------------------
 */
int
Buf_Size (buf)
    Buffer  buf;
{
    return (((BufPtr)buf)->inPtr - ((BufPtr)buf)->outPtr);
}

/*-
 *-----------------------------------------------------------------------
 * Buf_Init --
 *	Initialize a buffer. If no initial size is given, a reasonable
 *	default is used.
 *
 * Results:
 *	A buffer to be given to other functions in this library.
 *
 * Side Effects:
 *	The buffer is created, the space allocated and pointers
 *	initialized.
 *
 *-----------------------------------------------------------------------
 */
Buffer
Buf_Init (size)
    int	    size; 	/* Initial size for the buffer */
{
    BufPtr  bp;	  	/* New Buffer */

    bp = (Buf *)emalloc(sizeof(Buf));

    if (size <= 0) {
	size = BUF_DEF_SIZE;
    }
    bp->size = size;
    bp->buffer = (Byte *)emalloc (size);
    bp->inPtr = bp->outPtr = bp->buffer;
    *bp->inPtr = 0;

    return ((Buffer) bp);
}

/*-
 *-----------------------------------------------------------------------
 * Buf_Destroy --
 *	Nuke a buffer and all its resources.
 *
 * Results:
 *	None.
 *
 * Side Effects:
 *	The buffer is freed.
 *
 *-----------------------------------------------------------------------
 */
void
Buf_Destroy (buf, freeData)
    Buffer  buf;  	/* Buffer to destroy */
    Boolean freeData;	/* TRUE if the data should be destroyed as well */
{
    BufPtr  bp = (BufPtr) buf;
    
    if (freeData) {
	free ((Address)bp->buffer);
    }
    free ((Address)bp);
}
