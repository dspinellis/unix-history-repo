/* $XConsortium: PEXlibint.h,v 1.8 92/08/26 13:05:15 mor Exp $ */

/******************************************************************************
Copyright 1987,1991 by Digital Equipment Corporation, Maynard, Massachusetts
Copyright 1992 by the Massachusetts Institute of Technology

                        All Rights Reserved

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation, and that
the name of Digital or M.I.T. not be used in advertising or publicity
pertaining to distribution of the software without specific, written prior
permission.  Digital and M.I.T. make no representations about the suitability
of this software for any purpose.  It is provided "as is" without express or
implied warranty.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************************/

#ifndef PEXLIBINT_H
#define PEXLIBINT_H

#include <X11/Xlibint.h>
#include <X11/Xfuncs.h>
#include "PEXproto.h"


/* -------------------------------------------------------------------------
 * Typedefs for referencing fields in requests. 
 * ------------------------------------------------------------------------- */

/*
 * Generic request header.
 */

typedef struct pexRequestHeader
{
    unsigned char       extOpcode;
    unsigned char       pexOpcode;
    unsigned short      reqLength;
} pexRequestHeader;


/*
 * OC request header.
 */

typedef struct pexOCRequestHeader
{
    unsigned char       extOpcode;
    unsigned char       pexOpcode;
    unsigned short      reqLength;
    unsigned short      fpFormat;
    unsigned short      pad;
    unsigned long       target;
    unsigned long       numCommands;
} pexOCRequestHeader;


/*
 * Element header for OC with list.
 */

typedef pexElementInfo pexOCListHeader;


/*
 * Element header for OC with list & count.
 */

typedef struct pexOCcListHeader
{
    pexElementInfo      head;
    unsigned short      length;
    unsigned short      pad;
} pexOCcListHeader;



/* -------------------------------------------------------------------------
 * Display extension data structures and macros.
 * ------------------------------------------------------------------------- */

/*
 * For each display initialized by PEXInitialize(), a record is allocated
 * which holds various information about that display.  These records are
 * maintained in a linked list.  The record for the most recently referenced
 * display is always kept at the beginning of the list (for quick access).
 */

typedef struct PEXDisplayInfo
{
    Display             *display;    /* pointer to X display structure */
    XExtCodes		*extCodes;   /* extension codes */
    PEXExtensionInfo	*extInfo;    /* extension info */
    unsigned char       extOpcode;   /* opcode for pex extension */
    unsigned short      fpFormat;    /* floating point format */
    char		fpConvert;   /* flag for floating point conversion */
    PEXEnumTypeDesc	*fpSupport;  /* float formats supported by server */
    int			fpCount;     /* number of float formats supported */
    XID       		lastResID;   /* renderer/structure ID of last OC */
    int                 lastReqType; /* request type (store/rend) of last OC */
    int			lastReqNum;  /* request number of last OC */
    struct PEXDisplayInfo *next;     /* next in list */
} PEXDisplayInfo;


/*
 * Pointer to head of list is defined externally.
 */

extern PEXDisplayInfo *PEXDisplayInfoHeader;


/*
 * Insert a new record in the beginning of the linked list.
 */

#define PEXAddDisplayInfo(_display, _info) \
\
{ \
    _info->display = _display; \
\
    _info->next = PEXDisplayInfoHeader; \
    PEXDisplayInfoHeader = _info; \
}


/*
 * Remove the record assosicated with '_display' from the linked list
 * and return a pointer to it in '_info'.
 */

#define PEXRemoveDisplayInfo(_display, _info) \
\
{ \
    PEXDisplayInfo	*prev = NULL; \
\
    _info = PEXDisplayInfoHeader; \
\
    while (_info && _info->display != _display) \
    { \
	prev = _info; \
	_info = _info->next; \
    } \
\
    if (_info) \
	if (!prev) \
	    PEXDisplayInfoHeader = _info->next; \
	else \
	    prev->next = _info->next; \
}	


/*
 * Return the info assosicated with '_display' in '_info'.
 * If the info is not the first in the list, move it to the front.
 */

#define PEXGetDisplayInfo(_display, _info) \
\
{ \
    _info = PEXDisplayInfoHeader; \
\
    if (PEXDisplayInfoHeader->display != _display) \
    { \
	PEXDisplayInfo	*prev = PEXDisplayInfoHeader; \
\
	_info = _info->next; \
	while (_info && _info->display != _display) \
	{ \
	    prev = _info; \
	    _info = _info->next; \
	} \
\
	if (_info) \
	{ \
	    prev->next = _info->next; \
	    _info->next = PEXDisplayInfoHeader; \
	    PEXDisplayInfoHeader = _info; \
	} \
    } \
}



/* -------------------------------------------------------------------------
 * Memory related macros.
 * ------------------------------------------------------------------------- */

#define PEXAllocBuf(size)          Xmalloc(size)
#define PEXFreeBuf(ptr)            Xfree(ptr)
#define PEXReallocBuf(ptr, size)   Xrealloc(ptr, size)

#define COPY_AREA(_from, _to, _size) \
    bcopy (_from, _to, _size)

#define COPY_SMALL_AREA(_from, _to, _size) \
{ \
    register char *_f = (char *) (_from), *_t = (char *) (_to); \
    register int _c = (_size); \
    while (--_c >= 0) *_t++ = *_f++; \
}

#define PAD(_size) (3 - (((_size) + 3) & 0x3))

#define PADDED_BYTES(_bytes) (_bytes + PAD (_bytes))

#define NUMWORDS(_size) (((unsigned int)((_size) + 3)) >> 2)

#define NUMBYTES(_len) (((unsigned int)(_len)) << 2)

#define LENOF(_ctype) (sizeof (_ctype) >> 2)


/* 
 * Count the number of ones in a longword.
 */

#define CountOnes(mask, countReturn) \
    countReturn = ((mask) - (((mask)>>1)&0x77777777) \
	- (((mask)>>2)&0x33333333) - (((mask)>>3)&0x11111111)); \
    countReturn = ((((countReturn)+((countReturn)>>4)) & 0x0F0F0F0F) % 255)



/* -------------------------------------------------------------------------
 * Macros for dealing with the transport buffer. 
 * ------------------------------------------------------------------------- */

/*
 * The maximum protocol request size.
 */

#define MAX_REQUEST_SIZE ((1<<16) - 1)


/*
 * Has the X transport buffer been flushed?
 */

#define XBufferFlushed(_display) \
    ((_display)->buffer == (_display)->bufptr)


/*
 * The number of bytes left in the X transport buffer.
 */

#define BytesLeftInXBuffer(_display) \
    ((_display)->bufmax - (_display)->bufptr)


/*
 * The number of words left in the X transport buffer.
 */

#define WordsLeftInXBuffer(_display) \
    (((_display)->bufmax - (_display)->bufptr) >> 2)


/*
 * Setup the OC element info header.
 */

#define STORE_ELEMENT_INFO(_reqPtr,_ocType,_ocLength) \
{ \
    ((pexElementInfo *)(_reqPtr))->elementType = (_ocType); \
    ((pexElementInfo *)(_reqPtr))->length = (_ocLength); \
}


/* 
 * PEXGetReq sets up a request to be sent to the X server.  If there isn't
 * enough room left in the X buffer, it is flushed before the new request
 * is started.
 *
 * PEXGetFPReq is similar to PEXGetReq, except that it sets up a request
 * that contains floating point values.  A return flag indicates whether
 * or not the client native floating point format has to be converted to
 * a server supported format.
 *
 * SETUP_REQ is a macro containing common code for PEXGetReq and PEXGetFPReq.
 */

#if __STDC__ && !defined(UNIXCPP)

#define SETUP_REQ(_name, _req) \
    PEXDisplayInfo *pexDisplayInfo; \
    PEXGetDisplayInfo (display, pexDisplayInfo); \
    if ((display->bufptr + sizeof (pex##_name##Req)) > display->bufmax) \
        _XFlush (display); \
    _req = (pex##_name##Req *) (display->last_req = display->bufptr); \
    _req->reqType = pexDisplayInfo->extOpcode; \
    _req->opcode = PEXRC##_name; \
    _req->length = (sizeof (pex##_name##Req)) >> 2; \
    display->bufptr += sizeof (pex##_name##Req); \
    display->request++

#else /* non-ANSI C uses empty comment instead of "##" for token concat */

#define SETUP_REQ(_name, _req) \
    PEXDisplayInfo *pexDisplayInfo; \
    PEXGetDisplayInfo (display, pexDisplayInfo); \
    if ((display->bufptr + sizeof (pex/**/_name/**/Req)) > display->bufmax) \
        _XFlush (display); \
    _req = (pex/**/_name/**/Req *) (display->last_req = display->bufptr); \
    _req->reqType = pexDisplayInfo->extOpcode; \
    _req->opcode = PEXRC/**/_name; \
    _req->length = (sizeof (pex/**/_name/**/Req)) >> 2; \
    display->bufptr += sizeof (pex/**/_name/**/Req); \
    display->request++
#endif

#define PEXGetReq(_name, _req)\
{ \
    SETUP_REQ (_name, _req); \
}

#define PEXGetFPReq(_name, _req, _fpConvert)\
{ \
    SETUP_REQ (_name, _req); \
    _req->fpFormat = pexDisplayInfo->fpFormat; \
    _fpConvert = pexDisplayInfo->fpConvert; \
}


/* 
 * PEXGetReqExtra and PEXGetFPReqExtra are the same as PEXGetReq and
 * PEXGetFPReq, except that an additional "n" bytes are allocated after
 * the request.  "n" will be padded to a word boundary.
 */

#if __STDC__ && !defined(UNIXCPP)

#define SETUP_REQ_EXTRA(_name, _n, _req) \
    PEXDisplayInfo *pexDisplayInfo; \
    PEXGetDisplayInfo (display, pexDisplayInfo); \
    if ((display->bufptr + sizeof (pex##_name##Req) + PADDED_BYTES (_n)) >\
        display->bufmax) \
        _XFlush (display); \
    _req = (pex##_name##Req *) (display->last_req = display->bufptr); \
    _req->reqType = pexDisplayInfo->extOpcode; \
    _req->opcode = PEXRC##_name; \
    _req->length = (sizeof(pex##_name##Req) + PADDED_BYTES (_n)) >> 2; \
    display->bufptr += sizeof (pex##_name##Req) + PADDED_BYTES (_n); \
    display->request++

#else /* non-ANSI C uses empty comment instead of "##" for token concat */

#define SETUP_REQ_EXTRA(_name, _n, _req) \
    PEXDisplayInfo *pexDisplayInfo; \
    PEXGetDisplayInfo (display, pexDisplayInfo); \
    if ((display->bufptr + sizeof (pex/**/_name/**/Req) + PADDED_BYTES (_n)) >\
        display->bufmax) \
        _XFlush (display); \
    _req = (pex/**/_name/**/Req *) (display->last_req = display->bufptr); \
    _req->reqType = pexDisplayInfo->extOpcode; \
    _req->opcode = PEXRC/**/_name; \
    _req->length = (sizeof(pex/**/_name/**/Req) + PADDED_BYTES (_n)) >> 2; \
    display->bufptr += sizeof (pex/**/_name/**/Req) + PADDED_BYTES (_n); \
    display->request++
#endif

#define PEXGetReqExtra(_name, _n, _req) \
{ \
    SETUP_REQ_EXTRA (_name, _n, _req); \
}

#define PEXGetFPReqExtra(_name, _n, _req, _fpConvert) \
{ \
    SETUP_REQ_EXTRA (_name, _n, _req); \
    _req->fpFormat = pexDisplayInfo->fpFormat; \
    _fpConvert = pexDisplayInfo->fpConvert; \
}


/*
 * PEXGetOCReq is similiar to PEXGetReq except that it does not update
 * display->bufptr.  This is used when writing ocs into the transport buffer.
 */

#define PEXGetOCReq(_display, _nBytes) \
{ \
    if ((_display)->bufptr + (_nBytes) > (_display)->bufmax) \
        _XFlush (_display); \
    (_display)->last_req = (_display)->bufptr; \
    (_display)->request++; \
}


/* 
 * See if XSynchronize has been called.  If so, send request right away.
 */

#define PEXSyncHandle(_display)\
    if ((_display)->synchandler) (*(_display)->synchandler) (_display)



/* -------------------------------------------------------------------------
 * Color related macros.
 * ------------------------------------------------------------------------- */

/* 
 * Return the size of the color in bytes by looking at the color type.  
 * Note that the size of an indexed color is pre-padded to a word boundary.
 */

#define GetColorSize(_type) \
    ((_type) == PEXColorTypeIndexed ? (sizeof (pexTableIndex) * 2) : \
    ((_type) == PEXColorTypeRGB8 ? sizeof (pexRgb8Color) : \
    ((_type) == PEXColorTypeRGB16 ? sizeof (pexRgb16Color) : \
	sizeof (pexRgbFloatColor))))

/*
 * Return the number of words in a color.  Note that all the PEX color
 * types are padded to end on a word boundary
 */

#define GetColorLength(_type_)\
    ((_type_) == PEXColorTypeIndexed ?  LENOF( PEXColorIndexed) :\
    ((_type_) == PEXColorTypeRGB8 ?  LENOF( PEXColorRGB8) :\
    ((_type_) == PEXColorTypeRGB16 ? LENOF( PEXColorRGB16) : \
					LENOF( PEXColorRGB) )))

/* 
 * How big, relative to the largest color specifier, is the color?
 * The users of this macro must subtract this value from the sizeof value.
 */

#define AdjustSizeFromType(_type) \
    (sizeof (pexColor) - GetColorSize (_type))


/* 
 * Initialize a color specifier.  'dst' is of type PEXColorSpecifier,
 * and 'src' is of type PEXColor.
 */

#define InitializeColorSpecifier(_dst, _src, _type)\
    (_dst).type = _type; \
    COPY_SMALL_AREA ((_src), &((_dst).value), GetColorSize (_type));


/*
 * PackColorSpecifier is similar to InitalizeColorSpecifier, except that
 * the destination is a pointer to memory, rather than a static structure.
 */

#define PackColorSpecifier(srcBuf, dstBuf, sizeColor) \
{ \
    ((PEXColorSpecifier *) (dstBuf))->type = \
        ((PEXColorSpecifier *) (srcBuf))->type; \
    sizeColor = \
        GetColorSize (((PEXColorSpecifier *) (srcBuf))->type); \
    COPY_SMALL_AREA (&(((PEXColorSpecifier *) (srcBuf))->value), \
        &(((PEXColorSpecifier *) (dstBuf))->value), sizeColor); \
}



/* -------------------------------------------------------------------------
 * Macros to compute the number of words in a facet/vertex with data.
 * ------------------------------------------------------------------------- */

/*
 * Compute the number of words in the facet data
 */

#define GetFacetDataLength(_fattribs, _lenofColor) \
    (((_fattribs & PEXGAColor) ? _lenofColor : 0) + \
    ((_fattribs & PEXGANormal) ? LENOF(pexVector3D) : 0))


/*
 * Compute the number of words in a vertex with optional colors and normals
 */

#define GetVertexWithDataLength(_vattribs, _lenofColor) \
    (LENOF (pexCoord3D) + \
    ((_vattribs & PEXGAColor) ? _lenofColor : 0) + \
    ((_vattribs & PEXGANormal) ? LENOF (pexVector3D) : 0))



/* -------------------------------------------------------------------------
 * Data structures useful for packing protocol data.
 * ------------------------------------------------------------------------- */

typedef struct
{
    unsigned long       attribute;
    unsigned char	value;
    unsigned char	reserved[3];
} PEXASFData;


typedef struct {
    short 		fp_format;
    short		reserved;
    unsigned long	renderer;
    PEXColorSpecifier	echo_color;
} PEXEchoColorData;



/* -------------------------------------------------------------------------
 * Miscellaneous.
 * ------------------------------------------------------------------------- */

/*
 * IEEE-754-32 is the most common floating point type.  Vendors who have
 * a different native floating point format should define NATIVE_FP_FORMAT
 * at compile time via the -D switch (this is done by modifying the vendors
 * config file.
 */

#ifndef NATIVE_FP_FORMAT
#define NATIVE_FP_FORMAT PEXIEEE_754_32
#endif


/*
 * INPUT and OUTPUT are defined to make looking at function arguments easier.
 */

#define INPUT  
#define OUTPUT  
#define INOUT


/*
 * Pick path cache.
 */

#define MAX_PICK_CACHE_SIZE 2048

extern PEXPickPath	*PEXPickCache;
extern unsigned int	PEXPickCacheSize;
extern int		PEXPickCacheInUse;


/*
 * _XAllocScratch is defined in Xlib.
 */

extern char *_XAllocScratch();


#endif /* PEXLIBINT_H */
