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
 *  file: hpFonts.c
 *
 *  Font optimization/removal and text output routines
 *
 *		Hewlett Packard -- Corvallis Workstation Operation
 *		Project -- port of X11 to HP9000
 *		Harry Phinney -- MTS
 *
 *
 */

#include "Xmd.h"
#include "Xproto.h"
#include "misc.h"
#include "dixfontstr.h"
#include "fontstruct.h"
#include "scrnintstr.h"
#include "hpOScrnBuf.h"
#include "hpFonts.h"

#include "hppriv.h"

extern char *NameForAtom();	/* in dix/atom.c */

static int hpAllocFontMem();
static void hpStoreFont();

static int
lookUpProperty(pFont, pName)
     register FontRec *pFont;
     register char *pName;
{
    register int n = FONTINFONPROPS(pFont);
    register FontPropPtr pFP = FONTPROPS(pFont);

    for (;n--; pFP++)
    {
	register char *pPropString;
	if((pPropString = NameForAtom(pFP->name)) && !strcmp(pPropString, pName))
	    return (int) pFP->value;
    }
    return 0;
}


/************************************************************************
 *  Routine:	hpRealizeFont
 *		Optimize the font i.e. store it in offscreen memory
 *		This implementation (like our X10) only stores
 *		the characters defined by STARTCHAR thru LASTCHAR in
 *		offscreen.  It breaks them into offscreen chunks
 *		of CHARSPERCHUNK size
 *
 *  Inputs: pScreen points to the ScreenRec we'll try to optimize for
 *	    pFont points to the font we'll store in offscreen
 *  
 *  Returns: TRUE (always)
 *
 *  Side Effects:  sets pFont->devPriv[pScreen->myNum] to point to an
 *		hpFontRec which identifies the location of the font
 *		in offscreen.
 *
 */


Bool
hpRealizeFont(pScreen, pFont)
  ScreenPtr pScreen;
  FontRec *pFont;
{
#if 0
    int index = pScreen->myNum;
    CharInfoPtr pCI = pFont->pCI;
    unsigned int chDefault =  FONTDEFAULTCH(pFont);
    unsigned int firstCol = FONTFIRSTCOL(pFont);
    unsigned int lastCol = FONTLASTCOL(pFont);
    int glyphWidth = FONTMAXBOUNDS(pFont,rightSideBearing) -
	FONTMINBOUNDS(pFont,leftSideBearing);
    int glyphHeight = FONTMAXBOUNDS(pFont,ascent) +
	FONTMAXBOUNDS(pFont,descent);
    hpFontRec *pHpFrec;
    hpCharRange *pRange;
    int i, prop, startChar, lastChar;

    /*
     * if it's right-to-left, or if the
     * glyphs are bigger than 24 pixels wide,
     * then don't optimize it.
     */
    if (pFont->info.drawDirection || (glyphWidth > 24))
    {
	pFont->devPriv[index] = (pointer) NULL;
	return TRUE;
    }

    /*
     * allocate an hpFontRec
     */
    pHpFrec = (hpFontRec *) xalloc(sizeof (hpFontRec));
    if (!pHpFrec)
    {
	pFont->devPriv[index] = (pointer) NULL;
	return TRUE;
    }

    /*
     * allocate the space for stippling a character
     */
    if (!(pHpFrec->stippleChunk = hpBufAlloc(pScreen, glyphWidth, glyphHeight)))
    {
	xfree(pHpFrec);
	pFont->devPriv[index] = (pointer) NULL;
	return TRUE;
    }
  
    /*
     * Check to see if the font has properties telling which characters
     * to optimize
     */
    if (prop = lookUpProperty(pFont, "HPSTARTOPTIMIZE"))
	startChar = prop;
    else
	startChar = STARTCHAR;
    if (prop = lookUpProperty(pFont, "HPENDOPTIMIZE"))
	lastChar = prop;
    else
	lastChar = LASTCHAR;

    /*
     * allocate the offscreen, and put the chunk info in the hpFontRec
     */
    if (!hpAllocFontMem(pScreen, pFont, pHpFrec, startChar, 
			lastChar, CHARSPERCHUNK))
    {
	hpBufFree(pScreen, pHpFrec->stippleChunk);
	xfree(pHpFrec);
	pFont->devPriv[index] = (pointer) NULL;
	return TRUE;
    }

    /*
     * attach the hpFontRec to the devPriv field in the Font
     */
    pFont->devPriv[index] = (pointer) pHpFrec;

    pRange = pHpFrec->pRange;

    /*
     * store the font in the offscreen memory
     */
    for (i = 0; i < pHpFrec->NumChunks; i++)
    {
	hpStoreFont(pScreen, pFont, i, startChar + (i * CHARSPERCHUNK), 
		    CHARSPERCHUNK);
	pRange[i].startChar = startChar + (i * CHARSPERCHUNK);
	pRange[i].endChar = pRange[i].startChar + CHARSPERCHUNK - 1;
    }
    pHpFrec->maxWidth = glyphWidth;
    pHpFrec->maxHeight = glyphHeight;
    pHpFrec->firstChar = startChar;
    pHpFrec->lastChar = lastChar;

    /*
     * check for the existence of the default glyph.  If it exists, then
     * hpStoreFont will have put it in place of any nonexistent glyphs.
     * If it doesn't exist, then the text output routines have to skip
     * any non-existent character.
     */
    pHpFrec->fDefaultExists = !((chDefault < firstCol) || 
				(chDefault > lastCol) ||
				!pCI[chDefault - firstCol].exists);

#endif
    return TRUE;
}


/************************************************************************
 *  Routine:	hpUnrealizeFont
 *		If the font had been stored in offscreen memory
 *		then free all the associated memory.
 *
 *  Inputs: pScreen points to the ScreenRec the font was realized for
 *	    pFont points to the font we're unrealizing
 *  
 *  Returns: nothing of importance
 *
 *  Side Effects:  none
 *
 */

Bool
hpUnrealizeFont(pScreen, pFont)
     register ScreenPtr pScreen;
     register FontRec *pFont;
{
#if 0
    int index = pScreen->myNum;
    register hpFontRec *pHpFrec;
    register int i;

    /*
     * test to see if we ever optimized this font
     */
    if (! pFont->devPriv[index])
	return TRUE; 
    
    pHpFrec = (hpFontRec *) pFont->devPriv[index];

    /*
     * mark the font as unoptimized
     */
    pFont->devPriv[index] = (pointer) NULL;

    /*
     * free the offscreen chunks
     */
    for (i = 0; i < pHpFrec->NumChunks; i++)
	hpBufFree(pScreen, pHpFrec->ppChunk[i]);
    
    /*
     * free the stippleChunk
     */
    hpBufFree(pScreen, pHpFrec->stippleChunk);

    /*
     * free the chunk table
     */
    xfree(pHpFrec->ppChunk);

    /*
     * free the range array
     */
    xfree(pHpFrec->pRange);

    /*
     * finally, free the hpFontRec itself
     */
    xfree(pHpFrec);
#endif
    return TRUE;
}


/************************************************************************
 *  Routine:	hpAllocFontMem
 *		Get enough offscreen memory on the screen to optomize
 *		the font and store the locations in an hpFontRec
 *
 *  Inputs: pScreen points to the ScreenRec we'll get the memory on 
 *	    pFont points to the font we're allocating for
 *	    phpFontRec we fill out telling where the memory is alloc'd
 *	    first and last specify the first and last glyphs to allocate for
 *	    size specifies the size of each chunk in glyphs
 *  
 *  Returns: 1 for success, 0 for not enough offscreen memory
 *
 *  Side Effects:  none
 *
 */

static int
hpAllocFontMem(pScn, pFnt, pHpFrec, first, last, size)
     ScreenPtr pScn;
     FontRec *pFnt;
     hpFontRec *pHpFrec;
     int first, last, size;
{
    int i;
    int width, height; /* width & height of each chunk */
    int total = last - first + 1; /* total num chars to allocate for */
    int num_chunks = total/size;

    if (total%size) num_chunks++;

    pHpFrec->ppChunk = (hpChunk **) xalloc(sizeof(hpChunk *) * num_chunks);
    if (!(pHpFrec->ppChunk))
	return 0;  
    pHpFrec->NumChunks = num_chunks;

    pHpFrec->pRange = (hpCharRange *) xalloc(sizeof(hpCharRange) * num_chunks);
    if (!(pHpFrec->pRange))
	return 0;  

    /*
     * figure out how much memory we need
     * allocates enough memory to contain "total"
     * number of maximum-sized glyphs
     */
    width = (FONTMAXBOUNDS(pFnt,rightSideBearing) -
	     FONTMINBOUNDS(pFnt,leftSideBearing)) * size;
  
    height = FONTMAXBOUNDS(pFnt,ascent) + FONTMAXBOUNDS(pFnt,descent);

    for (i = 0; i < num_chunks; i++)
    {

	/*
	 * get offscreen memory
	 */
	if ((pHpFrec->ppChunk[i] = hpBufAlloc(pScn, width, height)) == NULL)
	{
	    /*
	     * if it fails, we've gotta free up all the data structures
	     */
	    while(i)
		hpBufFree(pScn, pHpFrec->ppChunk[--i]);
	    pHpFrec->NumChunks = 0;
	    xfree(pHpFrec->ppChunk);
	    pHpFrec->ppChunk = (hpChunk **) NULL;
	    xfree(pHpFrec->pRange);
	    pHpFrec->pRange = (hpCharRange *) NULL;
	    return 0;
	}
    }
    return 1;
}


/************************************************************************
 *  Routine:	hpStoreFont
 *		Write font glyphs into previously allocated offscreen memory
 *		Stores a contiguous range of glyphs e.g. 32-63
 *		Calls hpWholeGlyph (LIE) to put the bits in the framebuffer
 *		with no clipping
 *
 *  Inputs: pScreen points to the ScreenRec whose memory we write to
 *	    pFont points to the font we'll store in offscreen
 *	    index gets us to the chunk to store the glyphs in
 *	    start and num define the range of glyphs to store
 *  
 *  Returns: nothing of importance
 *
 *  Side Effects:  none
 *
 */

static void
hpStoreFont(pScn, pFnt, chunkNum, startGlyph, numGlyphs)
     ScreenPtr pScn;
     register FontRec *pFnt;
     int chunkNum; /* index into array of chunks */
     int startGlyph;
     register numGlyphs;
{
#if 0
    int glyphNum = startGlyph;
    char *pCurrentGlyph;
    register CharInfoPtr pCi;
    int cellWidth = FONTMAXBOUNDS(pFnt,rightSideBearing) -
	FONTMINBOUNDS(pFnt,leftSideBearing);
    int defaultChar = FONTDEFAULTCH(pFnt);
    hpChunk *pChunk = ((hpFontRec *)pFnt->devPriv[pScn->myNum])->
	ppChunk[chunkNum];
    hpPrivScreenPtr pPrivScn = getPrivScreenPtr(pScn);
    register x;
    int y;
    unsigned int firstCol = FONTFIRSTCOL(pFnt);
    unsigned int lastCol = FONTLASTCOL(pFnt);
    CharInfoPtr pDefaultCi = (CharInfoPtr) &(pFnt->pCI[defaultChar - firstCol]);
    char *pDefaultGlyph = FONTGLYPHS(pFnt) + pDefaultCi->byteOffset;
    int fDefaultExists = !((defaultChar < firstCol) ||
			   (defaultChar > lastCol) ||
			   !pDefaultCi->exists);

    x = pChunk->x;
    y = pChunk->y;

    while (numGlyphs--)
    {
	if (glyphNum > lastCol) break;

	pCi = &(pFnt->pCI[glyphNum - firstCol]);
	if ((glyphNum >= firstCol) && pCi->exists)
	{
	    pCurrentGlyph = FONTGLYPHS(pFnt) + pCi->byteOffset;
	    (*pPrivScn->WholeGlyph)(pScn, pCurrentGlyph, pCi, x, y,
				    GXcopy, ~0, 0, pPrivScn->planesMask);
	}
	else
	{
	    if (fDefaultExists)
		(*pPrivScn->WholeGlyph)(pScn, pDefaultGlyph, pDefaultCi, x, y,
					GXcopy, ~0, 0, pPrivScn->planesMask);
	}
	glyphNum++;
	x += cellWidth;
	pCi++;
    }
#endif
}
