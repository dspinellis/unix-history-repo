/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gsmisc.c */
/* Miscellaneous utilities for Ghostscript library */
#include "gx.h"
#include "malloc_.h"
#include "memory_.h"

/* Ghostscript writes to gs_out instead of stdout, */
/* and writes debugging output to gs_debug_out. */
FILE *gs_out;
/* We define gs_debug and gs_debug_out even if DEBUG isn't defined, */
/* so that we can compile individual modules with DEBUG set. */
char gs_debug[128];
FILE *gs_debug_out;
/* We define gs_log_errors here for the same reason. */
int gs_log_errors = 0;

/* We can turn on allocator debugging even if DEBUG isn't defined. */
int gs_alloc_debug = 0;
byte gs_alloc_fill_alloc = 0xa1;
byte gs_alloc_fill_free = 0xf1;

/* Generate a block of unique IDs. */
static ulong gs_next_id = 0;
ulong
gs_next_ids(uint count)
{	ulong id;
	if ( gs_next_id == 0 ) gs_next_id++;
	id = gs_next_id;
	gs_next_id += count;
	return id;
}

/* Versions of malloc and free compatible with Ghostscript's */
/* model of memory management.  We keep track of all allocated */
/* blocks so we can free them at cleanup time. */
/* We must make sure that malloc_blocks leave the block aligned. */
typedef struct malloc_block_s malloc_block;
#define malloc_block_data\
	malloc_block *next;\
	uint size;\
	const char *cname
struct malloc_block_data_s { malloc_block_data; };
struct malloc_block_s {
	malloc_block_data;
	byte _pad[-sizeof(struct malloc_block_data_s) & 7];	/* pad to double */
};
private malloc_block *malloc_list = 0;
char *
gs_malloc(uint num_elts, uint elt_size, const char *client_name)
{	char *ptr;
	const char *msg = "";
	if ( num_elts > (max_uint - sizeof(malloc_block)) / elt_size )
	   {	/* Can't represent the size in a uint! */
		msg = "too large for size_t";
		ptr = 0;
	   }
	else
	   {	uint size = num_elts * elt_size;
		ptr = malloc(size + sizeof(malloc_block));
		if ( ptr == 0 )
			msg = "failed";
		else
		   {	malloc_block *bp = (malloc_block *)ptr;
			bp->next = malloc_list;
			bp->size = size;
			bp->cname = client_name;
			malloc_list = bp;
			msg = "OK";
			ptr = (char *)(bp + 1);
			if ( gs_alloc_debug )
			  { /* Clear the block in an attempt to track down */
			    /* uninitialized data errors. */
			    memset(ptr, gs_alloc_fill_alloc, size);
			  }
		   }
	   }
	if ( gs_alloc_debug || !*msg )
		dprintf5("gs_malloc(%s)(%u, %u) = 0x%lx: %s\n", client_name,
			 num_elts, elt_size, (ulong)ptr, msg);
	return ptr;
}
void
gs_free(char *ptr, uint num_elts, uint elt_size, const char *client_name)
{	malloc_block *bp = malloc_list;
	if ( gs_alloc_debug )
		dprintf4("gs_free(%s)(0x%lx, %u, %u)\n", client_name,
			 (ulong)ptr, num_elts, elt_size);
	if ( ptr == (char *)(bp + 1) )
	  {
#ifdef DEBUG
	    if ( bp->size != num_elts * elt_size )
	      lprintf5("%s: free 0x%lx(%u,%u) size ~= %u\n",
		       client_name, (ulong)ptr, num_elts, elt_size,
		       bp->size);
#endif
	    malloc_list = bp->next;
	    if ( gs_alloc_debug )
	      memset((char *)(bp + 1), gs_alloc_fill_free, bp->size);
	    free(bp);
	  }
	else
	  { malloc_block *np;
	    for ( ; (np = bp->next) != 0; bp = np )
	      { if ( ptr == (char *)(np + 1) )
		  {
#ifdef DEBUG
		    if ( np->size != num_elts * elt_size )
		      lprintf5("%s: free 0x%lx(%u,%u) size ~= %u\n",
			       client_name, (ulong)ptr, num_elts, elt_size,
			       np->size);
#endif
		    bp->next = np->next;
		    if ( gs_alloc_debug )
		      memset((char *)(np + 1), gs_alloc_fill_free, np->size);
		    free(np);
		    return;
		  }
	      }
	    lprintf4("%s: free 0x%lx(%u,%u) not found\n",
		     client_name, (ulong)ptr, num_elts, elt_size);
	    free((char *)((malloc_block *)ptr - 1));
	  }
}
void
gs_malloc_release()
{	malloc_block *bp = malloc_list;
	malloc_block *np;
	for ( ; bp != 0; bp = np )
	   {	np = bp->next;
		if ( gs_alloc_debug )
		  memset((char *)(bp + 1), gs_alloc_fill_free, bp->size);
		free(bp);
	   }
	malloc_list = 0;
}

/* Swap even and odd bytes. */
/* This routine may be supplanted by assembly code. */
#if !USE_ASM
#undef memswab				/* see memory_.h */
void
memswab(const char *src, char *dest, int count)
{	register const uint *sptr = (const uint *)src;
	register uint *dptr = (uint *)dest;
	register int x;
	register uint w;
	for ( x = count >> 3; --x >= 0; )
	   {	w = *sptr;
#if arch_ints_are_short
		*dptr = (w >> 8) + (w << 8);
		w = sptr[1];
		dptr[1] = (w >> 8) + (w << 8);
		w = sptr[2];
		dptr[2] = (w >> 8) + (w << 8);
		w = sptr[3];
		dptr[3] = (w >> 8) + (w << 8);
		sptr += 4, dptr += 4;
#else
		*dptr = ((w << 8) & 0xff00ff00) + ((w >> 8) & 0x00ff00ff);
		w = sptr[1];
		dptr[1] = ((w << 8) & 0xff00ff00) + ((w >> 8) & 0x00ff00ff);
		sptr += 2, dptr += 2;
#endif
	   }
	switch ( count & 6 )
	   {
	case 6:
		w = ((const ushort *)sptr)[2];
		((ushort *)dptr)[2] = (w >> 8) + (w << 8);
	case 4:
		w = ((const ushort *)sptr)[1];
		((ushort *)dptr)[1] = (w >> 8) + (w << 8);
	case 2:
		w = ((const ushort *)sptr)[0];
		((ushort *)dptr)[0] = (w >> 8) + (w << 8);
	case 0:
		;
	   }
}
#endif					/* !USE_ASM */

/* Transpose an 8 x 8 block of bits.  line_size is the raster of */
/* the input data.  dist is the distance between output bytes. */
/* This routine may be supplanted by assembly code. */
#if !USE_ASM

#if 1		/* This is the better of the two algorithms. */

void
memflip8x8(const byte *inp, int line_size, byte *outp, int dist)
{	register uint ae, bf, cg, dh;
	   {	const byte *ptr4 = inp + (line_size << 2);
		ae = ((uint)*inp << 8) + *ptr4;
		inp += line_size, ptr4 += line_size;
		bf = ((uint)*inp << 8) + *ptr4;
		inp += line_size, ptr4 += line_size;
		cg = ((uint)*inp << 8) + *ptr4;
		inp += line_size, ptr4 += line_size;
		dh = ((uint)*inp << 8) + *ptr4;
	   }

	/* Check for all 8 bytes being the same. */
	/* This is especially worth doing for the case where all are zero. */
	if ( ae == bf && ae == cg && ae == dh && (ae >> 8) == (ae & 0xff) )
	   {	if ( ae == 0 ) goto store;
		*outp = -((ae >> 7) & 1);
		outp += dist;
		*outp = -((ae >> 6) & 1);
		outp += dist;
		*outp = -((ae >> 5) & 1);
		outp += dist;
		*outp = -((ae >> 4) & 1);
		outp += dist;
		*outp = -((ae >> 3) & 1);
		outp += dist;
		*outp = -((ae >> 2) & 1);
		outp += dist;
		*outp = -((ae >> 1) & 1);
		outp += dist;
		*outp = -(ae & 1);
		return;
	   }

	   {	register uint temp;

/* Transpose a block of bits between registers. */
#define transpose(r,s,mask,shift)\
  r ^= (temp = ((s >> shift) ^ r) & mask);\
  s ^= temp << shift

/* Transpose blocks of 4 x 4 */
#define transpose4(r) transpose(r,r,0x00f0,4)
	transpose4(ae);
	transpose4(bf);
	transpose4(cg);
	transpose4(dh);

/* Transpose blocks of 2 x 2 */
	transpose(ae, cg, 0x3333, 2);
	transpose(bf, dh, 0x3333, 2);

/* Transpose blocks of 1 x 1 */
	transpose(ae, bf, 0x5555, 1);
	transpose(cg, dh, 0x5555, 1);

	   }

store:	*outp = ae >> 8;
	outp += dist;
	*outp = bf >> 8;
	outp += dist;
	*outp = cg >> 8;
	outp += dist;
	*outp = dh >> 8;
	outp += dist;
	*outp = (byte)ae;
	outp += dist;
	*outp = (byte)bf;
	outp += dist;
	*outp = (byte)cg;
	outp += dist;
	*outp = (byte)dh;
}

#else		/* This looked like a good idea, but it's no faster. */

/* Transpose an 8 x 8 block of bits.  line_size is the raster of */
/* the input data.  dist is the distance between output bytes. */
void
memflip8x8(const byte *inp, int line_size, byte *outp, int dist)
{	/* Define a table that spreads the bits of its index as follows: */
	/* 0->0-3, 1->8-11, 2->16-19, 3->24-27, */
	/* 4->4-7, 5->12-15, 6->20-23, 7->28-31. */
#define b4(v) v,v+0xf,v+0xf00,v+0xf0f
#define b8(v) b4(v),b4(v+0xf0000)
#define b16(v) b8(v),b8(v+0xf000000)
	static const ulong spread[256] =
	 { b16(0), b16(0xf0), b16(0xf000), b16(0xf0f0),
	   b16(0xf00000), b16(0xf000f0), b16(0xf0f000), b16(0xf0f0f0),
	   b16(0xf0000000), b16(0xf00000f0), b16(0xf000f000), b16(0xf000f0f0),
	   b16(0xf0f00000), b16(0xf0f000f0), b16(0xf0f0f000), b16(0xf0f0f0f0)
	 };
	register ulong hi, lo, temp;
	hi = spread[*inp] & 0x88888888;
	inp += line_size;
	hi |= spread[*inp] & 0x44444444;
	inp += line_size;
	hi |= spread[*inp] & 0x22222222;
	inp += line_size;
	hi |= spread[*inp] & 0x11111111;
	inp += line_size;
	lo = spread[*inp] & 0x88888888;
	inp += line_size;
	lo |= spread[*inp] & 0x44444444;
	inp += line_size;
	lo |= spread[*inp] & 0x22222222;
	inp += line_size;
	lo |= spread[*inp] & 0x11111111;
	temp = (hi & 0xf0f0f0f0) | ((lo >> 4) & 0x0f0f0f0f);
	*outp = (byte)((uint)(temp >> 16) >> 8); outp += dist;
	*outp = (byte)(temp >> 16); outp += dist;
	*outp = (byte)((uint)temp >> 8); outp += dist;
	*outp = (byte)(temp); outp += dist;
	temp = ((hi << 4) & 0xf0f0f0f0) | (lo & 0x0f0f0f0f);
	*outp = (byte)((uint)(temp >> 16) >> 8); outp += dist;
	*outp = (byte)(temp >> 16); outp += dist;
	*outp = (byte)((uint)temp >> 8); outp += dist;
	*outp = (byte)(temp);
}

#endif		/* memflip8x8 */

#endif					/* !USE_ASM */
