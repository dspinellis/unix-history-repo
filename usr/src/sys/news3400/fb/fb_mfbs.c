/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: fb_mfbs.c,v 4.300 91/06/27 20:42:43 root Rel41 $ SONY
 *
 *	@(#)fb_mfbs.c	7.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <news3400/iop/framebuf.h>
#include <news3400/iop/fbreg.h>
#include <news3400/fb/fbdefs.h>

u_short mfbstarttab16[16] =
    {
	 ( 0x0000 ), ( 0x7FFF ), ( 0x3FFF ), ( 0x1FFF ),
	 ( 0x0FFF ), ( 0x07FF ), ( 0x03FF ), ( 0x01FF ),
	 ( 0x00FF ), ( 0x007F ), ( 0x003F ), ( 0x001F ),
	 ( 0x000F ), ( 0x0007 ), ( 0x0003 ), ( 0x0001 ),
    };
u_short mfbendtab16[16] =
    {
	 ( 0x0000 ), ( 0x8000 ), ( 0xC000 ), ( 0xE000 ),
	 ( 0xF000 ), ( 0xF800 ), ( 0xFC00 ), ( 0xFE00 ),
	 ( 0xFF00 ), ( 0xFF80 ), ( 0xFFC0 ), ( 0xFFE0 ),
	 ( 0xFFF0 ), ( 0xFFF8 ), ( 0xFFFC ), ( 0xFFFE ),
    };

u_short mfbpartmasks16[16][16] = {
     { ( 0xFFFF ),  ( 0x8000 ),  ( 0xC000 ),  ( 0xE000 ),
       ( 0xF000 ),  ( 0xF800 ),  ( 0xFC00 ),  ( 0xFE00 ),
       ( 0xFF00 ),  ( 0xFF80 ),  ( 0xFFC0 ),  ( 0xFFE0 ),
       ( 0xFFF0 ),  ( 0xFFF8 ),  ( 0xFFFC ),  ( 0xFFFE )},
     { ( 0x0000 ),  ( 0x4000 ),  ( 0x6000 ),  ( 0x7000 ),
       ( 0x7800 ),  ( 0x7C00 ),  ( 0x7E00 ),  ( 0x7F00 ),
       ( 0x7F80 ),  ( 0x7FC0 ),  ( 0x7FE0 ),  ( 0x7FF0 ),
       ( 0x7FF8 ),  ( 0x7FFC ),  ( 0x7FFE ),  ( 0x7FFF )},
     { ( 0x0000 ),  ( 0x2000 ),  ( 0x3000 ),  ( 0x3800 ),
       ( 0x3C00 ),  ( 0x3E00 ),  ( 0x3F00 ),  ( 0x3F80 ),
       ( 0x3FC0 ),  ( 0x3FE0 ),  ( 0x3FF0 ),  ( 0x3FF8 ),
       ( 0x3FFC ),  ( 0x3FFE ),  ( 0x3FFF ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x1000 ),  ( 0x1800 ),  ( 0x1C00 ),
       ( 0x1E00 ),  ( 0x1F00 ),  ( 0x1F80 ),  ( 0x1FC0 ),
       ( 0x1FE0 ),  ( 0x1FF0 ),  ( 0x1FF8 ),  ( 0x1FFC ),
       ( 0x1FFE ),  ( 0x1FFF ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0800 ),  ( 0x0C00 ),  ( 0x0E00 ),
       ( 0x0F00 ),  ( 0x0F80 ),  ( 0x0FC0 ),  ( 0x0FE0 ),
       ( 0x0FF0 ),  ( 0x0FF8 ),  ( 0x0FFC ),  ( 0x0FFE ),
       ( 0x0FFF ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0400 ),  ( 0x0600 ),  ( 0x0700 ),
       ( 0x0780 ),  ( 0x07C0 ),  ( 0x07E0 ),  ( 0x07F0 ),
       ( 0x07F8 ),  ( 0x07FC ),  ( 0x07FE ),  ( 0x07FF ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0200 ),  ( 0x0300 ),  ( 0x0380 ),
       ( 0x03C0 ),  ( 0x03E0 ),  ( 0x03F0 ),  ( 0x03F8 ),
       ( 0x03FC ),  ( 0x03FE ),  ( 0x03FF ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0100 ),  ( 0x0180 ),  ( 0x01C0 ),
       ( 0x01E0 ),  ( 0x01F0 ),  ( 0x01F8 ),  ( 0x01FC ),
       ( 0x01FE ),  ( 0x01FF ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0080 ),  ( 0x00C0 ),  ( 0x00E0 ),
       ( 0x00F0 ),  ( 0x00F8 ),  ( 0x00FC ),  ( 0x00FE ),
       ( 0x00FF ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0040 ),  ( 0x0060 ),  ( 0x0070 ),
       ( 0x0078 ),  ( 0x007C ),  ( 0x007E ),  ( 0x007F ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0020 ),  ( 0x0030 ),  ( 0x0038 ),
       ( 0x003C ),  ( 0x003E ),  ( 0x003F ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0010 ),  ( 0x0018 ),  ( 0x001C ),
       ( 0x001E ),  ( 0x001F ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0008 ),  ( 0x000C ),  ( 0x000E ),
       ( 0x000F ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0004 ),  ( 0x0006 ),  ( 0x0007 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0002 ),  ( 0x0003 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
     { ( 0x0000 ),  ( 0x0001 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),
       ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 ),  ( 0x0000 )},
};

u_short mfbmask16[16] =
    {
     ( 1<<15 ),  ( 1<<14 ), ( 1<<13 ),
     ( 1<<12 ),  ( 1<<11 ), ( 1<<10 ),
     ( 1<<9 ),  ( 1<<8 ),  ( 1<<7 ),
     ( 1<<6 ),  ( 1<<5 ),  ( 1<<4 ),
     ( 1<<3 ),  ( 1<<2 ),  ( 1<<1 ),
     ( 1<<0 )
    }; 
u_short mfbrmask16[16] = 
    {
    0xffffffff ^  ( 1<<15 ), 0xffffffff ^  ( 1<<14),
    0xffffffff ^  ( 1<<13 ), 0xffffffff ^  ( 1<<12 ),
    0xffffffff ^  ( 1<<11 ), 0xffffffff ^  ( 1<<10), 
    0xffffffff ^  ( 1<<9 ),  0xffffffff ^  ( 1<<8),
    0xffffffff ^  ( 1<<7 ),  0xffffffff ^  ( 1<<6),  
    0xffffffff ^  ( 1<<5 ),  0xffffffff ^  ( 1<<4),  
    0xffffffff ^  ( 1<<3 ),  0xffffffff ^  ( 1<<2),
    0xffffffff ^  ( 1<<1 ),  0xffffffff ^  ( 1<<0)
    };

u_int mfbstarttab32[32] =
    {
	 ( 0x00000000 ), ( 0x7FFFFFFF ), ( 0x3FFFFFFF ), ( 0x1FFFFFFF ),
	 ( 0x0FFFFFFF ), ( 0x07FFFFFF ), ( 0x03FFFFFF ), ( 0x01FFFFFF ),
	 ( 0x00FFFFFF ), ( 0x007FFFFF ), ( 0x003FFFFF ), ( 0x001FFFFF ),
	 ( 0x000FFFFF ), ( 0x0007FFFF ), ( 0x0003FFFF ), ( 0x0001FFFF ),
	 ( 0x0000FFFF ), ( 0x00007FFF ), ( 0x00003FFF ), ( 0x00001FFF ),
	 ( 0x00000FFF ), ( 0x000007FF ), ( 0x000003FF ), ( 0x000001FF ),
	 ( 0x000000FF ), ( 0x0000007F ), ( 0x0000003F ), ( 0x0000001F ),
	 ( 0x0000000F ), ( 0x00000007 ), ( 0x00000003 ), ( 0x00000001 )
    };
u_int mfbendtab32[32] =
    {
	 ( 0x00000000 ), ( 0x80000000 ), ( 0xC0000000 ), ( 0xE0000000 ),
	 ( 0xF0000000 ), ( 0xF8000000 ), ( 0xFC000000 ), ( 0xFE000000 ),
	 ( 0xFF000000 ), ( 0xFF800000 ), ( 0xFFC00000 ), ( 0xFFE00000 ),
	 ( 0xFFF00000 ), ( 0xFFF80000 ), ( 0xFFFC0000 ), ( 0xFFFE0000 ),
	 ( 0xFFFF0000 ), ( 0xFFFF8000 ), ( 0xFFFFC000 ), ( 0xFFFFE000 ),
	 ( 0xFFFFF000 ), ( 0xFFFFF800 ), ( 0xFFFFFC00 ), ( 0xFFFFFE00 ),
	 ( 0xFFFFFF00 ), ( 0xFFFFFF80 ), ( 0xFFFFFFC0 ), ( 0xFFFFFFE0 ),
	 ( 0xFFFFFFF0 ), ( 0xFFFFFFF8 ), ( 0xFFFFFFFC ), ( 0xFFFFFFFE )
    };

u_int mfbpartmasks32[32][32] = {
     { ( 0xFFFFFFFF ),  ( 0x80000000 ),  ( 0xC0000000 ),  ( 0xE0000000 ),
       ( 0xF0000000 ),  ( 0xF8000000 ),  ( 0xFC000000 ),  ( 0xFE000000 ),
       ( 0xFF000000 ),  ( 0xFF800000 ),  ( 0xFFC00000 ),  ( 0xFFE00000 ),
       ( 0xFFF00000 ),  ( 0xFFF80000 ),  ( 0xFFFC0000 ),  ( 0xFFFE0000 ),
       ( 0xFFFF0000 ),  ( 0xFFFF8000 ),  ( 0xFFFFC000 ),  ( 0xFFFFE000 ),
       ( 0xFFFFF000 ),  ( 0xFFFFF800 ),  ( 0xFFFFFC00 ),  ( 0xFFFFFE00 ),
       ( 0xFFFFFF00 ),  ( 0xFFFFFF80 ),  ( 0xFFFFFFC0 ),  ( 0xFFFFFFE0 ),
       ( 0xFFFFFFF0 ),  ( 0xFFFFFFF8 ),  ( 0xFFFFFFFC ),  ( 0xFFFFFFFE )},
     { ( 0x00000000 ),  ( 0x40000000 ),  ( 0x60000000 ),  ( 0x70000000 ),
       ( 0x78000000 ),  ( 0x7C000000 ),  ( 0x7E000000 ),  ( 0x7F000000 ),
       ( 0x7F800000 ),  ( 0x7FC00000 ),  ( 0x7FE00000 ),  ( 0x7FF00000 ),
       ( 0x7FF80000 ),  ( 0x7FFC0000 ),  ( 0x7FFE0000 ),  ( 0x7FFF0000 ),
       ( 0x7FFF8000 ),  ( 0x7FFFC000 ),  ( 0x7FFFE000 ),  ( 0x7FFFF000 ),
       ( 0x7FFFF800 ),  ( 0x7FFFFC00 ),  ( 0x7FFFFE00 ),  ( 0x7FFFFF00 ),
       ( 0x7FFFFF80 ),  ( 0x7FFFFFC0 ),  ( 0x7FFFFFE0 ),  ( 0x7FFFFFF0 ),
       ( 0x7FFFFFF8 ),  ( 0x7FFFFFFC ),  ( 0x7FFFFFFE ),  ( 0x7FFFFFFF )},
     { ( 0x00000000 ),  ( 0x20000000 ),  ( 0x30000000 ),  ( 0x38000000 ),
       ( 0x3C000000 ),  ( 0x3E000000 ),  ( 0x3F000000 ),  ( 0x3F800000 ),
       ( 0x3FC00000 ),  ( 0x3FE00000 ),  ( 0x3FF00000 ),  ( 0x3FF80000 ),
       ( 0x3FFC0000 ),  ( 0x3FFE0000 ),  ( 0x3FFF0000 ),  ( 0x3FFF8000 ),
       ( 0x3FFFC000 ),  ( 0x3FFFE000 ),  ( 0x3FFFF000 ),  ( 0x3FFFF800 ),
       ( 0x3FFFFC00 ),  ( 0x3FFFFE00 ),  ( 0x3FFFFF00 ),  ( 0x3FFFFF80 ),
       ( 0x3FFFFFC0 ),  ( 0x3FFFFFE0 ),  ( 0x3FFFFFF0 ),  ( 0x3FFFFFF8 ),
       ( 0x3FFFFFFC ),  ( 0x3FFFFFFE ),  ( 0x3FFFFFFF ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x10000000 ),  ( 0x18000000 ),  ( 0x1C000000 ),
       ( 0x1E000000 ),  ( 0x1F000000 ),  ( 0x1F800000 ),  ( 0x1FC00000 ),
       ( 0x1FE00000 ),  ( 0x1FF00000 ),  ( 0x1FF80000 ),  ( 0x1FFC0000 ),
       ( 0x1FFE0000 ),  ( 0x1FFF0000 ),  ( 0x1FFF8000 ),  ( 0x1FFFC000 ),
       ( 0x1FFFE000 ),  ( 0x1FFFF000 ),  ( 0x1FFFF800 ),  ( 0x1FFFFC00 ),
       ( 0x1FFFFE00 ),  ( 0x1FFFFF00 ),  ( 0x1FFFFF80 ),  ( 0x1FFFFFC0 ),
       ( 0x1FFFFFE0 ),  ( 0x1FFFFFF0 ),  ( 0x1FFFFFF8 ),  ( 0x1FFFFFFC ),
       ( 0x1FFFFFFE ),  ( 0x1FFFFFFF ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x08000000 ),  ( 0x0C000000 ),  ( 0x0E000000 ),
       ( 0x0F000000 ),  ( 0x0F800000 ),  ( 0x0FC00000 ),  ( 0x0FE00000 ),
       ( 0x0FF00000 ),  ( 0x0FF80000 ),  ( 0x0FFC0000 ),  ( 0x0FFE0000 ),
       ( 0x0FFF0000 ),  ( 0x0FFF8000 ),  ( 0x0FFFC000 ),  ( 0x0FFFE000 ),
       ( 0x0FFFF000 ),  ( 0x0FFFF800 ),  ( 0x0FFFFC00 ),  ( 0x0FFFFE00 ),
       ( 0x0FFFFF00 ),  ( 0x0FFFFF80 ),  ( 0x0FFFFFC0 ),  ( 0x0FFFFFE0 ),
       ( 0x0FFFFFF0 ),  ( 0x0FFFFFF8 ),  ( 0x0FFFFFFC ),  ( 0x0FFFFFFE ),
       ( 0x0FFFFFFF ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x04000000 ),  ( 0x06000000 ),  ( 0x07000000 ),
       ( 0x07800000 ),  ( 0x07C00000 ),  ( 0x07E00000 ),  ( 0x07F00000 ),
       ( 0x07F80000 ),  ( 0x07FC0000 ),  ( 0x07FE0000 ),  ( 0x07FF0000 ),
       ( 0x07FF8000 ),  ( 0x07FFC000 ),  ( 0x07FFE000 ),  ( 0x07FFF000 ),
       ( 0x07FFF800 ),  ( 0x07FFFC00 ),  ( 0x07FFFE00 ),  ( 0x07FFFF00 ),
       ( 0x07FFFF80 ),  ( 0x07FFFFC0 ),  ( 0x07FFFFE0 ),  ( 0x07FFFFF0 ),
       ( 0x07FFFFF8 ),  ( 0x07FFFFFC ),  ( 0x07FFFFFE ),  ( 0x07FFFFFF ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x02000000 ),  ( 0x03000000 ),  ( 0x03800000 ),
       ( 0x03C00000 ),  ( 0x03E00000 ),  ( 0x03F00000 ),  ( 0x03F80000 ),
       ( 0x03FC0000 ),  ( 0x03FE0000 ),  ( 0x03FF0000 ),  ( 0x03FF8000 ),
       ( 0x03FFC000 ),  ( 0x03FFE000 ),  ( 0x03FFF000 ),  ( 0x03FFF800 ),
       ( 0x03FFFC00 ),  ( 0x03FFFE00 ),  ( 0x03FFFF00 ),  ( 0x03FFFF80 ),
       ( 0x03FFFFC0 ),  ( 0x03FFFFE0 ),  ( 0x03FFFFF0 ),  ( 0x03FFFFF8 ),
       ( 0x03FFFFFC ),  ( 0x03FFFFFE ),  ( 0x03FFFFFF ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x01000000 ),  ( 0x01800000 ),  ( 0x01C00000 ),
       ( 0x01E00000 ),  ( 0x01F00000 ),  ( 0x01F80000 ),  ( 0x01FC0000 ),
       ( 0x01FE0000 ),  ( 0x01FF0000 ),  ( 0x01FF8000 ),  ( 0x01FFC000 ),
       ( 0x01FFE000 ),  ( 0x01FFF000 ),  ( 0x01FFF800 ),  ( 0x01FFFC00 ),
       ( 0x01FFFE00 ),  ( 0x01FFFF00 ),  ( 0x01FFFF80 ),  ( 0x01FFFFC0 ),
       ( 0x01FFFFE0 ),  ( 0x01FFFFF0 ),  ( 0x01FFFFF8 ),  ( 0x01FFFFFC ),
       ( 0x01FFFFFE ),  ( 0x01FFFFFF ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00800000 ),  ( 0x00C00000 ),  ( 0x00E00000 ),
       ( 0x00F00000 ),  ( 0x00F80000 ),  ( 0x00FC0000 ),  ( 0x00FE0000 ),
       ( 0x00FF0000 ),  ( 0x00FF8000 ),  ( 0x00FFC000 ),  ( 0x00FFE000 ),
       ( 0x00FFF000 ),  ( 0x00FFF800 ),  ( 0x00FFFC00 ),  ( 0x00FFFE00 ),
       ( 0x00FFFF00 ),  ( 0x00FFFF80 ),  ( 0x00FFFFC0 ),  ( 0x00FFFFE0 ),
       ( 0x00FFFFF0 ),  ( 0x00FFFFF8 ),  ( 0x00FFFFFC ),  ( 0x00FFFFFE ),
       ( 0x00FFFFFF ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00400000 ),  ( 0x00600000 ),  ( 0x00700000 ),
       ( 0x00780000 ),  ( 0x007C0000 ),  ( 0x007E0000 ),  ( 0x007F0000 ),
       ( 0x007F8000 ),  ( 0x007FC000 ),  ( 0x007FE000 ),  ( 0x007FF000 ),
       ( 0x007FF800 ),  ( 0x007FFC00 ),  ( 0x007FFE00 ),  ( 0x007FFF00 ),
       ( 0x007FFF80 ),  ( 0x007FFFC0 ),  ( 0x007FFFE0 ),  ( 0x007FFFF0 ),
       ( 0x007FFFF8 ),  ( 0x007FFFFC ),  ( 0x007FFFFE ),  ( 0x007FFFFF ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00200000 ),  ( 0x00300000 ),  ( 0x00380000 ),
       ( 0x003C0000 ),  ( 0x003E0000 ),  ( 0x003F0000 ),  ( 0x003F8000 ),
       ( 0x003FC000 ),  ( 0x003FE000 ),  ( 0x003FF000 ),  ( 0x003FF800 ),
       ( 0x003FFC00 ),  ( 0x003FFE00 ),  ( 0x003FFF00 ),  ( 0x003FFF80 ),
       ( 0x003FFFC0 ),  ( 0x003FFFE0 ),  ( 0x003FFFF0 ),  ( 0x003FFFF8 ),
       ( 0x003FFFFC ),  ( 0x003FFFFE ),  ( 0x003FFFFF ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00100000 ),  ( 0x00180000 ),  ( 0x001C0000 ),
       ( 0x001E0000 ),  ( 0x001F0000 ),  ( 0x001F8000 ),  ( 0x001FC000 ),
       ( 0x001FE000 ),  ( 0x001FF000 ),  ( 0x001FF800 ),  ( 0x001FFC00 ),
       ( 0x001FFE00 ),  ( 0x001FFF00 ),  ( 0x001FFF80 ),  ( 0x001FFFC0 ),
       ( 0x001FFFE0 ),  ( 0x001FFFF0 ),  ( 0x001FFFF8 ),  ( 0x001FFFFC ),
       ( 0x001FFFFE ),  ( 0x001FFFFF ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00080000 ),  ( 0x000C0000 ),  ( 0x000E0000 ),
       ( 0x000F0000 ),  ( 0x000F8000 ),  ( 0x000FC000 ),  ( 0x000FE000 ),
       ( 0x000FF000 ),  ( 0x000FF800 ),  ( 0x000FFC00 ),  ( 0x000FFE00 ),
       ( 0x000FFF00 ),  ( 0x000FFF80 ),  ( 0x000FFFC0 ),  ( 0x000FFFE0 ),
       ( 0x000FFFF0 ),  ( 0x000FFFF8 ),  ( 0x000FFFFC ),  ( 0x000FFFFE ),
       ( 0x000FFFFF ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00040000 ),  ( 0x00060000 ),  ( 0x00070000 ),
       ( 0x00078000 ),  ( 0x0007C000 ),  ( 0x0007E000 ),  ( 0x0007F000 ),
       ( 0x0007F800 ),  ( 0x0007FC00 ),  ( 0x0007FE00 ),  ( 0x0007FF00 ),
       ( 0x0007FF80 ),  ( 0x0007FFC0 ),  ( 0x0007FFE0 ),  ( 0x0007FFF0 ),
       ( 0x0007FFF8 ),  ( 0x0007FFFC ),  ( 0x0007FFFE ),  ( 0x0007FFFF ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00020000 ),  ( 0x00030000 ),  ( 0x00038000 ),
       ( 0x0003C000 ),  ( 0x0003E000 ),  ( 0x0003F000 ),  ( 0x0003F800 ),
       ( 0x0003FC00 ),  ( 0x0003FE00 ),  ( 0x0003FF00 ),  ( 0x0003FF80 ),
       ( 0x0003FFC0 ),  ( 0x0003FFE0 ),  ( 0x0003FFF0 ),  ( 0x0003FFF8 ),
       ( 0x0003FFFC ),  ( 0x0003FFFE ),  ( 0x0003FFFF ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00010000 ),  ( 0x00018000 ),  ( 0x0001C000 ),
       ( 0x0001E000 ),  ( 0x0001F000 ),  ( 0x0001F800 ),  ( 0x0001FC00 ),
       ( 0x0001FE00 ),  ( 0x0001FF00 ),  ( 0x0001FF80 ),  ( 0x0001FFC0 ),
       ( 0x0001FFE0 ),  ( 0x0001FFF0 ),  ( 0x0001FFF8 ),  ( 0x0001FFFC ),
       ( 0x0001FFFE ),  ( 0x0001FFFF ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00008000 ),  ( 0x0000C000 ),  ( 0x0000E000 ),
       ( 0x0000F000 ),  ( 0x0000F800 ),  ( 0x0000FC00 ),  ( 0x0000FE00 ),
       ( 0x0000FF00 ),  ( 0x0000FF80 ),  ( 0x0000FFC0 ),  ( 0x0000FFE0 ),
       ( 0x0000FFF0 ),  ( 0x0000FFF8 ),  ( 0x0000FFFC ),  ( 0x0000FFFE ),
       ( 0x0000FFFF ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00004000 ),  ( 0x00006000 ),  ( 0x00007000 ),
       ( 0x00007800 ),  ( 0x00007C00 ),  ( 0x00007E00 ),  ( 0x00007F00 ),
       ( 0x00007F80 ),  ( 0x00007FC0 ),  ( 0x00007FE0 ),  ( 0x00007FF0 ),
       ( 0x00007FF8 ),  ( 0x00007FFC ),  ( 0x00007FFE ),  ( 0x00007FFF ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00002000 ),  ( 0x00003000 ),  ( 0x00003800 ),
       ( 0x00003C00 ),  ( 0x00003E00 ),  ( 0x00003F00 ),  ( 0x00003F80 ),
       ( 0x00003FC0 ),  ( 0x00003FE0 ),  ( 0x00003FF0 ),  ( 0x00003FF8 ),
       ( 0x00003FFC ),  ( 0x00003FFE ),  ( 0x00003FFF ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00001000 ),  ( 0x00001800 ),  ( 0x00001C00 ),
       ( 0x00001E00 ),  ( 0x00001F00 ),  ( 0x00001F80 ),  ( 0x00001FC0 ),
       ( 0x00001FE0 ),  ( 0x00001FF0 ),  ( 0x00001FF8 ),  ( 0x00001FFC ),
       ( 0x00001FFE ),  ( 0x00001FFF ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000800 ),  ( 0x00000C00 ),  ( 0x00000E00 ),
       ( 0x00000F00 ),  ( 0x00000F80 ),  ( 0x00000FC0 ),  ( 0x00000FE0 ),
       ( 0x00000FF0 ),  ( 0x00000FF8 ),  ( 0x00000FFC ),  ( 0x00000FFE ),
       ( 0x00000FFF ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000400 ),  ( 0x00000600 ),  ( 0x00000700 ),
       ( 0x00000780 ),  ( 0x000007C0 ),  ( 0x000007E0 ),  ( 0x000007F0 ),
       ( 0x000007F8 ),  ( 0x000007FC ),  ( 0x000007FE ),  ( 0x000007FF ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000200 ),  ( 0x00000300 ),  ( 0x00000380 ),
       ( 0x000003C0 ),  ( 0x000003E0 ),  ( 0x000003F0 ),  ( 0x000003F8 ),
       ( 0x000003FC ),  ( 0x000003FE ),  ( 0x000003FF ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000100 ),  ( 0x00000180 ),  ( 0x000001C0 ),
       ( 0x000001E0 ),  ( 0x000001F0 ),  ( 0x000001F8 ),  ( 0x000001FC ),
       ( 0x000001FE ),  ( 0x000001FF ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000080 ),  ( 0x000000C0 ),  ( 0x000000E0 ),
       ( 0x000000F0 ),  ( 0x000000F8 ),  ( 0x000000FC ),  ( 0x000000FE ),
       ( 0x000000FF ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000040 ),  ( 0x00000060 ),  ( 0x00000070 ),
       ( 0x00000078 ),  ( 0x0000007C ),  ( 0x0000007E ),  ( 0x0000007F ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000020 ),  ( 0x00000030 ),  ( 0x00000038 ),
       ( 0x0000003C ),  ( 0x0000003E ),  ( 0x0000003F ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000010 ),  ( 0x00000018 ),  ( 0x0000001C ),
       ( 0x0000001E ),  ( 0x0000001F ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000008 ),  ( 0x0000000C ),  ( 0x0000000E ),
       ( 0x0000000F ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000004 ),  ( 0x00000006 ),  ( 0x00000007 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000002 ),  ( 0x00000003 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
     { ( 0x00000000 ),  ( 0x00000001 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),
       ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 ),  ( 0x00000000 )},
};

u_int mfbmask32[32] =
    {
     ( 1<<31 ),  ( 1<<30 ),  ( 1<<29 ), 
     ( 1<<28 ),  ( 1<<27 ),  ( 1<<26 ), 
     ( 1<<25 ),  ( 1<<24 ),  ( 1<<23 ), 
     ( 1<<22 ),  ( 1<<21 ),  ( 1<<20 ), 
     ( 1<<19 ),  ( 1<<18 ),  ( 1<<17 ), 
     ( 1<<16 ),  ( 1<<15 ),  ( 1<<14 ), 
     ( 1<<13 ),  ( 1<<12 ),  ( 1<<11 ), 
     ( 1<<10 ),  ( 1<<9 ),  ( 1<<8 ),
     ( 1<<7 ),  ( 1<<6 ),  ( 1<<5 ), 
     ( 1<<4 ),  ( 1<<3 ),  ( 1<<2 ), 
     ( 1<<1 ),  ( 1<<0 )
    }; 

u_int mfbrmask32[32] = 
    {
    0xffffffff ^  ( 1<<31 ), 0xffffffff ^  ( 1<<30 ), 
    0xffffffff ^  ( 1<<29 ), 0xffffffff ^  ( 1<<28), 
    0xffffffff ^  ( 1<<27 ), 0xffffffff ^  ( 1<<26),
    0xffffffff ^  ( 1<<25 ), 0xffffffff ^  ( 1<<24 ), 
    0xffffffff ^  ( 1<<23 ), 0xffffffff ^  ( 1<<22), 
    0xffffffff ^  ( 1<<21 ), 0xffffffff ^  ( 1<<20),
    0xffffffff ^  ( 1<<19 ), 0xffffffff ^  ( 1<<18 ), 
    0xffffffff ^  ( 1<<17 ), 0xffffffff ^  ( 1<<16),
    0xffffffff ^  ( 1<<15 ), 0xffffffff ^  ( 1<<14),
    0xffffffff ^  ( 1<<13 ), 0xffffffff ^  ( 1<<12 ),
    0xffffffff ^  ( 1<<11 ), 0xffffffff ^  ( 1<<10), 
    0xffffffff ^  ( 1<<9 ),  0xffffffff ^  ( 1<<8),
    0xffffffff ^  ( 1<<7 ),  0xffffffff ^  ( 1<<6),  
    0xffffffff ^  ( 1<<5 ),  0xffffffff ^  ( 1<<4),  
    0xffffffff ^  ( 1<<3 ),  0xffffffff ^  ( 1<<2),
    0xffffffff ^  ( 1<<1 ),  0xffffffff ^  ( 1<<0)
    };

mergeRopRec mergeRopBits[16] = {
	 0, 0, 0, 0,		/* BF_0		0 */
	~0, 0, 0, 0,		/* BF_SDA	src & dst */
	~0, 0,~0, 0,		/* BF_SDIA	src & ~dst */
	 0, 0,~0, 0,		/* BF_S		src */
	~0,~0, 0, 0,		/* BF_SIDA	~src & dst */
	 0,~0, 0, 0,		/* BF_D		dst */
	 0,~0,~0, 0,		/* BF_SDX	src ^ dst */
	~0,~0,~0, 0,		/* BF_SDO	src | dst */
	~0,~0,~0,~0,		/* BF_SDOI	~(src | dst) */
	 0,~0,~0,~0,		/* BF_SDXI	~(src ^ dst) */
	 0,~0, 0,~0,		/* BF_DI	~dst */
	~0,~0, 0,~0,		/* BF_SDIO	src | ~dst */
	 0, 0,~0,~0,		/* BF_SI	~src */
	~0, 0,~0,~0,		/* BF_SIDO	~src | dst */
	~0, 0, 0,~0,		/* BF_SDAI	~(src & dst) */
	 0, 0, 0,~0,		/* BF_1		1 */
};

#define Duff(counter, block) { \
	switch (counter & 7) { \
	case 7: { block; } \
	case 6: { block; } \
	case 5: { block; } \
	case 4: { block; } \
	case 3: { block; } \
	case 2: { block; } \
	case 1: { block; } \
	case 0:; \
	} \
	while ((counter -= 8) >= 0) { \
		{ block; } \
		{ block; } \
		{ block; } \
		{ block; } \
		{ block; } \
		{ block; } \
		{ block; } \
		{ block; } \
	} \
}

#ifdef mc68020
#define	Duff_plus(n, dst, src, op) { \
	switch ((n) & 7) { \
	case 7: \
		*(dst)++ op *(src)++; \
	case 6: \
		*(dst)++ op *(src)++; \
	case 5: \
		*(dst)++ op *(src)++; \
	case 4: \
		*(dst)++ op *(src)++; \
	case 3: \
		*(dst)++ op *(src)++; \
	case 2: \
		*(dst)++ op *(src)++; \
	case 1: \
		*(dst)++ op *(src)++; \
	} \
	while (((n) -= 8) >= 0) { \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
		*(dst)++ op *(src)++; \
	} \
} \

#define Duff_minus(n, dst, src, op) { \
	switch ((n) & 7) { \
	case 7: \
		*--(dst) op *--(src); \
	case 6: \
		*--(dst) op *--(src); \
	case 5: \
		*--(dst) op *--(src); \
	case 4: \
		*--(dst) op *--(src); \
	case 3: \
		*--(dst) op *--(src); \
	case 2: \
		*--(dst) op *--(src); \
	case 1: \
		*--(dst) op *--(src); \
	} \
	while (((n) -= 8) >= 0) { \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
		*--(dst) op *--(src); \
	} \
}

#define	Duff_shift_plus(n, dst, src, op) { \
	switch ((n) & 3) { \
	case 3: \
		bits = *(src)++; \
		*(dst)++ op (bits1 << leftShift) | (bits >> rightShift); \
	case 2: \
		bits1 = *(src)++; \
		*(dst)++ op (bits << leftShift) | (bits1 >> rightShift); \
	case 1: \
		bits = *(src)++; \
		*(dst)++ op (bits1 << leftShift) | (bits >> rightShift); \
	} \
	while (((n) -= 4) >= 0) { \
		bits1 = *(src)++; \
		*(dst)++ op (bits << leftShift) | (bits1 >> rightShift); \
		bits = *(src)++; \
		*(dst)++ op (bits1 << leftShift) | (bits >> rightShift); \
		bits1 = *(src)++; \
		*(dst)++ op (bits << leftShift) | (bits1 >> rightShift); \
		bits = *(src)++; \
		*(dst)++ op (bits1 << leftShift) | (bits >> rightShift); \
	} \
}

#define	Duff_shift_minus(n, dst, src, op) { \
	switch ((n) & 3) { \
	case 3: \
		bits = *--(src); \
		*--(dst) op ((bits1 >> rightShift) | (bits << leftShift)); \
	case 2: \
		bits1 = *--(src); \
		*--(dst) op ((bits >> rightShift) | (bits1 << leftShift)); \
	case 1: \
		bits = *--(src); \
		*--(dst) op ((bits1 >> rightShift) | (bits << leftShift)); \
	} \
	while (((n) -= 4) >= 0) { \
		bits1 = *--(src); \
		*--(dst) op ((bits >> rightShift) | (bits1 << leftShift)); \
		bits = *--(src); \
		*--(dst) op ((bits1 >> rightShift) | (bits << leftShift)); \
		bits1 = *--(src); \
		*--(dst) op ((bits >> rightShift) | (bits1 << leftShift)); \
		bits = *--(src); \
		*--(dst) op ((bits1 >> rightShift) | (bits << leftShift)); \
	} \
}

#define	Duff_single(nlw, addr, op) { \
	switch ((nlw) & 7) { \
	case 7: \
		*(addr)++ op ; \
	case 6: \
		*(addr)++ op ; \
	case 5: \
		*(addr)++ op ; \
	case 4: \
		*(addr)++ op ; \
	case 3: \
		*(addr)++ op ; \
	case 2: \
		*(addr)++ op ; \
	case 1: \
		*(addr)++ op ; \
	} \
	while (((nlw) -= 8) >= 0) { \
		*(addr)++ op ; \
		*(addr)++ op ; \
		*(addr)++ op ; \
		*(addr)++ op ; \
		*(addr)++ op ; \
		*(addr)++ op ; \
		*(addr)++ op ; \
		*(addr)++ op ; \
	} \
}
#else /* mc68020 */
#define	Duff_plus(n, dst, src, op) { \
	(src) += (n) & 7; \
	(dst) += (n) & 7; \
	switch ((n) & 7) { \
	case 7: \
		(dst)[-7] op (src)[-7]; \
	case 6: \
		(dst)[-6] op (src)[-6]; \
	case 5: \
		(dst)[-5] op (src)[-5]; \
	case 4: \
		(dst)[-4] op (src)[-4]; \
	case 3: \
		(dst)[-3] op (src)[-3]; \
	case 2: \
		(dst)[-2] op (src)[-2]; \
	case 1: \
		(dst)[-1] op (src)[-1]; \
	} \
	while (((n) -= 8) >= 0) { \
		(dst) += 8; \
		(src) += 8; \
		(dst)[-8] op (src)[-8]; \
		(dst)[-7] op (src)[-7]; \
		(dst)[-6] op (src)[-6]; \
		(dst)[-5] op (src)[-5]; \
		(dst)[-4] op (src)[-4]; \
		(dst)[-3] op (src)[-3]; \
		(dst)[-2] op (src)[-2]; \
		(dst)[-1] op (src)[-1]; \
	} \
} \

#define Duff_minus(n, dst, src, op) { \
	(src) -= (n) & 7; \
	(dst) -= (n) & 7; \
	switch ((n) & 7) { \
	case 7: \
		(dst)[7-1] op (src)[7-1]; \
	case 6: \
		(dst)[6-1] op (src)[6-1]; \
	case 5: \
		(dst)[5-1] op (src)[5-1]; \
	case 4: \
		(dst)[4-1] op (src)[4-1]; \
	case 3: \
		(dst)[3-1] op (src)[3-1]; \
	case 2: \
		(dst)[2-1] op (src)[2-1]; \
	case 1: \
		(dst)[1-1] op (src)[1-1]; \
	} \
	while (((n) -= 8) >= 0) { \
		(dst) -= 8; \
		(src) -= 8; \
		(dst)[8-1] op (src)[8-1]; \
		(dst)[7-1] op (src)[7-1]; \
		(dst)[6-1] op (src)[6-1]; \
		(dst)[5-1] op (src)[5-1]; \
		(dst)[4-1] op (src)[4-1]; \
		(dst)[3-1] op (src)[3-1]; \
		(dst)[2-1] op (src)[2-1]; \
		(dst)[1-1] op (src)[1-1]; \
	} \
}

#define	Duff_shift_plus(n, dst, src, op) { \
	(src) += (n) & 7; \
	(dst) += (n) & 7; \
	switch ((n) & 7) { \
	case 7: \
		bits = (src)[-7]; \
		(dst)[-7] op (bits1 << leftShift) | (bits >> rightShift); \
	case 6: \
		bits1 = (src)[-6]; \
		(dst)[-6] op (bits << leftShift) | (bits1 >> rightShift); \
	case 5: \
		bits = (src)[-5]; \
		(dst)[-5] op (bits1 << leftShift) | (bits >> rightShift); \
	case 4: \
		bits1 = (src)[-4]; \
		(dst)[-4] op (bits << leftShift) | (bits1 >> rightShift); \
	case 3: \
		bits = (src)[-3]; \
		(dst)[-3] op (bits1 << leftShift) | (bits >> rightShift); \
	case 2: \
		bits1 = (src)[-2]; \
		(dst)[-2] op (bits << leftShift) | (bits1 >> rightShift); \
	case 1: \
		bits = (src)[-1]; \
		(dst)[-1] op (bits1 << leftShift) | (bits >> rightShift); \
	} \
	while (((n) -= 8) >= 0) { \
		(dst) += 8; \
		(src) += 8; \
		bits1 = (src)[-8]; \
		(dst)[-8] op (bits << leftShift) | (bits1 >> rightShift); \
		bits = (src)[-7]; \
		(dst)[-7] op (bits1 << leftShift) | (bits >> rightShift); \
		bits1 = (src)[-6]; \
		(dst)[-6] op (bits << leftShift) | (bits1 >> rightShift); \
		bits = (src)[-5]; \
		(dst)[-5] op (bits1 << leftShift) | (bits >> rightShift); \
		bits1 = (src)[-4]; \
		(dst)[-4] op (bits << leftShift) | (bits1 >> rightShift); \
		bits = (src)[-3]; \
		(dst)[-3] op (bits1 << leftShift) | (bits >> rightShift); \
		bits1 = (src)[-2]; \
		(dst)[-2] op (bits << leftShift) | (bits1 >> rightShift); \
		bits = (src)[-1]; \
		(dst)[-1] op (bits1 << leftShift) | (bits >> rightShift); \
	} \
}

#define	Duff_shift_minus(n, dst, src, op) { \
	(src) -= (n) & 7; \
	(dst) -= (n) & 7; \
	switch ((n) & 7) { \
	case 7: \
		bits = (src)[7-1]; \
		(dst)[7-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
	case 6: \
		bits1 = (src)[6-1]; \
		(dst)[6-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
	case 5: \
		bits = (src)[5-1]; \
		(dst)[5-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
	case 4: \
		bits1 = (src)[4-1]; \
		(dst)[4-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
	case 3: \
		bits = (src)[3-1]; \
		(dst)[3-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
	case 2: \
		bits1 = (src)[2-1]; \
		(dst)[2-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
	case 1: \
		bits = (src)[1-1]; \
		(dst)[1-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
	} \
	while (((n) -= 8) >= 0) { \
		(dst) -= 8; \
		(src) -= 8; \
		bits1 = (src)[8-1]; \
		(dst)[8-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
		bits = (src)[7-1]; \
		(dst)[7-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
		bits1 = (src)[6-1]; \
		(dst)[6-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
		bits = (src)[5-1]; \
		(dst)[5-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
		bits1 = (src)[4-1]; \
		(dst)[4-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
		bits = (src)[3-1]; \
		(dst)[3-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
		bits1 = (src)[2-1]; \
		(dst)[2-1] op ((bits >> rightShift) | (bits1 << leftShift)); \
		bits = (src)[1-1]; \
		(dst)[1-1] op ((bits1 >> rightShift) | (bits << leftShift)); \
	} \
}

#define	Duff_single(nlw, addr, op) { \
	(addr) += (nlw) & 7; \
	switch ((nlw) & 7) { \
	case 7: \
		(addr)[-7] op; \
	case 6: \
		(addr)[-6] op; \
	case 5: \
		(addr)[-5] op; \
	case 4: \
		(addr)[-4] op; \
	case 3: \
		(addr)[-3] op; \
	case 2: \
		(addr)[-2] op; \
	case 1: \
		(addr)[-1] op; \
	} \
	while (((nlw) -= 8) >= 0) { \
		(addr) += 8; \
		(addr)[-8] op; \
		(addr)[-7] op; \
		(addr)[-6] op; \
		(addr)[-5] op; \
		(addr)[-4] op; \
		(addr)[-3] op; \
		(addr)[-2] op; \
		(addr)[-1] op; \
	} \
}
#endif /* mc68020 */

void
mfb_copy_area32(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_int	*addrSrc;
u_int	*addrDst;
u_int	widthSrc;
u_int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_int *psrcLine, *psrc;
	register u_int *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_int startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_int bits;
	u_int bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0x1f) +w <= 32) {
		startmask = mfbpartmasks32[dp->x & 0x1f][w & 0x1f];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab32[dp->x & 0x1f];
		endmask = mfbendtab32[(dp->x + w) & 0x1f];
		if (startmask) {
			nlMiddle = (w - (32 - (dp->x & 0x1f))) >> 5;
		} else {
			nlMiddle = w >> 5;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0x1f;
		xoffDst = dp->x & 0x1f;
		pdstLine += (dp->x >> 5);
		psrcLine += (sr->origin.x >> 5);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (startmask) {
					*pdst = (*pdst & ~startmask |
						 *psrc & startmask);
					psrc++;
					pdst++;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, =);
				if (endmask) {
					*pdst = (*pdst & ~endmask |
						 *psrc & endmask);
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst = (*pdst & ~startmask |
						 bits1 & startmask);
					pdst++;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, =);
				if (endmask) {
					bits1 = bits << leftShift;
					if (endmask << rightShift) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst = (*pdst & ~endmask |
						 bits1 & endmask);
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0x1f;
		xoffDst = (dp->x + w - 1) & 0x1f;
		pdstLine += ((dp->x + w - 1) >> 5) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 5) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (endmask) {
					pdst--;
					psrc--;
					*pdst = (*pdst & ~endmask |
						 *psrc & endmask);
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, =);
				if (startmask) {
					--pdst;
					--psrc;
					*pdst = (*pdst & ~startmask |
						 *psrc & startmask);
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					pdst--;
					*pdst = (*pdst & ~endmask |
						 bits1 & endmask);
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, =);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					--pdst;
					*pdst = (*pdst & ~startmask |
						 bits1 & startmask);
				}
			}
		}
	}
}

void
mfb_copyinv_area32(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_int	*addrSrc;
u_int	*addrDst;
int	widthSrc;
int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_int *psrcLine, *psrc;
	register u_int *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_int startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_int bits;
	u_int bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0x1f) +w <= 32) {
		startmask = mfbpartmasks32[dp->x & 0x1f][w & 0x1f];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab32[dp->x & 0x1f];
		endmask = mfbendtab32[(dp->x + w) & 0x1f];
		if (startmask) {
			nlMiddle = (w - (32 - (dp->x & 0x1f))) >> 5;
		} else {
			nlMiddle = w >> 5;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0x1f;
		xoffDst = dp->x & 0x1f;
		pdstLine += (dp->x >> 5);
		psrcLine += (sr->origin.x >> 5);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (startmask) {
					*pdst = (*pdst & ~startmask |
						 ~*psrc & startmask);
					psrc++;
					pdst++;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, = ~);
				if (endmask) {
					*pdst = (*pdst & ~endmask |
						 ~*psrc & endmask);
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst = (*pdst & ~startmask |
						 ~bits1 & startmask);
					pdst++;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, = ~);
				if (endmask) {
					bits1 = bits << leftShift;
					if (endmask << rightShift) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst = (*pdst & ~endmask |
						 ~bits1 & endmask);
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0x1f;
		xoffDst = (dp->x + w - 1) & 0x1f;
		pdstLine += ((dp->x + w - 1) >> 5) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 5) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (endmask) {
					pdst--;
					psrc--;
					*pdst = (*pdst & ~endmask |
						 ~*psrc & endmask);
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, = ~);
				if (startmask) {
					--pdst;
					--psrc;
					*pdst = (*pdst & ~startmask |
						 ~*psrc & startmask);
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					pdst--;
					*pdst = (*pdst & ~endmask |
						 ~bits1 & endmask);
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, = ~);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					--pdst;
					*pdst = (*pdst & ~startmask |
						 ~bits1 & startmask);
				}
			}
		}
	}
}

void
mfb_or_area32(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_int	*addrSrc;
u_int	*addrDst;
int	widthSrc;
int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_int *psrcLine, *psrc;
	register u_int *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_int startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_int bits;
	u_int bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0x1f) +w <= 32) {
		startmask = mfbpartmasks32[dp->x & 0x1f][w & 0x1f];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab32[dp->x & 0x1f];
		endmask = mfbendtab32[(dp->x + w) & 0x1f];
		if (startmask) {
			nlMiddle = (w - (32 - (dp->x & 0x1f))) >> 5;
		} else {
			nlMiddle = w >> 5;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0x1f;
		xoffDst = dp->x & 0x1f;
		pdstLine += (dp->x >> 5);
		psrcLine += (sr->origin.x >> 5);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (startmask) {
					*pdst++ |= *psrc++ & startmask;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, |=);
				if (endmask) {
					*pdst |= *psrc & endmask;
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst++ |= bits1 & startmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, |=);
				if (endmask) {
					bits1 = bits << leftShift;
					if (endmask << rightShift) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst |= bits1 & endmask;
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0x1f;
		xoffDst = (dp->x + w - 1) & 0x1f;
		pdstLine += ((dp->x + w - 1) >> 5) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 5) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (endmask) {
					*--pdst |= *--psrc & endmask;
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, |=);
				if (startmask) {
					*--pdst |= *--psrc & startmask;
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					*--pdst |= bits1 & endmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, |=);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					*--pdst |= bits1 & startmask;
				}
			}
		}
	}
}

void
mfb_xor_area32(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_int	*addrSrc;
u_int	*addrDst;
int	widthSrc;
int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_int *psrcLine, *psrc;
	register u_int *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_int startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_int bits;
	u_int bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0x1f) +w <= 32) {
		startmask = mfbpartmasks32[dp->x & 0x1f][w & 0x1f];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab32[dp->x & 0x1f];
		endmask = mfbendtab32[(dp->x + w) & 0x1f];
		if (startmask) {
			nlMiddle = (w - (32 - (dp->x & 0x1f))) >> 5;
		} else {
			nlMiddle = w >> 5;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0x1f;
		xoffDst = dp->x & 0x1f;
		pdstLine += (dp->x >> 5);
		psrcLine += (sr->origin.x >> 5);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (startmask) {
					*pdst++ ^= *psrc++ & startmask;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, ^=);
				if (endmask) {
					*pdst ^= *psrc & endmask;
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst++ ^= bits1 & startmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, ^=);
				if (endmask) {
					bits1 = bits << leftShift;
					if (endmask << rightShift) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst ^= bits1 & endmask;
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0x1f;
		xoffDst = (dp->x + w - 1) & 0x1f;
		pdstLine += ((dp->x + w - 1) >> 5) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 5) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (endmask) {
					*--pdst ^= *--psrc & endmask;
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, ^=);
				if (startmask) {
					*--pdst ^= *--psrc & startmask;
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					*--pdst ^= bits1 & endmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, ^=);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					*--pdst ^= bits1 & startmask;
				}
			}
		}
	}
}

void
mfb_general_area32(func, addrSrc, addrDst, widthSrc, widthDst, sr, dp)
int		func;
u_int	*addrSrc;
u_int	*addrDst;
int	widthSrc;
int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_int *psrcLine, *psrc;
	register u_int *pdstLine, *pdst;
	register int leftShift, rightShift;
	u_int bits;
	u_int bits1;
	register int nl;
	u_int _ca1, _cx1, _ca2, _cx2;
	u_int startmask, endmask;
	int w, h;
	int xdir, ydir;
	register int nlMiddle;
	int xoffSrc, xoffDst;

	_ca1 = mergeRopBits[func].ca1;
	_cx1 = mergeRopBits[func].cx1;
	_ca2 = mergeRopBits[func].ca2;
	_cx2 = mergeRopBits[func].cx2;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0x1f) +w <= 32) {
		startmask = mfbpartmasks32[dp->x & 0x1f][w & 0x1f];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab32[dp->x & 0x1f];
		endmask = mfbendtab32[(dp->x + w) & 0x1f];
		if (startmask) {
			nlMiddle = (w - (32 - (dp->x & 0x1f))) >> 5;
		} else {
			nlMiddle = w >> 5;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0x1f;
		xoffDst = dp->x & 0x1f;
		pdstLine += (dp->x >> 5);
		psrcLine += (sr->origin.x >> 5);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (startmask) {
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       startmask);
					psrc++;
					pdst++;
				}
				nl = nlMiddle;
#ifdef mc68020
				Duff(nl, *pdst = DoMergeRop(*psrc, *pdst);
					  psrc++; pdst++);
#else /* mc68020 */
				psrc += nl & 7;
				pdst += nl & 7;
				switch (nl & 7) {
				case 7:
					pdst[-7] = DoMergeRop(psrc[-7], pdst[-7]);
				case 6:
					pdst[-6] = DoMergeRop(psrc[-6], pdst[-6]);
				case 5:
					pdst[-5] = DoMergeRop(psrc[-5], pdst[-5]);
				case 4:
					pdst[-4] = DoMergeRop(psrc[-4], pdst[-4]);
				case 3:
					pdst[-3] = DoMergeRop(psrc[-3], pdst[-3]);
				case 2:
					pdst[-2] = DoMergeRop(psrc[-2], pdst[-2]);
				case 1:
					pdst[-1] = DoMergeRop(psrc[-1], pdst[-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst += 8;
					psrc += 8;
					pdst[-8] = DoMergeRop(psrc[-8], pdst[-8]);
					pdst[-7] = DoMergeRop(psrc[-7], pdst[-7]);
					pdst[-6] = DoMergeRop(psrc[-6], pdst[-6]);
					pdst[-5] = DoMergeRop(psrc[-5], pdst[-5]);
					pdst[-4] = DoMergeRop(psrc[-4], pdst[-4]);
					pdst[-3] = DoMergeRop(psrc[-3], pdst[-3]);
					pdst[-2] = DoMergeRop(psrc[-2], pdst[-2]);
					pdst[-1] = DoMergeRop(psrc[-1], pdst[-1]);
				}
#endif /* mc68020 */
				if (endmask) {
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       endmask);
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst = DoMergeRopMask(bits1, *pdst,
							       startmask);
					pdst++;
				}
				nl = nlMiddle;
				bits1 = bits;
				psrc += nl & 7;
				pdst += nl & 7;
				switch (nl & 7) {
				case 7:
					bits = psrc[-7];
					pdst[-7] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-7]);
				case 6:
					bits1 = psrc[-6];
					pdst[-6] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-6]);
				case 5:
					bits = psrc[-5];
					pdst[-5] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-5]);
				case 4:
					bits1 = psrc[-4];
					pdst[-4] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-4]);
				case 3:
					bits = psrc[-3];
					pdst[-3] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-3]);
				case 2:
					bits1 = psrc[-2];
					pdst[-2] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-2]);
				case 1:
					bits = psrc[-1];
					pdst[-1] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst += 8;
					psrc += 8;
					bits1 = psrc[-8];
					pdst[-8] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-8]);
					bits = psrc[-7];
					pdst[-7] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-7]);
					bits1 = psrc[-6];
					pdst[-6] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-6]);
					bits = psrc[-5];
					pdst[-5] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-5]);
					bits1 = psrc[-4];
					pdst[-4] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-4]);
					bits = psrc[-3];
					pdst[-3] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-3]);
					bits1 = psrc[-2];
					pdst[-2] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-2]);
					bits = psrc[-1];
					pdst[-1] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-1]);
				}
				if (endmask) {
					bits1 = bits << leftShift;
					if (endmask << rightShift) {
						bits = *psrc;
						bits1 |= (bits >> rightShift);
					}
					*pdst = DoMergeRopMask(bits1, *pdst,
							       endmask);
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0x1f;
		xoffDst = (dp->x + w - 1) & 0x1f;
		pdstLine += ((dp->x + w - 1) >> 5) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 5) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				if (endmask) {
					pdst--;
					psrc--;
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       endmask);
				}
				nl = nlMiddle;
#ifdef mc68020
				Duff(nl, pdst--; psrc--; 
					  *pdst = DoMergeRop(*psrc, *pdst));
#else /* mc68020 */
				psrc -= nl & 7;
				pdst -= nl & 7;
				switch (nl & 7) {
				case 7:
					pdst[7-1] = DoMergeRop(psrc[7-1], pdst[7-1]);
				case 6:
					pdst[6-1] = DoMergeRop(psrc[6-1], pdst[6-1]);
				case 5:
					pdst[5-1] = DoMergeRop(psrc[5-1], pdst[5-1]);
				case 4:
					pdst[4-1] = DoMergeRop(psrc[4-1], pdst[4-1]);
				case 3:
					pdst[3-1] = DoMergeRop(psrc[3-1], pdst[3-1]);
				case 2:
					pdst[2-1] = DoMergeRop(psrc[2-1], pdst[2-1]);
				case 1:
					pdst[1-1] = DoMergeRop(psrc[1-1], pdst[1-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst -= 8;
					psrc -= 8;
					pdst[8-1] = DoMergeRop(psrc[8-1], pdst[8-1]);
					pdst[7-1] = DoMergeRop(psrc[7-1], pdst[7-1]);
					pdst[6-1] = DoMergeRop(psrc[6-1], pdst[6-1]);
					pdst[5-1] = DoMergeRop(psrc[5-1], pdst[5-1]);
					pdst[4-1] = DoMergeRop(psrc[4-1], pdst[4-1]);
					pdst[3-1] = DoMergeRop(psrc[3-1], pdst[3-1]);
					pdst[2-1] = DoMergeRop(psrc[2-1], pdst[2-1]);
					pdst[1-1] = DoMergeRop(psrc[1-1], pdst[1-1]);
				}
#endif /* mc68020 */
				if (startmask) {
					--pdst;
					--psrc;
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       startmask);
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 32 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 32 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				pdstLine += widthDst;
				psrcLine += widthSrc;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					pdst--;
					*pdst = DoMergeRopMask(bits1, *pdst,
							       endmask);
				}
				nl = nlMiddle;
				bits1 = bits;
				psrc -= nl & 7;
				pdst -= nl & 7;
				switch (nl & 7) {
				case 7:
					bits = psrc[7-1];
					pdst[7-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[7-1]);
				case 6:
					bits1 = psrc[6-1];
					pdst[6-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[6-1]);
				case 5:
					bits = psrc[5-1];
					pdst[5-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[5-1]);
				case 4:
					bits1 = psrc[4-1];
					pdst[4-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[4-1]);
				case 3:
					bits = psrc[3-1];
					pdst[3-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[3-1]);
				case 2:
					bits1 = psrc[2-1];
					pdst[2-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[2-1]);
				case 1:
					bits = psrc[1-1];
					pdst[1-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[1-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst -= 8;
					psrc -= 8;
					bits1 = psrc[8-1];
					pdst[8-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[8-1]);
					bits = psrc[7-1];
					pdst[7-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[7-1]);
					bits1 = psrc[6-1];
					pdst[6-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[6-1]);
					bits = psrc[5-1];
					pdst[5-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[5-1]);
					bits1 = psrc[4-1];
					pdst[4-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[4-1]);
					bits = psrc[3-1];
					pdst[3-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[3-1]);
					bits1 = psrc[2-1];
					pdst[2-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[2-1]);
					bits = psrc[1-1];
					pdst[1-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[1-1]);
				}
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits = *--psrc;
						bits1 |= (bits << leftShift);
					}
					--pdst;
					*pdst = DoMergeRopMask(bits1, *pdst,
							       startmask);
				}
			}
		}
	}
}

void
mfb_clr_area32(x, y, w, h, addr, nlwidth)
int x;
int y;
int w;
register int h;
register u_int *addr;
int nlwidth;
{
	register u_int startmask;
	u_int endmask;
	register int nlw, nlwExtra;
	int nlwMiddle;
	int startoff, endoff;

	addr += (y * nlwidth + (x >> 5));

	startoff = x & 0x1f;
	if (((startoff) + w) < 32) {
		startmask = ~mfbpartmasks32[startoff][w & 0x1f];
		nlwExtra = nlwidth;
#ifdef mc68020
		asm("	lsl.l	#2,d4");
		Duff(h, asm("	and.l	d6,(a5)");
			asm("	adda.l	d4,a5"))
#else /* mc68020 */
		Duff(h, *addr &= startmask; addr += nlwExtra)
#endif /* mc68020 */
		return;
	}
	endoff = (x + w) & 0x1f;
	if (startoff) {
		startmask = ~mfbstarttab32[startoff];
		nlwMiddle = (w - (32 - (startoff))) >> 5;
		nlwExtra = nlwidth - nlwMiddle - 1;
		if (endoff) {
			endmask = ~mfbendtab32[endoff];
			while (h--) {
				nlw = nlwMiddle;
				*addr++ &= startmask;
				Duff_single(nlw, addr, = 0);
				*addr &= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				*addr++ &= startmask;
				Duff_single(nlw, addr, = 0);
				addr += nlwExtra;
			}
		}
	} else {
		nlwMiddle = w >> 5;
		nlwExtra = nlwidth - nlwMiddle;
		if (endoff) {
			endmask = ~mfbendtab32[endoff];
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, = 0);
				*addr &= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, = 0);
				addr += nlwExtra;
			}
		}
	}
}

void
mfb_inv_area32(x, y, w, h, addr, nlwidth)
int x;
int y;
int w;
register int h;
register u_int *addr;
int nlwidth;
{
	register u_int startmask;
	u_int endmask;
	register int nlw, nlwExtra;
	int nlwMiddle;
	int startoff, endoff;

	addr += (y * nlwidth + (x >> 5));

	startoff = x & 0x1f;
	if (((startoff) + w) < 32) {
		startmask = mfbpartmasks32[startoff][w & 0x1f];
		nlwExtra = nlwidth;
#ifdef mc68020
		asm("	lsl.l	#2,d4");
		Duff(h, asm("	eor.l	d6,(a5)");
			asm("	adda.l	d4,a5"))
#else /* mc68020 */
		Duff(h, *addr ^= startmask; addr += nlwExtra)
#endif /* mc68020 */
		return;
	}
	endoff = (x + w) & 0x1f;
	if (startoff) {
		startmask = mfbstarttab32[startoff];
		nlwMiddle = (w - (32 - (startoff))) >> 5;
		nlwExtra = nlwidth - nlwMiddle - 1;
		if (endoff) {
			endmask = mfbendtab32[endoff];
			while (h--) {
				nlw = nlwMiddle;
				*addr++ ^= startmask;
				Duff_single(nlw, addr, ^= ~0);
				*addr ^= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				*addr++ ^= startmask;
				Duff_single(nlw, addr, ^= ~0);
				addr += nlwExtra;
			}
		}
	} else {
		nlwMiddle = w >> 5;
		nlwExtra = nlwidth - nlwMiddle;
		if (endoff) {
			endmask = mfbendtab32[endoff];
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, ^= ~0);
				*addr ^= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, ^= ~0);
				addr += nlwExtra;
			}
		}
	}
}

void
mfb_set_area32(x, y, w, h, addr, nlwidth)
int x;
int y;
int w;
register int h;
register u_int *addr;
int nlwidth;
{
	register u_int startmask;
	u_int endmask;
	register int nlw, nlwExtra;
	int nlwMiddle;
	int startoff, endoff;

	addr += (y * nlwidth + (x >> 5));

	startoff = x & 0x1f;
	if (((startoff) + w) < 32) {
		startmask = mfbpartmasks32[startoff][w & 0x1f];
		nlwExtra = nlwidth;
#ifdef mc68020
		asm("	lsl.l	#2,d4");
		Duff(h, asm("	or.l	d6,(a5)");
			asm("	adda.l	d4,a5"))
#else /* mc68020 */
		Duff(h, *addr |= startmask; addr += nlwExtra)
#endif /* mc68020 */
		return;
	}
	endoff = (x + w) & 0x1f;
	if (startoff) {
		startmask = mfbstarttab32[startoff];
		nlwMiddle = (w - (32 - (startoff))) >> 5;
		nlwExtra = nlwidth - nlwMiddle - 1;
		if (endoff) {
			endmask = mfbendtab32[endoff];
			while (h--) {
				nlw = nlwMiddle;
				*addr++ |= startmask;
				Duff_single(nlw, addr, = ~0);
				*addr |= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				*addr++ |= startmask;
				Duff_single(nlw, addr, = ~0);
				addr += nlwExtra;
			}
		}
	} else {
		nlwMiddle = w >> 5;
		nlwExtra = nlwidth - nlwMiddle;
		if (endoff) {
			endmask = mfbendtab32[endoff];
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, = ~0);
				*addr |= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, = ~0);
				addr += nlwExtra;
			}
		}
	}
}

void
mfb_copy_area16(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_short	*addrSrc;
u_short	*addrDst;
u_int	widthSrc;
u_int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_short *psrcLine, *psrc;
	register u_short *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_short startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_short bits;
	u_short bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0xf) +w <= 16) {
		startmask = mfbpartmasks16[dp->x & 0xf][w & 0xf];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab16[dp->x & 0xf];
		endmask = mfbendtab16[(dp->x + w) & 0xf];
		if (startmask) {
			nlMiddle = (w - (16 - (dp->x & 0xf))) >> 4;
		} else {
			nlMiddle = w >> 4;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0xf;
		xoffDst = dp->x & 0xf;
		pdstLine += (dp->x >> 4);
		psrcLine += (sr->origin.x >> 4);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (startmask) {
					*pdst = (*pdst & ~startmask |
						 *psrc & startmask);
					psrc++;
					pdst++;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, =);
				if (endmask) {
					*pdst = (*pdst & ~endmask |
						 *psrc & endmask);
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst = (*pdst & ~startmask |
						 bits1 & startmask);
					pdst++;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, =);
				if (endmask) {
					bits1 = bits << leftShift;
					if ((endmask << rightShift) & 0xffff) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst = (*pdst & ~endmask |
						 bits1 & endmask);
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0xf;
		xoffDst = (dp->x + w - 1) & 0xf;
		pdstLine += ((dp->x + w - 1) >> 4) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 4) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (endmask) {
					pdst--;
					psrc--;
					*pdst = (*pdst & ~endmask |
						 *psrc & endmask);
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, =);
				if (startmask) {
					--pdst;
					--psrc;
					*pdst = (*pdst & ~startmask |
						 *psrc & startmask);
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					pdst--;
					*pdst = (*pdst & ~endmask |
						 bits1 & endmask);
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, =);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					--pdst;
					*pdst = (*pdst & ~startmask |
						 bits1 & startmask);
				}
			}
		}
	}
}

void
mfb_copyinv_area16(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_short	*addrSrc;
u_short	*addrDst;
register int	widthSrc;
register int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_short *psrcLine, *psrc;
	register u_short *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_short startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_short bits;
	u_short bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0xf) +w <= 16) {
		startmask = mfbpartmasks16[dp->x & 0xf][w & 0xf];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab16[dp->x & 0xf];
		endmask = mfbendtab16[(dp->x + w) & 0xf];
		if (startmask) {
			nlMiddle = (w - (16 - (dp->x & 0xf))) >> 4;
		} else {
			nlMiddle = w >> 4;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0xf;
		xoffDst = dp->x & 0xf;
		psrcLine += (sr->origin.x >> 4);
		pdstLine += (dp->x >> 4);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (startmask) {
					*pdst = (*pdst & ~startmask |
						 ~*psrc & startmask);
					psrc++;
					pdst++;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, = ~);
				if (endmask) {
					*pdst = (*pdst & ~endmask |
						 ~*psrc & endmask);
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst = (*pdst & ~startmask |
						 ~bits1 & startmask);
					pdst++;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, = ~);
				if (endmask) {
					bits1 = bits << leftShift;
					if ((endmask << rightShift) & 0xffff) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst = (*pdst & ~endmask |
						 ~bits1 & endmask);
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0xf;
		xoffDst = (dp->x + w - 1) & 0xf;
		pdstLine += ((dp->x + w - 1) >> 4) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 4) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (endmask) {
					pdst--;
					psrc--;
					*pdst = (*pdst & ~endmask |
						 ~*psrc & endmask);
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, = ~);
				if (startmask) {
					--pdst;
					--psrc;
					*pdst = (*pdst & ~startmask |
						 ~*psrc & startmask);
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					pdst--;
					*pdst = (*pdst & ~endmask |
						 ~bits1 & endmask);
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, = ~);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					--pdst;
					*pdst = (*pdst & ~startmask |
						 ~bits1 & startmask);
				}
			}
		}
	}
}

void
mfb_or_area16(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_short	*addrSrc;
u_short	*addrDst;
register int	widthSrc;
register int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_short *psrcLine, *psrc;
	register u_short *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_short startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_short bits;
	u_short bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0xf) +w <= 16) {
		startmask = mfbpartmasks16[dp->x & 0xf][w & 0xf];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab16[dp->x & 0xf];
		endmask = mfbendtab16[(dp->x + w) & 0xf];
		if (startmask) {
			nlMiddle = (w - (16 - (dp->x & 0xf))) >> 4;
		} else {
			nlMiddle = w >> 4;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0xf;
		xoffDst = dp->x & 0xf;
		pdstLine += (dp->x >> 4);
		psrcLine += (sr->origin.x >> 4);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (startmask) {
					*pdst++ |= *psrc++ & startmask;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, |=);
				if (endmask) {
					*pdst |= *psrc & endmask;
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst++ |= bits1 & startmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, |=);
				if (endmask) {
					bits1 = bits << leftShift;
					if ((endmask << rightShift) & 0xffff) {
						bits1 |= *psrc >> rightShift;
					}
					*pdst |= bits1 & endmask;
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0xf;
		xoffDst = (dp->x + w - 1) & 0xf;
		pdstLine += ((dp->x + w - 1) >> 4) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 4) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (endmask) {
					*--pdst |= *--psrc & endmask;
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, |=);
				if (startmask) {
					*--pdst |= *--psrc & startmask;
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					*--pdst |= bits1 & endmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, |=);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= *--psrc << leftShift;
					}
					*--pdst |= bits1 & startmask;
				}
			}
		}
	}
}

void
mfb_xor_area16(addrSrc, addrDst, widthSrc, widthDst, sr, dp)
u_short	*addrSrc;
u_short	*addrDst;
int	widthSrc;
int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_short *psrcLine, *psrc;
	register u_short *pdstLine, *pdst;
	int w, h;
	int xdir, ydir;
	u_short startmask, endmask;
	register int nlMiddle;
	int xoffSrc, xoffDst;
	register int leftShift, rightShift;
	u_short bits;
	u_short bits1;
	register int nl;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0xf) +w <= 16) {
		startmask = mfbpartmasks16[dp->x & 0xf][w & 0xf];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab16[dp->x & 0xf];
		endmask = mfbendtab16[(dp->x + w) & 0xf];
		if (startmask) {
			nlMiddle = (w - (16 - (dp->x & 0xf))) >> 4;
		} else {
			nlMiddle = w >> 4;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0xf;
		xoffDst = dp->x & 0xf;
		pdstLine += (dp->x >> 4);
		psrcLine += (sr->origin.x >> 4);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (startmask) {
					*pdst++ ^= *psrc++ & startmask;
				}
				nl = nlMiddle;
				Duff_plus(nl, pdst, psrc, ^=);
				if (endmask) {
					*pdst ^= *psrc & endmask;
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst++ ^= bits1 & startmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_plus(nl, pdst, psrc, ^=);
				if (endmask) {
					bits1 = bits << leftShift;
					if ((endmask << rightShift) & 0xffff) {
						bits1 |= (*psrc >> rightShift);
					}
					*pdst ^= bits1 & endmask;
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0xf;
		xoffDst = (dp->x + w - 1) & 0xf;
		pdstLine += ((dp->x + w - 1) >> 4) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 4) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (endmask) {
					*--pdst ^= *--psrc & endmask;
				}
				nl = nlMiddle;
				Duff_minus(nl, pdst, psrc, ^=);
				if (startmask) {
					*--pdst ^= *--psrc & startmask;
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					*--pdst ^= bits1 & endmask;
				}
				nl = nlMiddle;
				bits1 = bits;
				Duff_shift_minus(nl, pdst, psrc, ^=);
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits1 |= (*--psrc << leftShift);
					}
					*--pdst ^= bits1 & startmask;
				}
			}
		}
	}
}

void
mfb_general_area16(func, addrSrc, addrDst, widthSrc, widthDst, sr, dp)
int		func;
u_short	*addrSrc;
u_short	*addrDst;
int	widthSrc;
int	widthDst;
lRectangle	*sr;	/* source rectangle */
lPoint		*dp;	/* destination point */
{
	register u_short *psrcLine, *psrc;
	register u_short *pdstLine, *pdst;
	register int leftShift, rightShift;
	u_short bits;
	u_short bits1;
	register int nl;
	u_short _ca1, _cx1, _ca2, _cx2;
	u_short startmask, endmask;
	int w, h;
	int xdir, ydir;
	register int nlMiddle;
	int xoffSrc, xoffDst;

	_ca1 = mergeRopBits[func].ca1;
	_cx1 = mergeRopBits[func].cx1;
	_ca2 = mergeRopBits[func].ca2;
	_cx2 = mergeRopBits[func].cx2;

	if (sr->origin.y < dp->y) {
		ydir = -1;
		widthSrc = -widthSrc;
		widthDst = -widthDst;
	} else {
		ydir = 1;
	}

	if (sr->origin.x < dp->x) {
		xdir = -1;
	} else {
		xdir = 1;
	}

	w = sr->extent.x;
	h = sr->extent.y;

	if (ydir == -1) {
		psrcLine = addrSrc + ((sr->origin.y + h - 1) * -widthSrc);
		pdstLine = addrDst + ((dp->y + h - 1) * -widthDst);
	} else {
		psrcLine = addrSrc + (sr->origin.y * widthSrc);
		pdstLine = addrDst + (dp->y * widthDst);
	}
	if ((dp->x & 0xf) +w <= 16) {
		startmask = mfbpartmasks16[dp->x & 0xf][w & 0xf];
		endmask = 0;
		nlMiddle = 0;
	} else {
		startmask = mfbstarttab16[dp->x & 0xf];
		endmask = mfbendtab16[(dp->x + w) & 0xf];
		if (startmask) {
			nlMiddle = (w - (16 - (dp->x & 0xf))) >> 4;
		} else {
			nlMiddle = w >> 4;
		}
	}

	if (xdir == 1) {
		xoffSrc = sr->origin.x & 0xf;
		xoffDst = dp->x & 0xf;
		pdstLine += (dp->x >> 4);
		psrcLine += (sr->origin.x >> 4);
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (startmask) {
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       startmask);
					psrc++;
					pdst++;
				}
				nl = nlMiddle;
				psrc += nl & 7;
				pdst += nl & 7;
				switch (nl & 7) {
				case 7:
					pdst[-7] = DoMergeRop(psrc[-7], pdst[-7]);
				case 6:
					pdst[-6] = DoMergeRop(psrc[-6], pdst[-6]);
				case 5:
					pdst[-5] = DoMergeRop(psrc[-5], pdst[-5]);
				case 4:
					pdst[-4] = DoMergeRop(psrc[-4], pdst[-4]);
				case 3:
					pdst[-3] = DoMergeRop(psrc[-3], pdst[-3]);
				case 2:
					pdst[-2] = DoMergeRop(psrc[-2], pdst[-2]);
				case 1:
					pdst[-1] = DoMergeRop(psrc[-1], pdst[-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst += 8;
					psrc += 8;
					pdst[-8] = DoMergeRop(psrc[-8], pdst[-8]);
					pdst[-7] = DoMergeRop(psrc[-7], pdst[-7]);
					pdst[-6] = DoMergeRop(psrc[-6], pdst[-6]);
					pdst[-5] = DoMergeRop(psrc[-5], pdst[-5]);
					pdst[-4] = DoMergeRop(psrc[-4], pdst[-4]);
					pdst[-3] = DoMergeRop(psrc[-3], pdst[-3]);
					pdst[-2] = DoMergeRop(psrc[-2], pdst[-2]);
					pdst[-1] = DoMergeRop(psrc[-1], pdst[-1]);
				}
				if (endmask) {
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       endmask);
				}
			}
		} else {
			if (xoffSrc > xoffDst) {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			} else {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffSrc > xoffDst)
					bits = *psrc++;
				if (startmask) {
					bits1 = bits << leftShift;
					bits = *psrc++;
					bits1 |= bits >> rightShift;
					*pdst = DoMergeRopMask(bits1, *pdst,
							       startmask);
					pdst++;
				}
				nl = nlMiddle;
				bits1 = bits;
				psrc += nl & 7;
				pdst += nl & 7;
				switch (nl & 7) {
				case 7:
					bits = psrc[-7];
					pdst[-7] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-7]);
				case 6:
					bits1 = psrc[-6];
					pdst[-6] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-6]);
				case 5:
					bits = psrc[-5];
					pdst[-5] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-5]);
				case 4:
					bits1 = psrc[-4];
					pdst[-4] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-4]);
				case 3:
					bits = psrc[-3];
					pdst[-3] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-3]);
				case 2:
					bits1 = psrc[-2];
					pdst[-2] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-2]);
				case 1:
					bits = psrc[-1];
					pdst[-1] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst += 8;
					psrc += 8;
					bits1 = psrc[-8];
					pdst[-8] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-8]);
					bits = psrc[-7];
					pdst[-7] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-7]);
					bits1 = psrc[-6];
					pdst[-6] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-6]);
					bits = psrc[-5];
					pdst[-5] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-5]);
					bits1 = psrc[-4];
					pdst[-4] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-4]);
					bits = psrc[-3];
					pdst[-3] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-3]);
					bits1 = psrc[-2];
					pdst[-2] = DoMergeRop(((bits << leftShift) | (bits1 >> rightShift)), pdst[-2]);
					bits = psrc[-1];
					pdst[-1] = DoMergeRop(((bits1 << leftShift) | (bits >> rightShift)), pdst[-1]);
				}
				if (endmask) {
					bits1 = bits << leftShift;
					if ((endmask << rightShift) & 0xffff) {
						bits = *psrc;
						bits1 |= (bits >> rightShift);
					}
					*pdst = DoMergeRopMask(bits1, *pdst,
							       endmask);
				}
			}
		}
	} else {
		xoffSrc = (sr->origin.x + w - 1) & 0xf;
		xoffDst = (dp->x + w - 1) & 0xf;
		pdstLine += ((dp->x + w - 1) >> 4) + 1;
		psrcLine += ((sr->origin.x + w - 1) >> 4) + 1;
		if (xoffSrc == xoffDst) {
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				if (endmask) {
					pdst--;
					psrc--;
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       endmask);
				}
				nl = nlMiddle;
				psrc -= nl & 7;
				pdst -= nl & 7;
				switch (nl & 7) {
				case 7:
					pdst[7-1] = DoMergeRop(psrc[7-1], pdst[7-1]);
				case 6:
					pdst[6-1] = DoMergeRop(psrc[6-1], pdst[6-1]);
				case 5:
					pdst[5-1] = DoMergeRop(psrc[5-1], pdst[5-1]);
				case 4:
					pdst[4-1] = DoMergeRop(psrc[4-1], pdst[4-1]);
				case 3:
					pdst[3-1] = DoMergeRop(psrc[3-1], pdst[3-1]);
				case 2:
					pdst[2-1] = DoMergeRop(psrc[2-1], pdst[2-1]);
				case 1:
					pdst[1-1] = DoMergeRop(psrc[1-1], pdst[1-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst -= 8;
					psrc -= 8;
					pdst[8-1] = DoMergeRop(psrc[8-1], pdst[8-1]);
					pdst[7-1] = DoMergeRop(psrc[7-1], pdst[7-1]);
					pdst[6-1] = DoMergeRop(psrc[6-1], pdst[6-1]);
					pdst[5-1] = DoMergeRop(psrc[5-1], pdst[5-1]);
					pdst[4-1] = DoMergeRop(psrc[4-1], pdst[4-1]);
					pdst[3-1] = DoMergeRop(psrc[3-1], pdst[3-1]);
					pdst[2-1] = DoMergeRop(psrc[2-1], pdst[2-1]);
					pdst[1-1] = DoMergeRop(psrc[1-1], pdst[1-1]);
				}
				if (startmask) {
					--pdst;
					--psrc;
					*pdst = DoMergeRopMask(*psrc, *pdst,
							       startmask);
				}
			}
		} else {
			if (xoffDst > xoffSrc) {
				rightShift = xoffDst - xoffSrc;
				leftShift = 16 - rightShift;
			} else {
				leftShift = xoffSrc - xoffDst;
				rightShift = 16 - leftShift;
			}
			while (h--) {
				psrc = psrcLine;
				pdst = pdstLine;
				psrcLine += widthSrc;
				pdstLine += widthDst;
				bits = 0;
				if (xoffDst > xoffSrc)
					bits = *--psrc;
				if (endmask) {
					bits1 = bits >> rightShift;
					bits = *--psrc;
					bits1 |= (bits << leftShift);
					pdst--;
					*pdst = DoMergeRopMask(bits1, *pdst,
							       endmask);
				}
				nl = nlMiddle;
				bits1 = bits;
				psrc -= nl & 7;
				pdst -= nl & 7;
				switch (nl & 7) {
				case 7:
					bits = psrc[7-1];
					pdst[7-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[7-1]);
				case 6:
					bits1 = psrc[6-1];
					pdst[6-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[6-1]);
				case 5:
					bits = psrc[5-1];
					pdst[5-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[5-1]);
				case 4:
					bits1 = psrc[4-1];
					pdst[4-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[4-1]);
				case 3:
					bits = psrc[3-1];
					pdst[3-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[3-1]);
				case 2:
					bits1 = psrc[2-1];
					pdst[2-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[2-1]);
				case 1:
					bits = psrc[1-1];
					pdst[1-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[1-1]);
				}
				while ((nl -= 8) >= 0) {
					pdst -= 8;
					psrc -= 8;
					bits1 = psrc[8-1];
					pdst[8-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[8-1]);
					bits = psrc[7-1];
					pdst[7-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[7-1]);
					bits1 = psrc[6-1];
					pdst[6-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[6-1]);
					bits = psrc[5-1];
					pdst[5-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[5-1]);
					bits1 = psrc[4-1];
					pdst[4-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[4-1]);
					bits = psrc[3-1];
					pdst[3-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[3-1]);
					bits1 = psrc[2-1];
					pdst[2-1] = DoMergeRop(((bits >> rightShift) | (bits1 << leftShift)), pdst[2-1]);
					bits = psrc[1-1];
					pdst[1-1] = DoMergeRop(((bits1 >> rightShift) | (bits << leftShift)), pdst[1-1]);
				}
				if (startmask) {
					bits1 = (bits >> rightShift);
					if (startmask >> leftShift) {
						bits = *--psrc;
						bits1 |= (bits << leftShift);
					}
					--pdst;
					*pdst = DoMergeRopMask(bits1, *pdst,
							       startmask);
				}
			}
		}
	}
}

void
mfb_clr_area16(x, y, w, h, addr, nlwidth)
int x;
int y;
int w;
register int h;
register u_short *addr;
int nlwidth;
{
	register u_short startmask;
	u_short endmask;
	register int nlw, nlwExtra;
	int nlwMiddle;

	addr += (y * nlwidth + (x >> 4));

	if (((x & 0xf) + w) < 16) {
		startmask = mfbpartmasks16[x & 0xf][w & 0xf];
		nlwExtra = nlwidth;
		Duff(h, *addr &= ~startmask; addr += nlwExtra)
	} else {
		startmask = mfbstarttab16[x & 0xf];
		endmask = mfbendtab16[(x + w) & 0xf];
		if (startmask) {
			nlwMiddle = (w - (16 - (x & 0xf))) >> 4;
		} else {
			nlwMiddle = w >> 4;
		}
		nlwExtra = nlwidth - nlwMiddle;
		if (startmask && endmask) {
			startmask ^= ~0;
			endmask ^= ~0;
			nlwExtra -= 1;
			while (h--) {
				nlw = nlwMiddle;
				*addr++ &= startmask;
				Duff_single(nlw, addr, = 0)
				*addr &= endmask;
				addr += nlwExtra;
			}
		} else if (startmask && !endmask) {
			startmask ^= ~0;
			nlwExtra -= 1;
			while (h--) {
				nlw = nlwMiddle;
				*addr++ &= startmask;
				Duff_single(nlw, addr, = 0)
				addr += nlwExtra;
			}
		} else if (!startmask && endmask) {
			endmask ^= ~0;
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, = 0)
				*addr &= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, = 0)
				addr += nlwExtra;
			}
		}
	}
}

void
mfb_inv_area16(x, y, w, h, addr, nlwidth)
int x;
int y;
int w;
register int h;
register u_short *addr;
int nlwidth;
{
	register u_short startmask;
	u_short endmask;
	register int nlw, nlwExtra;
	int nlwMiddle;

	addr += (y * nlwidth + (x >> 4));

	if (((x & 0xf) + w) < 16) {
		startmask = mfbpartmasks16[x & 0xf][w & 0xf];
		nlwExtra = nlwidth;
		Duff(h, *addr ^= startmask; addr += nlwExtra)
	} else {
		startmask = mfbstarttab16[x & 0xf];
		endmask = mfbendtab16[(x + w) & 0xf];
		if (startmask) {
			nlwMiddle = (w - (16 - (x & 0xf))) >> 4;
		} else {
			nlwMiddle = w >> 4;
		}
		nlwExtra = nlwidth - nlwMiddle;
		if (startmask && endmask) {
			nlwExtra -= 1;
			while (h--) {
				nlw = nlwMiddle;
				*addr++ ^= startmask;
				Duff_single(nlw, addr, ^= ~0)
				*addr ^= endmask;
				addr += nlwExtra;
			}
		} else if (startmask && !endmask) {
			nlwExtra -= 1;
			while (h--) {
				nlw = nlwMiddle;
				*addr++ ^= startmask;
				Duff_single(nlw, addr, ^= ~0)
				addr += nlwExtra;
			}
		} else if (!startmask && endmask) {
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, ^= ~0)
				*addr ^= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
				Duff_single(nlw, addr, ^= ~0)
				addr += nlwExtra;
			}
		}
	}
}

void
mfb_set_area16(x, y, w, h, addr, nlwidth)
int x;
int y;
int w;
register int h;
register u_short *addr;
int nlwidth;
{
	register u_short startmask;
	u_short endmask;
	register int nlw, nlwExtra;
	int nlwMiddle;

	addr += (y * nlwidth + (x >> 4));

	if (((x & 0xf) + w) < 16) {
		startmask = mfbpartmasks16[x & 0xf][w & 0xf];
		nlwExtra = nlwidth;
		Duff(h, *addr |= startmask; addr += nlwExtra)
	} else {
		startmask = mfbstarttab16[x & 0xf];
		endmask = mfbendtab16[(x + w) & 0xf];
		if (startmask) {
			nlwMiddle = (w - (16 - (x & 0xf))) >> 4;
		} else {
			nlwMiddle = w >> 4;
		}
		nlwExtra = nlwidth - nlwMiddle;
		if (startmask && endmask) {
			nlwExtra -= 1;
			while (h--) {
				nlw = nlwMiddle;
				*addr++ |= startmask;
#ifdef mc68020
				asm(" move.w #-1,d3");
				Duff(nlw, asm(" move.w d3,(a5)+"))
#else /* mc68020 */
				Duff_single(nlw, addr, = ~0)
#endif /* mc68020 */
				*addr |= endmask;
				addr += nlwExtra;
			}
		} else if (startmask && !endmask) {
			nlwExtra -= 1;
			while (h--) {
				nlw = nlwMiddle;
				*addr++ |= startmask;
#ifdef mc68020
				asm(" move.w #-1,d3");
				Duff(nlw, asm(" move.w d3,(a5)+"));
#else /* mc68020 */
				Duff_single(nlw, addr, = ~0)
#endif /* mc68020 */
				addr += nlwExtra;
			}
		} else if (!startmask && endmask) {
			while (h--) {
				nlw = nlwMiddle;
#ifdef mc68020
				asm(" move.w #-1,d3");
				Duff(nlw, asm(" move.w d3,(a5)+"));
#else /* mc68020 */
				Duff_single(nlw, addr, = ~0)
#endif /* mc68020 */
				*addr |= endmask;
				addr += nlwExtra;
			}
		} else {
			while (h--) {
				nlw = nlwMiddle;
#ifdef mc68020
				asm(" move.w #-1,d3");
				Duff(nlw, asm(" move.w d3,(a5)+"));
#else /* mc68020 */
				Duff_single(nlw, addr, = ~0)
#endif /* mc68020 */
				addr += nlwExtra;
			}
		}
	}
}

void
mfb_clrvvector32(addr, nlwidth, x, len, lpf)
register u_int *addr;
register int nlwidth;
register int x;
register int len;
{
	register u_int bitmask;

	if (len < 0) {
		nlwidth = -nlwidth;
		len = -len;
	}
	if (lpf)
		len++;
	addr += (x >> 5);

	bitmask = mfbrmask32[x & 0x1f];

	Duff(len, *addr &= bitmask; addr += nlwidth)
}

void
mfb_clrhvector32(addr, x, len, lpf)
register u_int *addr;
int x;
int len;
int lpf;
{
	register u_int	startmask;
	register u_int 	endmask;
	register int	nl, off;

	if (len < 0) {
		x += len;
		len = -len;
		if (lpf) {
			len++;
		} else {
			x++;
		}
	} else {
		if (lpf) {
			len++;
		}
	}
	addr += (x >> 5);

	off = x & 0x1f;
	if (off + len < 32) {
		*addr &= ~mfbpartmasks32[off][len & 0x1f];
	} else {
		startmask = mfbstarttab32[off];
		endmask = mfbendtab32[(x + len) & 0x1f];
		if (startmask) {
			nl = (len - (32 - off)) >> 5;
			*addr++ &= ~startmask;
		} else
			nl = len >> 5;
		Duff_single(nl, addr, = 0);
		if (endmask)
			*addr &= ~endmask;
	}
}

void
mfb_clrvector32(fb, addr, ddy, p0, p1, lpf)
struct fbdev	*fb;
register u_int *addr;
register int ddy;
register lPoint *p0, *p1;
int lpf;		/* if 0, don't draw last point */
{
	register int i;
	register int lim;
#ifdef mc68020
	register int x = p0->x;
#else /* mc68020 */
	register u_int bit, leftbit, rightbit;
#endif /* mc68020 */
	int ddx;
	int dx = p1->x - p0->x;
	int dy = p1->y - p0->y;
	int s, d, c;

	addr += (p0->y * ddy);

	if (dx == 0) {
		mfb_clrvvector32(addr, ddy, p0->x, dy, lpf);
		return;
	}
	if (dy == 0) {
		mfb_clrhvector32(addr, p0->x, dx, lpf);
		return;
	}

	if (dx < 0) {
		ddx = -1;
		dx = -dx;
	} else {
		ddx = 1;
	}
	if (dy < 0) {
		dy = -dy;
		ddy = -ddy;
	}
	
#ifndef mc68020
	bit = mfbmask32[p0->x & 0x1f];
	leftbit = mfbmask32[0];
	rightbit = mfbmask32[31];
	addr += (p0->x >> 5);
#endif /* mc68020 */
	if (dx > dy) {	/* case x */
		lim = dx;
		if (lpf)
			lim++;

		s = -dx;
		d = dx << 1;
		c = dy << 1;

		if (ddx > 0) {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfclr	(a5){d4:1}");
				x++;
#else /* mc68020 */
				*addr &= ~bit;
				bit >>= 1;
				if (!bit) {
					bit = leftbit;
					addr++;
				}
#endif /* mc68020 */
				if ((s += c) >= 0) {
					s -= d;
					addr += ddy;
				}
			}
		} else {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfclr	(a5){d4:1}");
				x--;
#else /* mc68020 */
				*addr &= ~bit;
				bit <<= 1;
				if (!bit) {
					bit = rightbit;
					addr--;
				}
#endif /* mc68020 */
				if ((s += c) >= 0) {
					s -= d;
					addr += ddy;
				}
			}
		}
	} else {			/* case y */
		lim = dy;
		if (lpf)
			lim++;
		s = -dy;
		d = dy << 1;
		c = dx << 1;

		if (ddx > 0) {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfclr	(a5){d4:1}");
				if ((s += c) >= 0) {
					s -= d;
					x++;
				}
#else /* mc68020 */
				*addr &= ~bit;
				if ((s += c) >= 0) {
					s -= d;
					bit >>= 1;
					if (!bit) {
						bit = leftbit;
						addr++;
					}
				}
#endif /* mc68020 */
				addr += ddy;
			}
		} else {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfclr	(a5){d4:1}");
				if ((s += c) >= 0) {
					s -= d;
					x--;
				}
#else /* mc68020 */
				*addr &= ~bit;
				if ((s += c) >= 0) {
					s -= d;
					bit <<= 1;
					if (!bit) {
						bit = rightbit;
						addr--;
					}
				}
#endif /* mc68020 */
				addr += ddy;
			}
		}
	}
}

void
mfb_invvvector32(addr, nlwidth, x, len, lpf)
register u_int *addr;
register int nlwidth;
register int x;
register int len;
{
	register u_int bitmask;

	if (len < 0) {
		nlwidth = -nlwidth;
		len = -len;
	}
	if (lpf)
		len++;
	addr += (x >> 5);

	bitmask = mfbmask32[x & 0x1f];

	Duff(len, *addr ^= bitmask; addr += nlwidth)
}

void
mfb_invhvector32(addr, x, len, lpf)
register u_int *addr;
int x;
int len;
int lpf;
{
	register u_int	startmask;
	register u_int 	endmask;
	register int nl, off;

	if (len < 0) {
		x += len;
		len = -len;
		if (lpf) {
			len++;
		} else {
			x++;
		}
	} else {
		if (lpf) {
			len++;
		}
	}
	addr += (x >> 5);

	off = x & 0x1f;
	if (off + len < 32) {
		*addr ^= mfbpartmasks32[off][len & 0x1f];
	} else {
		startmask = mfbstarttab32[off];
		endmask = mfbendtab32[(x + len) & 0x1f];
		if (startmask) {
			nl = (len - (32 - off)) >> 5;
			*addr++ ^= startmask;
		} else
			nl = len >> 5;
		Duff_single(nl, addr, ^= ~0);
		if (endmask)
			*addr ^= endmask;
	}
}

void
mfb_invvector32(fb, addr, ddy, p0, p1, lpf)
struct fbdev	*fb;
register u_int *addr;
register int	ddy;
register lPoint *p0, *p1;
int lpf;		/* if 0, don't draw last point */
{
	register int i;
	register int lim;
#ifdef mc68020
	register int x = p0->x;
#else /* mc68020 */
	register u_int bit, leftbit, rightbit;
#endif /* mc68020 */
	int ddx;
	int dx = p1->x - p0->x;
	int dy = p1->y - p0->y;
	int s, d, c;

	addr += (p0->y * ddy);

	if (dx == 0) {
		mfb_invvvector32(addr, ddy, p0->x, dy, lpf);
		return;
	}
	if (dy == 0) {
		mfb_invhvector32(addr, p0->x, dx, lpf);
		return;
	}

	if (dx < 0) {
		dx = -dx;
		ddx = -1;
	} else {
		ddx = 1;
	}
	if (dy < 0) {
		dy = -dy;
		ddy = -ddy;
	}
	
#ifndef mc68020
	bit = mfbmask32[p0->x & 0x1f];
	leftbit = mfbmask32[0];
	rightbit = mfbmask32[31];
	addr += (p0->x >> 5);
#endif /* mc68020 */
	if (dx > dy) {	/* case x */
		lim = dx;
		if (lpf)
			lim++;

		s = -dx;
		d = dx << 1;
		c = dy << 1;

		if (ddx > 0) {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfchg	(a5){d4:1}");
				x++;
#else /* mc68020 */
				*addr ^= bit;
				bit >>= 1;
				if (!bit) {
					bit = leftbit;
					addr++;
				}
#endif /* mc68020 */
				if ((s += c) >= 0) {
					s -= d;
					addr += ddy;
				}
			}
		} else {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfchg	(a5){d4:1}");
				x--;
#else /* mc68020 */
				*addr ^= bit;
				bit <<= 1;
				if (!bit) {
					bit = rightbit;
					addr--;
				}
#endif /* mc68020 */
				if ((s += c) >= 0) {
					s -= d;
					addr += ddy;
				}
			}
		}
	} else {			/* case y */
		lim = dy;
		if (lpf)
			lim++;
		s = -dy;
		d = dy << 1;
		c = dx << 1;

		if (ddx > 0) {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfchg	(a5){d4:1}");
				if ((s += c) >= 0) {
					s -= d;
					x++;
				}
#else /* mc68020 */
				*addr ^= bit;
				if ((s += c) >= 0) {
					s -= d;
					bit >>= 1;
					if (!bit) {
						bit = leftbit;
						addr++;
					}
				}
#endif /* mc68020 */
				addr += ddy;
			}
		} else {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfchg	(a5){d4:1}");
				if ((s += c) >= 0) {
					s -= d;
					x--;
				}
#else /* mc68020 */
				*addr ^= bit;
				if ((s += c) >= 0) {
					s -= d;
					bit <<= 1;
					if (!bit) {
						bit = rightbit;
						addr--;
					}
				}
#endif /* mc68020 */
				addr += ddy;
			}
		}
	}
}

void
mfb_setvvector32(addr, nlwidth, x, len, lpf)
register u_int *addr;
register int nlwidth;
register int x;
register int len;
{
	register u_int bitmask;

	if (len < 0) {
		nlwidth = -nlwidth;
		len = -len;
	}
	if (lpf)
		len++;
	addr += (x >> 5);

	bitmask = mfbmask32[x & 0x1f];

	Duff(len, *addr |= bitmask; addr += nlwidth)
}

void
mfb_sethvector32(addr, x, len, lpf)
register u_int *addr;
int x;
int len;
int lpf;
{
	register u_int startmask;
	register u_int endmask;
	register int nl, off;

	if (len < 0) {
		x += len;
		len = -len;
		if (lpf) {
			len++;
		} else {
			x++;
		}
	} else {
		if (lpf) {
			len++;
		}
	}
	addr += (x >> 5);

	off = x & 0x1f;
	if (off + len < 32) {
		*addr |= mfbpartmasks32[off][len & 0x1f];
	} else {
		startmask = mfbstarttab32[off];
		endmask = mfbendtab32[(x + len) & 0x1f];
		if (startmask) {
			nl = (len - (32 - off)) >> 5;
			*addr++ |= startmask;
		} else
			nl = len >> 5;
#ifdef mc68020
		;
		asm(" move.l #-1,d3");
		Duff(nl, asm(" move.l d3,(a5)+"))
#else /* mc68020 */
		Duff_single(nl, addr, = ~0);
#endif /* mc68020 */
		if (endmask)
			*addr |= endmask;
	}
}

void
mfb_setvector32(fb, addr, ddy, p0, p1, lpf)
struct fbdev	*fb;
register u_int *addr;
register int ddy;
register lPoint *p0, *p1;
int lpf;		/* if 0, don't draw last point */
{
	register int i;
	register int lim;
#ifdef mc68020
	register int x = p0->x;
#else /* mc68020 */
	register u_int bit, leftbit, rightbit;
#endif /* mc68020 */
	int ddx;
	int dx = p1->x - p0->x;
	int dy = p1->y - p0->y;
	int s, d, c;

	ddx = 1;
	addr += (p0->y * ddy);

	if (dx == 0) {
		mfb_setvvector32(addr, ddy, p0->x, dy, lpf);
		return;
	}
	if (dy == 0) {
		mfb_sethvector32(addr, p0->x, dx, lpf);
		return;
	}

	if (dx < 0) {
		dx = -dx;
		ddx = -ddx;
	}
	if (dy < 0) {
		dy = -dy;
		ddy = -ddy;
	}
	
#ifndef mc68020
	bit = mfbmask32[p0->x & 0x1f];
	leftbit = mfbmask32[0];
	rightbit = mfbmask32[31];
	addr += (p0->x >> 5);
#endif /* mc68020 */
	if (dx > dy) {	/* case x */
		lim = dx;
		if (lpf)
			lim++;

		s = -dx;
		d = dx << 1;
		c = dy << 1;

		if (ddx > 0) {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfset	(a5){d4:1}");
				x++;
#else /* mc68020 */
				*addr |= bit;
				bit >>= 1;
				if (!bit) {
					bit = leftbit;
					addr++;
				}
#endif /* mc68020 */
				if ((s += c) >= 0) {
					s -= d;
					addr += ddy;
				}
			}
		} else {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfset	(a5){d4:1}");
				x--;
#else /* mc68020 */
				*addr |= bit;
				bit <<= 1;
				if (!bit) {
					bit = rightbit;
					addr--;
				}
#endif /* mc68020 */
				if ((s += c) >= 0) {
					s -= d;
					addr += ddy;
				}
			}
		}
	} else {			/* case y */
		lim = dy;
		if (lpf)
			lim++;
		s = -dy;
		d = dy << 1;
		c = dx << 1;

		if (ddx > 0) {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfset	(a5){d4:1}");
				if ((s += c) >= 0) {
					s -= d;
					x++;
				}
#else /* mc68020 */
				*addr |= bit;
				if ((s += c) >= 0) {
					s -= d;
					bit >>= 1;
					if (!bit) {
						bit = leftbit;
						addr++;
					}
				}
#endif /* mc68020 */
				addr += ddy;
			}
		} else {
			for (i = lim; i > 0; i--) {
#ifdef mc68020
				asm("	bfset	(a5){d4:1}");
				if ((s += c) >= 0) {
					s -= d;
					x--;
				}
#else /* mc68020 */
				*addr |= bit;
				if ((s += c) >= 0) {
					s -= d;
					bit <<= 1;
					if (!bit) {
						bit = rightbit;
						addr--;
					}
				}
#endif /* mc68020 */
				addr += ddy;
			}
		}
	}
}

void
mfb_point(p, x, s, f)
register u_int *p;
register int x;
register u_int s;
register char *f;
{
#ifdef mc68020
	asm("	andi.l	#31, d7");
	asm("	move.l	d7,d0");
	asm("	neg.l	d7");
	asm("	addi.l	#31, d7");

	asm("	move.l	(a5), d1");
	asm("	lsr.l	d7, d1");
	asm("	andi.l	#1, d1");

	asm("	andi.l	#1, d6");
	asm("	lsl.l	#1, d6");
	asm("	or.l	d6, d1");
	asm("	neg.l	d1");
	asm("	addq.l	#3, d1");

	asm("	btst.b	d1, (a4)");
	asm("	beq	bcl");
	asm("	bfset	(a5){d0:1}");
	asm("	bra	bend");
	asm("bcl:	bfclr	(a5){d0:1}");
	asm("bend:	");
#else /* mc68020 */
	x = 31 - (x & 31);
	if ((1 << (3 - (((s & 1) << 1) | ((*p >> x) & 1)))) & *f)
		*p |= (1 << x);
	else
		*p &= ~(1 << x);
#endif /* mc68020 */
}

void
mfb_vector32(fb, addr, ddy, p0, p1, lpf)
struct fbdev	*fb;
register u_int *addr;
int	ddy;
register lPoint *p0, *p1;
int lpf;		/* if 0, don't draw last point */
{
	register char *fp = fb->funcvec;
	register int x = p0->x;
	register u_int pat = fb->pat;
	int lim;
	register int i;
	register int ddx;
	int s, d, c;
	int dx = p1->x - x;
	int dy = p1->y - p0->y;

	ddx = 1;
	addr += (p0->y * ddy);

	if (dx == 0) {
		ddx = 0;
	} else if (dx < 0) {
		dx = -dx;
		ddx = -ddx;
	}

	if (dy == 0)
		ddy = 0;
	else if (dy < 0) {
		dy = -dy;
		ddy = -ddy;
	}
	
	if (dx > dy) {			/* case x */
		lim = dx;
		if (lpf)
			lim++;

		s = -dx;
		d = dx << 1;
		c = dy << 1;

		for (i = lim; i > 0; i--) {
#ifdef mc68020
			asm(" rol.l	#1, d6 ");
#else /* mc68020 */
			pat = (pat << 1) | ((pat & 0x80000000) ? 1: 0);
#endif /* mc68020 */
			mfb_point(addr + (x >> 5), x, pat, fp);

			if ((s += c) >= 0) {
				s -= d;
				addr += ddy;
			}

			x += ddx;
		}
	} else {			/* case y */
		lim = dy;
		if (lpf)
			lim++;
		s = -dy;
		d = dy << 1;
		c = dx << 1;

		for (i = lim; i > 0; i--) {
#ifdef mc68020
			asm(" rol.l	#1, d6 ");
#else /* mc68020 */
			pat = (pat << 1) | ((pat & 0x80000000) ? 1: 0);
#endif /* mc68020 */
			mfb_point(addr + (x >> 5), x, pat, fp);

			if ((s += c) >= 0) {
				s -= d;
				x += ddx;
			}

			addr += ddy;
		}
	}
	
	/* rotate pattern */
	pat = fb->pat;

#ifdef mc68020
	asm("	move.l	(-8, fp), d0");
	asm("	andi.l	#31, d0");
	asm("	rol.l	d0, d6");
#else /* mc68020 */
	{
		register int tmp;

		tmp = lim & 31;
		pat = (pat << tmp) | (pat >> (32 - tmp));
	}
#endif /* mc68020 */

	fb->pat = pat;
}

void
mem_to_mem(func, mapSrc, offSrc, widthSrc, mapDst, offDst, widthDst, sr, dp)
int		func;
struct fb_map	*mapSrc;
int		offSrc;
u_int		widthSrc;
struct fb_map	*mapDst;
int		offDst;
u_int		widthDst;
register lRectangle *sr;	/* source rectangle */
register lPoint	 *dp;	/* destination point */
{
	register u_short *addrSrc;
	register u_short *addrDst;

	addrSrc = (u_short *)TypeAt(mapSrc, offSrc);
	addrDst = (u_short *)TypeAt(mapDst, offDst);

	if ((!((u_int)addrSrc & 3) && !(widthSrc & 1)) &&
	    (!((u_int)addrDst & 3) && !(widthDst & 1))) {
		switch (func) {

		case BF_0:
			mfb_clr_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, widthDst / 2);
			break;
		case BF_S:
			mfb_copy_area32(addrSrc, addrDst,
			    widthSrc / 2, widthDst / 2, sr, dp);
			break;
		case BF_D:
			break;
		case BF_SDX:
			mfb_xor_area32(addrSrc, addrDst,
			    widthSrc / 2, widthDst / 2, sr, dp);
			break;
		case BF_SDO:
			mfb_or_area32(addrSrc, addrDst,
			    widthSrc / 2, widthDst / 2, sr, dp);
			break;
		case BF_DI:
			mfb_inv_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, widthDst / 2);
			break;
		case BF_SI:
			mfb_copyinv_area32(addrSrc, addrDst,
			    widthSrc / 2, widthDst / 2, sr, dp);
			break;
		case BF_1:
			mfb_set_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, widthDst / 2);
			break;
		default:
			mfb_general_area32(func, addrSrc, addrDst,
			    widthSrc / 2, widthDst / 2, sr, dp);
			break;
		}
	} else {
		switch (func) {

		case BF_0:
			mfb_clr_area16(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, widthDst);
			break;
		case BF_S:
			mfb_copy_area16(addrSrc, addrDst,
			    widthSrc, widthDst, sr, dp);
			break;
		case BF_D:
			break;
		case BF_SDX:
			mfb_xor_area16(addrSrc, addrDst,
			    widthSrc, widthDst, sr, dp);
			break;
		case BF_SDO:
			mfb_or_area16(addrSrc, addrDst,
			    widthSrc, widthDst, sr, dp);
			break;
		case BF_DI:
			mfb_inv_area16(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, widthDst);
			break;
		case BF_SI:
			mfb_copyinv_area16(addrSrc, addrDst,
			    widthSrc, widthDst, sr, dp);
			break;
		case BF_1:
			mfb_set_area16(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, widthDst);
			break;
		default:
			mfb_general_area16(func, addrSrc, addrDst,
			    widthSrc, widthDst, sr, dp);
			break;
		}
	}
}

void
mem_clear(func, map, offset, width, dr, mode)
register int	func;
struct fb_map	*map;
int		offset;
u_int		width;
register lRectangle *dr;
int		mode;
{
	u_short *addr;

	if (!mode)
		func >>= 2;
	func &= 0x3;

	addr = (u_short *)TypeAt(map, offset);

	if (!((u_int)addr & 3) && !(width & 1)) {
		switch (func) {

		case 0:
			mfb_clr_area32(dr->origin.x, dr->origin.y,
			    dr->extent.x, dr->extent.y, addr, width / 2);
			break;
		case 2:
			mfb_inv_area32(dr->origin.x, dr->origin.y,
			    dr->extent.x, dr->extent.y, addr, width / 2);
			break;
		case 3:
			mfb_set_area32(dr->origin.x, dr->origin.y,
			    dr->extent.x, dr->extent.y, addr, width / 2);
			break;
		}
	} else {
		switch (func) {

		case 0:
			mfb_clr_area16(dr->origin.x, dr->origin.y,
			    dr->extent.x, dr->extent.y, addr, width);
			break;
		case 2:
			mfb_inv_area16(dr->origin.x, dr->origin.y,
			    dr->extent.x, dr->extent.y, addr, width);
			break;
		case 3:
			mfb_set_area16(dr->origin.x, dr->origin.y,
			    dr->extent.x, dr->extent.y, addr, width);
			break;
		}
	}
}

#ifdef CPU_SINGLE
#define	VRAM_START(fb)		(((struct mfbdev *)(fb)->private)->vram_start)
#define	VRAM_WIDTH(fb)		(((struct mfbdev *)(fb)->private)->vram_width)

fbmem_rop_init(fb, func)
struct fbdev	*fb;
char	*func;
{
	fb->func = *func;
}

void
fbmem_rop_winit(fb)
struct fbdev	*fb;
{
}

void
fbmem_rop_copy(fb, sr, dp, mode, wmask)
register struct fbdev	*fb;
register lRectangle	*sr;	/* source rectangle */
register lPoint		*dp;	/* destination point */
{
	if (!(wmask & 1)) {
		return;
	}

	switch (fb->func) {
	case BF_0:
		mfb_clr_area32(dp->x, dp->y,
		    sr->extent.x, sr->extent.y, VRAM_START(fb), VRAM_WIDTH(fb));
		break;
	case BF_S:
		mfb_copy_area32(VRAM_START(fb), VRAM_START(fb),
		    VRAM_WIDTH(fb), VRAM_WIDTH(fb), sr, dp);
		break;
	case BF_D:
		break;
	case BF_SDX:
		mfb_xor_area32(VRAM_START(fb), VRAM_START(fb),
		    VRAM_WIDTH(fb), VRAM_WIDTH(fb), sr, dp);
		break;
	case BF_SDO:
		mfb_or_area32(VRAM_START(fb), VRAM_START(fb),
		    VRAM_WIDTH(fb), VRAM_WIDTH(fb), sr, dp);
		break;
	case BF_DI:
		mfb_inv_area32(dp->x, dp->y,
		    sr->extent.x, sr->extent.y, VRAM_START(fb), VRAM_WIDTH(fb));
		break;
	case BF_SI:
		mfb_copyinv_area32(VRAM_START(fb), VRAM_START(fb),
		    VRAM_WIDTH(fb), VRAM_WIDTH(fb), sr, dp);
		break;
	case BF_1:
		mfb_set_area32(dp->x, dp->y,
		    sr->extent.x, sr->extent.y, VRAM_START(fb), VRAM_WIDTH(fb));
		break;
	default:
		mfb_general_area32(fb->func, VRAM_START(fb), VRAM_START(fb),
		    VRAM_WIDTH(fb), VRAM_WIDTH(fb), sr, dp);
		break;
	}
}

void
fbmem_rop_read(fb, map, offset, width, sr, dp, rplane, wplane)
register struct fbdev	*fb;
struct fb_map	*map;
u_int		offset;
u_int		width;
register lRectangle *sr;	/* source rectangle */
register lPoint	*dp;	/* destination point */
int		rplane;
int		wplane;
{
	register u_short *addrDst;

	addrDst = (u_short *)TypeAt(map, offset);

	if (!((u_int)addrDst & 3) && !(width & 1)) {
		switch (fb->funcvec[wplane]) {
		case BF_0:
			mfb_clr_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, width / 2);
			break;
		case BF_S:
			mfb_copy_area32(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb), width / 2, sr, dp);
			break;
		case BF_D:
			break;
		case BF_SDX:
			mfb_xor_area32(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb), width / 2, sr, dp);
			break;
		case BF_SDO:
			mfb_or_area32(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb), width / 2, sr, dp);
			break;
		case BF_DI:
			mfb_inv_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, width / 2);
			break;
		case BF_SI:
			mfb_copyinv_area32(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb), width / 2, sr, dp);
			break;
		case BF_1:
			mfb_set_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, width / 2);
			break;
		default:
			mfb_general_area32(fb->funcvec[wplane], VRAM_START(fb),
			    addrDst, VRAM_WIDTH(fb), width/2, sr, dp);
			break;
		}
	} else {
		switch (fb->funcvec[wplane]) {
		case BF_0:
			mfb_clr_area16(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, width);
			break;
		case BF_S:
			mfb_copy_area16(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb) * 2, width, sr, dp);
			break;
		case BF_D:
			break;
		case BF_SDX:
			mfb_xor_area16(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb) * 2, width, sr, dp);
			break;
		case BF_SDO:
			mfb_or_area16(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb) * 2, width, sr, dp);
			break;
		case BF_DI:
			mfb_inv_area16(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, width);
			break;
		case BF_SI:
			mfb_copyinv_area16(VRAM_START(fb), addrDst,
			    VRAM_WIDTH(fb) * 2, width, sr, dp);
			break;
		case BF_1:
			mfb_set_area16(dp->x, dp->y,
			    sr->extent.x, sr->extent.y, addrDst, width);
			break;
		default:
			mfb_general_area16(fb->funcvec[wplane], VRAM_START(fb),
			    addrDst, VRAM_WIDTH(fb)*2, width, sr, dp);
			break;
		}
	}
}

void
fbmem_rop_write(fb, map, offset, width, sr, dp, wmask)
register struct fbdev *fb;
struct fb_map	*map;
int		offset;
u_int		width;
register lRectangle *sr;	/* source rectangle */
register lPoint	*dp;	/* destination point */
int		wmask;
{
	register u_short *addrSrc;

	addrSrc = (u_short *)TypeAt(map, offset);

	if (!(wmask & 1)) {
		return;
	}

	if (!((u_int)addrSrc & 3) && !(width & 1)) {
		switch (fb->funcvec[0]) {

		case BF_0:
			mfb_clr_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y,
			    VRAM_START(fb), VRAM_WIDTH(fb));
			break;
		case BF_S:
			mfb_copy_area32(addrSrc, VRAM_START(fb),
			    width / 2, VRAM_WIDTH(fb), sr, dp);
			break;
		case BF_D:
			break;
		case BF_SDX:
			mfb_xor_area32(addrSrc, VRAM_START(fb),
			    width / 2, VRAM_WIDTH(fb), sr, dp);
			break;
		case BF_SDO:
			mfb_or_area32(addrSrc, VRAM_START(fb),
			    width / 2, VRAM_WIDTH(fb), sr, dp);
			break;
		case BF_DI:
			mfb_inv_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y,
			    VRAM_START(fb), VRAM_WIDTH(fb));
			break;
		case BF_SI:
			mfb_copyinv_area32(addrSrc, VRAM_START(fb),
			    width / 2, VRAM_WIDTH(fb), sr, dp);
			break;
		case BF_1:
			mfb_set_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y,
			    VRAM_START(fb), VRAM_WIDTH(fb));
			break;
		default:
			mfb_general_area32(fb->funcvec[0], addrSrc,
			    VRAM_START(fb), width / 2, VRAM_WIDTH(fb), sr, dp);
			break;
		}
	} else {
		switch (fb->funcvec[0]) {
		case BF_0:
			mfb_clr_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y,
			    VRAM_START(fb), VRAM_WIDTH(fb));
			break;
		case BF_S:
			mfb_copy_area16(addrSrc, VRAM_START(fb),
			    width, VRAM_WIDTH(fb) * 2, sr, dp);
			break;
		case BF_D:
			break;
		case BF_SDX:
			mfb_xor_area16(addrSrc, VRAM_START(fb),
			    width, VRAM_WIDTH(fb) * 2, sr, dp);
			break;
		case BF_SDO:
			mfb_or_area16(addrSrc, VRAM_START(fb),
			    width, VRAM_WIDTH(fb) * 2, sr, dp);
			break;
		case BF_DI:
			mfb_inv_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y,
			    VRAM_START(fb), VRAM_WIDTH(fb));
			break;
		case BF_SI:
			mfb_copyinv_area16(addrSrc, VRAM_START(fb),
			    width, VRAM_WIDTH(fb) * 2, sr, dp);
			break;
		case BF_1:
			mfb_set_area32(dp->x, dp->y,
			    sr->extent.x, sr->extent.y,
			    VRAM_START(fb), VRAM_WIDTH(fb));
			break;
		default:
			mfb_general_area16(fb->funcvec[0], addrSrc,
			    VRAM_START(fb), width, VRAM_WIDTH(fb) * 2, sr, dp);
			break;
		}
	}
}

void
fbmem_rop_cinit(fb, wplane, sw)
struct fbdev	*fb;
{
	fb->Pmask = wplane;
	fb->Mode = sw;
}

void
fbmem_rop_clear(fb, dr)
register struct fbdev *fb;
register lRectangle *dr;
{
	register int func;

	if (!(fb->Pmask & 1)) {
		return;
	}
	func = fb->funcvec[0];
	if (!fb->Mode)
		func >>= 2;
	func &= 3;

	switch (func) {
	case 0:
		mfb_clr_area32(dr->origin.x, dr->origin.y,
		    dr->extent.x, dr->extent.y, VRAM_START(fb), VRAM_WIDTH(fb));
		break;
	case 2:
		mfb_inv_area32(dr->origin.x, dr->origin.y,
		    dr->extent.x, dr->extent.y, VRAM_START(fb), VRAM_WIDTH(fb));
		break;
	case 3:
		mfb_set_area32(dr->origin.x, dr->origin.y,
		    dr->extent.x, dr->extent.y, VRAM_START(fb), VRAM_WIDTH(fb));
		break;
	}
}

void
fbmem_rop_vect(fb, clip, ropf, forc, auxc, transp, wplane,
						np, ps, lptn, lpf, joint)
register struct fbdev *fb;
register lRectangle *clip;
int ropf, forc, auxc, transp;
register int np;
register lPoint *ps;
register u_int lptn;
{
	lPoint p0, p1;
	register void (*line_func)();
	register int func;

	if (!(wplane & 1))
		return;
	linerop(fb, ropf, forc, auxc, transp);
	func = fb->funcvec[0];
	if (lptn == 0xffffffff || lptn == 0) {
		if (!lptn)
			func >>= 2;
		switch (func & 3) {
		case 0:
			line_func = mfb_clrvector32;
			break;
		case 1:
			return;
		case 2:
			line_func = mfb_invvector32;
			break;
		default:
			line_func = mfb_setvector32;
			break;
		}
	} else
		line_func = mfb_vector32;
	if (joint) {
		fb->pat = lptn;
		p0 = *ps++;
		np--;
		if (clip) {
			while (--np > 0) {
				p1 = *ps;
				if (lineclip(&p0, &p1, clip)) {
					(*line_func)(fb,
						VRAM_START(fb), VRAM_WIDTH(fb),
						&p0, &p1,
						ps->x != p1.x || ps->y != p1.y);
				}
				p0 = *ps++;
			}
			p1 = *ps;
			if (lineclip(&p0, &p1, clip)) {
				(*line_func)(fb, VRAM_START(fb), VRAM_WIDTH(fb),
					&p0, &p1,
					ps->x != p1.x || ps->y != p1.y || lpf);
			}
		} else {
			while (--np > 0) {
				p1 = *ps;
				(*line_func)(fb, VRAM_START(fb), VRAM_WIDTH(fb),
					&p0, &p1, 0);
				p0 = *ps++;
			}
			p1 = *ps;
			(*line_func)(fb, VRAM_START(fb), VRAM_WIDTH(fb),
				&p0, &p1, lpf);
		}
	} else {
		np >>= 1;
		if (lpf) {
			if (clip) {
				while (--np >= 0) {
					p0 = *ps++;
					p1 = *ps++;
					fb->pat = lptn;
					if (lineclip(&p0, &p1, clip)) {
						(*line_func)(fb,
							VRAM_START(fb),
							VRAM_WIDTH(fb),
							&p0, &p1, 1);
					}
				}
			} else {
				while (--np >= 0) {
					p0 = *ps++;
					p1 = *ps++;
					fb->pat = lptn;
					(*line_func)(fb,
						VRAM_START(fb), VRAM_WIDTH(fb),
						&p0, &p1, 1);
				}
			}
		} else {
			if (clip) {
				while (--np >= 0) {
					p0 = *ps++;
					p1 = *ps;
					fb->pat = lptn;
					if (lineclip(&p0, &p1, clip)) {
						(*line_func)(fb,
							VRAM_START(fb),
							VRAM_WIDTH(fb),
							&p0, &p1,
							ps->x != p1.x ||
							ps->y != p1.y);
					}
					ps++;
				}
			} else {
				while (--np >= 0) {
					p0 = *ps++;
					p1 = *ps++;
					fb->pat = lptn;
					(*line_func)(fb,
						VRAM_START(fb), VRAM_WIDTH(fb),
						&p0, &p1, 0);
				}
			}
		}
	}
}

#define	mfb_clrdot32(fb, addr, ddy, p) \
{ *((u_int *)addr + (p->y * ddy) + (p->x >> 5)) &= mfbrmask32[p->x & 0x1f]; }

#define	mfb_invdot32(fb, addr, ddy, p) \
{ *((u_int *)addr + (p->y * ddy) + (p->x >> 5)) ^= mfbmask32[p->x & 0x1f]; }

#define	mfb_setdot32(fb, addr, ddy, p) \
{ *((u_int *)addr + (p->y * ddy) + (p->x >> 5)) |= mfbmask32[p->x & 0x1f]; }

void
fbmem_rop_dot_BF_clr(fb, clip, np, ps)
register struct fbdev *fb;
lRectangle *clip;
register int np;
register lPoint *ps;
{
	register int x0, y0, x1, y1;

	if (clip) {
		x0 = clip->origin.x;
		y0 = clip->origin.y;
		x1 = x0 + clip->extent.x - 1;
		y1 = y0 + clip->extent.y - 1;
		if (x1 <= 0 || y1 <= 0) return;

		while (np-- > 0) {
			if ((ps->x >= x0) && (ps->y >= y0)
			 && (ps->x <= x1) && (ps->y <= y1))
				mfb_clrdot32(fb, VRAM_START(fb), VRAM_WIDTH(fb), ps);
			ps++;
		}
	} else {
		while (np-- > 0) {
			mfb_clrdot32(fb, VRAM_START(fb), VRAM_WIDTH(fb), ps);
			ps++;
		}
	}
}

void
fbmem_rop_dot_BF_inv(fb, clip, np, ps)
register struct fbdev *fb;
lRectangle *clip;
register int np;
register lPoint *ps;
{
	register int x0, y0, x1, y1;

	if (clip) {
		x0 = clip->origin.x;
		y0 = clip->origin.y;
		x1 = x0 + clip->extent.x - 1;
		y1 = y0 + clip->extent.y - 1;
		if (x1 <= 0 || y1 <= 0) return;

		while (np-- > 0) {
			if ((ps->x >= x0) && (ps->y >= y0)
			 && (ps->x <= x1) && (ps->y <= y1))
				mfb_invdot32(fb, VRAM_START(fb), VRAM_WIDTH(fb), ps);
			ps++;
		}
	} else {
		while (np-- > 0) {
			mfb_invdot32(fb, VRAM_START(fb), VRAM_WIDTH(fb), ps);
			ps++;
		}
	}
}

void
fbmem_rop_dot(fb, clip, ropf, forc, auxc, transp, wplane, np, ps)
register struct fbdev *fb;
lRectangle *clip;
int ropf, forc, auxc, transp;
register int np;
register lPoint *ps;
{
	register int x0, y0, x1, y1;

	if (!(wplane & 1))
		return;

	linerop(fb, ropf, forc, auxc, transp);

	switch (fb->funcvec[0] & 3) {
	case 1:
		break;

	case 0:
		fbmem_rop_dot_BF_clr(fb, clip, np, ps);
		break;
	case 2:
		fbmem_rop_dot_BF_inv(fb, clip, np, ps);
		break;

	default:
		if (clip) {
			x0 = clip->origin.x;
			y0 = clip->origin.y;
			x1 = x0 + clip->extent.x - 1;
			y1 = y0 + clip->extent.y - 1;
			if (x1 <= 0 || y1 <= 0) return;

			while (np-- > 0) {
				if ((ps->x >= x0) && (ps->y >= y0)
				 && (ps->x <= x1) && (ps->y <= y1))
					mfb_setdot32(fb,
					    VRAM_START(fb), VRAM_WIDTH(fb), ps);
				ps++;
			}
		} else {
			while (np-- > 0) {
				mfb_setdot32(fb,
				    VRAM_START(fb), VRAM_WIDTH(fb), ps);
				ps++;
			}
		}
	}
}

#ifdef notdef

#ifndef mfb_clrdot32
void
mfb_clrdot32(fb, addr, ddy, p)
struct fbdev	*fb;
register unsigned int *addr;
register int ddy;
register lPoint *p;
{
	addr += (p->y * ddy) + (p->x >> 5);
	*addr &= mfbrmask32[p->x & 0x1f];
}
#endif /* ! mfb_clrdot32 */

#ifndef mfb_invdot32
void
mfb_invdot32(fb, addr, ddy, p)
struct fbdev	*fb;
register unsigned int *addr;
register int	ddy;
register lPoint *p;
{
	addr += (p->y * ddy) + (p->x >> 5);
	*addr ^= mfbmask32[p->x & 0x1f];
}
#endif /* ! mfb_invdot32 */

#ifndef mfb_setdot32
void
mfb_setdot32(fb, addr, ddy, p)
struct fbdev	*fb;
register unsigned int *addr;
register int ddy;
register lPoint *p;
{
	addr += (p->y * ddy) + (p->x >> 5);
	*addr |= mfbmask32[p->x & 0x1f];
}
#endif /* ! mfb_setdot32 */
#endif

#endif /* CPU_SINGLE */
