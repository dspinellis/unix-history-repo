/*
 *	$Source: /u1/X/libis/RCS/debug.c,v $
 *	$Header: debug.c,v 1.1 86/11/17 14:33:35 swick Rel $
 */

#ifndef lint
static char *rcsid_debug_c = "$Header: debug.c,v 1.1 86/11/17 14:33:35 swick Rel $";
#endif	lint

int isi_debug_jnk;		/* shuts up ranlib complaints*/
#ifdef DEBUG
#include "is-copyright.h"

/*
 *	debug.c		various debugging printf's
 *
 *	printf_pixmap
 *	printf_bitmap
 *	printf_clip
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

#include "Xis.h"

/* debug is used to indicate which debug printf's are turned on. */
unsigned long	debug = (	/* enabled printf's */
		    D_None);

/* disabled printf's
		    D_Color		|
		    D_PixFill		|
		    D_Pixmaps		|
		    D_TileFill		|
		    D_Bitmaps		|
		    D_CopyArea		|
		    D_Cursor		|
		    D_DrawCurve		|
		    D_Font		|
		    D_FontPixmap	|
		    D_PixFill_data	|
		    D_Text		|
		    D_TileFill_data	|
 */

printf_pixmap(name, p)
char	*name;
PIXMAP	*p;
{
    printf("%s=0x%x: width=%d, height=%d, refcnt=%d, tile=%d, kind=0x%x, data=0x%x\n",
	name, p, p->width, p->height, p->refcnt, p->tile, p->kind, p->data);
}

printf_bitmap(name, p)
char	*name;
BITMAP	*p;
{
    printf("%s=0x%x: width=%d, height=%d, refcnt=%d, kind=0x%x, data=0x%x\n",
	name, p, p->width, p->height, p->refcnt, p->kind, p->data);
}

printf_clip(name, clip)
char	*name;
CLIP	clip;
{
    printf("%s left=%d, top=%d, width=%d, height=%d\n",
	name, clip.left, clip.top, clip.width, clip.height);
}
#endif DEBUG
