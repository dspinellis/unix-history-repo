    /*

    Copyright 1986 by the University of Utah

    Permission to use, copy, modify, and distribute this
    software and its documentation for any purpose and without
    fee is hereby granted, provided that the above copyright
    notice appear in all copies and that both that copyright
    notice and this permission notice appear in supporting
    documentation, and that the name of the University of Utah
    not be used in advertising or publicity pertaining to 
    distribution of the software without specific, written 
    prior permission. The University of Utah makes no
    representations about the suitability of this software for
    any purpose.  It is provided "as is" without express or
    implied warranty.

    */

/* cvtfont.c	Reads an X font from a file and stores an Apollo format
 *              copy on disk
 *
 *	GetFont		Takes a font name and converts the font
 *
 */

#include "Xapollo.h"
#include "vssite.h"
#include "../libvs100/param.h"
#include <errno.h>
#include <sys/file.h>
#include "/sys/ins/smdu.ins.c"

extern int errno;
static gpr_$offset_t off;
status_$t status;

char *strcpy(), *strcat();
long lseek();

#define CHARPERFONT 256

static short ReverseBitsInByte[256] = {
0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0, 0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8, 0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4, 0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec, 0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2, 0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea, 0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6, 0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee, 0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1, 0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9, 0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5, 0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed, 0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3, 0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb, 0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7, 0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef, 0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff,
};

#define ReverseBitsInShort(s) ((ReverseBitsInByte[(s)&0xff]<<8)|ReverseBitsInByte[((s)>>8)&0xff])


BITMAP *
make_bitmap( area, width, height )
short * area;
int width;
int height;
    {

    BITMAP * bm; 
    gpr_$bitmap_desc_t bitmap;
    static gpr_$attribute_desc_t attr = -1;
    gpr_$offset_t size;

    short * pointer;
    short bwid;
    int rowwid;

    register int i;

    if( (bm = (BITMAP *)malloc(sizeof(BITMAP))) == NULL )
        return( NULL );
    
    bm->width = width;
    bm->height = height;
    bm->refcnt = 1;
    bm->kind = (int)apollo_bitmap;

    size.x_size = bm->width;
    size.y_size = bm->height;
    
    gpr_$allocate_attribute_block( attr, status );
    check_status(status, "make_bitmap ");
    gpr_$allocate_bitmap_nc( size, (gpr_$plane_t)0, attr, bitmap, status );
    check_status(status, "make_bitmap ");

    bm->data = (caddr_t)bitmap;
    gpr_$inq_bitmap_pointer( bitmap, pointer, bwid,  status );
                     
    rowwid = ((width+15)>>4);
    for( i=0; i<height; i++ )
        {
        bcopy( area, pointer, rowwid*2 );
        pointer += bwid;
        area += rowwid;
        }

    return( bm );

    }

make_fontfile( fd, name, invert )
FONT *fd;
char *name;
boolean invert;

    {
    FontPriv *fpriv = (FontPriv *)fd->data;
    BITMAP *bm;
    short namelen;
    smd_$font_table_t *ap_font_info; 
    smd_$v1_fdte_t *desc_entry;
    int line_width, i, j;
    short *memptr;
    short *image_ptr;
    long file_length;
    char lname[64];

    strcpy(lname, DEFAULT_APOLLO_FONT_PATH);
    strcat(lname, name);
    bm = fpriv->strike;
    if (invert) {
        gpr_$window_t window;
        gpr_$position_t dest;

        strncat( lname, ".i", 2);
        window.x_coord = 0;
        window.y_coord = 0;
        window.x_size = 224;
        window.y_size = bm->height;
        dest.x_coord = 0;
        dest.y_coord = 0;
        gpr_$set_bitmap( bm->data, status);
        gpr_$set_raster_op( (gpr_$plane_t)0, (gpr_$raster_op_t)12, status );
        gpr_$bit_blt(bm->data, window, (short)0, dest, (short)0, status );
        }
    gpr_$inq_bitmap_pointer(bm->data, (long)memptr, (short)line_width, status);

    namelen = strlen(lname);
    file_length = 224 * 224 + sizeof( smd_$font_table_t ) + 65535;
    ap_font_info = (smd_$font_table_t *)
      ms_$crmapl( *lname, namelen, (long)0, file_length, ms_$nr_xor_1w, status );
    check_status(status, "Creating apollo font file");      

    image_ptr = (short *)&(ap_font_info->desc_table[127]);

    for (i=0; i<bm->height; i++) {
      for (j=0; j<14; j++) {
        *image_ptr = *memptr; 
        image_ptr++;
        memptr++;
        }
      for (j=0; j<50; j++)
        memptr++;
      }
    ap_font_info->v_spacing = 0;
    ap_font_info->h_spacing = 0;		

    ap_font_info->version = 1;
    ap_font_info->chars_in_font = 127;
    ap_font_info->max_up = fd->height;
    ap_font_info->max_width = fpriv->maxwidth;
    ap_font_info->max_right = fpriv->maxwidth;
    ap_font_info->max_down = 0;
    ap_font_info->max_height = fd->height;
    ap_font_info->space_size =  fpriv->widths[fd->space];

    desc_entry = &(ap_font_info->desc_table);
    for (i=0; i<127; i++) {
        ap_font_info->index_table[i] = i+1;
        ap_font_info->desc_table[i].left = 0;
        ap_font_info->desc_table[i].right = fpriv->widths[i];
        ap_font_info->desc_table[i].up = fd->height;
        ap_font_info->desc_table[i].down = 0;
        ap_font_info->desc_table[i].width = fpriv->widths[i];
        ap_font_info->desc_table[i].x_pos = fpriv->leftarray[i] &0777;
        ap_font_info->desc_table[i].y_pos = (fpriv->leftarray[i] >> 10) * fd->height;
        desc_entry++;
        }

    ap_font_info->raster_lines = (--desc_entry)->y_pos + fd->height;
    if (ap_font_info->raster_lines <= (65535 / 28)) 
        ap_font_info->image_size = (ap_font_info->raster_lines + 4) * 28;
    else ap_font_info->image_size = 65535;
    ap_font_info->image_offset = smd_$font_header_size + 
      smd_$font_index_table_size + (smd_$v1_fdte_size * 127) ;
    ms_$truncate( ap_font_info, ap_font_info->image_offset +
		 ap_font_info->image_size, status );
    ms_$unmap( ap_font_info, ap_font_info->image_offset +
	      ap_font_info->image_size, status );
}

FONT *
make_fontmap( fd )
FONT *fd;
    {

    int width;
    int height;
    BITMAP * bm; 
    gpr_$bitmap_desc_t bitmap;
    static gpr_$attribute_desc_t attr = -1;
    gpr_$window_t window;
    gpr_$position_t dest;
    static int got_attr = 0;
    gpr_$offset_t size;
    FontPriv *fpriv;

    short col, row;
    short left, oldleft, wid, oldwid;
    int start, stop;

    register int i;

    fpriv = (FontPriv *)fd->data;
    bm = fpriv->strike;
    width = fd->last * fd->avg_width;
    height = (width / 224) + 1;
    height = height * fd->height;
/* should check for height > ???some max value */
    size.x_size = 224;
    size.y_size = height;
    
    gpr_$allocate_attribute_block( attr, status );
    check_status(status, "make_fontfile ");
    gpr_$acquire_display( status );
    gpr_$allocate_bitmap( size, (gpr_$plane_t)0, attr, bitmap, status );
    check_status(status, "make_fontfile ");

    row = start = col = 0;
    gpr_$set_bitmap( bitmap, status);
    gpr_$set_raster_op(0, (gpr_$raster_op_t)12, status);
    for (i=0; i<=fd->last; i++) {
      left = fpriv->leftarray[i];
      wid = fpriv->widths[i];
      if ((left + wid) > (start + 224)) {
        stop = oldleft + oldwid;
        window.x_coord = start;
        window.y_coord = 0;
        window.x_size = stop - start;
        window.y_size = fd->height;
        dest.x_coord = 0;
        dest.y_coord = row * fd->height;
        gpr_$bit_blt(bm->data, window, (short)0, dest, (short)0, status);
        start = stop;
        row++;
        col = 0;
        }
      oldleft = left;
      oldwid = wid;
      fpriv->leftarray[i] = col + (row << 10);
      col += fpriv->widths[i];
      }
    if (start != (oldleft + oldwid)) {
      window.x_coord = start;
      window.x_size = (left + wid) - start;
      window.y_size = fd->height;
      dest.y_coord = row * fd->height;
      gpr_$bit_blt(bm->data, window, (short)0, dest, (short)0, status);
      check_status( status, make_fontmap );
      }
    bm->width = 224;
    bm->height = height;
    bm->refcnt = 1;
    bm->kind = (int)apollo_bitmap;

    bm->data = (caddr_t)bitmap;
    return( fd );

    }


FONT
*GetFont (name)
	char *name;
{
	char fontname[256];
	int fontfile = -1;
	FontData font;
#define chars ((BitMap *) font.f_characters)
	int fontsize, leftsize, width, i, j;
	char *fontarea;
	register short *leftarea, *leftarray;
	register FONT *fd;
	register FontPriv *fpriv;
	int tablesize = (CHARPERFONT + 1) * sizeof(short);
	extern char *getenv(), *index();
	static char *Fontpath = NULL;
	char *fontpath;
	char lname[40];
	int namelen;

	if ((fontpath = Fontpath) == NULL)
	    fontpath = DEFAULT_FONT_PATH;

	fontfile = open (name, 0, 0);

	while (fontfile < 0) {
	    char *fend;

	    if ((fend = index(fontpath, ':')) != 0)
		*fend = '\0';
	    if (*fontpath == '\0') {
		errno = EINVAL;
		return (NULL);
	    }
	    
	    if (*fontpath == '~') {
		/* XXX - should implement ~foobar as well */
		strcpy (fontname, getenv("HOME"));
		strcat (fontname, "/");
		fontpath++;
	    } else
	        *fontname = '\0';
	    strcat (fontname, fontpath);
	    strcat (fontname, "/");
	    strcat (fontname, name);
	    strcat (fontname, DEFAULT_FONT_SUFFIX);

	    fontfile = open (fontname, 0, 0);
	    if (fend) {
		*fend = ':';
	        fontpath = ++fend;
	    } else
	        break;
	}
	if (read (fontfile, (caddr_t) &font, sizeof (FontData)) != sizeof (FontData)) {
	    close (fontfile);
	    errno = EINVAL;
	    return (NULL);
	}
	Swap_shorts((short *) &font, sizeof (FontData)>>1);

	fontsize = BitmapSize(chars->bm_width, chars->bm_height);
	fontarea = (char *) malloc (fontsize);
	bzero( fontarea, fontsize );
	lseek (fontfile, (long) font.f_characters[0], 0);
	if (read (fontfile, fontarea, fontsize) != fontsize) {
	    close (fontfile);
	    free (fontarea);
	    errno = EINVAL;
	    return (NULL);
	}
	Swap_shorts((short *)fontarea, fontsize>>1);

	leftarea  = (short *) malloc (tablesize);
	bzero(leftarea, tablesize);
	leftarray = (short *) malloc (tablesize);
	if (font.f_fixedWidth == 0) {
	    leftsize = (font.f_lastChar - font.f_firstChar + 2) * sizeof (short);
	    lseek (fontfile, (long) font.f_leftArray[0], 0);
	    if (read (fontfile, & leftarea[font.f_firstChar], leftsize) 
	    		!= leftsize) {
		close (fontfile);
		free (fontarea);
		free ((caddr_t) leftarea);
		free ((caddr_t) leftarray);
		errno = EINVAL;
		return (NULL);
	    }
	    Swap_shorts(((short *)& leftarea[font.f_firstChar]), leftsize>>1);
	} else { /* if fixed with font, generate leftarray for use later */
	    j = 0;
	    for (i = font.f_firstChar; i <= font.f_lastChar + 1; i++) {
		leftarea[i] = j;
		j += font.f_fixedWidth;
	    }
	}
	bcopy(leftarea, leftarray, tablesize);

	close (fontfile);
    /*
     *  set up font data structures
     */

	fd = (FONT *) malloc (sizeof (FONT));

	fd->height = chars->bm_height;
	fd->first = font.f_firstChar;
	fd->last = font.f_lastChar;
	fd->base = font.f_baseline;
	fd->space = font.f_spaceIndex;
	fd->space += fd->first;
	fd->refcnt = 1;



	fd->name = (char *) malloc (strlen (name) + 1);
	strcpy (fd->name, name);


    /*
     * set up the private font structure
     */
	fpriv = (FontPriv *) malloc (sizeof (FontPriv));

	if (fd->avg_width = font.f_fixedWidth) {
	    fd->fixed = 1;
	    fpriv->maxwidth = fd->avg_width;
	    }
	else
	    fd->fixed = 0;
	fd->data = (caddr_t) fpriv;

	fpriv->widths = leftarea;
	fpriv->leftarray = leftarray;
	fpriv->maxwidth = 0;

    /*  of course... the bits are all switched... naturally... of course... */
	{   register short *sh = (short *) fontarea;
	    register short limit = ((chars->bm_width+15)>>4)*chars->bm_height;
	    do *sh = ReverseBitsInShort(*sh), sh++;
	    while (--limit != -1);
	}

	/* convert the leftarray to the width table */
	for (i = fd->first; i <= fd->last; i++) {
	    width = fpriv->leftarray[i + 1] - fpriv->leftarray[i];
	    if (width > fpriv->maxwidth) fpriv->maxwidth = width;
	    if (width < 0) {
	 	    width = 0;	/* font sanity check */
		    errno = EINVAL;
		    return(NULL);
		}
	    fpriv->widths[i] = width;
    }
    fd->avg_width = ((fpriv->leftarray[fd->last]  - 
      	fpriv->leftarray[fd->first]) / (fd->last - fd->first));

    if ((fpriv->strike = make_bitmap( fontarea, chars->bm_width, chars->bm_height) ) == NULL) {
	    free (fontarea);
	    free ((caddr_t) leftarea);
	    free ((caddr_t) leftarray);
	    free ((caddr_t) fd);
	    free ((caddr_t) fpriv);
	    errno = EINVAL;
	    return (NULL);
	}

    make_fontmap(fd);
    make_fontfile(fd, name, false);
    make_fontfile(fd, name, true);
    /*  XXX free fontarea */

	return (fd);
#undef chars
}

OpenDisplay ()
{                     
    gpr_$bitmap_desc_t bm;
                       
    off.x_size = 1024;
    off.y_size = 1024;
    gpr_$init( gpr_$direct, (short)1, off, (short)0, bm, status );
    if ( status.all != status_$ok ) {
        error_$print( status );
        return( NULL );             
        }
    return;

}

#define swaps(x) n = ((char *) (x))[0];\
		 ((char *) (x))[0] = ((char *) (x))[1];\
		 ((char *) (x))[1] = n

Swap_shorts (list, count)
	register short *list;
	register int count;
{
    unsigned int n; 

	while (--count >= 0) {
	    swaps(list);
	    list++;
	}
}

check_status(status, name)
status_$t status;        
char *name;
{
    if (status.all != status_$ok) {
        fprintf(stderr, "%s", name);
        error_$print(status);   
        exit(1);
    }
}
main(argc, argv)
int argc;
char **argv;
{
    char *fontname;

    OpenDisplay();
    fontname = *(++argv);
    GetFont(fontname);
}
