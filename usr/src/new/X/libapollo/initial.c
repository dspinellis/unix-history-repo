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

/* initial.c	Routines to open & close display
 *
 *	OpenDisplay		Open it
 *	InitDisplay		Download it
 *	DisplayDead		Check if dead
 *	Allocate_space		Allocate some temporary storage
 *
 */

/*
 *	ToDo:
 *		Look in environment/defaults for programs to start
 */


#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/signal.h>
#include "Xapollo.h"

unsigned char InvertedPixelArray[] = { 
 0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
 0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
 0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
 0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
 0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
 0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
 0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
 0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
 0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
 0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
 0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
 0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
 0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
 0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
 0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
 0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
 0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
 0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
 0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
 0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
 0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
 0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
 0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
 0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
 0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
 0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
 0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
 0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
 0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
 0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
 0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
 0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff,
 };

boolean borrow_flag = true;
long     old_zmask;
int      old_op = -1; 
struct Scr Screen;
int Xdbg;
extern int errno;
DEVICE *CurrentDevice;
gpr_$attribute_desc_t tmp_ab;
static gpr_$offset_t off;
gpr_$bitmap_desc_t TileBM;
gpr_$plane_t plane;
status_$t status;

extern int InputReader();
extern int InitInput();      
extern int check_status();

/*ARGSUSED*/
OpenDisplay (devname)
	char *devname;
{                     
    pad_$window_desc_t win;
    int vsdev = -1;

    win.top = 0;
    win.left = 0;
    win.width = 950;
    win.height = 750;
                       
    off.x_size = 1280;
    off.y_size = 1024;
    if (!borrow_flag || getenv("XPAD") )
        /* direct mode will probably not work correctly */
        {
        borrow_flag = false;
        pad_$create_window( "", (short)0, pad_$transcript, (short)1, win, Screen.fd, status );
        pad_$set_border( (short)Screen.fd, (short)1, 0, status );
        gpr_$init( gpr_$direct, Screen.fd, off, (short)7, Screen.bm, status );
	gpr_$acquire_display(status);
        }
    else
        gpr_$init( gpr_$borrow, (short)1, off, (short)7, Screen.bm, status );

    if( status.all != status_$ok ) {
        error_$print( status );
        return( NULL );             
        }
    vsdev = MakeGPRStream();
    return (vsdev);

}

InitDisplay (info)
	register DEVICE *info;
{
    register int i;
    static vsCursor vsc;
    static vsBox vsm;
    static vsEventQueue vsq = {
			       NULL,
			       0,
			       0,
			       0,
    };

    gpr_$set_bitmap( Screen.bm, status );
    gpr_$inq_bitmap_pointer( Screen.bm, Screen.ptr, Screen.line_width, status );
    gpr_$inq_bitmap_dimensions( Screen.bm, off, (short)plane, status );
    gpr_$allocate_attribute_block( Screen.ab, status);
    gpr_$set_attribute_block( Screen.ab, status );
    gpr_$set_obscured_opt(gpr_$block_if_obs, status);
    gpr_$set_clipping_active(true, status);
    Screen.depth = plane+1;
    Screen.plane_mask = (plane == 0) ? 1 : ((1<<Screen.depth) - 1);
    info->height = Screen.height = off.y_size;
    info->width = Screen.width = off.x_size;

    info->id = Screen.bm;
    info->planes = Screen.depth;
    info->entries = (plane == 0) ? 0 : 1<<Screen.depth;
    info->mouse = &vsc;
    info->mbox = &vsm;
    info->queue = &vsq;  

    /*  Allocate a bitmap for caching tiles */
    gpr_$allocate_attribute_block( tmp_ab, status);
    off.x_size = 32;
    off.y_size = 32;
    gpr_$allocate_bitmap( off, (short)plane, tmp_ab, TileBM, status);

    Define_input_handler(InputReader);
    CurrentDevice = info;

    InitInput();
    return (0); 
}

/* Check if display is dead */

DisplayDead ()
{
	return(0);
}

/* the presumption here is that only one Allocate_space call is made/request */

#define ABUFSIZE 3072
static char ABuffer[3072];	/* arbitrary size buffer for allocate space */
caddr_t AllocateSpace (size)
	register int size;
{
	if (size < ABUFSIZE) return(ABuffer);
	errno = ENOMEM;
	return (NULL);
}

#ifdef	notdef
Setenv (var, value)
/*
   sets the value of var to be arg in the Unix 4.2 BSD environment env.
   Var should end with '='.
   (bindings are of the form "var=value")
   This procedure assumes the memory for the first level of environ
   was allocated using malloc.
 */
register char *var, *value;
{
    extern char **environ;
    register int index = 0;

    while (environ[index] != NULL) {
	if (strncmp(environ[index], var, strlen(var)) == 0) {
	    /* found it */
	    environ[index] = (char *) malloc(strlen(var) + strlen(value));
	    strcpy(environ[index], var);
	    strcat(environ[index], value);
	    return;
	}
	index++;
    }

    if ((environ = (char **) realloc(environ, sizeof(char *) *
				     (index + 2))) == NULL) {
	fprintf(stderr, "Setenv: malloc out of memory\n");
	exit(1);
    }

    environ[index] = (char *) malloc(strlen(var) + strlen(value));
    strcpy(environ[index], var);
    strcat(environ[index], value);
    environ[++index] = NULL;
}
#endif

