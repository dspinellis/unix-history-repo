/* sunview.c - sunview display process */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/sunview.c,v 7.2 91/02/22 09:29:25 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/sunview.c,v 7.2 91/02/22 09:29:25 mrose Interim $
 *
 *
 * $Log:	sunview.c,v $
 * Revision 7.2  91/02/22  09:29:25  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:40:30  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:01:47  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* sunview display process */

#include <stdio.h>
#include "quipu/photo.h"
#include <suntool/tool_hs.h>
#include <suntool/panel.h>
#include <suntool/gfxsw.h>
#include  <sys/resource.h>

static struct gfxsubwindow *gfx;
static struct tool *tool;
static struct toolsw *gfx_sw;
static int        sx=20,sy=20,x,y;
extern int PIC_LINESIZE;


sigwinched ()
{ 
	tool_sigwinch (tool);
}

photo_start (name)
char * name;
{
char * getenv ();

	/* Initialise a window to recieve a photo of 'name' */

	if (getenv ("WINDOW_PARENT") == (char *)NULL) {
		(void) fprintf (stderr,"PHOTO: Must be running suntools on the console");
		return (-1);
	}

	if (( tool = tool_make (WIN_LABEL,name,WIN_TOP,sx,WIN_LEFT,sy,0)) == NULL) {
		(void) fprintf (stderr,"PHOTO: can't create window");
		return (-1);
	}

	signal (SIGWINCH,sigwinched);

	gfx_sw = gfxsw_createtoolsubwindow (tool,"",TOOL_SWEXTENDTOEDGE,TOOL_SWEXTENDTOEDGE,NULL);
	gfx = (struct gfxsubwindow *) gfx_sw->ts_data;
	gfxsw_getretained (gfx);

	tool_install (tool);

	/* return 0 if sucessful -1 if not */
	return (0);
}


photo_end (name)
char * name;
{
	/* Decoding has finished - display the photo */

	(void) printf ("(See sunview window)");
	(void) fflush (stdout);
	(void) close (1);

	/* return 0 if sucessful -1 if not */
	tool_set_attributes (tool,WIN_WIDTH,PIC_LINESIZE+40,WIN_HEIGHT,sy+40,0);
	tool_select (tool,0);

	return (0);
}

photo_black (length)
int length;
{
	/* draw a black line of 'length' pixels */
}

photo_white (length)
int length;
{
	/* draw a white line of 'length' pixels */
}


photo_line_end (line)
bit_string * line;
{
struct  pixrect * pix;

	/* the end of a line has been reached */
	/* A bit string is stored in line->dbuf_top */

	pix = mem_point (PIC_LINESIZE,1,1,line->dbuf_top);
	pw_write (gfx->gfx_pixwin, sx, sy, PIC_LINESIZE-sx, 1, PIX_SRC,pix,0,0);
	sy++;

}


