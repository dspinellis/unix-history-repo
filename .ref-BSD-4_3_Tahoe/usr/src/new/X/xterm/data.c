/*
 *	$Source: /u1/X/xterm/RCS/data.c,v $
 *	$Header: data.c,v 10.102 86/12/01 17:12:39 swick Rel $
 */

#include <setjmp.h>
#include <X/Xlib.h>
#include "scrollbar.h"
#include "ptyx.h"

#ifndef lint
static char sccs_id[] = "@(#)data.c\tX10/6.6B\t12/26/86";
#endif	lint

Vertex T_boxlarge[NBOX] = {
	{0, 0, VertexDontDraw},
	{8, 0, VertexRelative},
	{0, 14, VertexRelative},
	{-8, 0, VertexRelative},
	{0, -14, VertexRelative},
};
Vertex T_box2[NBOX] = {
	{0, 0, VertexDontDraw},
	{7, 0, VertexRelative},
	{0, 12, VertexRelative},
	{-7, 0, VertexRelative},
	{0, -12, VertexRelative},
};
Vertex T_box3[NBOX] = {
	{0, 0, VertexDontDraw},
	{5, 0, VertexRelative},
	{0, 12, VertexRelative},
	{-5, 0, VertexRelative},
	{0, -12, VertexRelative},
};
Vertex T_boxsmall[NBOX] = {
	{0, 0, VertexDontDraw},
	{5, 0, VertexRelative},
	{0, 9, VertexRelative},
	{-5, 0, VertexRelative},
	{0, -9, VertexRelative},
};
Vertex T_boxicon[NBOX] = {		/* is filled-in in TekInit() */
	{0, 0, VertexDontDraw},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
};
jmp_buf Tekend;
int Tbcnt = 0;
char *Tbuffer;
char *Tbptr;
TekLink *TekRefresh;
char *Tpushb;
char *Tpushback;
int Ttoggled = 0;
int bcnt = 0;
char buffer[BUF_SIZE];
char *bptr = buffer;
jmp_buf VTend;
Vertex VTbox[NBOX] = {
	{0, 0, VertexDontDraw},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
};
Vertex status_box[NBOX] = {
	{0, 0, VertexDontDraw},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
};
Vertex icon_box[NBOX] = {
	{0, 0, VertexDontDraw},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
	{0, 0, VertexRelative},
};
T_fontsize Tfontsize[TEKNUMFONTS] = {
	{9, 15},	/* large */
	{8, 13},	/* #2 */
	{6, 13},	/* #3 */
	{6, 10},	/* small */
	{0, 0},		/* icon is filled-in later */
};


#ifdef DEBUG
int debug = 0; 		/* true causes error messages to be displayed */
#endif DEBUG
Terminal term;		/* master data structure for client */
char *xterm_name;	/* argv[0] */
int am_slave = 0;	/* set to 1 if running as a slave process */
char *icon_geom;
int B_Pixel;
Pixmap B_Pixmap;
int L_flag;
int max_plus1;
int n_marginbell = N_MARGINBELL;
int pty_mask;
int re_verse;
int save_lines = SAVELINES;
int Select_mask;
int W_Pixel;
Pixmap W_Pixmap;
char *win_name;
int X_mask;
char *back_color;
char *curs_color;
char *curs_shape = 0;
char *f_b;
char *f_n;
char *f_t;
char *f_i;
char *fore_color;
char *geo_metry;
char *mous_color;
char *T_geometry = 0;
char *ptydev = "/dev/ptyxx";
char *ttydev = "/dev/ttyxx";
char log_def_name[] = "XtermLog.XXXXX";
int T_lastx = -1;
int T_lasty = -1;
int dropmenu = 0;
int doonalarmcode = 0;
int douniquesuffix = 1;
int grayborder = TRUE;
