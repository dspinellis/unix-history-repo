/*
 *	$Source: /u1/X/xterm/RCS/data.h,v $
 *	$Header: data.h,v 10.101 86/12/01 16:57:37 swick Rel $
 */

/* @(#)data.h       X10/6.6B 12/26/86 */
extern Pixmap B_Pixmap;
extern Pixmap W_Pixmap;
extern TekLink *TekRefresh;
extern Terminal term;
extern Vertex T_box2[];
extern Vertex T_box3[];
extern Vertex T_boxlarge[];
extern Vertex T_boxsmall[];
extern Vertex T_boxicon[];
extern Vertex VTbox[];
extern Vertex icon_box[];
extern T_fontsize Tfontsize[];
extern Vertex status_box[];
extern char *T_geometry;
extern char *Tbptr;
extern char *Tbuffer;
extern char *Tpushb;
extern char *Tpushback;
extern char *back_color;
extern char *bptr;
extern char *curs_color;
extern char *curs_shape;
extern char *f_b;
extern char *f_n;
extern char *f_i;
extern char *f_t;
extern char *fore_color;
extern char *geo_metry;
extern char *icon_geom;
extern char log_def_name[];
extern char *mous_color;
extern char *ptydev;
extern char *ttydev;
extern char *win_name;
extern char *xterm_name;
extern char buffer[];
extern int B_Pixel;
extern int L_flag;
extern int Select_mask;
extern int T_lastx;
extern int T_lasty;
extern int Tbcnt;
extern int Ttoggled;
extern int W_Pixel;
extern int X_mask;
extern int am_slave;
extern int bcnt;
#ifdef DEBUG
extern int debug;
#endif DEBUG
extern int errno;
extern int max_plus1;
extern int n_marginbell;
extern int pty_mask;
extern int re_verse;
extern int save_lines;
extern int switchfb[];
extern jmp_buf Tekend;
extern jmp_buf VTend;
extern int dropmenu;
extern int doonalarmcode;
extern int douniquesuffix;
extern int grayborder;
