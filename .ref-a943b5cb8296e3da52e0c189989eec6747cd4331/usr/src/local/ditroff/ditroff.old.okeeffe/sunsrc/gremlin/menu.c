/*
 * @(#)menu.c	1.2  %G%
 *
 * Menu subwindow routines for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include <suntool/menu.h>
#include <errno.h>
#include "gremlin.h"
#include "icondata.h"

/* imports from graphics.c */

extern GRCurrentSetOn();

/*  imports from short.c */

extern int adj[];

/* imports from text.c */

extern TxMsgOK();

/* imports from sun.c */

extern pw_box();

/* imports from main.c */

extern Artmode;			/* type of point layed down */
extern Adjustment;
extern Alignment;
extern GravityOn;
extern menu_fd;
extern struct pixfont *text_pf;
extern struct rect menu_size;
extern ELT *MEN[];
extern SymbolicLines;
extern FLASH_READY;

extern (*lastcommand)();	/* previous command */
extern lasttext;		/* TRUE if previous cmd uses text input */

/* forward references */

extern align_up();
extern align_down();


#define M_CLEAR_POINTS (caddr_t) 1
#define M_SHOW_POINTS (caddr_t) 2
#define M_GRIPE (caddr_t) 3
#define M_POINT (caddr_t) 4

struct menuitem misc_items[] = {
    { MENU_IMAGESTRING, "Clear Points", M_CLEAR_POINTS},
    { MENU_IMAGESTRING, "Show Points", M_SHOW_POINTS},
    { MENU_IMAGESTRING, "Gripe", M_GRIPE},
    { MENU_IMAGESTRING, "Point", M_POINT}
};
				    
struct menu misc_menu = {
    MENU_IMAGESTRING, "Misc",
    sizeof(misc_items) / sizeof(struct menuitem), misc_items, 0, 0
}; 
					       
struct menu *misc_menu_stack = &misc_menu;

#define M_EDIT (caddr_t) 1
#define M_PATH (caddr_t) 2
#define M_READ (caddr_t) 3
#define M_SAVE_SET (caddr_t) 4
#define M_WRITE (caddr_t) 5

struct menuitem file_items[] = {
    { MENU_IMAGESTRING, "Edit", M_EDIT},
    { MENU_IMAGESTRING, "Path", M_PATH},
    { MENU_IMAGESTRING, "Read", M_READ},
    { MENU_IMAGESTRING, "Save Set", M_SAVE_SET},
    { MENU_IMAGESTRING, "Write", M_WRITE}
}; 
								      
struct menu file_menu = {
    MENU_IMAGESTRING, "File",
    sizeof(file_items) / sizeof(struct menuitem), file_items, 0, 0
}; 
								 
struct menu *file_menu_stack = &file_menu;

int icon_x, icon_y;	/* location of currently selected menu icon */
int icon_last = -1;	/* menu index of currently selected menu icon */
int just_x, just_y;	/* location of justify marker within justify icon */
int just_last = -1;	/* justify index of currently selected mode */
struct pixrect *save_just_pr;

#define MNReverseJustify()  pw_write(menu_pw, just_x + 1, just_y, 5, 5, \
		     PIX_SRC ^ PIX_DST, &dot_pr, 0, 0)

/* imports from main.c */

extern struct pixwin *menu_pw;
extern struct toolsw *menu_sw;
extern CFONT;
extern CSIZE;
extern CJUST;
extern CBRUSH;
extern CSTIPPLE;

/* imports from help.c */

extern justify_help(), help(), horizontal_help(), vertical_help(), 
       size1_help(), size2_help(), size3_help(), size4_help(), 
       roman_help(), italics_help(), bold_help(), special_help(),
       scale_help(), move_help(), hmirror_help(), vmirror_help(), 
       include_help(), linestyle_help(), align_help(), pan_help(),
       polygon_help(), bpolygon_help(),
       get1_help(), get2_help(), get3_help(), get4_help(), 
       put1_help(), put2_help(), put3_help(), put4_help(), 
       brush1_help(), brush2_help(), brush3_help(),
       brush4_help(), brush5_help(), brush6_help(), 
       stipple1_help(), stipple2_help(), stipple3_help(), stipple4_help(), 
       stipple5_help(), stipple6_help(), stipple7_help(), stipple8_help(), 
       copy_help(), arrow_help(), text_help(), misc_help(), undo_help(),
       erase_help(), movepoint_help(), rotate_help(), filecabinet_help(),
       boxinc_help(), manhattan_help(), gravity_help(), arc_help(),
       curve_help(), vector_help(), box_help(), grid_help(), 
       littlepoint_help(), menusw_help();

/* imports from sunlong.c */

extern LGBrush1(), LGBrush2(), LGBrush3(), 
       LGBrush4(), LGBrush5(), LGBrush6(),
       LGClearPoints(), LGPath(), LGEdit(),
       LGFont1(), LGFont2(), LGFont3(), LGFont4(), 
       LGGet1(), LGGet2(), LGGet3(), LGGet4(), 
       LGGripe(), LGHMirror(), 
       LGHAdjust(), LGIncludeSet(), LGLineStyle(), LGLittlePoint(), 
       LGMBrush1(), LGMBrush2(), LGMBrush3(), 
       LGMBrush4(), LGMBrush5(), LGMBrush6(), LGMCurve(),
       LGMFont1(), LGMFont2(), LGMFont3(), LGMFont4(),
       LGMJustify(), LGMPan(),
       LGMSize1(), LGMSize2(), LGMSize3(), LGMSize4(), 
       LGMStipple1(), LGMStipple2(), LGMStipple3(), LGMStipple4(), 
       LGMStipple5(), LGMStipple6(), LGMStipple7(), LGMStipple8(), 
       LGMText(), LGMPoint(), LGMVector(), nop(), LGPan(),
       LGPolygon(), LGBPolygon(), LGMPolygon(), LGMBPolygon(),
       LGOPoint(), LGRead(), LGSave(), LGShowPoints(), 
       LGSize1(), LGSize2(), LGSize3(), LGSize4(), 
       LGStipple1(), LGStipple2(), LGStipple3(), LGStipple4(), 
       LGStipple5(), LGStipple6(), LGStipple7(), LGStipple8(),
       LGText(), LGUndo(), LGVAdjust(), LGVMirror(), LGWrite();


/* imports from short.c */

extern SHArrow(), SHBox(), SHCopy(), SHDefineSet(), SHDrawArc(), SHDrawCurve(), 
       SHDrawVector(), SHErase(), SHGravity(), SHGrid(), SHMAdjust(), 
       SHSave1(), SHSave2(), SHSave3(), SHSave4(), 
       SHRotate(), SHScale(), SHSetArea(), SHMSetArea(), SHTranslate();

/* imports from suntools */

extern struct menuitem *menu_display();

/* justify_mark marks the text handle positions for the JUSTIFY icon
   relative to the icon's upper left corner */

struct pr_pos justify_mark[9];

/* just_to_indx maps the justification values (TOPLEFT, ..., BOTRIGHT)
   to the corresponding index in the justify_mark array */

int just_to_indx[] = { 6, 8, 4, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 5, 7 };

/* indx_to_just maps the internal justification values (0, ..., 8)
   to the gremlin justification values (TOPLEFT, ..., BOTRIGHT) */

int indx_to_just[] = { TOPLEFT, TOPCENT, TOPRIGHT, 
		       CENTLEFT, CENTCENT, CENTRIGHT, 
		       BOTLEFT, BOTCENT, BOTRIGHT };

struct _menu menu[] = {
#define M_SIZE1 0
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP +  ICON_BORDER + MENU_Y * 1, &size1_pr, 
      LGSize1, LGMSize1, size1_help },

#define M_ROMAN (M_SIZE1+1)
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 2, &roman_pr, 
      LGFont1, LGMFont1, roman_help },

#define M_JUSTIFY (M_ROMAN+1)
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 1, &justify_pr, 
      nop, nop, nop },

#define M_SCALE (M_JUSTIFY+1)
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 9, &scale_pr, 
      SHScale, nop, scale_help },

#define M_TRANSLATE (M_SCALE+1)
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 8, &move_pr, 
      SHTranslate, nop, move_help },

#define M_HMIRROR (M_TRANSLATE+1)
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 11, &hmirror_pr, 
      LGHMirror, nop, hmirror_help },

#define M_VMIRROR (M_HMIRROR+1)
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 11, &vmirror_pr, 
      LGVMirror, nop, vmirror_help },

#define M_DEFINE_SET (M_VMIRROR+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 14, &include_pr, 
      SHDefineSet, LGIncludeSet, include_help },

#define M_PUT1 (M_DEFINE_SET+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 15, &put1_pr, 
      SHSave1, nop, put1_help },

#define M_PUT3 (M_PUT1+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 16, &put3_pr, 
      SHSave3, nop, put3_help },

#define M_HORZ_ADJUST (M_PUT3+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 17, &horizontal_pr, 
      LGHAdjust, nop, horizontal_help },

#define M_VERT_ADJUST (M_HORZ_ADJUST+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 18, &vertical_pr, 
      LGVAdjust, nop, vertical_help },

#define M_STIPPLE1 (M_VERT_ADJUST+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 3, &white_pr, 
      LGStipple1, LGMStipple1, stipple1_help },

#define M_STIPPLE5 (M_STIPPLE1+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 3, &stipple5_pr, 
      LGStipple5, LGMStipple5, stipple5_help },

#define M_SIZE2 (M_STIPPLE5+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 1, &size2_pr, 
      LGSize2, LGMSize2, size2_help },

#define M_ITALICS (M_SIZE2+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 2, &italics_pr, 
      LGFont2, LGMFont2, italics_help },

#define M_COPY (M_ITALICS+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 8, &copy_pr, 
      SHCopy, nop, copy_help },

#define M_ERASE (M_COPY+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 9, &erase_pr, 
      SHErase, nop, erase_help },

#define M_MOVE_POINT (M_ERASE+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 10, &movepoint_pr, 
      LGMPoint, nop, movepoint_help },

#define M_ROTATE (M_MOVE_POINT+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 10, &rotate_pr, 
      SHRotate, nop, rotate_help },

#define M_FILE (M_ROTATE+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 13, &filecabinet_pr, 
      nop, nop, filecabinet_help },

#define M_PAN (M_FILE+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 14, &pan_pr, 
      LGPan, LGMPan, pan_help },

#define M_SET_AREA (M_PAN+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 14, &boxinc_pr, 
      SHSetArea, SHMSetArea, boxinc_help },

#define M_PUT2 (M_SET_AREA+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 15, &put2_pr, 
      SHSave2, nop, put2_help },

#define M_PUT4 (M_PUT2+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 16, &put4_pr, 
      SHSave4, nop, put4_help },

#define M_MANHATTAN_ADJUST (M_PUT4+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 17, &horvert_pr, 
      SHMAdjust, nop, manhattan_help },

#define M_GRAVITY (M_MANHATTAN_ADJUST+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 18, &gravity_pr, 
      SHGravity, nop, gravity_help },

#define M_STIPPLE6 (M_GRAVITY+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 4, &stipple6_pr, 
      LGStipple6, LGMStipple6, stipple6_help },

#define M_STIPPLE2 (M_STIPPLE6+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 4, &gray_pr, 
      LGStipple2, LGMStipple2, stipple2_help },

#define M_SIZE3 (M_STIPPLE2+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 2, &size3_pr, 
      LGSize3, LGMSize3, size3_help },

#define M_BOLD (M_SIZE3+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 3, &bold_pr, 
      LGFont3, LGMFont3, bold_help },

#define M_BRUSH1 (M_BOLD+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 4, &dotted_pr, 
      LGBrush1, LGMBrush1, brush1_help },

#define M_BRUSH2 (M_BRUSH1+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 6, &broken_pr, 
      LGBrush2, LGMBrush2, brush2_help },

#define M_BRUSH3 (M_BRUSH2+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 6, &thick_pr, 
      LGBrush3, LGMBrush3, brush3_help },

#define M_TEXT (M_BRUSH3+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 8, &text_pr, 
      LGText, LGMText, text_help },

#define M_BPOLYGON (M_TEXT+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 9, &bpolygon_pr, 
      LGBPolygon, LGMBPolygon, bpolygon_help },

#define M_POLYGON (M_BPOLYGON+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 10, &polygon_pr, 
      LGPolygon, LGMPolygon, polygon_help },

#define M_ARROW (M_POLYGON+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 11, &arrow_pr, 
      SHArrow, nop, arrow_help },

#define M_HELP (M_ARROW+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 13, &question_pr, 
      help, help, help },

#define M_MISC (M_HELP+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 14, &misc_pr, 
      nop, nop, misc_help },

#define M_GET1 (M_MISC+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 15, &get1_pr, 
      LGGet1, nop, get1_help },

#define M_GET3 (M_GET1+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 16, &get3_pr, 
      LGGet3, nop, get3_help },

#define M_LINESTYLE (M_GET3+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 13, &linestyle_pr, 
      LGLineStyle, nop, linestyle_help },

#define M_ALIGN (M_LINESTYLE+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 17, &align_pr, 
      nop, nop, align_help },

#define M_STIPPLE3 (M_ALIGN+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 5, &_50_pr, 
      LGStipple3, LGMStipple3, stipple3_help },

#define M_STIPPLE7 (M_STIPPLE3+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 5, &stipple7_pr, 
      LGStipple7, LGMStipple7, stipple7_help },

#define M_STIPPLE8 (M_STIPPLE7+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 6, &stipple8_pr, 
      LGStipple8, LGMStipple8, stipple8_help },

#define M_STIPPLE4 (M_STIPPLE8+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 0, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 6, &black_pr, 
      LGStipple4, LGMStipple4, stipple4_help },

#define M_SIZE4 (M_STIPPLE4+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 1, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 2, &size4_pr, 
      LGSize4, LGMSize4, size4_help },

#define M_SPECIAL (M_SIZE4+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 3, &special_pr, 
      LGFont4 , LGMFont4, special_help },

#define M_BRUSH4 (M_SPECIAL+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3,
      MENU_TOP  + ICON_BORDER + MENU_Y * 5, &dashed_pr, 
      LGBrush4, LGMBrush4, brush4_help },

#define M_BRUSH5 (M_BRUSH4+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 4, &narrow_pr, 
      LGBrush5, LGMBrush5, brush5_help },

#define M_BRUSH6 (M_BRUSH5+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 5, &medium_pr, 
      LGBrush6, LGMBrush6, brush6_help },

#define M_DRAW_ARC (M_BRUSH6+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 8, &arc_pr, 
      SHDrawArc, nop, arc_help },

#define M_DRAW_CURVE (M_DRAW_ARC+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 9, &curve_pr, 
      SHDrawCurve, LGMCurve, curve_help },

#define M_DRAW_VECTOR (M_DRAW_CURVE+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 10, &vector_pr, 
      SHDrawVector, LGMVector, vector_help },

#define M_BOX (M_DRAW_VECTOR+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 11, &box_pr, 
      SHBox, nop, box_help },

#define M_UNDO (M_BOX+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 13, &undo_pr, 
      LGUndo, nop, undo_help },

#define M_GET2 (M_UNDO+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 15, &get2_pr, 
      LGGet2, nop, get2_help },

#define M_GET4 (M_GET2+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 16, &get4_pr, 
      LGGet4, nop, get4_help },

#define M_GRID (M_GET4+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 3, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 18, &grid_pr, 
      SHGrid, nop, grid_help },

#define M_LITTLE_POINT (M_GRID+1) 
    { MENU_LEFT + ICON_BORDER + MENU_X * 2, 
      MENU_TOP  + ICON_BORDER + MENU_Y * 18, &littlepoint_pr, 
      LGLittlePoint, nop, littlepoint_help },

    { -1, -1, (struct pixrect *)0, (int(*)())0 }
};


/* 
 * Following is a map of brushes and mode made available to the
 * outside world for selection of these items for highlighting.
 * The numbers indicate the items location in the menu.
 */
    
int HiBuffer[] = { M_PUT1, M_PUT2, M_PUT3, M_PUT4 };	/* user symbols */
int HiFont[] = { M_ROMAN, M_ITALICS, M_BOLD, M_SPECIAL }; 
int HiSize[] = { M_SIZE1, M_SIZE2, M_SIZE3, M_SIZE4 }; 
int HiBrush[] = { M_BRUSH1, M_BRUSH2, M_BRUSH3, 
		   M_BRUSH4, M_BRUSH5, M_BRUSH6 };	/* brushes */
int HiMode[] = { M_HORZ_ADJUST, M_VERT_ADJUST, M_MANHATTAN_ADJUST, 
		  M_GRAVITY };			/* horz, vert, man, grav. */
int HiStipple[] = { M_STIPPLE1, M_STIPPLE2, M_STIPPLE3, M_STIPPLE4, 
		    M_STIPPLE5, M_STIPPLE6, M_STIPPLE7, M_STIPPLE8 }; 
int HiArtMode = M_LITTLE_POINT;
int HiLineStyle = M_LINESTYLE;

     

/*
 * This routine initializatizes the positioning of menu items -
 * currently only necessary for the justify icon.
 */
MNInitMenu()
{
    int justify_x, justify_y;
    int i;

    justify_x = menu[M_JUSTIFY].menu_x;
    justify_y = menu[M_JUSTIFY].menu_y;

    for (i=0; i<9; i++) {
	switch (i % 3) {
	    case 0:		/* left */
		justify_mark[i].x = justify_x - 1;
		break;
	    case 1:		/* center */
		justify_mark[i].x = justify_x + 18;
		break;
	    case 2:		/* right */
		justify_mark[i].x = justify_x + 35;
		break;
	}

	switch (i / 3) {
	    case 0:		/* top */
		justify_mark[i].y = justify_y - 2;
		break;
	    case 1:		/* center */
		justify_mark[i].y = justify_y + 5;
		break;
	    case 2:		/* bottom */
		justify_mark[i].y = justify_y + 12;
		break;
	}
    }

    if ((save_just_pr = mem_create(6, 6, 1)) == NULL) {
	printf("cannot create save_just_pr.\n");
	exit(1);
    }
}  /* end MNInitMenu */


/*
 * This routine displays the menu defined by initmenu
 */
MNDisplayMenu()
{
    register i;
    register struct _menu *m;
    char buf[4];

    pw_text(menu_pw, 28, menu[M_SIZE1].menu_y - MENU_Y + TEXT_BASELINE,
		PIX_SRC | PIX_DST, text_pf, "styles");
    pw_text(menu_pw, 24, menu[M_DRAW_ARC].menu_y - MENU_Y + TEXT_BASELINE,
		PIX_SRC | PIX_DST, text_pf, "drawing");
    pw_text(menu_pw, 32, menu[M_FILE].menu_y - MENU_Y + TEXT_BASELINE,
		PIX_SRC | PIX_DST, text_pf, "other");

    pw_box(menu_pw, 2, 2, 
		MENUSW_WIDTH - 3, menu[M_BRUSH2].menu_y + ICON_SIZE + 3,
		PIX_SRC | PIX_DST, 1);
    pw_box(menu_pw, 2, menu[M_DRAW_ARC].menu_y - MENU_Y - 2, 
		MENUSW_WIDTH - 3, menu[M_BOX].menu_y + ICON_SIZE + 3,
		PIX_SRC | PIX_DST, 1);
    pw_box(menu_pw, 2, menu[M_FILE].menu_y - MENU_Y - 2, 
		MENUSW_WIDTH - 3, 509 
		/*menu[M_LITTLE_POINT].menu_y + ICON_SIZE + 3*/,
		PIX_SRC | PIX_DST, 1);

    i = 0;
    do {
	m = &menu[i];
	pw_write(menu_pw, m->menu_x, m->menu_y, 
			m->menu_icon->pr_width, m->menu_icon->pr_height, 
			PIX_SRC, m->menu_icon, 0, 0);

	if (is_stipple(m->menu_icon)) {
	    pw_write(menu_pw, m->menu_x, m->menu_y, 
			openbox_pr.pr_width, openbox_pr.pr_height, 
			PIX_SRC | PIX_DST, &openbox_pr, 0, 0);
	}
    } while (menu[++i].menu_x != -1);

    if (Adjustment != NOADJ)
	MNHighLt(HiMode[adj[Adjustment]]);

    if (GravityOn)
	MNHighLt(HiMode[3]);

    if (!SymbolicLines)
	MNHighLt(HiLineStyle);

    if (!Artmode)
	MNHighLt(HiArtMode);

    MNHighLt(HiFont[CFONT-1]);
    MNHighLt(HiSize[CSIZE-1]);
    MNHighLt(HiBrush[CBRUSH-1]);
    MNHighLt(HiStipple[CSTIPPLE-1]);

    pw_read(save_just_pr, 0, 0, 6, 6, PIX_SRC, menu_pw, 
			    justify_mark[just_to_indx[CJUST]].x, 
			    justify_mark[just_to_indx[CJUST]].y);

    pw_write(menu_pw, justify_mark[just_to_indx[CJUST]].x + 1,
			    justify_mark[just_to_indx[CJUST]].y,
			    5, 5, PIX_SRC, &dot_pr, 0, 0);

    for (i = 0; i<NUSER; ++i) {
	if (!DBNullelt(MEN[i]))
	    MNHighLt(HiBuffer[i]);
    }

    sprintf(buf, "%3d", Alignment);
    pw_text(menu_pw, menu[M_ALIGN].menu_x + 16, 
		     menu[M_ALIGN].menu_y + 12,
		     PIX_SRC ^ PIX_DST, text_pf, buf);
}


menu_left(ie)
register struct inputevent *ie;
{
    register index;

    TxMsgOK();
    lasttext = FALSE;

    if ((index = icon_last) >= 0) {
	if (index == M_ALIGN) {
	    lastcommand = align_up;
	    align_up();
	    return;
	}

	/* current set on, un-highlight icon */
	GRCurrentSetOn();
	MNReverse(index);
	icon_last = -1;

	if (index == M_FILE) {
	    menu_filer(ie);
	    return;
	}

	if (index == M_MISC) {
	    menu_misc(ie);
	    return;
	}

	lastcommand = menu[index].menu_function;
	if (index == M_TEXT)
	    lasttext = TRUE;
	(*lastcommand)();
	/* (*(menu[index].menu_function))(); */
	FLASH_READY = 0;	/* don't flash; it may have taken a while */
	return;
    }

    if (just_last >= 0) {	/* in JUSTIFY icon */
	/* un-highlight */
	MNReverseJustify();

	/* update justification mode */
	pw_write(menu_pw, justify_mark[just_to_indx[CJUST]].x, 
			justify_mark[just_to_indx[CJUST]].y, 
			6, 6, PIX_SRC, save_just_pr, 0, 0);

	CJUST = indx_to_just[just_last];

	pw_read(save_just_pr, 0, 0, 6, 6, PIX_SRC, menu_pw, 
			justify_mark[just_to_indx[CJUST]].x, 
			justify_mark[just_to_indx[CJUST]].y);

	pw_write(menu_pw, justify_mark[just_to_indx[CJUST]].x + 1,
			justify_mark[just_to_indx[CJUST]].y,
			5, 5, PIX_SRC, &dot_pr, 0, 0);
	just_last = -1;
    }
}


menu_middle(ie)
register struct inputevent *ie;
{
    register index;

    TxMsgOK();
    lasttext = FALSE;

    if ((index = icon_last) >= 0) {
	if (index == M_ALIGN) {
	    lastcommand = align_down;
	    align_down();
	    return;
	}

	/* current set on, un-highlight icon */
	GRCurrentSetOn();
	MNReverse(index);
	icon_last = -1;

	lastcommand = menu[index].menu_modify;
	(*lastcommand)();
	if (index == M_TEXT)
	    lasttext = TRUE;
	/* (*(menu[index].menu_modify))(); */
	FLASH_READY = 0;	/* don't flash; it may have taken a while */
	return;
    }

    if ((index = just_last) >= 0) {
	just_last = -1;

	/* current set on, un-highlight icon */
	GRCurrentSetOn();
	MNReverseJustify();

	LGMJustify(indx_to_just[index]);
    }
}


menu_right(ie)
register struct inputevent *ie;
{
    register index;

    TxMsgOK();

    if ((index = icon_last) >= 0) {
	icon_last = -1;

	/* un-highlight icon */
	MNReverse(index);

	(*(menu[index].menu_help))();
	return;
    }

    if ((index = just_last) >= 0) {
	just_last = -1;

	/* un-highlight icon */
	MNReverseJustify();

	justify_help();
	return;
    }

    menusw_help();
}


menu_winexit(ie)
register struct inputevent *ie;
{
    if (icon_last >= 0) {
	MNReverse(icon_last);
	icon_last = -1;
    }
    else if (just_last >= 0) {
	MNReverseJustify();
	just_last = -1;
    }

    /* check if leaving left or bottom edge of menu subwindow */
    if ((ie->ie_locx < 0) || (ie->ie_locy >= menu_size.r_height))
	GRCurrentSetOn();
}


/*
 * Move align to next higher value.
 */
align_up()
{
    menu_align(TRUE);
}


/*
 * Move align to next lower value.
 */
align_down()
{
    menu_align(FALSE);
}


#define ALIGNMAX 512
menu_align(up)
int up;
{
    char buf[4];


    sprintf(buf, "%3d", Alignment);
    pw_text(menu_pw, menu[M_ALIGN].menu_x + 16, 
		     menu[M_ALIGN].menu_y + 12,
		     PIX_SRC ^ PIX_DST, text_pf, buf);

    if (up) {
	if ((Alignment <<= 1) > ALIGNMAX)
	    Alignment = 1;
    }
    else {
	if ((Alignment >>= 1) == 0)
	    Alignment = ALIGNMAX;
    }

    sprintf(buf, "%3d", Alignment);
    pw_text(menu_pw, menu[M_ALIGN].menu_x + 16, 
		     menu[M_ALIGN].menu_y + 12,
		     PIX_SRC ^ PIX_DST, text_pf, buf);
}


menu_filer(ie)
register struct inputevent *ie;
{
    register struct menuitem *item;

    item = menu_display(&file_menu_stack, ie, menu_fd);

    if (item == (struct menuitem *) NULL)
	return;

    switch (item->mi_data) {
	case M_EDIT:
	    lastcommand = LGEdit;
	    break;
	case M_PATH:
	    lastcommand = LGPath;
	    break;
	case M_READ:
	    lastcommand = LGRead;
	    break;
	case M_SAVE_SET:
	    lastcommand = LGSave;
	    break;
	case M_WRITE:
	    lastcommand = LGWrite;
	    break;
	default:
	    return;
    }
    (*lastcommand)();
    lasttext = TRUE;
}


menu_misc(ie)
register struct inputevent *ie;
{
    register struct menuitem *item;

    item = menu_display(&misc_menu_stack, ie, menu_fd);

    if (item == (struct menuitem *) NULL)
	return;

    switch (item->mi_data) {
	case M_CLEAR_POINTS:
	    lastcommand = LGClearPoints;
	    break;
	case M_SHOW_POINTS:
	    lastcommand = LGShowPoints;
	    break;
	case M_GRIPE:
	    lastcommand = LGGripe;
	    break;
	case M_POINT:
	    lastcommand = LGOPoint;
	    lasttext = TRUE;
	    break;
	default:
	    return;
    }
    (*lastcommand)();
}


/* 
 *  find justification mode determined by (x, y) screen position 
 *  return -1 if not in justify icon
 */
just_index(x, y)
register x;
register y;
{
    x -= menu[M_JUSTIFY].menu_x - 1;
    y -= menu[M_JUSTIFY].menu_y - 2;

    if ((x > 41) || (x < 0) || (y > 19) || (y < 0))
	return(-1);
    else
	return((y / 7) * 3 + (x / 14));
}


mouse_move(ie)
register struct inputevent *ie;
{
    register index, in_icon, in_xy;

    /* handle justify icon as a special case */
    if ((index = just_index(ie->ie_locx, ie->ie_locy)) >= 0) {
	if (icon_last >= 0) {	/* erase last highlighted icon */
	    MNReverse(icon_last);
	    icon_last = -1;
	}

	if (just_last >= 0)	/* erase current justification marker */
	    MNReverseJustify();

	just_x = justify_mark[index].x;
	just_y = justify_mark[index].y;
	just_last = index;
	MNReverseJustify();
	return;
    }


    /* ... not currently in justify icon */

    if (just_last >= 0) {	/* previously in justify icon */
	MNReverseJustify();
	just_last = -1;
    }

    in_xy = ((index = MNFindMenuItem(ie->ie_locx, ie->ie_locy)) >= 0);
    in_icon = (icon_last >= 0);

    switch ((in_xy << 1) | in_icon) {
	case 0:		/* was outside still outside */
	    break;
	case 1:		/* was inside now outside */
	    MNReverse(icon_last);
	    icon_last = -1;
	    break;
	case 2:		/* was outside now inside */
	    icon_x = menu[index].menu_x;
	    icon_y = menu[index].menu_y;
	    MNReverse(icon_last = index);
	    break;
	case 3:		/* was inside still inside */
	    if ((icon_y != menu[index].menu_y) || 
		(icon_x != menu[index].menu_x)) {
		MNReverse(icon_last);
		icon_x = menu[index].menu_x;
		icon_y = menu[index].menu_y;
		MNReverse(icon_last = index);
	    }
	    break;
    }
}


/*
 *  return index into menu[] for icon at (x, y)
 */
MNFindMenuItem(x, y)
int x, y;
{
    register struct _menu *m;
    register mleft, mright, mtop, mbottom;
    int i;

    i = 0;

    do {
	m = &menu[i];
	mleft = m->menu_x;
	mtop = m->menu_y;
	mright = mleft + m->menu_icon->pr_width - 1;
	mbottom = mtop + m->menu_icon->pr_height - 1;

	if ((x <= mright) && (y <= mbottom) && (x >= mleft) && (y >= mtop))
	    return(i);

    } while (menu[++i].menu_x != -1);

    return(-1);		/* not found */

} /* end MNFindMenuItem */


/*
 *  Highlight menu item specified by menu array index.
 */
MNHighLt(index)
int index;
{
    pw_write(menu_pw, menu[index].menu_x-2, menu[index].menu_y-2, 
		    bigopenbox_pr.pr_width, bigopenbox_pr.pr_height, 
		    PIX_SRC | PIX_DST, &bigopenbox_pr, 0, 0);
}


/*
 *  Un-highlight menu item specified by menu array index.
 */
MNUnHighLt(index)
int index;
{
    pw_write(menu_pw, menu[index].menu_x - 2, menu[index].menu_y - 2, 
		    bigopenbox_pr.pr_width, bigopenbox_pr.pr_height, 
		    PIX_NOT(PIX_SRC) & PIX_DST, &bigopenbox_pr, 0, 0);
}


/*
 *  Reverse video menu item specified by menu array index.
 */
MNReverse(index)
int index;
{
    pw_write(menu_pw, menu[index].menu_x - 2, menu[index].menu_y - 2, 
		    menu[index].menu_icon->pr_width + 4,
		    menu[index].menu_icon->pr_height + 4,
		    PIX_SRC ^ PIX_DST, &bigblack_pr, 0, 0);
}
