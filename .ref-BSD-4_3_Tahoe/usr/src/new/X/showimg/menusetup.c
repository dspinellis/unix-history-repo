/* menu selection and setup */

#include <stdio.h>
#include <X/Xlib.h>
#include <X/XMenu.h>

#include "shimg.h"

/* This structure is mainly created to easily initialize and update the
 * menu panes and selections. The null elements indicate to the initializing
 * routine that that particular pane (and eventually the menu) are complete.
 *
 * When a menu selection is successful, it returns a pointer to the data
 * array in the particular element. The first element contains the pane
 * number in the upper word and the selection flag in the lower word. The
 * second element contains the inverse of the mask to be applied to the
 * corresponding flags location to remove the other panes' selection flags
 * that are orthogonal to this one. The program can then `or' in this
 * pane's selection flag (if desireable) and take the appropriate action.
 */

struct menupane {
  char *label;
  int active;
  int data[2];
};

struct menupane menuset[] = {
    /* View Ops pane */
    { "View Ops",   1, VOP, 0 },
    { "gray scale", 1, { VOP | VOP_GrayScale, VOP_RGB }  },
    { "rgb",        1, { VOP | VOP_RGB, VOP_GrayScale }  },
    { "normal",     1, { VOP | VOP_Normal, VOP_Inverse } },
    { "inverse",    1, { VOP | VOP_Inverse, VOP_Normal } },
    { "initialize", 1, { VOP | VOP_Initialize, 0 }       },
    { "EXIT",       1, { 0, 0 } },
    { 0, 0, 0, 0 },

    /* Cursor Ops pane */
    { "Cursor Ops", 1, COP, 0 },
    { "print",      1, { COP | COP_Print, COP_Pan|COP_Zoom2|COP_Zoom4 } },
    { "pan",        1, { COP | COP_Pan, COP_Print|COP_Zoom2|COP_Zoom4 } },
    { "zoom x2",    1, { COP | COP_Zoom2, COP_Print|COP_Pan|COP_Zoom4 } },
    { "zoom x4",    1, { COP | COP_Zoom4, COP_Print|COP_Pan|COP_Zoom2 } },
/*  { "set origin", 0, COP | COP_SetOrigin, 0 },
    { "box",        0, COP | COP_Box, 0       },
*/  { 0, 0, 0, 0 },

    /* Scaling Ops pane */
    { "Scaling",   1, SOP, 0 },
    { "linear",    1, { SOP | SOP_Linear, SOP_Log|SOP_Sqrt|SOP_Histogram }  },
    { "sqrt",      1, { SOP | SOP_Sqrt, SOP_Linear|SOP_Sqrt|SOP_Histogram } },
/*  { "log",       0, { SOP | SOP_Log, SOP_Linear|SOP_Log|SOP_Histogram }   },
    { "histogram", 0, { SOP | SOP_Histogram, SOP_Linear|SOP_Log|SOP_Sqrt }  },
*/  { 0, 0, 0, 0 },
    
    /* File Ops pane */
/*    { "I/O",        0, FOP, 0 },
    { "read image", 0, FOP | FOP_Read, 0      },
    { "flatfield",  0, FOP | FOP_Flatfield, 0 },
    { "add image",  0, FOP | FOP_AddImage, 0  },
    { 0, 0, 0, 0 },
*/  
    { 0, 0, 0, 0 }
};

XMenu *menusetup(program)
     char *program;
{
     XMenu *menu, *XMenuCreate();
     int XMenuAddPane(), XMenuAddSelection(), XMenuRecompute();
     int pane = 0, sel;

     register struct menupane *imenu = &menuset[0];

     if((menu = XMenuCreate(RootWindow,program)) == NULL) {
       fprintf(stderr,"Menu creation failed!\n");
       exit(1);
     }
     /* insert the panes & selections into the menu structure */
	
     while(imenu->label) {
       sel = 0;
       if((XMenuAddPane(menu,imenu->label,imenu->active))
	   == XM_FAILURE) {
                fprintf(stderr,"Can't add pane %d!\n",pane);
		exit(1);
       }
       ++imenu;
       while(imenu->label) {
	 if((XMenuAddSelection(menu,pane,(char *)&imenu->data[0],
	       imenu->label,imenu->active)) == XM_FAILURE) {
	        fprintf(stderr,"Can't add selection %d to pane %d!\n",
			sel, pane);
		exit(1);
	      }
	 sel++;
	 ++imenu;
       }
       pane++;
       ++imenu;
     }
     XMenuRecompute(menu);
     return(menu);
}
