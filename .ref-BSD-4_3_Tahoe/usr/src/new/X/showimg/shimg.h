/* defaults for subwindow stuff */

#define PALBORDER 1
#define PALHEIGHT 32

/* operation flags for image display, manipulation operations */

#define PANEMASK 0xffff0000
#define SELMASK 0xffff

#define VOP  0x10000
#define VOP_GrayScale  0x01
#define VOP_RGB        0x02
#define VOP_Normal     0x04
#define VOP_Inverse    0x08
#define VOP_Initialize 0x10

#define COP  0x20000
#define COP_Print      0x01
#define COP_Pan        0x02
#define COP_Zoom2      0x04
#define COP_Zoom4      0x08
#define COP_Box        0x10
#define COP_SetOrigin  0x20

#define SOP  0x40000
#define SOP_Linear     0x01
#define SOP_Log        0x02
#define SOP_Sqrt       0x04
#define SOP_Histogram  0x08

#define FOP  0x80000
#define FOP_Read       0x01
#define FOP_Flatfield  0x02
#define FOP_AddImage   0x04

#define FITSBUFLEN 2880
