/* 
 * Structures to facilitate option passing in showimg.
 */

/* main window structures */
struct windmain {
    char *display;             /* display name */
    char filegeometry[30];     /* geometry from file */
    char *geometry;            /* geometry from command line, if any */
    OpaqueFrame frame;         /* main window descriptor */
    WindowInfo winfo;          /* main window information struct */
    Cursor windcursor;         /* current main window cursor */
    Cursor lastcursor;         /* previous cursor */
};

/* Color info */
struct colorwind {
    int nplanes;               /* number of color planes */
    int pixels[1];             /* base for color map */
    int ncolors;               /* number of colors used */
    int planes;                /* planes mask */
    int shift;                 /* #left shifts to adjust zero-based */
                               /*     pixels to pixels[1] */
    Color cmap[256];           /* GPX max color map limited to 256 */
};

/* image, image info, options, operational flags */
struct imagewind {
    char *filename;
    short *header;             /* header data from file */
    short *picture;            /* 16-bit image */
    unsigned char *image;      /* 8-bit (or fewer) displayed image */
    int nrows;
    int ncols;
    int pmin;                  /* high min pixel flags not set */
    int pmax;                  /* low max pixel flags not set */
    int headskip;              /* bytes to skip before reading */
    int calibration;           /* flag for test pattern */
    int fitsflag;              /* file format indicator */

#ifdef XRAY
    /* added by egm */

    int ein; /* flag for Einstein xray data */
    int ros; /* flag for Rosat xray data */
    int iy, iz, zoom, energy; /* parameters on -xray switch */
    short int ict[513]; /* image file control table */
    int muthict[2400];  /* it's a muther ict table */
    char poename[132], hdrname[132];
    
    /* end of egm code */
#endif

    u_short VOP_Flags;
    u_short SOP_Flags;
    u_short COP_Flags;
    u_short FOP_Flags;
};
