#ifdef NEWSTRUCT
    char *geometry = NULL;
    char filegeometry[30];   /* created from file unless overridden */
    char *display = NULL;
    char *border_color, *back_color;
    OpaqueFrame frame;
    int border_width;
    Pixmap border_pixmap;
    Color cdef;
    u_short VOP_Flags = VOP_GrayScale | VOP_Initialize;
    u_short SOP_Flags = SOP_Linear;
    u_short COP_Flags = 0, COP_OldFlags = 0;
    u_short FOP_Flags = 0;
    char *option;
    int nplanes = -1;	/* number of planes to allocate */
    int ncolors;
    int pixels[1];		/* color cell from allocation */
    int planes;		/* plane mask */
    Color cmap[256];	/* much larger than can be on CAYLITH */
    char *filename;
    unsigned char *image;   /* written to display */
    short *header;          /* header of real image */
    short *pict;		/* real image */
    int pmax = -3000;
    int pmin = 70000;   /* will be 16-bite max, min values (approx.)*/
    int shift = 0;
    int fitsflag = 0;   /* indicator to read in FITS images */
    /* 1=true FITS, 2=disk FITS (swapped bytes)*/
    int calibration = 0;
    int headskip = 0;       /* number of header bytes to skip */
    int nrows = 0, ncols = 0;
#endif
