
/* collect and interpret runtime options for showimg,
 * 
 * 8/12/86  Bill Wyatt
 * 
 * Also opens the display, since to set up certain color
 * options, the display must be accessible.
 */

#include <stdio.h>
#include <X/Xlib.h>

#include "shimg.h"
#include "shopt.h"

short gray_bits[16] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555};

shgetopt(argc, argv, maininfo, colorinfo, imageinfo)
     int argc;
     char *argv[];
     struct windmain *maininfo;
     struct colorwind *colorinfo;
     struct imagewind *imageinfo;
{
    Color cdef;
    char *option, *border_color, *back_color;
    int i, strcmp(), atoi();
    char *index();


    if ((option = XGetDefault(argv[0],"BorderWidth")) != NULL)
      maininfo->frame.bdrwidth = atoi(option);
    else maininfo->frame.bdrwidth = 0;

    if ((border_color = XGetDefault(argv[0],"Border")) == NULL)
      border_color = XGetDefault(argv[0],"BorderColor");

    back_color = XGetDefault(argv[0],"Background");


    for (i = 1; i < argc; i++) {
	if (argv[i][0] == '=') {
	    maininfo->geometry = argv[i];
	    continue;
	}
	if (index(argv[i], ':') != NULL) {        /* host:display */
	    maininfo->display = argv[i];
	    continue;
	}
	if (strcmp(argv[i], "-bw") == 0 ||
	    strcmp(argv[i], "-border") == 0) {   /* border width */
		if (++i >= argc) usage(argv[0]);
		maininfo->frame.bdrwidth = atoi(argv[i]);
		continue;
	    }
	if (strcmp(argv[i], "-bd") == 0 ||
	    strcmp(argv[i], "-color") == 0) {    /* border color */
		if (++i >= argc) usage(argv[0]);
		border_color = argv[i];
		continue;
	    }
	if(strcmp(argv[i], "-bg") == 0 ||
	   strcmp(argv[i], "background") == 0) {  /* background color */
	       if(++i >= argc) usage(argv[0]);
	       back_color = argv[i];
	       continue;
	   }
	if (strcmp(argv[i], "-p") == 0 ||
	    strcmp(argv[i], "-planes") == 0) {    /* # of planes */
		if (++i >= argc) usage(argv[0]);
		colorinfo->nplanes = 
		  (atoi(argv[i]) > 8) ? 8 : atoi(argv[i]);
		continue;
	    }
	if (strcmp(argv[i],"-n") == 0 ||
	    strcmp(argv[i],"-neg") == 0) {        /* negative map */
		imageinfo->VOP_Flags |= VOP_Inverse;
		continue;
	    }
	if (strcmp(argv[i],"-rgb") == 0) {	  /* manipulate RGB */
	    /*   rgbmap++; */
	    imageinfo->VOP_Flags &= ~(VOP_GrayScale);
	    imageinfo->VOP_Flags |= VOP_RGB;
	    continue;
	}
	if(strcmp(argv[i],"-skip") == 0 ||
	   strcmp(argv[i],"-sk") == 0)    {	  /* skip header */
	       if(++i >= argc) usage(argv[0]);
	       imageinfo->headskip = atoi(argv[i]);
	       if(imageinfo->headskip < 0) usage(argv[0]);
	       continue;
	   }
	if(strcmp(argv[i],"-nrows") == 0 ||
	   strcmp(argv[i],"-nr") == 0)   {       /* explicit set */
	       if(++i >= argc) usage(argv[0]);
	       imageinfo->nrows = atoi(argv[i]);
	       if(imageinfo->nrows <= 0) usage(argv[0]);
	       continue;
	   }
	if(strcmp(argv[i],"-ncols") == 0 ||
	   strcmp(argv[i],"-nc") == 0)   {	  /* explicit set */
	       if(++i >= argc) usage(argv[0]);
	       imageinfo->ncols = atoi(argv[i]);
	       if(imageinfo->ncols <= 0) usage(argv[0]);
	       continue;
	   }
	if(strcmp(argv[i],"-sqrt") == 0 ||
	   strcmp(argv[i],"-sq") == 0) {          /* sqrt scaling */
	       imageinfo->SOP_Flags = SOP_Sqrt;
	       continue;
	   }
	if(strcmp(argv[i],"-test") == 0 ||
	   strcmp(argv[i],"-t") == 0) {          /* test gray scale*/
	       imageinfo->calibration++;
	       continue;
	   }
	if(strcmp(argv[i],"-saturate") == 0 ||   /* set sat. lev.*/
	   strcmp(argv[i],"-sat") == 0) {
	       if(++i >= argc) usage(argv[0]);
	       if((imageinfo->pmax = atoi(argv[i])) > 16383)
		 usage(argv[0]);
	       continue;
	   }
	if(strcmp(argv[i],"-threshold") == 0 ||  /* set thresh lev.*/
	   strcmp(argv[i],"-thr") == 0) {
	       if(++i >= argc) usage(argv[0]);
	       if((imageinfo->pmin = atoi(argv[i])) < -16383)
		 usage(argv[0]);
	       continue;
	   }
	if(strcmp(argv[i],"-fits") == 0) { /* select fits file*/
	    imageinfo->fitsflag = 1;
	    continue;
	}
	if(strcmp(argv[i],"-dfits") == 0) { /* select disk fits file*/
	    imageinfo->fitsflag = 2;
	    continue;
	}

#ifdef XRAY
	/* added by egm */
	if(strcmp(argv[i],"-ein") ==0 ){
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->iy = atoi(argv[i]);
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->iz = atoi(argv[i]);
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->zoom = atoi(argv[i]);
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->energy = atoi(argv[i]);
	    imageinfo->ein = 1;
	    imageinfo->ncols = 512;
	    imageinfo->nrows = 512;
	    continue;
	}
	
	if(strcmp(argv[i],"-ros") ==0 ){
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->iy = atoi(argv[i]);
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->iz = atoi(argv[i]);
	    if(++i >= argc) usage(argv[0]);
	    imageinfo->zoom = atoi(argv[i]);
	    imageinfo->ros = 1;
	    imageinfo->ncols = 512;
	    imageinfo->nrows = 512;
	    continue;
	}
	/* end of egm code */
#endif
	
	if (argv[i][0] == '-') usage(argv[0]);
	imageinfo->filename = argv[i];
    }


/* do some consistency checks on selected options */
    if(imageinfo->headskip > 0 && 
       (imageinfo->nrows == 0 || imageinfo->ncols == 0)) usage(argv[0]);
    
    if((imageinfo->pmax == -3000 && imageinfo->pmin != 70000) ||
       (imageinfo->pmin == 70000 && imageinfo->pmax != -3000)) 
      usage(argv[0]);
    
    if(imageinfo->fitsflag != 0 && 
       (imageinfo->headskip > 0 || imageinfo->nrows != 0 || 
	imageinfo->ncols != 0))
      usage(argv[0]);
    
    if (!XOpenDisplay(maininfo->display)) {
	perror(argv[0]);
	exit(1);
    }

    if (border_color && DisplayCells() > 2 &&
	XParseColor(border_color, &cdef) && XGetHardwareColor(&cdef))
      maininfo->frame.border = XMakeTile(cdef.pixel);
    else if (border_color && strcmp(border_color, "black") == 0)
      maininfo->frame.border = BlackPixmap;
    else if (border_color && strcmp(border_color, "white") == 0)
      maininfo->frame.border = WhitePixmap;
    else
      maininfo->frame.border = XMakePixmap (XStoreBitmap (16, 16, gray_bits),
				   BlackPixel, WhitePixel);

    if (back_color && DisplayCells() > 2 &&
	XParseColor(back_color, &cdef) && XGetHardwareColor(&cdef))
      maininfo->frame.background = XMakeTile(cdef.pixel);
    else if (back_color && strcmp(back_color, "white") == 0)
      maininfo->frame.background = WhitePixmap;
    else 
      maininfo->frame.background = BlackPixmap;
    
    return;
}


usage (program)
    char *program;
{
    fprintf(stderr,"usage: %s [ options ] filename\n",program);
    fprintf(stderr,"     where options are one or more of:\n");
    fprintf(stderr,"  [host:display] [=geom] [-bw] [-bd] [-bg]\n");
    fprintf(stderr,"  [-planes #] [-rgb] [-neg] [-sqrt]\n");
    fprintf(stderr,"  [-skip # -nrows # -ncols #]\n");
    fprintf(stderr,"  [-saturate # -threshold #]\n");
    fprintf(stderr,"  [-fits] [ -dfits ]\n");
    exit(1);
}
