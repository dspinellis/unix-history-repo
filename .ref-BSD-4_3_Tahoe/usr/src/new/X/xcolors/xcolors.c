/* Copyright, 1986, Massachusetts Institute of Technology */
#include <X/mit-copyright.h>
/*
 * Make a window that shows all the colors in rgb.txt - JT 5/21/86
 */
#include <stdio.h>
#include <sys/file.h>
#include <X/Xlib.h>

#ifndef lint
static char *rcsid_xcolors_c = "$Header: xcolors.c,v 10.1 86/07/25 16:28:22 jg Rel $";
#endif lint


short gray_bits[16] = {
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555,
    0xaaaa, 0x5555, 0xaaaa, 0x5555};

#define curs_width 16
#define curs_height 16
#define curs_x_hot 7
#define curs_y_hot 7
static short curs_bits[] = {
   0x0000, 0x03e0, 0x01c0, 0x0080,
   0x0080, 0x2082, 0x3006, 0x3e3e,
   0x3006, 0x2082, 0x0080, 0x0080,
   0x01c0, 0x03e0, 0x0000, 0x0000};
static short curs_mask_bits[] = {
   0x0aa8, 0x07f0, 0x03e0, 0x41c1,
   0x21c2, 0x7087, 0x3c9e, 0x7f7f,
   0x3c9e, 0x7087, 0x21c2, 0x41c1,
   0x03e0, 0x07f0, 0x0aa8, 0x0000};

#define PICTDEFAULT "+100+5"
#define MAXCOLORS 256
#define DEFAULT_FONT "6x10"
#define MAXHEIGHT 864
#define CWIDTH 100
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#define ABS(a) (((a) > 0) ? (a) : (-a))

main(argc, argv)
int argc;
char **argv;
{
  Window wind ;
  WindowInfo winfo;
  char *geometry = NULL;
  char filegeometry[30];
  char *display = NULL;
  char *option;
  char *border_color, *back_color, *fore_color;
  Font font;
  char *fontname=DEFAULT_FONT;
  FontInfo fontinfo;
  OpaqueFrame frame;
  int border_width, border=BlackPixel;
  int foreground=WhitePixel, background=BlackPixel;
  int ncolors;
  int cmadr[MAXCOLORS];	/* color cell from allocation */
  int planes;		/* plane mask */
  Color cmap[256];
  short readpix;
  Cursor curs;
  char line[80];
  char cname[MAXCOLORS][40];
  char chex[MAXCOLORS][8];
  int rval[MAXCOLORS], gval[MAXCOLORS], bval[MAXCOLORS];
  int order[MAXCOLORS];
  FILE *fp, *fopen();
  int height, width, cheight, maxwidth, x, y, ncols;

  XEvent event, peekevent;
  XButtonEvent *but = (XButtonEvent *)&event;
  register int i, j, k;

  if ((option = XGetDefault(argv[0],"BorderWidth")) != NULL)
    border_width = atoi(option);
  border_color = XGetDefault(argv[0],"BorderColor");
  back_color = XGetDefault(argv[0],"Background");
  fore_color = XGetDefault(argv[0],"Highlight");
  fontname = XGetDefault(argv[0],"BodyFont");

  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '=') {
      geometry = argv[i];
      continue;
    }
    if (index(argv[i], ':') != NULL) {        /* host:display */
      display = argv[i];
      continue;
    }
    if (strcmp(argv[i], "-bw") == 0 ||
	strcmp(argv[i], "-border") == 0) {   /* border width */
	  if (++i >= argc) usage(argv[0]);
	  border_width = atoi(argv[i]);
	  continue;
	}
    if (strcmp(argv[i], "-bd") == 0 ||
	strcmp(argv[i], "-color") == 0) {    /* border color */
	  if (++i >= argc) usage(argv[0]);
	  border_color = argv[i];
	  continue;
	}
    if (strcmp(argv[i], "-bg") == 0) {    /* background color */
	  if (++i >= argc) usage(argv[0]);
	  back_color = argv[i];
	  continue;
	}
    if (strcmp(argv[i], "-fg") == 0) {    /* foreground color */
	  if (++i >= argc) usage(argv[0]);
	  fore_color = argv[i];
	  continue;
	}
    if (strcmp(argv[i], "-fn") == 0 ||
	strcmp(argv[i],"-font") == 0) {    /* font name */
	  if (++i >= argc) usage(argv[0]);
	  fontname = argv[i];
	  continue;
	}
    if (argv[i][0] == '-') usage(argv[0]);
  }

  if (!XOpenDisplay(display)) {
    fprintf(stderr,"Can't open display; set DISPLAY variable\n");
    exit(1);
  }
  
  font = XGetFont(fontname);
  if(!font) {
    fprintf(stderr,"Can't open font %s",fontname);
    exit(1);
  }
  XQueryFont(font,&fontinfo);

  if((fp=fopen("/usr/lib/rgb.txt","r"))==NULL) {
    fprintf(stderr,"Can't open rgb.txt");
    exit(1);
  }

  ncolors = 0;
  i = j = k = -1;
  maxwidth = 0;
  while(fgets(line,sizeof(line),fp)) {
    sscanf(line,"%d %d %d %[^\n]\n",
	   rval+ncolors,gval+ncolors,bval+ncolors,cname+ncolors);
    maxwidth = MAX(maxwidth,strlen(cname[ncolors]));
    order[ncolors] = ncolors;
    sprintf(chex[ncolors],"#%02x%02x%02x",
	    rval[ncolors],gval[ncolors],bval[ncolors]);
    if(i!=rval[ncolors] || j!=gval[ncolors] || k!=bval[ncolors]) {
      i = rval[ncolors];
      j = gval[ncolors];
      k = bval[ncolors];
      ncolors++;
    }
  }
  fclose(fp);
  arrange(ncolors,rval,gval,bval,order);

  if (XGetColorCells(0, ncolors+3, 0, &planes, cmadr) == 0) {
    fprintf(stderr,"Insufficient colors available\n");
    exit(1);
  }
  
  cheight = fontinfo.height+4 ;
  ncols = (cheight*ncolors+2*border_width) / MAXHEIGHT + 1;
  maxwidth = (maxwidth+8)*fontinfo.width;
  height = ((ncolors+ncols-1)/ncols)*cheight;
  width = ncols*(maxwidth + CWIDTH) + (ncols-1)*20;

  XParseColor(back_color, cmap+ncolors);
  background = cmap[ncolors].pixel = cmadr[ncolors];

  XParseColor(fore_color, cmap+ncolors+1);
  foreground = cmap[ncolors+1].pixel = cmadr[ncolors+1];

  XParseColor(border_color, cmap+ncolors+2);
  border = cmap[ncolors+2].pixel = cmadr[ncolors+2];

  frame.bdrwidth = border_width;
  frame.border = XMakeTile(border);
  frame.background = XMakeTile(background);

  sprintf(filegeometry,"=%dx%d%s",width,height,PICTDEFAULT);
  wind = XCreate(argv[0], argv[0], 
		 geometry, filegeometry, &frame, 100, 100);
  if (!wind) { 
    fprintf(stderr, "XCreateWindow failed\n");
    exit(1);
  }
  
  curs = XCreateCursor(curs_width,curs_height,curs_bits,curs_mask_bits,
			curs_x_hot,curs_y_hot,WhitePixel,BlackPixel,GXcopy);
  XUndefineCursor(wind);
  XDefineCursor(wind,curs);

  XSelectInput(wind, ExposeRegion | ExposeWindow | ButtonPressed);
  
  XMapWindow(wind);
  
  for(i=0;i<ncolors;i++) {
    cmap[i].pixel = cmadr[i];
    cmap[i].red = rval[i]<<8;
    cmap[i].green = gval[i]<<8;
    cmap[i].blue = bval[i]<<8;
  }
  XStoreColors(ncolors+3, cmap);

  while(1) {
    XNextEvent(&event); 
    switch((int)event.type) {
	
    case ButtonPressed:
      XPixmapGetZ(wind,but->x,but->y,1,1,&readpix);
      for(i=0;cmadr[i] != readpix && i<ncolors+2;i++) ;
      switch((int)but->detail & ValueMask) {
      case LeftButton:
	cmap[ncolors+1].red = cmap[i].red;
	cmap[ncolors+1].green = cmap[i].green;
	cmap[ncolors+1].blue = cmap[i].blue;
	break;
      case MiddleButton:
	cmap[ncolors+2].red = cmap[i].red;
	cmap[ncolors+2].green = cmap[i].green;
	cmap[ncolors+2].blue = cmap[i].blue;
	XChangeBorder(wind,XMakeTile(cmap[i].pixel));
	break;
      case RightButton:
	cmap[ncolors].red = cmap[i].red;
	cmap[ncolors].green = cmap[i].green;
	cmap[ncolors].blue = cmap[i].blue;
      }
      XStoreColors(3,cmap+ncolors);
      break;
    case ExposeWindow:
      if(QLength() > 0) {
	XPeekEvent(&peekevent);
	if(peekevent.type == ExposeWindow) break;
      }
      XQueryWindow(wind,&winfo);
      
    case ExposeRegion:
      XClear(wind);
      XPixSet(wind,x,y,width,height,background);
      for(i=0;i<ncolors;i++) {
	x = (i / ((ncolors+ncols-1) / ncols)) ;
	x = x * (maxwidth+CWIDTH) + x * 20;
	y = (i%((ncolors+ncols-1)/ncols)) * cheight;
	XPixSet(wind,x,y,CWIDTH-10,cheight,cmadr[order[i]]);
	XTextMask(wind,x+CWIDTH-5,y,
		  cname[order[i]],strlen(cname[order[i]]),
		  font,foreground);
	XTextMask(wind,x+CWIDTH-5+maxwidth-7*fontinfo.width,y,
		  chex[order[i]],strlen(chex[order[i]]),
		  font,foreground);
      }
    }
  }
}

usage (program)
    char *program;
{
    fprintf(stderr,"usage: %s [ options ]\n",program);
    fprintf(stderr,"     where options are one or more of:\n");
    fprintf(stderr,"  [host:display] [=geom] [-bw] [-bd] [-bg] [-fg] [-fn]\n");
    exit(1);
}

arrange(n,r,g,b,o)
int n, *r, *g, *b, *o;
{
  int i, j, k, m;

  for(j=0;j<n-1;j++) {
    k = j;
    for(i=j+1;i<n;i++) {
      if(greater(o[i],o[k],r,g,b)) k=i;
    }
    if(k>j) {
      m = o[k];
      for(i=k;i>j;i--) o[i] = o[i-1];
      o[j] = m;
    }
  }
}

greater(i,j,r,g,b)
int i, j, *r, *g, *b;
{
  int ci, cj, si, sj;
  ci = color(r[i],g[i],b[i]);
  cj = color(r[j],g[j],b[j]);
  if(ci>cj) return(1);
  else if(ci<cj) return(0);
  si = bright(ci,r[i],g[i],b[i]);
  sj = bright(cj,r[j],g[j],b[j]);
  if(si>=sj) return(1);
  return(0);
}

strictcolor(r,g,b)
int r, g, b;
{
  if(r>g  && r>b) return(6);
  if(r==g && r>b) return(5);
  if(g>r  && g>b) return(4);
  if(g==b && g>r) return(3);
  if(b>g  && b>r) return(2);
  if(r==b && r>g) return(1);
  return(0);
}

color(r,g,b)
int r, g, b;
{
  int r0, g0, b0, m, f=2;
  m = MIN(MIN(r,g),b);
  r0 = r - m;
  g0 = g - m;
  b0 = b - m;
  if(r0==0 && g0==0 && b0==0) return(0);
  if((b0==0 && r0> f*g0) || (g0==0 && r0> f*b0)) return(6);
  if((r0==0 && g0> f*b0) || (b0==0 && g0> f*r0)) return(4);
  if((g0==0 && b0> f*r0) || (r0==0 && b0> f*g0)) return(2);
  if(b0==0 && r0<=f*g0) return(5);
  if(r0==0 && g0<=f*b0) return(3);
  if(g0==0 && b0<=f*r0) return(1);
}

bright(c,r,g,b)
int c, r, g, b;
{
  if(c==6 || c==5) return((r<<16)+(g<<8)+b);
  if(c==4 || c==3) return((g<<16)+(b<<8)+r);
  if(c==2 || c==1) return((b<<16)+(r<<8)+g);
  return((r<<16)+(g<<8)+b);
}
