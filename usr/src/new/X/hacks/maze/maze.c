/* Copyright    Massachusetts Institute of Technology    1984, 1985	*/

#include <X/Xlib.h>
#include <stdio.h>
#include <sgtty.h>
#define MAZE_SIZE 36
#define PIXSIZE 768
#define SCANSIZE (PIXSIZE/16)
#define HALFSIZE (SCANSIZE/2)
#define CENTER (PIXSIZE/2)
#define SIDE (PIXSIZE/3)
#define MASK_SIZE ((PIXSIZE*PIXSIZE)/16)
#define STRIP_SIZE (((PIXSIZE/6)*PIXSIZE)/16)

struct room {
    struct room *north;
    struct room *south;
    struct room *east;
    struct room *west;
};

struct room *front(dir, rm)
int dir;
struct room *rm;
{
    if(rm == NULL) {
	printf("NULL room\n");
	exit(1);
    }
    switch (dir) {
	case 2:  return(rm->south);
	case 1:  return(rm->east);
	case 0:  return(rm->north);
	case 3:  return(rm->west);
	default: printf("dir = %d\n",dir);
		 exit(1);
	}
}

struct room *left(dir, rm)
int dir;
struct room *rm;
{
    return(front((dir + 3) % 4, rm));
}

struct room *right(dir, rm)
int dir;
struct room *rm;
{
    return(front((dir + 1) % 4, rm));
}

Window win;
int forepix, backpix;
Bitmap pat[20];

main(argc,argv)
    int argc;
    char **argv;
{
    int dir,i,fd,moved;
    struct room m[MAZE_SIZE],*this_room,*next_room;
    char c;
    XEvent ev;
    char *str, *prog;
    char *maze_file = NULL;
    char *display = NULL;
    int nbytes;
    int bwidth;
    char *fore_color, *back_color, *brdr_color;
    Pixmap bground, border;
    int reverse = 0;
    ColorDef cdef;
    WindowInfo info;

    prog = *argv;
    if ((str = XGetDefault(prog, "ReverseVideo")) && strcmp(str, "on") == 0)
	    reverse = 1;
    bwidth = 2;
    if (str = XGetDefault(prog, "BorderWidth"))
	    bwidth = atoi(str);
    fore_color = XGetDefault(prog, "ForeGround");
    back_color = XGetDefault(prog, "BackGround");
    brdr_color = XGetDefault(prog, "Border");
    for (argv++; --argc; argv++) {
	if (!strcmp(*argv, "-rv")) {
	    reverse = 1;
	} else if (!strcmp(*argv, "-fg") && argc) {
	    argv++;
	    argc--;
	    fore_color = *argv;
	} else if (!strcmp(*argv, "-bg") && argc) {
	    argv++;
	    argc--;
	    back_color = *argv;
	} else if (!strcmp(*argv, "-bd") && argc) {
	    argv++;
	    argc--;
	    brdr_color = *argv;
	} else if (index(*argv, ':')) {
	    display = *argv;
	} else if (**argv == '-' || maze_file != NULL) {
	    printf("usage: maze [-rv] [-fg <color>] [-bg <color>] [-bd <color>] [host:display] [file]\n");
	    exit(1);
	} else {
	    maze_file = *argv;
	}
    }
    if (!XOpenDisplay(display)) {
	perror(prog);
	exit(1);
    }
    if (reverse) {
	    forepix = BlackPixel;
	    backpix = WhitePixel;
	    bground = WhitePixmap;
	    border = BlackPixmap;
    } else {
	    forepix = WhitePixel;
	    backpix = BlackPixel;
	    bground = BlackPixmap;
	    border = WhitePixmap;
    }
    if (DisplayCells() > 2) {
	    if (fore_color && XParseColor(fore_color, &cdef) &&
		    XGetHardwareColor(&cdef))
		    forepix = cdef.pixel;
	    if (back_color && XParseColor(back_color, &cdef) &&
		    XGetHardwareColor(&cdef))
		    bground = XMakeTile(backpix = cdef.pixel);
	    if (brdr_color && XParseColor(brdr_color, &cdef) &&
		    XGetHardwareColor(&cdef))
		    border = XMakeTile(cdef.pixel);
    }

    get_maze(m, maze_file);
    this_room = &m[0];
    next_room = NULL;
    dir = 1;
    XQueryWindow(RootWindow, &info);
    win = XCreateWindow(RootWindow,
			(info.width - PIXSIZE - (bwidth<<1))>>1,
			(info.height - PIXSIZE - (bwidth<<1))>>1,
			PIXSIZE,PIXSIZE,bwidth,border,bground);
    XMapWindow(win);
    XSelectInput(win, ButtonPressed|KeyPressed|ExposeWindow);
    set_up_maze();
    while (1) {
	XClear(win);
        next_room = this_room;
	for(i=0;i<9;i++){
	    if(right(dir,next_room) != NULL) draw_pass(i-1,1);
	    else draw_wall(i-1,1);
	    if(left(dir,next_room) != NULL) draw_pass(i-1,-1);
	    else draw_wall(i-1,-1);
	    if((next_room = front(dir,next_room)) == NULL){
	        draw_block(i);
		break;
	    }
	}
	if(i==9) XPixSet(win,CENTER-1,0,2,PIXSIZE,backpix);
	moved = 0;
	while(!moved){
	    XNextEvent(&ev);
	    switch(ev.type){
	    case ButtonPressed:
		switch(((XButtonPressedEvent *)&ev)->detail & ValueMask) {
		case LeftButton:
			str = "l";
			break;
		case MiddleButton:
			if (((XButtonPressedEvent *)&ev)->detail & ShiftMask)
			    str = "b";
			else
			    str = "f";
			break;
		case RightButton:
			str = "r";
			break;
		}
		nbytes = 1;
		break;
	    case KeyPressed:
		str = XLookupMapping (&ev, &nbytes);
		break;
	    case ExposeWindow:
		XSync(0);
		while (QLength()) {
			XPeekEvent(&ev);
			if (ev.type != ExposeWindow)
				break;
			XNextEvent(&ev);
		}
		nbytes = 0;
		moved++;
	    }
	    if (nbytes == 1)
            switch (*str) {
	        case 'r':
	            dir = (dir + 1) % 4;
		    moved++;
		    break;
	        case 'l':
	            dir = (dir + 3) % 4;
		    moved++;
		    break;
	        case 'f':
		   next_room = front(dir,this_room);
		    if(next_room != NULL){
		        this_room = next_room;
			moved++;
		    }
		    break;
	        case 'b':
		    next_room = front((dir+2)%4,this_room);
		    if(next_room != NULL){
		        this_room = next_room;
			moved++;
		    }
		    break;
	        case 'q':
		    exit(0);
	        default:
	            break;
	    }
	}
    }
}
		    
#ifdef notdef
#define MAZE_SIDE 6
print_maze(m)
struct room m[MAZE_SIZE];
{
    register int x, y;

    for(x=0;x<MAZE_SIDE;x++){
	for(y=0;y<MAZE_SIDE;y++){
	    if(m[6*x+y].north == NULL){
		printf("+--+");
	    }
	    else printf("+  +");
	}
	printf("\n");
	for(y=0;y<MAZE_SIDE;y++){
	    if(m[6*x+y].west == NULL){
		printf("|  ");
	    }
	    else printf("   ");
	    if(m[6*x+y].east == NULL){
		printf("|");
	    }
	    else printf(" ");
	}
	printf("\n");
	for(y=0;y<MAZE_SIDE;y++){
	    if(m[6*x+y].south == NULL){
		printf("+--+");
	    }
	    else printf("+  +");
	}
	printf("\n");
    }
}
#endif

draw_wall(dist,side)
int dist,side;
{
    int x3,x4;
    
    x3 = SIDE>>dist;
    x4 = SIDE>>(dist+1);
 
    if (x3>CENTER) {
	x4 >>= 1;
	if (side < 0) {
	    XPixFill(win,0,0,x4,PIXSIZE,forepix,pat[0],GXcopy,AllPlanes);
	} else {
	    XPixFill(win,PIXSIZE-x4+1,0,x4-1,PIXSIZE,forepix,pat[19-dist-1],GXcopy,AllPlanes);
	}
    } else {
	if(side<0) {
	    XPixFill(win,CENTER-x3+1,0,x4-1,PIXSIZE,forepix,pat[dist+1],GXcopy,AllPlanes);
	} else {
	    XPixFill(win,CENTER+x4+1,0,x4-1,PIXSIZE,forepix,pat[19-dist-1],GXcopy,AllPlanes);
	}
    }
}

draw_pass(dist,side)
int dist,side;
{
    int x3,x4,i;

    x3 = SIDE>>dist;
    x4 = SIDE>>(dist+1);

    if(side < 0){
	XPixSet(win,CENTER-x3,CENTER-x4,x4,x3,forepix);
	XPixSet(win,CENTER-x3,0,1,PIXSIZE,backpix);
	XPixSet(win,CENTER-x4,0,1,PIXSIZE,backpix);
    }
    else {
	XPixSet(win,CENTER+x4,CENTER-x4,x4,x3,forepix);
	XPixSet(win,CENTER+x4,0,1,PIXSIZE,backpix);
	XPixSet(win,CENTER+x3,0,1,PIXSIZE,backpix);
    }
}

draw_block(dist)
int dist;
{
    int x3,x4,i;
    Vertex verts[5];

    x3 = SIDE>>dist;
    x4 = SIDE>>(dist+1);
    
    XPixSet(win,CENTER-x3+1,CENTER-x3,2*x3-1,2*x3,forepix);
}

int default_maze[] = {-1,-1,6,-1,
		      -1,2,-1,-1,
		      -1,-1,8,1,
		      -1,4,9,-1,
		      -1,5,-1,3,
		      -1,-1,-1,4,
		      0,7,12,11,
		      -1,8,-1,6,
		      2,9,-1,7,
		      3,10,-1,8,
		      -1,11,16,9,
		      -1,6,-1,10,
		      6,-1,18,-1,
		      -1,14,19,-1,
		      -1,15,20,13,
		      -1,-1,21,-1,
		      10,17,-1,-1,
		      -1,-1,-1,16,
		      12,19,-1,-1,
		      13,-1,-1,18,
		      14,-1,26,-1,
		      15,22,27,-1,
		      -1,23,28,21,
		      -1,-1,-1,22,
		      -1,25,30,-1,
		      -1,-1,31,24,
		      20,27,-1,-1,
		      21,-1,-1,26,
		      22,29,34,-1,
		      -1,-1,-1,28,
		      24,-1,-1,-1,
		      25,32,-1,-1,
		      -1,33,-1,31,
		      -1,34,-1,32,
		      28,35,-1,33,
		      -1,-1,-1,34};

get_maze(m, maze_file)
struct room m[MAZE_SIZE];
char *maze_file;
{
    FILE *fp;
    int i,j,n,e,s,w;
    
    if (maze_file == NULL) {
	for (i=0,j=0;i<MAZE_SIZE;i++){
		n=default_maze[j++];
		e=default_maze[j++];
		s=default_maze[j++];
		w=default_maze[j++];
		if(n == -1) m[i].north = NULL;
		else m[i].north = &m[n];
		if(s == -1) m[i].south = NULL;
		else m[i].south = &m[s];
		if(e == -1) m[i].east = NULL;
		else m[i].east = &m[e];
		if(w == -1) m[i].west = NULL;
		else m[i].west = &m[w];
	}
	return;
    }
    if((fp = fopen(maze_file,"r")) == NULL){
	perror("maze");
	exit(1);
    }
    for(i=0;i<MAZE_SIZE;i++){
	fscanf(fp,"%d,%d,%d,%d\n",&n,&e,&s,&w);
	if(n == -1) m[i].north = NULL;
	else m[i].north = &m[n];
	if(s == -1) m[i].south = NULL;
	else m[i].south = &m[s];
	if(e == -1) m[i].east = NULL;
	else m[i].east = &m[e];
	if(w == -1) m[i].west = NULL;
	else m[i].west = &m[w];
    }
    fclose(fp);
}

set_up_maze()
{
    Bitmap b;
    int i,bound[21],top[21],k,j,x;
    short *strips[21],*mask;

    bound[0] = 0;
    bound[10] = CENTER;
    bound[20] = PIXSIZE;
    for(i=1;i<10;i++){
	bound[10-i]=CENTER-(SIDE/(1<<(9-i)));
	bound[10+i]=CENTER+(SIDE/(1<<(9-i)));
    }
    for(i=0;i<21;i++)
	top[i] = 0;
    mask = (short *)calloc(MASK_SIZE, sizeof(short));
    for (i=0;i<21;i++)
	strips[i]=(short *)malloc(STRIP_SIZE * sizeof(short));
    for(i=0;i<HALFSIZE;i++){
	for(j=0;j<16;j++){
	    mask[SCANSIZE*(16*i+j)+i] = 0xaaaa>>(15-j);
	    mask[SCANSIZE*(16*i+j)+SCANSIZE-1-i] = 0xaaaa<<(15-j);
	    mask[SCANSIZE*(PIXSIZE-1-16*i-j)+i] = 0x5555>>(15-j);
	    mask[SCANSIZE*(PIXSIZE-1-16*i-j)+SCANSIZE-1-i] = 0x5555<<(15-j);
	}
    }
    for(i=0;i<(HALFSIZE-1);i++){
	for(j=(i+1)*16;j<CENTER;j++){
	    if((j % 2) == 1){
		mask[SCANSIZE*j+i] = 0xaaaa;
		mask[SCANSIZE*j+SCANSIZE-1-i] = 0xaaaa;
		mask[SCANSIZE*(PIXSIZE-1-j)+i] = 0x5555;
		mask[SCANSIZE*(PIXSIZE-1-j)+SCANSIZE-1-i] = 0x5555;
	    }
	    else {
		mask[SCANSIZE*j+i] = 0x5555;
		mask[SCANSIZE*j+SCANSIZE-1-i] = 0x5555;
		mask[SCANSIZE*(PIXSIZE-1-j)+i] = 0xaaaa;
		mask[SCANSIZE*(PIXSIZE-1-j)+SCANSIZE-1-i] = 0xaaaa;
	    }
	}
    }
    for(j=0;j<PIXSIZE;j++){
        for(i=0;i<SCANSIZE;i++){
	    for(k=1;k<21;k++){
		if((i*16) < bound[k]) {
		    strips[k-1][top[k-1]++] = mask[SCANSIZE*j+i];
		    break;
		}
	    }
	}
    }
    for(i=5;i<15;i++)
	top[i] = 0;
    for(i=0;i<PIXSIZE;i++){
	strips[5][top[5]++] = mask[SCANSIZE*i+HALFSIZE-1];
	strips[6][top[6]++] = mask[SCANSIZE*i+HALFSIZE-1]>>8;
	strips[7][top[7]++] = mask[SCANSIZE*i+HALFSIZE-1]>>12;
	strips[8][top[8]++] = mask[SCANSIZE*i+HALFSIZE-1]>>14;
	strips[9][top[9]++] = mask[SCANSIZE*i+HALFSIZE-1]>>15;
	strips[10][top[10]++] = mask[SCANSIZE*i+HALFSIZE];
	strips[11][top[11]++] = mask[SCANSIZE*i+HALFSIZE]>>1;
	strips[12][top[12]++] = mask[SCANSIZE*i+HALFSIZE]>>2;
	strips[13][top[13]++] = mask[SCANSIZE*i+HALFSIZE]>>4;
	strips[14][top[14]++] = mask[SCANSIZE*i+HALFSIZE]>>8;
    }
    for(k=1;k<21;k++){
	if (bound[k] != bound[k-1]) {
	    pat[k-1] = XStoreBitmap(bound[k]-bound[k-1],PIXSIZE,strips[k-1]);
	    if (pat[k-1] == NULL)
		exit(1);
	} else
	   pat[k-1] = NULL;
    }
    free(mask);
    for (i=0;i<21;i++)
	free(strips[i]);
}
