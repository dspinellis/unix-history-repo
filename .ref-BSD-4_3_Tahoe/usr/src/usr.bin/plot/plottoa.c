#ifndef lint
static char sccsid[] = "@(#)plottoa.c	4.2 (Berkeley) 1/9/85";
#endif

/*
 * Convert the standard plot input into a readable form for debugging.
 */

#include <stdio.h>

float deltx;
float delty;

main(argc, argv)
char **argv;
{
	int std=1;
	FILE *fin;

	while(argc-- > 1) {
		if(*argv[1] == '-')
			switch(argv[1][1]) {
			case 'l':
				deltx = atoi(&argv[1][2]) - 1;
				break;
			case 'w':
				delty = atoi(&argv[1][2]) - 1;
				break;
			}
		else {
			std = 0;
			if ((fin = fopen(argv[1], "r")) == NULL) {
				fprintf(stderr, "can't open %s\n", argv[1]);
				exit(1);
			}
			fplt(fin);
			fclose(fin);
		}
		argv++;
	}
	if (std)
		fplt( stdin );
	exit(0);
}


fplt(fin)
FILE *fin;
{
	int c;
	char s[256];
	int xi,yi,x0,y0,x1,y1,r,dx,n,i;
	int pat[256];

	openpl();
	while((c = getc(fin)) != EOF){
		switch(c){
		case 'm':
			xi = getsi(fin);
			yi = getsi(fin);
			move(xi,yi);
			break;
		case 'l':
			x0 = getsi(fin);
			y0 = getsi(fin);
			x1 = getsi(fin);
			y1 = getsi(fin);
			line(x0,y0,x1,y1);
			break;
		case 't':
			getstr(s,fin);
			label(s);
			break;
		case 'e':
			erase();
			break;
		case 'p':
			xi = getsi(fin);
			yi = getsi(fin);
			point(xi,yi);
			break;
		case 'n':
			xi = getsi(fin);
			yi = getsi(fin);
			cont(xi,yi);
			break;
		case 's':
			x0 = getsi(fin);
			y0 = getsi(fin);
			x1 = getsi(fin);
			y1 = getsi(fin);
			space(x0,y0,x1,y1);
			break;
		case 'a':
			xi = getsi(fin);
			yi = getsi(fin);
			x0 = getsi(fin);
			y0 = getsi(fin);
			x1 = getsi(fin);
			y1 = getsi(fin);
			arc(xi,yi,x0,y0,x1,y1);
			break;
		case 'c':
			xi = getsi(fin);
			yi = getsi(fin);
			r = getsi(fin);
			circle(xi,yi,r);
			break;
		case 'f':
			getstr(s,fin);
			linemod(s);
			break;
		case 'd':
			xi = getsi(fin);
			yi = getsi(fin);
			dx = getsi(fin);
			n = getsi(fin);
			for(i=0; i<n; i++)pat[i] = getsi(fin);
			dot(xi,yi,dx,n,pat);
			break;
		}
	}
	closepl();
}

/* get an integer stored in 2 ascii bytes. */
getsi(fin)
FILE *fin;
{
	short a, b;
	if((b = getc(fin)) == EOF)
		return(EOF);
	if((a = getc(fin)) == EOF)
		return(EOF);
	a = a<<8;
	return(a|b);
}

getstr(s,fin)
char *s;
FILE *fin;
{
	for( ; *s = getc(fin); s++)
		if(*s == '\n')
			break;
	*s = '\0';
}

/* Print out the arguments to plot routines. */

space(x0,y0,x1,y1)
int x0,y0,x1,y1;
{
	printf( "s %d %d %d %d\n", x0, y0, x1, y1 );
}

openpl()
{
}

closepl()
{
}

erase()
{
	printf( "e\n" );
}

move(xi,yi)
int xi,yi;
{
	printf( "m %d %d\n", xi, yi );
}

cont(xi,yi)
int xi,yi;
{
	printf( "n %d %d\n", xi, yi );
}

line(x0,y0,x1,y1)
int x0,y0,x1,y1;
{
	printf( "l %d %d %d %d\n", x0, y0, x1, y1 );
}

point(xi,yi)
int xi,yi;
{
	printf( "p %d %d\n", xi, yi );
}

label(s)
char *s;
{
	printf( "t%s\n\n", s );
}


arc(xcent,ycent,xbeg,ybeg,xend,yend)
int xcent,ycent,xbeg,ybeg,xend,yend;
{
	printf( "a %d %d %d %d %d %d\n", xcent, ycent, xbeg, ybeg, xend, yend );
}

circle (xc,yc,r)
int xc,yc,r;
{
	printf( "c %d %d %d\n", xc, yc, r );
}

linemod( line )
char	*line;
{
	printf( "f%s\n\n", line );
}

/* don't know what this should do */
dot(xi,yi,dx,n,pat)
int xi,yi,dx,n;
char *pat;
{
	printf("d %d %d %d %d %s\n\n", xi, yi, dx, n, pat);
}
