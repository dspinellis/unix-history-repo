#ifndef lint
static char sccsid[] = "@(#)atoplot.c	4.2 (Berkeley) %G%";
#endif

#include <stdio.h>

float deltx;
float delty;

char	*mapLineType();

main(argc,argv)  char **argv; {
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


fplt(fin)  FILE *fin; {
	int c;
	char s[256];
	int xi,yi,x0,y0,x1,y1,r,dx,n,i;
	int pat[256];

	openpl();
	while((c=getc(fin)) != EOF){
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
			linemod( mapLineType(s) );
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
		/* scan to newline */
		while( (c = getc( fin )) != '\n' ) {
			if ( c == EOF ) {
				break;
			}
		    }
		}
	closepl();
	}
getsi(fin)  FILE *fin; {	/* get an integer stored in 2 ascii bytes. */
	int	i;

	if ( fscanf(fin, " %d", & i) != 1 ) {
		return(EOF);
	}
	return( i );
}
getstr(s,fin)  char *s;  FILE *fin; {
	for( ; *s = getc(fin); s++)
		if(*s == '\n')
			break;
	*s = '\0';
}

char	*lineMap[] = {
    "solid",		/* line type 0 */
    "solid",		/* line type 1 */
    "dotted",		/* line type 2 */
    "dotdashed",	/* line type 3 */
    "shortdashed",	/* line type 4 */
    "longdashed",	/* line type 5 */
    "dotlongdash",	/* line type 6 */
    "dotshortdash",	/* line type 7 */
    "dotdotdash",	/* line type 8 */
}	;

char *
mapLineType( cp )
    char	*cp;
{
    int		i;

    if ( sscanf(cp, "%d", &i) == 1 ) {
	if ( i < 0 || i > sizeof(lineMap)/sizeof(char *) ) {
	    i = 1;
	}
	return( lineMap[i] );
    }
    else {
	return( cp );
    }
}
