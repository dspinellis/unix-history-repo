#include <stdio.h>

#ifndef CINDEXFILE
#define CINDEXFILE	"/usr/games/lib/dunlib/dindx.dat"
#endif

main(numargs, argptr)

int numargs;
char *argptr[];
{
int  chr;
FILE *fpin;


	fprintf(stderr,"Yawn... \n");

/* 	open init file */
	fpin = fopen(CINDEXFILE, "r");
	if (fpin == NULL)  {
		fclose(fpin);
		fprintf(stderr,"Init file missing.\n");
		exit(0);
	}

/* 	transfer init file into the pipe */

	while ((chr = getc(fpin)) != EOF)
		putchar((char)chr);

	fclose(fpin);

/*	check for restore file argument  */

	if(numargs > 1){
		fpin = fopen(*++argptr,"r");
		if( fpin == NULL)
			fprintf(stderr,"Restore file missing.\n");
		
		else {
			putchar('R');
			while((chr = getc(fpin)) != EOF)
				putchar((char)chr);
			fprintf(stderr,"Now, where were we...\n");
			fclose(fpin);
		}
	}

	fprintf(stderr,"Oh hello .. \n");

/*	send end of init data flag	*/

	putchar('?');
	fflush(stdout);

/*	send lines of standard input to pipe	*/

	while ((chr = getchar()) != EOF){
		putchar(chr);
		if (chr == '\n')
			fflush(stdout);
	}

/* 	end the process	*/
	fprintf(stderr,"Goodnight .. \n");
}
