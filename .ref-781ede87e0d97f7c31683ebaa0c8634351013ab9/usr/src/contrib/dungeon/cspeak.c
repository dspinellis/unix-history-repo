#include <stdio.h>

/* 	routine to sort out input stream  */
/*	first character determines destination of the following data
		n - get arguments for 'rspeak'
		s - put data on dungeon save file until 'e' is received
	     else - pass text to screen	*/

inprd_(pa,pb,pc)

int *pa, *pb, *pc;
{
int chr;

	(*pa)=(*pb)=(*pc)=0;

	while((chr = getchar()) != EOF) {
		if (chr == '\n')
			continue;
		switch (chr) {

			case 'n':	/* get args for rspeak  */
				if (scanf("%d%d%d",pa,pb,pc) > 0)
					return;
				else
					printf("Speak input error\n");
				break;
	
			case 's':	/* send save data to file  */
				wrtsave();
				break;

			default:
				putchar(chr);
				break;
	
		}
		/* send text to screen  */
		while((chr = getchar()) != EOF){
			if (chr == '~')
				break;
			putchar(chr);
			if (chr == '\n')
				break;
		}
	}

	/* terminate process */
	printf("Goodbye ... GASP\n");
	exit(0);
}

/*	write a save file  */

wrtsave()
{
	FILE *savptr, *fopen();
	char chr;

	savptr = fopen("dungeon.sav","w");

	while ((chr = getchar()) != EOF) {
		if (chr == 'e')	{		/* check for end char */
			fclose(savptr);
			return;
		}
		putc(chr,savptr);
	}
	printf("EOF during save\n");
	exit(0);
}
