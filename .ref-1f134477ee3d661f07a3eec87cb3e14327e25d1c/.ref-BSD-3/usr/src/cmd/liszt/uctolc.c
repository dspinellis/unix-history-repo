#include <stdio.h>
#include <ctype.h>
/*   uctolc   					j foderaro
	convert upper case to lower case
	in the first field of a tags fil
 */
#define TRUE -1
main()
{
	register char c;
	while(TRUE)
	{
	  pt1:
		while( (c=getchar()) != EOF)
		{	
		    if(isupper(c))putchar( c + ('a' - 'A'));
		    else if(c == ' ') goto pt2;
		    else putchar(c);
		}
	        exit();
	pt2:
		putchar(c);
		while( (c=getchar()) != EOF)
		{
		    putchar(c);
		    if(c == '\n') goto pt1;
		}
		exit();
	}
}
