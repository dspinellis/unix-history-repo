#include <stdio.h>

main ()

{
int copy = 0;
char c;

while (1) {
	if ((c= getchar ()) == ':')
		copy=1;
		
	else if  (copy) {
		if ( c == ' ')
			continue;
		if (c == '*')
			{
			copy=0;
			putchar ('\n');
			continue;
			}
		else putchar (c);
		}
		
	}
}

