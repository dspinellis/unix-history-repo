/*
 * Decode dtext.dat file into readable ASCII.
 * John Gilmore (hoptoad!gnu), December 1986
 */
#include <stdio.h>
#define STRLEN 74
char string[STRLEN+1];
int recno = 0;

main() {
	unsigned char byte, byte2;
	int i;

	while (1) {
		recno++;
		byte = getchar();
		byte2 = getchar();
		if (1 != fread (string, STRLEN, 1, stdin)) exit(0);
		for (i = 1; i <= STRLEN; i++) 
			string[i-1] ^= (recno&31)+i;
		printf("%2x%02x %s\n",
			byte2, byte, string);
	}
}
		
