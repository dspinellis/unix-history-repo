#ifndef lint
static char sccsid[] = "@(#)mktable.c	1.2 (Berkeley) 1/24/88";
#endif /* not lint */

#ifndef lint
static char *mktable_c = "$Header: mktable.c,v 10.5 86/02/01 15:44:59 tony Rel $";
#endif 	lint

#include <stdio.h>

char reverse_byte[0x100] = { 0 };

main()
{
	seed_table();
	fill_in_table();
	print_table();
	exit(0);
}

seed_table()
{
	int i = 0;
	int j = 0x100/2;
	int b = 0;

	for (;;) {
		reverse_byte[i] = b;
		if (j == 0)
			break;
		i = 2*i + 1;
		b = b + j;
		j = j/2;
	}
}

fill_in_table()
{
	int b;

	for (b = 1; b < 0x100-2; b += 1)
		reverse_byte[b+1] =
			reverse_byte[b] ^ reverse_byte[b^(b+1)];
}

print_table()
{
	int b;

	printf("char reverse_byte[0x100] = {\n\t");
	b = 0;
	for (;;) {
		printf("0x%02x", reverse_byte[b] & 0xff);
		b += 1;
		if (b == 0x100) {
			printf("\n};\n");
			return;
		} else if (b % 8 == 0) {
			printf(",\n\t");
		} else {
			printf(", ");
		}
	}
}
