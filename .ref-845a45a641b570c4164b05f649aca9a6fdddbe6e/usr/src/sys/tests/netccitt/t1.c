#define LITTLE_ENDIAN 1
typedef unsigned char u_char;
#define BYTE_ORDER LITTLE_ENDIAN
#include "pk.h"

union foo {
	struct x25_packet pk;
	char	data[10];
} testme;

main() {
	register struct x25_packet *xp = &(testme.pk);

	xp -> fmt_identifier = 1;
	xp -> packet_type = 0xfb;

#define t testme.data 
	printf("%x %x %x %x \n", t[0], t[1], t[2], t[3]);
}
