typedef unsigned char u_char;
#define LITTLE_ENDIAN 1
#define BYTE_ORDER LITTLE_ENDIAN
#include "pk.h"
unsigned char data[] = {16, 0, 0xfb, 1, 0x34, 0 };
main()
{
	register struct x25_packet *xp = data;

#define e(q) printf("%s %x\n", "q", xp->q);
	e(lc_group_number);
	e(fmt_identifier);
	e(q_bit);
	e(logical_channel_number);
	e(packet_type);
	e(packet_data);
	putchar('n');
}
