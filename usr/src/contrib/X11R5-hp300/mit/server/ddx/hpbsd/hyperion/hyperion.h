/*
 */

#include "../hpDisplays.h"

/*
 * Map of the hyperion chip in memory ...
 */

typedef VOLATILE struct {
    u_short id_reset;		/* id and reset register 	*/ /* 0x01 */
    u_char filler2;
    u_char interrupts;		/* interrupts			*/ /* 0x03 */
    u_char filler2a;
    u_char t_memwide;		/* top half frame buf width	*/ /* 0x05 */
    u_char filler2b;
    u_char b_memwide;		/* bottom half frame buf width	*/ /* 0x07 */
    u_char filler2c;
    u_char t_memhigh;		/* top half frame buf height 	*/ /* 0x09 */
    u_char filler2d;
    u_char b_memhigh;		/* bot half frame buf height	*/ /* 0x0b */
    u_char filler2e;
    u_char t_dispwide;		/* top half display width	*/ /* 0x0d */
    u_char filler2f;
    u_char b_dispwide;		/* bot half display width	*/ /* 0x0f */
    u_char filler2g;
    u_char t_disphigh;		/* top half display height	*/ /* 0x11 */
    u_char filler2h;
    u_char b_disphigh;		/* bot half display height	*/ /* 0x13 */
    u_char filler2i;
    u_char id_second;		/* secondary id 5=LCC 6=HRC 	*/ /* 0x15 */
    u_char filler2j;            /* 7=HRM, 9=319X		*/
    u_char bits;		/* 0 square pixels, 1 double hi */ /* 0x17 */
    u_char filler3[0x4000-0x17-1];
    u_char nblank;		/* video output enable		*/ /* 0x4000 */
} HYPER;

#define HY_VIDEO_ON  0x05
#define HY_VIDEO_OFF 0x00

#define getHyHardware(pScreen) ((HYPER *) getGpHardware(pScreen))

extern void hyperMoveBits();
