/*	fp.h	1.2	86/01/03	*/

#define	EXPMASK		0x7f800000
#define	SIGNBIT		0x80000000
#define	ONE_EXP		0x40800000
#define	TWO_EXP		0x40000000
#define	EXPSHIFT	23
#define	HID_POS		24
#define	HID_R0R1	24+32
#define	CLEARHID	0xff7fffff
#define	EXPSIGN		0x40000000
#define	MAX_EXP_DIF	55
#define	SMAX_EXP_DIF	23
#define	BIAS		0x80
#define	BIASP1		0x81
#define	BIASM1		0x7f
#define	HUGE0		0x7fffffff
#define	HUGE1		0xffffffff
