#include "../h/param.h"
#include "../machine/pte.h"
#include "../h/buf.h"
#include "../h/map.h"

#include "../tahoevba/vbavar.h"

#define C (caddr_t)

extern struct vba_driver hdcdriver;
extern Xhdintr0();
int	 (*hdcint0[])() = { Xhdintr0, 0 } ;
extern struct vba_driver hdcdriver;
extern Xhdintr1();
int	 (*hdcint1[])() = { Xhdintr1, 0 } ;
extern struct vba_driver hdcdriver;
extern Xhdintr2();
int	 (*hdcint2[])() = { Xhdintr2, 0 } ;
extern struct vba_driver vddriver;
extern Xvdintr0();
int	 (*vdint0[])() = { Xvdintr0, 0 } ;
extern struct vba_driver vddriver;
extern Xvdintr1();
int	 (*vdint1[])() = { Xvdintr1, 0 } ;
extern struct vba_driver vddriver;
extern Xvdintr2();
int	 (*vdint2[])() = { Xvdintr2, 0 } ;
extern struct vba_driver vxdriver;
extern Xvackint0(), Xvcmdrsp0(), Xvunsol0();
int	 (*vxint0[])() = { Xvackint0, Xvcmdrsp0, Xvunsol0, 0 } ;
extern struct vba_driver vxdriver;
extern Xvackint1(), Xvcmdrsp1(), Xvunsol1();
int	 (*vxint1[])() = { Xvackint1, Xvcmdrsp1, Xvunsol1, 0 } ;
extern struct vba_driver exdriver;
extern Xexintr0();
int	 (*exint0[])() = { Xexintr0, 0 } ;

struct vba_ctlr vbminit[] = {
/*	 driver,	ctlr,	vbanum,	alive,	intr,	addr */
	{ &hdcdriver,	0,	'?',	0,	hdcint0, C 0xc4010000 },
	{ &hdcdriver,	1,	'?',	0,	hdcint1, C 0xc5010000 },
	{ &hdcdriver,	2,	'?',	0,	hdcint2, C 0xc6010000 },
	{ &vddriver,	0,	'?',	0,	vdint0, C 0xffff2000 },
	{ &vddriver,	1,	'?',	0,	vdint1, C 0xffff2100 },
	{ &vddriver,	2,	'?',	0,	vdint2, C 0xffff2200 },
	0
};

struct vba_device vbdinit[] = {
	/* driver,  unit, ctlr,  vbanum, slave,   intr,    addr,    dk, flags*/
	{ &hdcdriver,   0,     0,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   1,     0,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   2,     0,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   3,     0,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   4,     1,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   5,     1,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   6,     1,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   7,     1,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   8,     2,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   9,     2,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,  10,     2,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,  11,     2,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   0,     0,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   1,     0,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   2,     0,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   3,     0,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   4,     1,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   5,     1,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   6,     1,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   7,     1,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   8,     2,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,   9,     2,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,  10,     2,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &vddriver,  11,     2,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &vxdriver,   0,    -1,  '?',    -1,   vxint0, C 0xcd020000,  0,  0x0 },
	{ &vxdriver,   1,    -1,  '?',    -1,   vxint1, C 0xce020000,  0,  0x0 },
	{ &exdriver,   0,    -1,  '?',    -1,   exint0, C 0xfff00000,  0,  0x0 },
	0
};
