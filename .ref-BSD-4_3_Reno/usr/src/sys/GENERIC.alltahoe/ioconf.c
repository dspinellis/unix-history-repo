#include "../sys/param.h"
#include "machine/pte.h"
#include "../sys/buf.h"
#include "../sys/map.h"

#include "../tahoevba/vbavar.h"

#define C (caddr_t)

extern struct vba_driver vddriver;
extern Xvdintr0();
int	 (*vdint0[])() = { Xvdintr0, 0 } ;
extern struct vba_driver vddriver;
extern Xvdintr1();
int	 (*vdint1[])() = { Xvdintr1, 0 } ;
extern struct vba_driver vddriver;
extern Xvdintr2();
int	 (*vdint2[])() = { Xvdintr2, 0 } ;
extern struct vba_driver cydriver;
extern Xcyintr0();
int	 (*cyint0[])() = { Xcyintr0, 0 } ;
extern struct vba_driver cydriver;
extern Xcyintr1();
int	 (*cyint1[])() = { Xcyintr1, 0 } ;
extern struct vba_driver vxdriver;
extern Xvackint0(), Xvcmdrsp0(), Xvunsol0();
int	 (*vxint0[])() = { Xvackint0, Xvcmdrsp0, Xvunsol0, 0 } ;
extern struct vba_driver vxdriver;
extern Xvackint1(), Xvcmdrsp1(), Xvunsol1();
int	 (*vxint1[])() = { Xvackint1, Xvcmdrsp1, Xvunsol1, 0 } ;
extern struct vba_driver mpdriver;
extern Xmpintr0(), Xmpdlintr0();
int	 (*mpint0[])() = { Xmpintr0, Xmpdlintr0, 0 } ;
extern struct vba_driver mpdriver;
extern Xmpintr1(), Xmpdlintr1();
int	 (*mpint1[])() = { Xmpintr1, Xmpdlintr1, 0 } ;
extern struct vba_driver acedriver;
extern Xacecint0(), Xacerint0();
int	 (*aceint0[])() = { Xacecint0, Xacerint0, 0 } ;
extern struct vba_driver acedriver;
extern Xacecint1(), Xacerint1();
int	 (*aceint1[])() = { Xacecint1, Xacerint1, 0 } ;
extern struct vba_driver enpdriver;
extern Xenpintr0();
int	 (*enpint0[])() = { Xenpintr0, 0 } ;
extern struct vba_driver enpdriver;
extern Xenpintr1();
int	 (*enpint1[])() = { Xenpintr1, 0 } ;
extern struct vba_driver drdriver;
extern Xdrintr0();
int	 (*drint0[])() = { Xdrintr0, 0 } ;
extern struct vba_driver ikdriver;
extern Xikintr0();
int	 (*ikint0[])() = { Xikintr0, 0 } ;
extern struct vba_driver hdcdriver;
extern Xhdintr0();
int	 (*hdcint0[])() = { Xhdintr0, 0 } ;
extern struct vba_driver hdcdriver;
extern Xhdintr1();
int	 (*hdcint1[])() = { Xhdintr1, 0 } ;
extern struct vba_driver vxdriver;
extern Xvackint2(), Xvcmdrsp2(), Xvunsol2();
int	 (*vxint2[])() = { Xvackint2, Xvcmdrsp2, Xvunsol2, 0 } ;
extern struct vba_driver vxdriver;
extern Xvackint3(), Xvcmdrsp3(), Xvunsol3();
int	 (*vxint3[])() = { Xvackint3, Xvcmdrsp3, Xvunsol3, 0 } ;
extern struct vba_driver exdriver;
extern Xexintr0();
int	 (*exint0[])() = { Xexintr0, 0 } ;

struct vba_ctlr vbminit[] = {
/*	 driver,	ctlr,	vbanum,	alive,	intr,	addr */
	{ &vddriver,	0,	'?',	0,	vdint0, C 0xffff2000 },
	{ &vddriver,	1,	'?',	0,	vdint1, C 0xffff2100 },
	{ &vddriver,	2,	'?',	0,	vdint2, C 0xffff2200 },
	{ &cydriver,	0,	'?',	0,	cyint0, C 0xffff4000 },
	{ &cydriver,	1,	'?',	0,	cyint1, C 0xffff6000 },
	{ &hdcdriver,	0,	'?',	0,	hdcint0, C 0xc5010000 },
	{ &hdcdriver,	1,	'?',	0,	hdcint1, C 0xc6010000 },
	0
};

struct vba_device vbdinit[] = {
	/* driver,  unit, ctlr,  vbanum, slave,   intr,    addr,    dk, flags*/
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
	{ &cydriver,   0,     0,  '?',     0,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   1,     0,  '?',     1,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   2,     0,  '?',     2,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   3,     0,  '?',     3,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   4,     1,  '?',     0,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   5,     1,  '?',     1,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   6,     1,  '?',     2,        0, C 0x0     ,  0,  0x0 },
	{ &cydriver,   7,     1,  '?',     3,        0, C 0x0     ,  0,  0x0 },
	{ &vxdriver,   0,    -1,  '?',    -1,   vxint0, C 0xfffe0000,  0,  0x0 },
	{ &vxdriver,   1,    -1,  '?',    -1,   vxint1, C 0xfffe4000,  0,  0x0 },
	{ &mpdriver,   0,    -1,  '?',    -1,   mpint0, C 0xffff5000,  0,  0x0 },
	{ &mpdriver,   1,    -1,  '?',    -1,   mpint1, C 0xffff5100,  0,  0x0 },
	{ &acedriver,   0,    -1,  '?',    -1,   aceint0, C 0xffff0000,  0,  0xfff80000 },
	{ &acedriver,   1,    -1,  '?',    -1,   aceint1, C 0xffff0100,  0,  0xfff90000 },
	{ &enpdriver,   0,    -1,  '?',    -1,   enpint0, C 0xfff41000,  0,  0x0 },
	{ &enpdriver,   1,    -1,  '?',    -1,   enpint1, C 0xfff61000,  0,  0x0 },
	{ &drdriver,   0,    -1,  '?',    -1,   drint0, C 0xffff7000,  0,  0x0 },
	{ &ikdriver,   0,    -1,  '?',    -1,   ikint0, C 0xffff8000,  0,  0x0 },
	{ &hdcdriver,   0,     0,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   1,     0,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   2,     0,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   3,     0,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   4,     1,  '?',     0,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   5,     1,  '?',     1,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   6,     1,  '?',     2,        0, C 0x0     ,  1,  0x0 },
	{ &hdcdriver,   7,     1,  '?',     3,        0, C 0x0     ,  1,  0x0 },
	{ &vxdriver,   2,    -1,  '?',    -1,   vxint2, C 0xcd020000,  0,  0x0 },
	{ &vxdriver,   3,    -1,  '?',    -1,   vxint3, C 0xce020000,  0,  0x0 },
	{ &exdriver,   0,    -1,  '?',    -1,   exint0, C 0xfff00000,  0,  0x0 },
	0
};
