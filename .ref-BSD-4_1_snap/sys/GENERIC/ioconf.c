#include "../h/param.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/map.h"
#include "../h/mbavar.h"
#include "../h/vm.h"
#include "../h/ubavar.h"

#define C (caddr_t)

extern struct mba_driver hpdriver;
extern struct mba_driver hpdriver;
extern struct mba_driver hpdriver;
extern struct mba_driver hpdriver;
extern struct mba_driver htdriver;
extern struct mba_driver mtdriver;

struct mba_device mbdinit[] = {
	/* Device,  Unit, Mba, Drive, Dk */
	{ &hpdriver, 0,   '?',    0,    1 },
	{ &hpdriver, 1,   '?',  '?',    1 },
	{ &hpdriver, 2,   '?',  '?',    1 },
	{ &hpdriver, 3,   '?',  '?',    1 },
	{ &htdriver, 0,   '?',  '?',    0 },
	{ &mtdriver, 0,   '?',  '?',    0 },
	0
};

struct mba_slave mbsinit [] = {
	/* Driver,  Ctlr, Unit, Slave */
	{ &htdriver,   0,   0,    0 },
	{ &htdriver,   0,   1,    1 },
	{ &mtdriver,   0,   0,    0 },
	{ &mtdriver,   0,   1,    1 },
	0
};

extern struct uba_driver hkdriver;
extern Xrkintr0();
int	 (*hkint0[])() = { Xrkintr0, 0 } ;
extern struct uba_driver lpdriver;
extern Xlpintr0();
int	 (*lpint0[])() = { Xlpintr0, 0 } ;
extern struct uba_driver tmdriver;
extern Xtmintr0();
int	 (*tmint0[])() = { Xtmintr0, 0 } ;
extern struct uba_driver scdriver;
extern Xupintr0();
int	 (*scint0[])() = { Xupintr0, 0 } ;
extern struct uba_driver udadriver;
extern Xudintr0();
int	 (*udaint0[])() = { Xudintr0, 0 } ;
extern struct uba_driver dndriver;
extern Xdnintr0();
int	 (*dnint0[])() = { Xdnintr0, 0 } ;
extern struct uba_driver dhdriver;
extern Xdhrint0(), Xdhxint0();
int	 (*dhint0[])() = { Xdhrint0, Xdhxint0, 0 } ;
extern struct uba_driver dhdriver;
extern Xdhrint1(), Xdhxint1();
int	 (*dhint1[])() = { Xdhrint1, Xdhxint1, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint0(), Xdzxint0();
int	 (*dzint0[])() = { Xdzrint0, Xdzxint0, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint1(), Xdzxint1();
int	 (*dzint1[])() = { Xdzrint1, Xdzxint1, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint2(), Xdzxint2();
int	 (*dzint2[])() = { Xdzrint2, Xdzxint2, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint3(), Xdzxint3();
int	 (*dzint3[])() = { Xdzrint3, Xdzxint3, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint4(), Xdzxint4();
int	 (*dzint4[])() = { Xdzrint4, Xdzxint4, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint5(), Xdzxint5();
int	 (*dzint5[])() = { Xdzrint5, Xdzxint5, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint6(), Xdzxint6();
int	 (*dzint6[])() = { Xdzrint6, Xdzxint6, 0 } ;
extern struct uba_driver dzdriver;
extern Xdzrint7(), Xdzxint7();
int	 (*dzint7[])() = { Xdzrint7, Xdzxint7, 0 } ;
extern struct uba_driver zsdriver;
extern Xtsintr0();
int	 (*zsint0[])() = { Xtsintr0, 0 } ;

struct uba_ctlr ubminit[] = {
/*	 driver,	ctlr,	ubanum,	alive,	intr,	addr */
	{ &hkdriver,	0,	'?',	0,	hkint0, C 0177440 },
	{ &tmdriver,	0,	'?',	0,	tmint0, C 0172520 },
	{ &scdriver,	0,	'?',	0,	scint0, C 0176700 },
	{ &udadriver,	0,	'?',	0,	udaint0, C 0177550 },
	{ &zsdriver,	0,	'?',	0,	zsint0, C 0172520 },
	0
};

struct uba_device ubdinit[] = {
	/* driver,  unit, ctlr,  ubanum, slave,   intr,    addr,    dk, flags*/
	{ &hkdriver,   0,     0,  '?',     0,        0, C 00     ,  1,  0x0 },
	{ &hkdriver,   1,     0,  '?',     1,        0, C 00     ,  1,  0x0 },
	{ &lpdriver,   0,    -1,  '?',    -1,   lpint0, C 0177514,  0,  0x0 },
	{ &tmdriver,   0,     0,  '?',     0,        0, C 00     ,  0,  0x0 },
	{ &tmdriver,   1,     0,  '?',     1,        0, C 00     ,  0,  0x0 },
	{ &scdriver,   0,     0,  '?',     0,        0, C 00     ,  1,  0x0 },
	{ &scdriver,   1,     0,  '?',     1,        0, C 00     ,  1,  0x0 },
	{ &udadriver,   0,     0,  '?',     0,        0, C 00     ,  1,  0x0 },
	{ &udadriver,   1,     0,  '?',     1,        0, C 00     ,  1,  0x0 },
	{ &dndriver,   0,    -1,  '?',    -1,   dnint0, C 0160020,  0,  0x0 },
	{ &dhdriver,   0,    -1,  '?',    -1,   dhint0, C 0160020,  0,  0x0 },
	{ &dhdriver,   1,    -1,  '?',    -1,   dhint1, C 0160040,  0,  0x0 },
	{ &dzdriver,   0,    -1,  '?',    -1,   dzint0, C 0160100,  0,  0xff },
	{ &dzdriver,   1,    -1,  '?',    -1,   dzint1, C 0160110,  0,  0xff },
	{ &dzdriver,   2,    -1,  '?',    -1,   dzint2, C 0160120,  0,  0xff },
	{ &dzdriver,   3,    -1,  '?',    -1,   dzint3, C 0160130,  0,  0xff },
	{ &dzdriver,   4,    -1,  '?',    -1,   dzint4, C 0160140,  0,  0xff },
	{ &dzdriver,   5,    -1,  '?',    -1,   dzint5, C 0160150,  0,  0xff },
	{ &dzdriver,   6,    -1,  '?',    -1,   dzint6, C 0160160,  0,  0xff },
	{ &dzdriver,   7,    -1,  '?',    -1,   dzint7, C 0160170,  0,  0xff },
	{ &zsdriver,   0,     0,  '?',     0,        0, C 00     ,  0,  0x0 },
	0
};
