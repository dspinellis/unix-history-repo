/*	vmsysent.c	4.10	81/04/13	*/

/*
 * This table is the switch used to transfer
 * to the appropriate routine for processing a vmunix special system call.
 * Each row contains the number of arguments expected
 * and a pointer to the routine.
 */

	0, nosys,			/* 64 +0 = nosys */
	0, nosys,			/* 64 +1 = nosys */
	0, vfork,			/* 64 +2 = vfork */
	3, vread,			/* 64 +3 = vread */
	3, vwrite,			/* 64 +4 = vwrite */
	3, segalloc,			/* 64 +5 = segalloc */
	1, segfree,			/* 64 +6 = segfree */
	1, segsync,			/* 64 +7 = segsync */
	1, vadvise,			/* 64 +8 = vadvise */
	0, nosys,			/* 64 +9 = nosys */
	0, nosys,			/* 64+10 = nosys */
	0, nosys,			/* 64+11 = nosys */
	1, vhangup,			/* 64+12 = vhangup */
	2, vlimit,			/* 64+13 = vlimit */
	0, nosys,			/* 64+14 = nosys */
	0, nosys,			/* 64+15 = nosys */
	0, nosys,			/* 64+16 = nosys */
	0, nosys,			/* 64+17 = nosys */
	0, nosys,			/* 64+18 = nosys */
	0, nosys,			/* 64+19 = nosys */
	0, nosys,			/* 64+20 = nosys */
	1, vswapon,			/* 64+21 = vswapon */
	0, nosys,			/* 64+22 = nosys */
	0, nosys,			/* 64+23 = nosys */
	0, nosys,			/* 64+24 = nosys */
	0, nosys,			/* 64+25 = nosys */
	0, nosys,			/* 64+26 = nosys */
	0, nosys,			/* 64+27 = nosys */
	0, nosys,			/* 64+28 = nosys */
	0, nosys,			/* 64+29 = nosys */
	0, nosys,			/* 64+30 = nosys */
	0, nosys,			/* 64+31 = nosys */
	0, nosys,			/* 64+32 = nosys */
	0, nosys,			/* 64+33 = nosys */
	0, nosys,			/* 64+34 = nosys */
	0, nosys,			/* 64+35 = nosys */
	0, nosys,			/* 64+36 = nosys */
	0, nosys,			/* 64+37 = nosys */
	0, nosys,			/* 64+38 = nosys */
	0, nosys,			/* 64+39 = nosys */
	0, nosys,			/* 64+40 = nosys */
	0, nosys,			/* 64+41 = nosys */
	0, nosys,			/* 64+42 = nosys */
	2, vtimes,			/* 64+43 = vtimes */
	0, nosys,			/* 64+44 = nosys */
	0, nosys,			/* 64+45 = nosys */
	0, nosys,			/* 64+46 = nosys */
	0, nosys,			/* 64+47 = nosys */
	0, nosys,			/* 64+48 = nosys */
	0, nosys,			/* 64+49 = nosys */
	0, nosys,			/* 64+50 = nosys */
#ifdef TRACE
	2, vtrace,			/* 64+51 = vtrace */
#else
	0, nosys,			/* 64+51 = nosys */
#endif
	0, nosys,			/* 64+52 = nosys */
	0, nosys,			/* 64+53 = nosys */
	0, nosys,			/* 64+54 = nosys */
	1, resuba,			/* 64+55 = resuba */
	0, nosys,			/* 64+56 = nosys */
	0, nosys,			/* 64+57 = nosys */
	0, nosys,			/* 64+58 = nosys */
	5, futz,			/* 64+59 = futz */
	0, nosys,			/* 64+60 = nosys */
	0, nosys,			/* 64+61 = nosys */
	0, nosys,			/* 64+62 = nosys */
	0, nosys,			/* 64+63 = nosys */
