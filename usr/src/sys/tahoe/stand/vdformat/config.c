#ifndef lint
static char sccsid[] = "@(#)config.c	1.1 (Berkeley/CCI) %G%";
#endif

/*
 * Drive configuration information.
 */
#include "vdfmt.h"

struct	flawpat defpats = {
	0x0264c993, 0x04c99326, 0x0993264c, 0x13264c98,
	0x264c9930, 0x4c993260, 0x993264c0, 0x3264c980,
	0x64c99300, 0xc9932600, 0x93264c00, 0x264c9800,
	0x4c993000, 0x99326000, 0x3264c000, 0x54c98000
};
struct	flawpat xfdpats = {
	0x0d9b366c, 0x1b366cd8, 0x366cd9b0, 0x6cd9b360,
	0xd9b366c0, 0xb366cd80, 0x66cd9b00, 0xcd9b3600,
	0x9b366300, 0x366cd800, 0x6cd9b000, 0xd9b36000,
	0xb366c000, 0x66cd8000, 0xcd9b0000, 0x9b360000
};

struct	vdconfig vdconfig[] = {
    { "xsd", { 48, 24, 711 }, 0, 3600, 30240, &defpats,
      "515Mb CDC Winchester" },
    { "egl", { 44, 20, 842 }, 0, 3600, 27720, &defpats,
      "474Mb Fujitsu Eagle Winchester" },
    { "fuj", { 64, 10, 823 }, 0, 3600, 40960, &defpats,
      "360Mb Fujitsu Winchester" },
    { "xfd", { 32, 24, 711 }, 0, 3600, 20160, &xfdpats,
      "340Mb CDC Winchester" },
    { "smd", { 32, 19, 823 }, 0, 3600, 20160, &xfdpats,
      "300Mb CDC 9766" },
    { "fsd", { 32, 10, 823 }, 0, 3600, 20160, &xfdpats,
      "160Mb CDC Winchester" },
};
int	ndrives = sizeof (vdconfig) / sizeof (vdconfig[0]);
