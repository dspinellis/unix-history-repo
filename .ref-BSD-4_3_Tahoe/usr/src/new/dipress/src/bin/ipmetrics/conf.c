/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * configure the system here.
 * The following table defines how each composition system that ipmetrics
 * can generate for is called.
 *
 * ADDING A NEW COMPOSITION SYSTEM:
 *	fill out a line somewhere detailing the new system.
 *
 * HISTORY
 * 08-Sep-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added entry for old troff.
 *
 * 18-Aug-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Created.
 *
 *
 */
#include "conf.h"

#define TRUE	1
#define FALSE	0

extern int PerTroffFont(), CleanUpTroff();
extern int PerOTroffFont(), CleanUpOTroff();
extern int PerTeXFont(), CleanUpTeX();
extern int InitTOC(), PerTOCFont();
extern int PerGenericFont(), CleanUpGeneric();
null() {}

struct CompositionSwitch CompositionSwitch[] = {
	"troff",	TRUE,	null,		PerTroffFont,	CleanUpTroff,
	"otroff",	TRUE,	null,		PerOTroffFont,	CleanUpOTroff,
	"TeX",		TRUE,	null,		PerTeXFont,	CleanUpTeX,
	"toc",		FALSE,	InitTOC,	PerTOCFont,	null,
	"generic",	TRUE,	null,		PerGenericFont,	CleanUpGeneric,
	0,
};

