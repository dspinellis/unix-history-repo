/*
 *  Interpress utilities
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  Written for Xerox Corporation by William LeFebvre
 *  29-May-1984
 *
 * HISTORY
 * 03-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Updated for Interpress 3.0 .
 *
 * 28-apr-85  ed flint
 *	add conditional compilation for vax11-c (vms)
 *	use "" instead of 0 for NULL op-names
 */

/*
 *  op_names maps an op-code into an operaton name.
 *
 */

char *op_names[] = 
{
/* 000 */ "nil", "nop", "", "", "", "", "", "", "", "",
	  "setxy", "setxyrel", "setxrel", "setyrel", "linetox",
	  	"linetoy", "space", "get", "iget", "iset",
	  "fget", "fset", "show", "lineto", "maskstroke", "moveto", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
/* 100 */ "metricMaster", "environmentMaster", "begin", "end", "noPages",
	  	"contentsInstructions", "{", "}", "", "",
	  "correct", "", "", "", "makesimpleco", "makeco", "makecompiledimage", "", "", "",
	  "dosavesimplebody", "dobody", "dosavebody", "dosaveallbody", "", 
	  	"", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "maskchar", "", "", "", "", "showandfixedxrel", "showandxrel", "findfont", "modifyfont", "finddecompressor", 
	  "findfontvec", "setfont", "", "", "setcorrectmeasure",
	  	"setcorrecttolerance", "correctmask", "correctspace", "", "getcp", 
	  "maket", "opent", "translate", "rotate", "scale",
	  	"concat", "scale2", "invert", "concatt", "move", 
	  "trans", "", "", "", "transform", "transformvewc", "roundxy", "roundxyvec", "", "",
	  "pop", "dup", "", "copy", "roll", "exch", "mark", "unmark", "count", "", 
	  "", "","unmark0", "", "", "", "", "", "", "",
/* 200 */ "abs", "add", "and", "ceiling", "div", "eq", "floor", "ge", "gt", "mod", 
	  "mul", "neg", "not", "or", "sub", "trunc", "rem", "round", "eqn", "",
	  "type", "atan", "cos", "exp", "log", "sin", "sqrt", "max", "min", "",
	  "", "do", "dosave", "dosaveall", "", "", "", "", "", "if", 
	  "ifcopy", "ifelse", "loop", "", "", "", "", "", "", "",
	  "frame", "", "", "poolop", "pool", "pget", "pset", "makepool", "nopool", "", 
	  "env", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "makeveclu", "makevec", "",
	  	"shape", "getp", "getprop", "mergeprop", "", 
	  "", "", "", "", "", "", "", "", "", "",
/* 300 */ "dround", "getcprounded", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
/* 400 */ "", "", "curveto", "arcto", "conicto", "", "", "", "", "maskfill", 
	  "maskrectangle", "masktrapezoidx", "masktrapezoidy", "startunderline",
	      "makeunderline", "", "makeoutlineodd","makeoutline", "clipoutline", "cliprectangle", 
	  "", "findcoloroperator", "findcolormodeloperator", "findcolor", "setgray", 
	      "makegray", "makesampledblack", "makesampledcolor", "setsampledblack",
	      "setsampledcolor", 
	  "", "", "", "", "", "", "", "", "", "",
	  "maskstrokeclosed", "maskvector", "maskdashedstroke", "", "", "", "", "", "", "",
	  "makepixelarray", "", "maskpixel", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
/* 500 */ "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
	  "", "", "", "", "", "", "", "", "", "",
/* 600 */ "error"
};
