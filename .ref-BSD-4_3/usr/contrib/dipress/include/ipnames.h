/*
 *  Interpress utilities
 *
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 *  Written for Xerox Corporation by William LeFebvre
 *  29-May-1984
 *
 * History:
 *	28-apr-85  ed flint	add conditional compilation for vax11-c (vms)
 *				use "" instead of 0 for NULL op-names
 */

/*
 *  op_names maps an op-code into an operaton name.
 *
 *  NOTE:  every "0" should be a "NULL", but "0" was used to save horizontal
 *  space.
 */

#ifdef vax1c

char *op_names[] = 
{
"nil", "nop", "", "", "", "", "", "", "", "",
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
"metricMaster", "environmentMaster", "begin", "end", "noPages",
	"pageInstructions", "{", "}", "", "",
"correct", "", "", "", "makesimpleco", "makeco", "makecompiledimage", "", "", "",
"dosavesimplebody", "dobody", "dosavebody", "dosaveallbody", "", 
	"", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "showandxrel", "findfont", "modifyfont", "finddecompressor", 
"findfontvec", "setfont", "", "", "setcorrectmeasure",
	"setcorrecttollerance", "correctmask", "correctspace", "", "getcp", 
"maket", "opent", "translate", "rotate", "scale",
	"concat", "scale2", "invert", "concatt", "move", 
"trans", "", "", "", "transform", "transformvewc", "roundxy", "roundxyvec", "", "",
"pop", "dup", "", "copy", "roll", "exch", "mark", "unmark", "count", "", 
"", "","unmark0", "", "", "", "", "", "", "",
"abs", "add", "and", "ceiling", "div", "eq", "floor", "ge", "gt", "mod", 
"mul", "neg", "not", "or", "sub", "trunc", "rem", "round", "eqn", "",
"type", "atan", "cos", "exp", "log", "sin", "sqrt", "max", "min", "",
"", "do", "dosave", "dosaveall", "", "", "", "", "", "if", 
"ifcopy", "ifelse", "loop", "", "", "", "", "", "", "",
"frame", "", "", "poolop", "pool", "pget", "pset", "makepool", "nopool", "", 
"env", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "makeveclu", "makevec", "",
	"shape", "openvec", "getprop", "getp", "mergeprop", 
"", "", "", "", "", "", "", "", "", "",
"dround", "getcprounded", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "",
"", "", "curveto", "arcto", "conicto", "", "", "", "", "maskfill", 
"maskrectangle", "masktrapezoidx", "masktrapezoidy", "startunderline",
    "makeunderline", "", "","makeoutline", "clipoutline", "cliprectangle", 
"", "findcoloroperator", "findcolormodeloperator", "findcolor", "setgray", 
    "makegray", "makesampledblack", "makesampledcolor", "", "", 
"", "", "", "", "", "", "", "", "", "",
"maskstrokeclosed", "maskvector", "", "", "", "", "", "", "", "",
"makepixelarray", "", "maskpixel"
};

#else

char *op_names[] = 
{
"nil", "nop", 0, 0, 0, 0, 0, 0, 0, 0,
"setxy", "setxyrel", "setxrel", "setyrel", "linetox",
	"linetoy", "space", "get", "iget", "iset",
"fget", "fset", "show", "lineto", "maskstroke", "moveto", 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
"metricMaster", "environmentMaster", "begin", "end", "noPages",
	"pageInstructions", "{", "}", 0, 0,
"correct", 0, 0, 0, "makesimpleco", "makeco", "makecompiledimage", 0, 0, 0,
"dosavesimplebody", "dobody", "dosavebody", "dosaveallbody", 0, 
	0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, "showandxrel", "findfont", "modifyfont", "finddecompressor", 
"findfontvec", "setfont", 0, 0, "setcorrectmeasure",
	"setcorrecttollerance", "correctmask", "correctspace", 0, "getcp", 
"maket", "opent", "translate", "rotate", "scale",
	"concat", "scale2", "invert", "concatt", "move", 
"trans", 0, 0, 0, "transform", "transformvewc", "roundxy", "roundxyvec", 0, 0,
"pop", "dup", 0, "copy", "roll", "exch", "mark", "unmark", "count", 0, 
0, 0,"unmark0", 0, 0, 0, 0, 0, 0, 0,
"abs", "add", "and", "ceiling", "div", "eq", "floor", "ge", "gt", "mod", 
"mul", "neg", "not", "or", "sub", "trunc", "rem", "round", "eqn", 0,
"type", "atan", "cos", "exp", "log", "sin", "sqrt", "max", "min", 0,
0, "do", "dosave", "dosaveall", 0, 0, 0, 0, 0, "if", 
"ifcopy", "ifelse", "loop", 0, 0, 0, 0, 0, 0, 0,
"frame", 0, 0, "poolop", "pool", "pget", "pset", "makepool", "nopool", 0, 
"env", 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, "makeveclu", "makevec", 0,
	"shape", "openvec", "getprop", "getp", "mergeprop", 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
"dround", "getcprounded", 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, "curveto", "arcto", "conicto", 0, 0, 0, 0, "maskfill", 
"maskrectangle", "masktrapezoidx", "masktrapezoidy", "startunderline",
    "makeunderline", 0, 0,"makeoutline", "clipoutline", "cliprectangle", 
0, "findcoloroperator", "findcolormodeloperator", "findcolor", "setgray", 
    "makegray", "makesampledblack", "makesampledcolor", 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
"maskstrokeclosed", "maskvector", 0, 0, 0, 0, 0, 0, 0, 0,
"makepixelarray", 0, "maskpixel"
};

#endif
