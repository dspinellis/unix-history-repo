/*******************************************************************
*                                                                  *
*    File: CIFPLOT/extractor.c                                     *
*    Written by Dan Fitzpatrick                                    *
*    copyright 1980 -- Regents of the University of California     *
*                                                                  *
********************************************************************/

#include <stdio.h>
#include "defs.h"
#include "globals.h"
#include "structs.h"
#include "out_structs.h"
#include "ext_defs.h"

#define EXTRACTOR	"/vb/grad/fitz/bin/extnmos"

#define TEMP_FILE	"/usr/tmp/cextXXXXXX"

#define EXT_WRITE(x,n)	if(write(extFileDesc,x,n) != n) { \
			    perror(extTempFile);\
			    Error("Bad write on extractor temp file",RUNTIME);\
			    }
#define DECL_LAYER(name,n)	l = FindLayer(name);\
				if(l == NIL) {\
				    Error("NMOS layer not defined",INTERNAL);\
				    }\
				ExtMap[l->LNum] = n;

IMPORT char *mktemp();
IMPORT struct LCell *FindLayer();

char *extTempFile;
int extPassword = EXT_MAGIC_WORD;
int extFileDesc;
int *ExtMap;	/* map from cifplot internal form to standard form */
int ExtRecordSize;	/* number of bytes for each record */


InitExtractor() {
    int i;
    struct LCell *l;

    ExtRecordSize = MAX(sizeof(struct ExtNewSwathRecord),
			sizeof(struct ExtEdgeRecord));
    ExtRecordSize = MAX(sizeof(struct ExtPointRecord),ExtRecordSize);
    SetScale = 1;
    scale = 0.02;

    /* Make Extractor Map */
    ExtMap = (int *) alloc((MaxLayers+9)*sizeof(int));
    for(i=0;i<MaxLayers+9;i++)
	ExtMap[i] = -1;
    DECL_LAYER("NM",METAL);
    DECL_LAYER("NI",IMPLANT);
    DECL_LAYER("NP",POLY);
    DECL_LAYER("ND",DIFFUSION);
    DECL_LAYER("NC",CUT);
    DECL_LAYER("NB",BURIED);
    }

OpenExtractor() {
    int i;

    extTempFile = mktemp(TEMP_FILE);
    if((extFileDesc = creat(extTempFile,0666)) < 0) {
	perror(extTempFile);
	Error("Can't create extractor temp file",RUNTIME);
	}
    EXT_WRITE(&extPassword,sizeof(int));
    EXT_WRITE(&ConvertFactor,sizeof(double));
    i = Window.ymin;
    EXT_WRITE(&i,sizeof(int));
    }

int extPrev;

ExtractorOutput(xcurrent)
int xcurrent;
{
    struct ExtNewSwathRecord tmp;
    tmp.type = NEW_SWATH;
    tmp.top = xcurrent;
    tmp.bottom = extPrev;
    EXT_WRITE(&tmp,ExtRecordSize);
    extPrev = xcurrent;
    }

/**** THIS SHOULD BE BUFFERED ****/
OutputExtEdge(x,deltax,layer,start)
real x,deltax;
int layer,start;
{
    struct ExtEdgeRecord tmp;
    tmp.layer = ExtMap[layer];
    if(tmp.layer == -1) return;
    tmp.type = EXT_EDGE;
    tmp.x = x;
    tmp.deltax = deltax;
    tmp.start = start;
    EXT_WRITE(&tmp,ExtRecordSize);
    }

OutputExtPoint(y,x,name,layer)
real x,y;
char *name;
int layer;
{
    struct ExtPointRecord tmp;
    if(layer == -1)
	tmp.layer = ALL;
      else
	tmp.layer = ExtMap[layer];
    tmp.type = EXT_POINT;
    tmp.x = CONVERT(x);
    tmp.y = CONVERT(y);
    tmp.name = strlen(name);
    EXT_WRITE(&tmp,ExtRecordSize);
    EXT_WRITE(name,tmp.name);
    }

Extract() {
    if(close(extFileDesc) < 0) {
	perror(extTempFile);
	Error("Can't close extractor temp file",RUNTIME);
	}
    if(!debug) {
	/*
	execl(EXTRACTOR,EXTRACTOR,extTempFile,baseName,0);
	perror(EXTRACTOR);
	*/
	Error("Can't run extractor",INTERNAL);
	}
    }
