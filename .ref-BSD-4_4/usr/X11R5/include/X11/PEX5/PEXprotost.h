/* $XConsortium: PEXprotost.h,v 1.4 92/08/26 13:05:20 mor Exp $ */

/******************************************************************************
Copyright 1989, 1990, 1991 by Sun Microsystems, Inc.
Copyright 1992 by the Massachusetts Institute of Technology

                        All Rights Reserved

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation, and that
the name of Sun Microsystems or M.I.T. not be used in advertising or publicity
pertaining to distribution of the software without specific, written prior
permission.  Sun Microsystems and M.I.T. make no representations about the
suitability of this software for any purpose.  It is provided "as is" without
express or implied warranty.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************************/

#ifndef PEXPROTOSTR_H
#define PEXPROTOSTR_H

/* Matches revision 5.1C */

#include <X11/Xmd.h>			/* defines things like CARD32 */


/* This is FLOAT as defined and used by the Protocol Encoding */
typedef float PEXFLOAT;


typedef CARD32  pexAsfAttribute;
typedef CARD8	pexAsfValue;
typedef CARD32	pexBitmask;
typedef CARD16	pexBitmaskShort;
typedef CARD16  pexCoordType; 	/* rational, nonrational */
typedef CARD16	pexComposition;
typedef CARD16	pexCullMode;
typedef BYTE 	pexDynamicType;
typedef INT16	pexEnumTypeIndex;
typedef XID 	pexLookupTable;
typedef CARD32 	pexName;
typedef XID 	pexNameSet;
typedef XID	pexPC;
typedef XID	pexFont;
typedef PEXFLOAT	pexMatrix[4][4];
typedef PEXFLOAT 	pexMatrix3X3[3][3];
typedef XID	pexPhigsWks;
typedef XID	pexPickMeasure;
typedef XID	pexRenderer;
typedef XID	pexSC;
typedef XID	pexStructure;
typedef CARD8	pexSwitch;
typedef CARD16	pexTableIndex;
typedef CARD16	pexTableType;	/* could be smaller if it ever helps */
typedef CARD16	pexTextHAlignment;
typedef CARD16	pexTextVAlignment;
typedef CARD16	pexTypeOrTableIndex;
typedef pexEnumTypeIndex	pexColorType; 	/* ColorType */

/* included in others */
typedef struct {
    CARD16	length B16;
    /* list of CARD8 -- don't swap */
} pexString;

typedef struct {
    pexStructure	sid B32;
    PEXFLOAT		priority;
} pexStructureInfo;

typedef struct {
    PEXFLOAT	x;
    PEXFLOAT	y;
} pexVector2D;

typedef struct {
    PEXFLOAT	x;
    PEXFLOAT	y;
    PEXFLOAT	z;
} pexVector3D;

/* Coord structures */

typedef struct {
    PEXFLOAT	x;
    PEXFLOAT	y;
} pexCoord2D;

typedef struct {
    PEXFLOAT	x;
    PEXFLOAT	y;
    PEXFLOAT	z;
} pexCoord3D;

typedef struct {
    PEXFLOAT	x;
    PEXFLOAT	y;
    PEXFLOAT	z;
    PEXFLOAT	w;
} pexCoord4D;


/* Color structures */
typedef struct {
    PEXFLOAT	red;
    PEXFLOAT	green;
    PEXFLOAT	blue;
} pexRgbFloatColor;

typedef struct {
    PEXFLOAT	hue;
    PEXFLOAT	saturation;
    PEXFLOAT	value;
} pexHsvColor;

typedef struct {
    PEXFLOAT	hue;
    PEXFLOAT	lightness;
    PEXFLOAT	saturation;
} pexHlsColor;

typedef struct {
    PEXFLOAT	x;
    PEXFLOAT	y;
    PEXFLOAT	z;
} pexCieColor;

typedef struct {
    CARD8	red;
    CARD8	green;
    CARD8	blue;
    CARD8	pad;
} pexRgb8Color;

typedef struct {
    CARD16	red B16;
    CARD16	green B16;
    CARD16	blue B16;
    CARD16	pad B16;
} pexRgb16Color;

typedef struct {
    pexTableIndex	index B16;
    CARD16		pad B16;
} pexIndexedColor;

typedef struct {
    union {
	pexIndexedColor		indexed;
	pexRgb8Color		rgb8;
	pexRgb16Color		rgb16;
	pexRgbFloatColor	rgbFloat;
	pexHsvColor		hsvFloat;
	pexHlsColor		hlsFloat;
	pexCieColor		cieFloat;
    } format;
} pexColor;

typedef struct {
    PEXFLOAT   first;
    PEXFLOAT   second;
    PEXFLOAT   third;
} pexFloatColor;

typedef struct {
    pexColorType	colorType B16;	/* ColorType enumerated type */
    CARD16		unused B16;
    /* SINGLE COLOR(colorType) */
} pexColorSpecifier;


typedef struct {
    pexEnumTypeIndex	approxMethod B16;
    CARD16		unused B16;
    PEXFLOAT		tolerance;
} pexCurveApprox;

typedef struct {
    INT16	x B16;
    INT16 	y B16;
    PEXFLOAT 	z;
} pexDeviceCoord;

typedef struct {
    INT16	x B16;
    INT16 	y B16;
} pexDeviceCoord2D;

typedef struct {
    INT16	xmin B16;
    INT16	ymin B16;
    INT16	xmax B16;
    INT16	ymax B16;
} pexDeviceRect;

typedef struct {
    CARD16	elementType B16;
    CARD16	length B16;
} pexElementInfo;

typedef struct {
    CARD16	whence B16;
    CARD16	unused B16;
    INT32	offset B32;
} pexElementPos;

typedef struct {
    pexElementPos	position1;
    pexElementPos	position2;
} pexElementRange;

typedef struct {
    pexStructure	structure B32;
    CARD32		offset B32;
} pexElementRef;

typedef struct {
	pexCoord2D lowerLeft;
	pexCoord2D upperRight;
	pexCoord2D concatpoint;
} pexExtentInfo;

typedef struct {
    pexEnumTypeIndex	index B16;
    pexString		descriptor;
} pexEnumTypeDesc;

typedef struct {
    pexCoord3D	point;
    pexVector3D	vector;
} pexHalfSpace;

typedef struct {
    pexNameSet	incl;
    pexNameSet	excl;
} pexNameSetPair;

typedef struct {
    pexCoord2D	point;
    pexVector2D	vector;
} pexHalfSpace2D;

typedef struct {
    CARD16	composition B16;
    CARD16	unused B16;
    pexMatrix	matrix;
} pexLocalTransform3DData;

typedef struct {
    CARD16		composition B16;
    CARD16		unused B16;
    pexMatrix3X3	matrix;
} pexLocalTransform2DData;

typedef struct {
    pexCoord3D	minval;
    pexCoord3D	maxval;
} pexNpcSubvolume;

/*  an OPT_DATA  structure cannot be defined because it has variable content
 *  and size.  An union structure could be used to define a template for
 *  the data. However, since unions pad to a fixed amount of space and the
 *  protocol uses variable lengths, this is not appropriate for protocol
 *  data types.  The most correct way of defining this data is to define
 *  one data structure for every possible combination of color, normal and
 *  edge data that could be given with a vertex or facet.
 */

typedef struct {
    pexStructure	sid B32;
    CARD32		offset B32;
    CARD32		pickid B32;
} pexPickElementRef;

/* pexPickPath is the old name of the above strucutre.
   This is wrong, since the above is a Pick Element Ref
   a Pick Path is a list of Pick Element Refs so naming
   this structure pexPickPath was wrong, but it can't just
   be changed without effecting lots of other code....... */

typedef pexPickElementRef pexPickPath;

typedef struct {
    pexTextVAlignment		vertical B16;
    pexTextHAlignment		horizontal B16;
} pexTextAlignmentData;

typedef struct {
    pexSwitch		visibility;
    CARD8		unused;
    CARD16		order B16;
    pexCoordType	type B16;
    INT16		approxMethod B16;
    PEXFLOAT		tolerance;
    PEXFLOAT		tMin;
    PEXFLOAT		tMax;
    CARD32		numKnots B32;
    CARD32		numCoord B32;
    /* LISTof FLOAT(numKnots) -- length = order + number of coords */
    /* LISTof {pexCoord3D|pexCoord4D}(numCoord) */
} pexTrimCurve;

typedef struct {
    CARD8		depth;
    CARD8		unused;
    CARD16		type B16;
    CARD32		visualID B32;
} pexRendererTarget;

typedef struct {
    pexEnumTypeIndex	pickType B16;
    CARD16		unused;
    /* SINGLE HITBOX() */
} pexPickRecord;

typedef struct {
    PEXFLOAT		ambient;
    PEXFLOAT		diffuse;
    PEXFLOAT		specular;
    PEXFLOAT		specularConc;
    PEXFLOAT		transmission;  /* 0.0 = opaque, 1.0 = transparent */
    pexColorSpecifier  specularColor;
    /* SINGLE COLOR() */
} pexReflectionAttr;

typedef struct {
    pexEnumTypeIndex	approxMethod B16;
    CARD16		unused B16;
    PEXFLOAT		uTolerance;
    PEXFLOAT		vTolerance;
} pexSurfaceApprox;


typedef struct {
    pexCoord3D	point;
    /* SINGLE OPT_DATA() */
} pexVertex;


typedef struct {
    pexDeviceCoord	minval;
    pexDeviceCoord	maxval;
    pexSwitch		useDrawable;
    BYTE		pad[3];
} pexViewport;

typedef struct {
    CARD16		clipFlags B16;
    CARD16		unused B16;
    pexNpcSubvolume	clipLimits;
    pexMatrix		orientation;
    pexMatrix		mapping;
} pexViewEntry;

typedef struct {
    pexTableIndex	index B16;
    CARD16		unused B16;
    pexViewEntry	view;
} pexViewRep;

/*
 * typedefs for lookup tables
 */

typedef struct {
    CARD16	definableEntries B16;
    CARD16	numPredefined B16;
    CARD16	predefinedMin B16;
    CARD16	predefinedMax B16;
} pexTableInfo;

typedef struct {
    pexEnumTypeIndex	lineType B16;
    pexEnumTypeIndex	polylineInterp B16;
    pexCurveApprox	curveApprox;
    PEXFLOAT		lineWidth;
    pexColorSpecifier	lineColor;
    /* SINGLE COLOR() */
} pexLineBundleEntry;

typedef struct {
    pexEnumTypeIndex	markerType B16;
    INT16		unused B16;
    PEXFLOAT		markerScale;
    pexColorSpecifier	markerColor;
    /* SINGLE COLOR() */
} pexMarkerBundleEntry;

typedef struct {
    CARD16		textFontIndex B16;
    CARD16		textPrecision B16;
    PEXFLOAT		charExpansion;
    PEXFLOAT		charSpacing;
    pexColorSpecifier	textColor;
    /* SINGLE COLOR() */
} pexTextBundleEntry;


/*
    Note that since an InteriorBundleEntry contains 4 embedded instances of 
    pexColorSpecifier, a variable-sized item, a data structure cannot be
    defined for it.
*/
typedef struct {
    pexEnumTypeIndex    interiorStyle B16;
    INT16		interiorStyleIndex B16;
    pexEnumTypeIndex    reflectionModel B16;
    pexEnumTypeIndex    surfaceInterp B16;
    pexEnumTypeIndex    bfInteriorStyle B16;
    INT16		bfInteriorStyleIndex B16;
    pexEnumTypeIndex    bfReflectionModel B16;
    pexEnumTypeIndex    bfSurfaceInterp B16;
    pexSurfaceApprox    surfaceApprox;
    /* SINGLE pexColorSpecifier		surfaceColor    */
    /* SINGLE pexReflectionAttr		reflectionAttr   */
    /* SINGLE pexColorSpecifier		bfSurfaceColor  */
    /* SINGLE pexReflectionAttr		bfReflectionAttr */
} pexInteriorBundleEntry;

typedef struct {
    pexSwitch		edges;
    CARD8		unused;
    pexEnumTypeIndex	edgeType B16;
    PEXFLOAT		edgeWidth;
    pexColorSpecifier	edgeColor;
    /* SINGLE COLOR() */
} pexEdgeBundleEntry;

typedef struct {
    pexColorType	colorType B16; 
    CARD16		numx B16;
    CARD16		numy B16;
    CARD16		unused B16;
    /* LISTof Color(numx, numy) 2D array of colors */
} pexPatternEntry;

/* a pexColorEntry is just a pexColorSpecifier
*/

typedef struct {
    CARD32	numFonts B32;
    /* LISTof pexFont( numFonts ) */
} pexTextFontEntry;

/* a pexViewEntry is defined above */

typedef struct {
    pexEnumTypeIndex	lightType B16;
    INT16		unused B16;
    pexVector3D		direction;
    pexCoord3D		point;
    PEXFLOAT		concentration;
    PEXFLOAT		spreadAngle;
    PEXFLOAT		attenuation1;
    PEXFLOAT		attenuation2;
    pexColorSpecifier	lightColor;
    /* SINGLE COLOR() */
} pexLightEntry;

typedef struct {
    pexSwitch		mode;
    CARD8		unused;
    CARD16		unused2 B16;
    PEXFLOAT		frontPlane;
    PEXFLOAT		backPlane;
    PEXFLOAT		frontScaling;
    PEXFLOAT		backScaling;
    pexColorSpecifier	depthCueColor;
    /* SINGLE COLOR() */
} pexDepthCueEntry;

typedef struct {
    INT16	approxType;
    INT16	approxModel;
    CARD16	max1 B16;
    CARD16	max2 B16;
    CARD16	max3 B16;
    CARD8	dither;
    CARD8	unused;
    CARD32	mult1 B32;
    CARD32	mult2 B32;
    CARD32	mult3 B32;
    PEXFLOAT	weight1;
    PEXFLOAT	weight2;
    PEXFLOAT	weight3;
    CARD32	basePixel B32;
} pexColorApproxEntry;


/*  Font structures */

typedef struct {
    Atom	name B32;
    CARD32	value B32;
} pexFontProp;

typedef struct {
    CARD32	firstGlyph B32;
    CARD32	lastGlyph B32;
    CARD32	defaultGlyph B32;
    pexSwitch	allExist;
    pexSwitch	strokeFont;
    CARD16	unused B16;
    CARD32	numProps B32;
    /* LISTof pexFontProp(numProps) */
} pexFontInfo;


/* Text Structures */

typedef struct {
    INT16	characterSet B16;
    CARD8	characterSetWidth;
    CARD8	encodingState;
    CARD16	unused B16;
    CARD16	numChars;
    /* LISTof CHARACTER( numChars ) */
    /* pad */
} pexMonoEncoding;

/* CHARACTER is either a CARD8, a CARD16, or a CARD32 */


/* Parametric Surface Characteristics types */

/* type 1 None */

/* type 2 Implementation Dependent */

typedef struct {
    CARD16	placementType B16;
    CARD16	unused B16;
    CARD16	numUcurves B16;
    CARD16	numVcurves B16;
} pexPSC_IsoparametricCurves;		/* type 3 */

typedef struct {
    pexCoord3D	    origin;
    pexVector3D	    direction;
    CARD16	    numberIntersections B16;
    CARD16	    pad B16;
    /* LISTof pexCoord3D( numIntersections ) */
} pexPSC_LevelCurves;			/*  type 4: MC
					    type 5: WC */

/* Pick Device data records */

typedef struct {
    pexDeviceCoord2D	position;
    PEXFLOAT		distance;
} pexPD_DC_HitBox;				/* pick device 1 */

typedef pexNpcSubvolume pexPD_NPC_HitVolume;	/* pick device 2 */


/* Output Command errors */

typedef struct {
    CARD8	type;		    /*  0 */
    CARD8	errorCode;	    /* 14 */
    CARD16	sequenceNumber B16;
    CARD32	resourceId B32;	    /* renderer or structure */
    CARD16	minorCode B16;
    CARD8	majorCode;
    CARD8	unused;
    CARD16	opcode B16;	    /* opcode of failed output command */
    CARD16	numCommands B16;    /* number successfully done before error */
    BYTE	pad[16];
} pexOutputCommandError;


/* Registered PEX Escapes */

typedef struct {
    INT16	fpFormat B16;
    CARD8	unused[2];
    CARD32	rdr B32;	    /* renderer ID */
    /* SINGLE ColorSpecifier()  */
} pexEscapeSetEchoColorData;

#endif /* PEXPROTOSTR_H */
