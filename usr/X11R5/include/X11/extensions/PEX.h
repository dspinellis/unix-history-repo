/* $XConsortium: PEX.h,v 5.10 92/04/23 17:43:16 hersh Exp $ */

/***********************************************************
Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. and the X Consortium.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Sun Microsystems,
the X Consortium, and MIT not be used in advertising or publicity 
pertaining to distribution of the software without specific, written 
prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/


#ifndef PEX_H
#define PEX_H

#ifdef LOCAL_PEX_NAME
#define PEX_NAME_STRING		LOCAL_PEX_NAME
#else
#define PEX_NAME_STRING		"X3D-PEX"
#endif

/* Matches revision 5.1C */

#define PEX_PROTO_MAJOR		5	/* current protocol version */
#define PEX_PROTO_MINOR		1	/* current minor version */

/* Events */
#define PEXMaxHitsReached     0
#define PEXNumberEvents       1

/* Subsets */
#define PEXCompleteImplementation	0
#define PEXImmediateModeOnly		1
#define PEXPhigsWksOnly			2
#define PEXStructureModeOnly		4

/* Resources */
#define PEXAlreadyFreed         1

/* Asf Attributes */

/* Masks for setting Asf's */
#define PEXMarkerTypeAsf		 (1L<<0)
#define PEXMarkerScaleAsf		 (1L<<1)
#define PEXMarkerColourAsf		 (1L<<2)
#define PEXTextFontIndexAsf		 (1L<<3)
#define PEXTextPrecAsf			 (1L<<4)
#define PEXCharExpansionAsf		 (1L<<5)
#define PEXCharSpacingAsf		 (1L<<6)
#define PEXTextColourAsf		 (1L<<7)
#define PEXLineTypeAsf			 (1L<<8)
#define PEXLineWidthAsf			 (1L<<9)
#define PEXLineColourAsf		(1L<<10)
#define PEXCurveApproxAsf		(1L<<11)
#define PEXPolylineInterpAsf		(1L<<12)
#define PEXInteriorStyleAsf		(1L<<13)
#define PEXInteriorStyleIndexAsf	(1L<<14)
#define PEXSurfaceColourAsf		(1L<<15)
#define PEXSurfaceInterpAsf		(1L<<16)
#define PEXReflectionModelAsf		(1L<<17)
#define PEXReflectionAttrAsf		(1L<<18)
#define PEXBfInteriorStyleAsf		(1L<<19)
#define PEXBfInteriorStyleIndexAsf	(1L<<20)
#define PEXBfSurfaceColourAsf		(1L<<21)
#define PEXBfSurfaceInterpAsf		(1L<<22)
#define PEXBfReflectionModelAsf		(1L<<23)
#define PEXBfReflectionAttrAsf		(1L<<24)
#define PEXSurfaceApproxAsf		(1L<<25)
#define PEXSurfaceEdgesAsf		(1L<<26)
#define PEXSurfaceEdgeTypeAsf		(1L<<27)
#define PEXSurfaceEdgeWidthAsf		(1L<<28)
#define PEXSurfaceEdgeColourAsf		(1L<<29)
#define PEXMaxAsfShift	29

/* Asf Values */
#define PEXBundled		0
#define PEXIndividual		1

/* Composition */
#define PEXPreConcatenate	0
#define PEXPostConcatenate	1
#define PEXReplace		2

/* Cull mode */
/* 0 None */
#define PEXBackFaces	1
#define PEXFrontFaces	2

/* Curve Type  and Surface Type */
#define PEXRational	0
#define PEXNonRational	1

/* Edit Mode */
#define PEXStructureInsert	0
#define PEXStructureReplace	1 

/* Whence values */
#define PEXBeginning	0
#define PEXCurrent	1
#define PEXEnd		2

/* Match Draw Type */
#define PEXDontCare     0
#define PEXWindow       1
#define PEXPixmap       2
#define PEXBuffer       3

/* Pick All State */
#define PEXMoreHits      0
#define PEXNoMoreHits    1
#define PEXMayBeMoreHits 2

/* PickOne Methods */
#define PEXLast			1
#define PEXClosestZ		2
#define PEXVisibleAny		3
#define PEXVisibleClosest	4

/* PickAll Methods */
#define PEXAllPicks		1
#define PEXVisible		2

/* Element Search */
#define PEXNotFound	1
#define PEXFound	2

/* GetEnumeratedType return format */
#define PEXETIndex	1
#define PEXETMnemonic	2
#define PEXETBoth	3

/* Enum Types */
#define PEXETMarkerType				 1
#define PEXETATextStyle				 2
#define PEXETInteriorStyle			 3
#define PEXETHatchStyle				 4
#define PEXETLineType				 5
#define PEXETSurfaceEdgeType			 6
#define PEXETPickDeviceType			 7
#define PEXETPolylineInterpMethod		 8
#define PEXETCurveApproxMethod			 9
#define PEXETReflectionModel			10
#define PEXETSurfaceInterpMethod		11
#define PEXETSurfaceApproxMethod		12
#define PEXETModelClipOperator			13
#define PEXETLightType				14
#define PEXETColourType				15
#define PEXETFloatFormat			16
#define PEXETHlhsrMode				17
#define PEXETPromptEchoType			18
#define PEXETDisplayUpdateMode			19
#define PEXETColourApproxType			20
#define PEXETColourApproxModel			21
#define PEXETGDP				22
#define PEXETGDP3				23
#define PEXETGSE				24
#define PEXETTrimCurveApproxMethod		25
#define PEXETRenderingColourModel		26
#define PEXETParaSurfCharacteristics		27
#define PEXETEscape				28
#define PEXETPickOneMethod			29
#define PEXETPickAllMethod			30

/* Renderer State */
#define PEXIdle 	0
#define PEXRendering	1
#define PEXPicking	2

/* Flags (e.g., Switches, Visibility, and Edges) */
#define PEXOff	0
#define PEXOn	1

/* Shape hints */
/* Complex, Nonconvex, Convex, are defined  as 0, 1, 2 in X.h */
#define PEXComplex		0
#define PEXNonconvex		1
#define PEXConvex		2
#define PEXUnknownShape		3

/* Contour hints */
#define PEXDisjoint		0
#define PEXNested		1
#define PEXIntersecting		2
#define PEXUnknownContour	3

/* Table Type */
#define PEXLineBundleLUT	 1
#define PEXMarkerBundleLUT	 2
#define PEXTextBundleLUT	 3
#define PEXInteriorBundleLUT	 4
#define PEXEdgeBundleLUT	 5
#define PEXPatternLUT		 6
#define PEXTextFontLUT		 7
#define PEXColourLUT		 8
#define PEXViewLUT		 9
#define PEXLightLUT		10
#define PEXDepthCueLUT		11
#define PEXColourApproxLUT	12
#define PEXMaxTableType		12

/* Status in GetTableEntry */
#define PEXDefaultEntry	0
#define PEXDefinedEntry	1

/* ValueType in GetTableEntr{y|ies} */
#define PEXSetValue		0
#define PEXRealizedValue	1

/* Constants for Path and Vertical and Horizontal alignment */
#define PEXPathRight	0
#define PEXPathLeft	1
#define PEXPathUp	2
#define PEXPathDown	3
#define PEXValignNormal	0
#define PEXValignTop	1
#define PEXValignCap	2
#define PEXValignHalf	3
#define PEXValignBase	4
#define PEXValignBottom	5
#define PEXHalignNormal	0
#define PEXHalignLeft	1
#define PEXHalignCenter	2
#define PEXHalignRight	3

/* Text precision */
#define PEXStringPrecision	0
#define PEXCharPrecision	1
#define PEXStrokePrecision	2

/* Character Set Widths */
#define PEXCSByte	0
#define PEXCSShort	1
#define PEXCSLong	2

/* Update State */
#define PEXNotPending	0
#define PEXPending	1

/* Visual State */
#define PEXCorrect	0
#define PEXDeferred	1
#define PEXSimulated	2

/* Display State */
#define PEXEmpty	0
#define PEXNotEmpty	1

/* Buffer Mode */
#define PEXSingleBuffered	0
#define PEXDoubleBuffered	1

/* Dynamic types */
#define PEXIMM	0
#define PEXIRG	1
#define PEXCBS	2

/* Geometric attributes (Vertex, Facet) */
#define PEXGAColour	0x0001
#define PEXGANormal	0x0002
#define PEXGAEdges	0x0004

/* Pick Status */
#define PEXNoPick	0
#define PEXOk		1
#define PEXAborted	2

/* Pick Echo Switch */
#define PEXNoEcho	0
#define PEXEcho		1
#define PEXUnEcho	2

/* Pick Path Order */
#define PEXTopFirst     0
#define PEXBottomFirst  1

/* Items for GetStructureInfo */
#define PEXElementPtr		0x0001
#define PEXNumElements		0x0002
#define PEXLengthStructure	0x0004
#define PEXHasRefs		0x0008
#define PEXEditMode		0x0010

/* Flags for GetStructuresInNetwork */
#define PEXAll             0
#define PEXOrphans         1

/* Path part for GetAncestors */
#define PEXTopPart	0
#define PEXBottomPart	1

/* Direction for ElementSearch */
#define PEXForward         0
#define PEXBackward        1

/* Nameset changes */
#define PEXNSAdd             0
#define PEXNSRemove          1
#define PEXNSReplace         2

/* Priorities */
#define PEXHigher          0
#define PEXLower           1

/* Enumerated Type Descriptors */

/* Marker Type */
#define PEXMarkerDot		1
#define PEXMarkerCross		2
#define PEXMarkerAsterisk	3
#define PEXMarkerCircle		4
#define PEXMarkerX		5
/* ATextStyle */
#define PEXATextNotConnected	1
#define PEXATextConnected	2
/* InteriorStyle */
#define PEXInteriorStyleHollow	1
#define PEXInteriorStyleSolid	2
#define PEXInteriorStylePattern	3
#define PEXInteriorStyleHatch	4
#define PEXInteriorStyleEmpty	5
/* HatchStyle */
/* LineType */
#define PEXLineTypeSolid	1
#define PEXLineTypeDashed	2
#define PEXLineTypeDotted	3
#define PEXLineTypeDashDot	4
/* SurfaceEdgeType */
#define PEXSurfaceEdgeSolid	1
#define PEXSurfaceEdgeDashed	2
#define PEXSurfaceEdgeDotted	3
#define PEXSurfaceEdgeDashDot	4
/* PickDeviceType */
#define	PEXPickDeviceDC_HitBox	1
#define	PEXPickDeviceNPC_HitVolume	2
/* PolylineInterpMethod */
#define PEXPolylineInterpNone	1
#define PEXPolylineInterpColour	2
/* Curve(and Surface)(and Trim Curve)ApproxMethods */
#define PEXApproxImpDep				1
#define	PEXApproxConstantBetweenKnots 		2
#define PEXApproxWcsChordalSize			3
#define PEXApproxNpcChordalSize			4
#define PEXApproxDcChordalSize			5
#define PEXCurveApproxWcsChordalDev		6
#define PEXCurveApproxNpcChordalDev		7
#define PEXCurveApproxDcChordalDev		8
#define PEXSurfaceApproxWcsPlanarDev		6
#define PEXSurfaceApproxNpcPlanarDev		7
#define PEXSurfaceApproxDcPlanarDev		8
#define PEXApproxWcsRelative			9
#define PEXApproxNpcRelative			10
#define PEXApproxDcRelative			11
/* ReflectionModel */
#define PEXReflectionNoShading	1
#define PEXReflectionAmbient	2
#define PEXReflectionDiffuse	3
#define PEXReflectionSpecular	4
/* SurfaceInterpMethod */
#define PEXSurfaceInterpNone		1
#define PEXSurfaceInterpColour		2
#define PEXSurfaceInterpDotProduct	3
#define PEXSurfaceInterpNormal		4
/* ModelClipOperator */
#define PEXModelClipReplace		1
#define PEXModelClipIntersection	2
/* LightType */
#define PEXLightAmbient			1
#define PEXLightWcsVector		2
#define PEXLightWcsPoint		3
#define PEXLightWcsSpot			4
/* ColourType */
#define PEXIndexedColour	0
#define PEXRgbFloatColour	1
#define PEXCieFloatColour	2
#define PEXHsvFloatColour	3
#define PEXHlsFloatColour	4
#define PEXRgb8Colour		5
#define PEXRgb16Colour		6
#define PEXMaxColour		6
/* FloatFormat */
#define PEXIeee_754_32		1
#define PEXDEC_F_Floating	2
#define PEXIeee_754_64		3
#define PEXDEC_D_Floating	4
/* HlhsrMode */
#define PEXHlhsrOff		1
#define PEXHlhsrZBuffer		2
#define PEXHlhsrPainters	3
#define PEXHlhsrScanline	4
#define PEXHlhsrHiddenLineOnly	5
#define PEXHlhsrZBufferId	6
/* PromptEchoType */
#define PEXEchoPrimitive	1
#define PEXEchoStructure	2
#define PEXEchoNetwork	3
/* DisplayUpdateMethod */
#define PEXVisualizeEach	1
#define PEXVisualizeEasy	2
#define PEXVisualizeNone	3
#define PEXSimulateSome		4
#define PEXVisualizeWhenever	5
/* ColourApproxType */
#define PEXColourSpace          1
#define PEXColourRange          2
/* ColourApproxMethod */
#define PEXColourApproxRGB      1
#define PEXColourApproxCIE      2
#define PEXColourApproxHSV      3
#define PEXColourApproxHLS      4
#define PEXColourApproxYIQ      5
/* Escape */
#define PEXEscapeSetEchoColour   1
/* RenderingColourModel	*/
#define PEXRdrColourModelImpDep	0
#define PEXRdrColourModelRGB	1
#define PEXRdrColourModelCIE	2
#define PEXRdrColourModelHSV	3
#define PEXRdrColourModelHLS	4
/* ParametricSurfaceCharacteristics */
#define	PEXPSCNone		1
#define	PEXPSCImpDep		2
#define	PEXPSCIsoCurves		3
#define	PEXPSCMcLevelCurves	4
#define	PEXPSCWcLevelCurves	5
/* Isoparametric Curves */
#define PEXICUniformPlacement		0
#define PEXICNonuniformPlacement	1

/* Clipping */
#define PEXClipXY 		0x0001
#define PEXClipBack 		0x0002
#define PEXClipFront 		0x0004

#define PEXClip			0
#define PEXNoClip		1

/* Implementation Dependent Constant Names */
#define PEXIDDitheringSupported		 1
#define PEXIDMaxEdgeWidth		 2
#define PEXIDMaxLineWidth		 3
#define PEXIDMaxMarkerSize		 4
#define PEXIDMaxModelClipPlanes		 5
#define PEXIDMaxNameSetNames		 6
#define PEXIDMaxNonAmbientLights	 7
#define PEXIDMaxNURBOrder		 8
#define PEXIDMaxTrimCurveOrder		 9
#define PEXIDMinEdgeWidth		10
#define PEXIDMinLineWidth		11
#define PEXIDMinMarkerSize		12
#define PEXIDNominalEdgeWidth		13
#define PEXIDNominalLineWidth		14
#define PEXIDNominalMarkerSize		15
#define PEXIDNumSupportedEdgeWidths	16
#define PEXIDNumSupportedLineWidths	17
#define PEXIDNumSupportedMarkerSizes	18
#define PEXIDBestColourApproximation	19
#define PEXIDTransparencySupported	20
#define PEXIDDoubleBufferingSupported	21
#define PEXIDChromaticityRedU		22
#define PEXIDChromaticityRedV		23
#define PEXIDLuminanceRed		24
#define PEXIDChromaticityGreenU		25
#define PEXIDChromaticityGreenV		26
#define PEXIDLuminanceGreen		27
#define PEXIDChromaticityBlueU		28
#define PEXIDChromaticityBlueV		29
#define PEXIDLuminanceBlue		30
#define PEXIDChromaticityWhiteU		31
#define PEXIDChromaticityWhiteV		32
#define PEXIDLuminanceWhite		33
/* have to stick this here since others are not in order */
#define PEXIDMaxHitsEventSupported      34

/* Constants for IDRgbBestApproximation */
#define PEXColourApproxAnyValues	0
#define PEXColourApproxPowersOf2	1

/** To convert a bit index to a mask number and a mask value, assuming
 ** 32 bit wide words.  For example, a bitIndex of 5 will return 
 ** maskNum == 0 and maskValue == (1 << 5) = 32, while a bitIndex of 39
 ** will return maskNum == 1 and maskValue == (1 << 7) == 128 
 **/
#define PEX_BITNUM_TO_BITMASK(bitIndex, maskNum, maskValue) \
    maskNum	= (bitIndex) / 32; \
    maskValue	= ((unsigned)1L << ((bitIndex) % 32));

#define CHECK_BITMASK_ARRAY(mask,bitIndex) \
    if (mask[((bitIndex)/32)] & ((unsigned)1L << ((bitIndex) % 32)))

#define PEX_BITMASK(i) ((unsigned)1 << ((i) & 31))
#define PEX_MASKIDX(i) ((i) >> 5)
#define PEX_MASKWORD(buf, i) buf[PEX_MASKIDX(i)]
#define PEX_BITSET(buf, i) PEX_MASKWORD(buf, i) |= PEX_BITMASK(i)
#define PEX_BITCLEAR(buf, i) PEX_MASKWORD(buf, i) &= ~PEX_BITMASK(i)
#define PEX_GETBIT(buf, i) (PEX_MASKWORD(buf, i) & PEX_BITMASK(i))

#define PEXMSGetWksInfo         2
#define PEXMSPipeline           3

/* Pipeline Context */
#define PEXPCMarkerType			 0
#define PEXPCMarkerScale		 1
#define PEXPCMarkerColour		 2
#define PEXPCMarkerBundleIndex		 3
#define PEXPCTextFont			 4
#define PEXPCTextPrecision		 5
#define PEXPCCharExpansion		 6
#define PEXPCCharSpacing		 7
#define PEXPCTextColour			 8
#define PEXPCCharHeight			 9
#define PEXPCCharUpVector		10
#define PEXPCTextPath			11
#define PEXPCTextAlignment		12
#define PEXPCAtextHeight		13
#define PEXPCAtextUpVector		14
#define PEXPCAtextPath			15
#define PEXPCAtextAlignment		16
#define PEXPCAtextStyle			17
#define PEXPCTextBundleIndex		18
#define PEXPCLineType			19
#define PEXPCLineWidth			20
#define PEXPCLineColour			21
#define PEXPCCurveApproximation		22
#define PEXPCPolylineInterp		23
#define PEXPCLineBundleIndex		24
#define PEXPCInteriorStyle		25
#define PEXPCInteriorStyleIndex		26
#define PEXPCSurfaceColour		27
#define PEXPCSurfaceReflAttr		28
#define PEXPCSurfaceReflModel		29
#define PEXPCSurfaceInterp		30
#define PEXPCBfInteriorStyle		31

#define PEXPCBfInteriorStyleIndex	32
#define PEXPCBfSurfaceColour		33
#define PEXPCBfSurfaceReflAttr		34
#define PEXPCBfSurfaceReflModel		35
#define PEXPCBfSurfaceInterp		36
#define PEXPCSurfaceApproximation	37
#define PEXPCCullingMode		38
#define PEXPCDistinguishFlag		39
#define PEXPCPatternSize		40
#define PEXPCPatternRefPt		41
#define PEXPCPatternRefVec1		42
#define PEXPCPatternRefVec2		43
#define PEXPCInteriorBundleIndex	44
#define PEXPCSurfaceEdgeFlag		45
#define PEXPCSurfaceEdgeType		46
#define PEXPCSurfaceEdgeWidth		47
#define PEXPCSurfaceEdgeColour		48
#define PEXPCEdgeBundleIndex		49
#define PEXPCLocalTransform		50
#define PEXPCGlobalTransform		51
#define PEXPCModelClip			52
#define PEXPCModelClipVolume		53
#define PEXPCViewIndex			54
#define PEXPCLightState			55
#define PEXPCDepthCueIndex		56
#define PEXPCSetAsfValues		57
#define PEXPCPickId			58
#define PEXPCHlhsrIdentifier		59
#define PEXPCNameSet			60
#define PEXPCColourApproxIndex		61
#define PEXPCRenderingColourModel	62
#define PEXPCParaSurfCharacteristics	63
#define PEXMaxPCIndex	63	

/* Renderer Bitmasks */
#define PEXRDPipelineContext		 (1L<<0)
#define PEXRDCurrentPath		 (1L<<1)
#define PEXRDMarkerBundle		 (1L<<2)
#define PEXRDTextBundle			 (1L<<3)
#define PEXRDLineBundle			 (1L<<4)
#define PEXRDInteriorBundle		 (1L<<5)
#define PEXRDEdgeBundle			 (1L<<6)
#define PEXRDViewTable			 (1L<<7)
#define PEXRDColourTable		 (1L<<8)
#define PEXRDDepthCueTable		 (1L<<9)
#define PEXRDLightTable			(1L<<10)
#define PEXRDColourApproxTable		(1L<<11)
#define PEXRDPatternTable		(1L<<12)
#define PEXRDTextFontTable		(1L<<13)
#define PEXRDHighlightIncl		(1L<<14)
#define PEXRDHighlightExcl		(1L<<15)
#define PEXRDInvisibilityIncl		(1L<<16)
#define PEXRDInvisibilityExcl		(1L<<17)
#define PEXRDRendererState		(1L<<18)
#define PEXRDHlhsrMode			(1L<<19)
#define PEXRDNpcSubvolume		(1L<<20)
#define PEXRDViewport			(1L<<21)
#define PEXRDClipList			(1L<<22)
#define PEXRDPickInclusion		(1L<<23)
#define PEXRDPickExclusion		(1L<<24)
#define PEXRDPickStartPath		(1L<<25)
#define PEXRDBackgroundColour		(1L<<26)
#define PEXRDClearI    			(1L<<27)
#define PEXRDClearZ    			(1L<<28)
#define PEXRDEchoMode			(1L<<29)
#define PEXMaxRDShift	29

/* Renderer Dynamics Bitmasks */
/*	tables		      */
#define PEXDynMarkerBundle			 (1L<<0)
#define PEXDynTextBundle			 (1L<<1)
#define PEXDynLineBundle			 (1L<<2)
#define PEXDynInteriorBundle			 (1L<<3)
#define PEXDynEdgeBundle			 (1L<<4)
#define PEXDynViewTable				 (1L<<5)
#define PEXDynColourTable			 (1L<<6)
#define PEXDynDepthCueTable			 (1L<<7)
#define PEXDynLightTable			 (1L<<8)
#define PEXDynColourApproxTable			 (1L<<9)
#define PEXDynPatternTable			(1L<<10)
#define PEXDynTextFontTable			(1L<<11)
#define PEXDynMarkerBundleContents		(1L<<16)
#define PEXDynTextBundleContents		(1L<<17)
#define PEXDynLineBundleContents		(1L<<18)
#define PEXDynInteriorBundleContents		(1L<<19)
#define PEXDynEdgeBundleContents		(1L<<20)
#define PEXDynViewTableContents			(1L<<21)
#define PEXDynColourTableContents		(1L<<22)
#define PEXDynDepthCueTableContents		(1L<<23)
#define PEXDynLightTableContents		(1L<<24)
#define PEXDynColourApproxContents		(1L<<25)
#define PEXDynPatternTableContents		(1L<<26)
#define PEXDynTextFontTableContents		(1L<<27)
/*	namesets	      */
#define PEXDynHighlightNameset			 (1L<<0)
#define PEXDynInvisibilityNameset		 (1L<<1)
#define PEXDynPickNameset        		 (1L<<2)
#define PEXDynHighlightNamesetContents		(1L<<16)
#define PEXDynInvisibilityNamesetContents	(1L<<17)
#define PEXDynPickNamesetContents		(1L<<18)
/*	attributes	      */
#define PEXDynHlhsrMode				 (1L<<0)
#define PEXDynNpcSubvolume			 (1L<<1)
#define PEXDynViewport				 (1L<<2)
#define PEXDynClipList				 (1L<<3)
#define PEXDynEchoMode				 (1L<<4)

#define PEXElementType		 (1L<<0)
#define PEXElementSize		 (1L<<1)
#define PEXElementData		 (1L<<2)

/* Search Context Bitmasks */
#define PEXSCPosition            (1L<<0)
#define PEXSCDistance            (1L<<1)
#define PEXSCCeiling             (1L<<2)
#define PEXSCModelClipFlag	 (1L<<3)
#define PEXSCStartPath           (1L<<4)
#define PEXSCNormalList          (1L<<5)
#define PEXSCInvertedList        (1L<<6)

/* Phigs Workstation Attribute Bitmasks */
#define PEXPWDisplayUpdate	0
#define PEXPWVisualState	1
#define PEXPWDisplaySurface	2
#define PEXPWViewUpdate		3
#define PEXPWDefinedViews	4
#define PEXPWWksUpdate		5
#define PEXPWReqNpcSubvolume	6
#define PEXPWCurNpcSubvolume	7
#define PEXPWReqWksViewport	8
#define PEXPWCurWksViewport	9
#define PEXPWHlhsrUpdate	10
#define PEXPWReqHlhsrMode	11
#define PEXPWCurHlhsrMode	12
#define PEXPWDrawable		13
#define PEXPWMarkerBundle	14
#define PEXPWTextBundle		15
#define PEXPWLineBundle		16
#define PEXPWInteriorBundle	17
#define PEXPWEdgeBundle		18
#define PEXPWColourTable	19
#define PEXPWDepthCueTable	20
#define PEXPWLightTable		21
#define PEXPWColourApproxTable	22
#define PEXPWPatternTable	23
#define PEXPWTextFontTable	24
#define PEXPWHighlightIncl	25
#define PEXPWHighlightExcl	26
#define PEXPWInvisibilityIncl	27
#define PEXPWInvisibilityExcl	28
#define PEXPWPostedStructures	29
#define PEXPWNumPriorities	30
#define PEXPWBufferUpdate	31

#define PEXPWReqBufferMode	32
#define PEXPWCurBufferMode	33

#define PEXMaxPWIndex		33

/* Indices for GetDynamics */
#define PEXPWDViewRep			 0
#define PEXPWDMarkerBundle		 1
#define PEXPWDTextBundle		 2
#define PEXPWDLineBundle		 3
#define PEXPWDInteriorBundle		 4
#define PEXPWDEdgeBundle		 5
#define PEXPWDColourTable		 6
#define PEXPWDPatternTable		 7
#define PEXPWDWksTransform		 8
#define PEXPWDHighlightFilter		 9
#define PEXPWDInvisibilityFilter	10
#define PEXPWDHlhsrMode			11
#define PEXPWDStructureModify		12
#define PEXPWDPostStructure		13
#define PEXPWDUnpostStructure		14
#define PEXPWDDeleteStructure		15
#define PEXPWDReferenceModify		16
#define PEXPWDBufferModify		17
#define PEXPWDLightTable		18
#define PEXPWDDepthCueTable		19
#define PEXPWDColourApproxTable		20

/* Pick Device Bitmasks */
#define PEXPDPickStatus			(1L<<0)
#define PEXPDPickPath			(1L<<1)
#define PEXPDPickPathOrder		(1L<<2)
#define PEXPDPickIncl			(1L<<3)
#define PEXPDPickExcl            	(1L<<4)
#define PEXPDPickDataRec		(1L<<5)
#define PEXPDPickPromptEchoType		(1L<<6)
#define PEXPDPickEchoVolume		(1L<<7)
#define PEXPDPickEchoSwitch		(1L<<8)

/* Pick Measure Bitmasks */
#define PEXPMStatus			(1L<<0)
#define PEXPMPath			(1L<<1)

/* Errors */
#define PEXColourTypeError		 0
#define PEXRendererStateError		 1
#define PEXFloatingPointFormatError	 2
#define PEXLabelError			 3
#define PEXLookupTableError		 4
#define PEXNameSetError			 5
#define PEXPathError			 6
#define PEXFontError			 7
#define PEXPhigsWksError		 8
#define PEXPickMeasureError		 9
#define PEXPipelineContextError		10
#define PEXRendererError		11
#define PEXSearchContextError		12
#define PEXStructureError		13
#define PEXOutputCommandError		14
#define PEXMaxError                     14

/* Requests */
#define PEX_GetExtensionInfo		 1
#define PEX_GetEnumeratedTypeInfo	 2
#define PEX_GetImpDepConstants	 	 3
#define PEX_CreateLookupTable		 4
#define PEX_CopyLookupTable		 5
#define PEX_FreeLookupTable		 6
#define PEX_GetTableInfo		 7
#define PEX_GetPredefinedEntries	 8
#define PEX_GetDefinedIndices		 9
#define PEX_GetTableEntry		10
#define PEX_GetTableEntries		11
#define PEX_SetTableEntries		12
#define PEX_DeleteTableEntries		13
#define PEX_CreatePipelineContext	14
#define PEX_CopyPipelineContext		15
#define PEX_FreePipelineContext		16
#define PEX_GetPipelineContext		17
#define PEX_ChangePipelineContext	18
#define PEX_CreateRenderer		19
#define PEX_FreeRenderer		20
#define PEX_ChangeRenderer		21
#define PEX_GetRendererAttributes	22
#define PEX_GetRendererDynamics		23
#define PEX_BeginRendering		24
#define PEX_EndRendering		25
#define PEX_BeginStructure		26
#define PEX_EndStructure		27
#define PEX_RenderOutputCommands	28
#define PEX_RenderNetwork		29
#define PEX_CreateStructure		30
#define PEX_CopyStructure		31
#define PEX_DestroyStructures		32
#define PEX_GetStructureInfo		33
#define PEX_GetElementInfo		34
#define PEX_GetStructuresInNetwork	35
#define PEX_GetAncestors		36
#define PEX_GetDescendants		37
#define PEX_FetchElements		38
#define PEX_SetEditingMode		39	
#define PEX_SetElementPointer		40
#define PEX_SetElementPointerAtLabel	41
#define PEX_ElementSearch		42
#define PEX_StoreElements		43
#define PEX_DeleteElements		44
#define PEX_DeleteElementsToLabel	45
#define PEX_DeleteBetweenLabels		46
#define PEX_CopyElements		47
#define PEX_ChangeStructureRefs		48
#define PEX_CreateNameSet		49
#define PEX_CopyNameSet			50
#define PEX_FreeNameSet			51
#define PEX_GetNameSet			52
#define PEX_ChangeNameSet		53
#define PEX_CreateSearchContext		54
#define PEX_CopySearchContext		55
#define PEX_FreeSearchContext		56
#define PEX_GetSearchContext		57
#define PEX_ChangeSearchContext		58
#define PEX_SearchNetwork		59
#define PEX_CreatePhigsWks		60
#define PEX_FreePhigsWks		61
#define PEX_GetWksInfo			62
#define PEX_GetDynamics			63
#define PEX_GetViewRep			64
#define PEX_RedrawAllStructures		65
#define PEX_UpdateWorkstation		66
#define PEX_RedrawClipRegion		67
#define PEX_ExecuteDeferredActions	68
#define PEX_SetViewPriority		69
#define PEX_SetDisplayUpdateMode	70
#define PEX_MapDCtoWC			71
#define PEX_MapWCtoDC			72
#define PEX_SetViewRep			73
#define PEX_SetWksWindow		74
#define PEX_SetWksViewport		75
#define PEX_SetHlhsrMode		76
#define PEX_SetWksBufferMode		77
#define PEX_PostStructure		78
#define PEX_UnpostStructure		79
#define PEX_UnpostAllStructures		80
#define PEX_GetWksPostings		81
#define PEX_GetPickDevice		82
#define PEX_ChangePickDevice		83
#define PEX_CreatePickMeasure		84
#define PEX_FreePickMeasure		85
#define PEX_GetPickMeasure		86
#define PEX_UpdatePickMeasure		87
#define PEX_OpenFont			88
#define PEX_CloseFont			89
#define PEX_QueryFont			90
#define PEX_ListFonts			91
#define PEX_ListFontsWithInfo		92
#define PEX_QueryTextExtents 		93
#define PEX_MatchRendererTargets        94
#define PEX_Escape                      95
#define PEX_EscapeWithReply             96
#define PEX_RenderElements              97
#define PEX_AccumulateState             98
#define PEX_BeginPickOne                99
#define PEX_EndPickOne                 100
#define PEX_PickOne                    101
#define PEX_BeginPickAll               102
#define PEX_EndPickAll                 103
#define PEX_PickAll                    104
#define PEXMaxRequest		       104

/* Output Commands */
#define PEXOCAll			  0
#define PEXOCMarkerType			  1
#define PEXOCMarkerScale		  2
#define PEXOCMarkerColourIndex		  3
#define PEXOCMarkerColour		  4
#define PEXOCMarkerBundleIndex		  5
#define PEXOCTextFontIndex		  6
#define PEXOCTextPrecision		  7
#define PEXOCCharExpansion		  8
#define PEXOCCharSpacing		  9
#define PEXOCTextColourIndex		 10
#define PEXOCTextColour			 11
#define PEXOCCharHeight			 12
#define PEXOCCharUpVector		 13
#define PEXOCTextPath			 14
#define PEXOCTextAlignment		 15
#define PEXOCAtextHeight		 16
#define PEXOCAtextUpVector		 17
#define PEXOCAtextPath			 18
#define PEXOCAtextAlignment		 19
#define PEXOCAtextStyle			 20
#define PEXOCTextBundleIndex		 21
#define PEXOCLineType			 22
#define PEXOCLineWidth			 23
#define PEXOCLineColourIndex		 24
#define PEXOCLineColour			 25
#define PEXOCCurveApproximation		 26
#define PEXOCPolylineInterp		 27
#define PEXOCLineBundleIndex		 28
#define PEXOCInteriorStyle		 29
#define PEXOCInteriorStyleIndex		 30
#define PEXOCSurfaceColourIndex		 31
#define PEXOCSurfaceColour		 32
#define PEXOCSurfaceReflAttr		 33
#define PEXOCSurfaceReflModel		 34
#define PEXOCSurfaceInterp		 35
#define PEXOCBfInteriorStyle		 36
#define PEXOCBfInteriorStyleIndex	 37
#define PEXOCBfSurfaceColourIndex	 38
#define PEXOCBfSurfaceColour		 39
#define PEXOCBfSurfaceReflAttr		 40
#define PEXOCBfSurfaceReflModel		 41
#define PEXOCBfSurfaceInterp		 42
#define PEXOCSurfaceApproximation	 43
#define PEXOCCullingMode		 44
#define PEXOCDistinguishFlag		 45
#define PEXOCPatternSize		 46
#define PEXOCPatternRefPt		 47
#define PEXOCPatternAttr		 48
#define PEXOCInteriorBundleIndex	 49
#define PEXOCSurfaceEdgeFlag		 50
#define PEXOCSurfaceEdgeType		 51
#define PEXOCSurfaceEdgeWidth		 52
#define PEXOCSurfaceEdgeColourIndex	 53
#define PEXOCSurfaceEdgeColour		 54
#define PEXOCEdgeBundleIndex		 55
#define PEXOCSetAsfValues		 56
#define PEXOCLocalTransform		 57
#define PEXOCLocalTransform2D		 58
#define PEXOCGlobalTransform		 59
#define PEXOCGlobalTransform2D		 60
#define PEXOCModelClip			 61
#define PEXOCModelClipVolume		 62
#define PEXOCModelClipVolume2D		 63
#define PEXOCRestoreModelClip		 64
#define PEXOCViewIndex			 65
#define PEXOCLightState			 66
#define PEXOCDepthCueIndex		 67
#define PEXOCPickId			 68
#define PEXOCHlhsrIdentifier		 69
#define PEXOCColourApproxIndex		 70
#define PEXOCRenderingColourModel	 71
#define PEXOCParaSurfCharacteristics	 72
#define PEXOCAddToNameSet		 73
#define PEXOCRemoveFromNameSet		 74
#define PEXOCExecuteStructure		 75
#define PEXOCLabel			 76
#define PEXOCApplicationData		 77
#define PEXOCGse			 78
#define PEXOCMarker			 79
#define PEXOCMarker2D			 80
#define PEXOCText			 81
#define PEXOCText2D			 82
#define PEXOCAnnotationText		 83
#define PEXOCAnnotationText2D		 84
#define PEXOCPolyline			 85
#define PEXOCPolyline2D			 86
#define PEXOCPolylineSet		 87
#define PEXOCNurbCurve			 88
#define PEXOCFillArea			 89
#define PEXOCFillArea2D			 90
#define PEXOCExtFillArea		 91
#define PEXOCFillAreaSet		 92
#define PEXOCFillAreaSet2D		 93
#define PEXOCExtFillAreaSet		 94
#define PEXOCTriangleStrip		 95
#define PEXOCQuadrilateralMesh		 96
#define PEXOCSOFAS			 97
#define PEXOCNurbSurface		 98
#define PEXOCCellArray			 99
#define PEXOCCellArray2D		100
#define PEXOCExtCellArray		101
#define PEXOCGdp			102
#define PEXOCGdp2D			103
#define PEXOCNoop			104
#define PEXMaxOC			104

#define PEXOCNil			0xffff

#endif /* PEX.h */

