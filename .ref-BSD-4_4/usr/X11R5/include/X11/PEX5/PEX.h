#ifndef PEX_H
#define PEX_H

/* $XConsortium: PEX.h,v 1.7 92/08/26 15:16:05 mor Exp $ */

/******************************************************************************/
/*  Copyright 1987,1991 by Digital Equipment Corporation, Maynard, Mass.      */
/*                                                                            */
/*  (c) Copyright Hewlett-Packard Company, 1992,  Fort Collins, Colorado      */
/*                                                                            */
/*                            All Rights Reserved                             */
/*                                                                            */
/*  Permission to use, copy, modify, and distribute this software and its     */
/*  documentation for any purpose and without fee is hereby granted,          */
/*  provided that the above copyright notices appear in all copies and that   */
/*  both the copyright notices and this permission notice appear in           */
/*  supporting documentation, and that the names of Digital or                */
/*  Hewlett-Packard not be used in advertising or publicity pertaining to     */
/*  distribution of the software without specific, written prior permission.  */
/*                                                                            */
/*  DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING  */
/*  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  */
/*  DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR   */
/*  ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,       */
/*  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,    */
/*  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS       */
/*  SOFTWARE.                                                                 */
/*                                                                            */
/*  HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD TO THIS         */
/*  SOFTWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF        */
/*  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  Hewlett-Packard    */
/*  shall not be liable for errors contained herein or direct, indirect,      */
/*  special, incidental or consequential damages in connection with the       */
/*  furnishing, performance or use of this software.                          */
/*                                                                            */
/******************************************************************************/

/*
 * PEX extension name
 */
#define PEX_NAME_STRING                 "X3D-PEX"

/*
 * PEX protocol major and minor numbers
 */
#define PEX_PROTO_MAJOR                 5               /* 5.1 protocol */
#define PEX_PROTO_MINOR                 1               /* 5.1 protocol */

/*
 * PEX standard subsets
 */
#define PEXCompleteImplementation       0
#define PEXImmediateMode                (1L<<0)
#define PEXWorkstationOnly              (1L<<1)
#define PEXStructureMode                (1L<<2)

/*
 * enumerated type information
 */
#define PEXETCounts                     0
#define PEXETIndex                      (1L<<0)
#define PEXETMnemonic                   (1L<<1)
#define PEXETAll                        (PEXETIndex | PEXETMnemonic)

/* enumerated types */
#define PEXETMarkerType                 1
#define PEXETATextStyle                 2
#define PEXETInteriorStyle              3
#define PEXETHatchStyle                 4
#define PEXETLineType                   5
#define PEXETSurfaceEdgeType            6
#define PEXETPickDeviceType             7
#define PEXETPolylineInterpMethod       8
#define PEXETCurveApproxMethod          9
#define PEXETReflectionModel            10
#define PEXETSurfaceInterpMethod        11
#define PEXETSurfaceApproxMethod        12
#define PEXETModelClipOperator          13
#define PEXETLightType                  14
#define PEXETColorType                  15
#define PEXETFloatFormat                16
#define PEXETHLHSRMode                  17
#define PEXETPromptEchoType             18
#define PEXETDisplayUpdateMode          19
#define PEXETColorApproxType            20
#define PEXETColorApproxModel           21
#define PEXETGDP2D                      22
#define PEXETGDP                        23
#define PEXETGSE                        24
#define PEXETTrimCurveApproxMethod      25
#define PEXETRenderingColorModel        26
#define PEXETParaSurfCharacteristics    27
#define PEXETEscape                     28
#define PEXETPickOneMethod              29
#define PEXETPickAllMethod              30

/* enumerated type values */

/* marker types */
#define PEXMarkerDot                    1
#define PEXMarkerCross                  2
#define PEXMarkerAsterisk               3
#define PEXMarkerCircle                 4
#define PEXMarkerX                      5

/* marker type mnemonics */
#define PEXETMMarkerDot                 "Dot"
#define PEXETMMarkerCross               "Cross"
#define PEXETMMarkerAsterisk            "Asterisk"
#define PEXETMMarkerCircle              "Circle"
#define PEXETMMarkerX                   "X"

/* annotation text styles */
#define PEXATextNotConnected            1
#define PEXATextConnected               2

/* annotation text style mnemonics */
#define PEXETMATextNotConnected         "NotConnected"
#define PEXETMATextConnected            "Connected"

/* interior styles */
#define PEXInteriorStyleHollow          1
#define PEXInteriorStyleSolid           2
#define PEXInteriorStylePattern         3
#define PEXInteriorStyleHatch           4
#define PEXInteriorStyleEmpty           5

/* interior style mnemonics */
#define PEXETMInteriorStyleHollow       "Hollow"
#define PEXETMInteriorStyleSolid        "Solid"
#define PEXETMInteriorStylePattern      "Pattern"
#define PEXETMInteriorStyleHatch        "Hatch"
#define PEXETMInteriorStyleEmpty        "Empty"

/* hatch style */

/* line types */
#define PEXLineTypeSolid                1
#define PEXLineTypeDashed               2
#define PEXLineTypeDotted               3
#define PEXLineTypeDashDot              4

/* line type mnemonics */
#define PEXETMLineTypeSolid             "Solid"
#define PEXETMLineTypeDashed            "Dashed"
#define PEXETMLineTypeDotted            "Dotted"
#define PEXETMLineTypeDashDot           "DashDot"

/* surface edge types */
#define PEXSurfaceEdgeSolid             1
#define PEXSurfaceEdgeDashed            2
#define PEXSurfaceEdgeDotted            3
#define PEXSurfaceEdgeDashDot           4

/* surface edge type mnemonics */
#define PEXETMSurfaceEdgeSolid          "Solid"
#define PEXETMSurfaceEdgeDashed         "Dashed"
#define PEXETMSurfaceEdgeDotted         "Dotted"
#define PEXETMSurfaceEdgeDashDot        "DashDot"

/* pick device types */
#define PEXPickDeviceDCHitBox           1
#define PEXPickDeviceNPCHitVolume       2

/* pick device type mnemonics */
#define PEXETMPickDeviceDCHitBox        "DC_HitBox"
#define PEXETMPickDeviceNPCHitVolume    "NPC_HitVolume"

/* polyline interpolation methods */
#define PEXPolylineInterpNone           1
#define PEXPolylineInterpColor          2

/* polyline interpolation method mnemonics */
#define PEXETMPolylineInterpNone        "None"
#define PEXETMPolylineInterpColor       "Color"

/* curve, surface and trim curve approximation methods */
#define PEXApproxImpDep                 1
#define PEXApproxConstantBetweenKnots   2
#define PEXApproxWCChordalSize          3
#define PEXApproxNPCChordalSize         4
#define PEXApproxDCChordalSize          5
#define PEXCurveApproxWCChordalDev      6
#define PEXCurveApproxNPCChordalDev     7
#define PEXCurveApproxDCChordalDev      8
#define PEXSurfaceApproxWCPlanarDev     6
#define PEXSurfaceApproxNPCPlanarDev    7
#define PEXSurfaceApproxDCPlanarDev     8
#define PEXApproxWCRelative             9
#define PEXApproxNPCRelative            10
#define PEXApproxDCRelative             11

/* curve, surface and trim curve approximation method mnemonics */
     /* PEXApproxImpDep                 is an implementation-dependent string */
#define PEXETMApproxConstantBetweenKnots "ConstantBetweenKnots"
#define PEXETMApproxWCChordalSize       "WCS_ChordalSize"
#define PEXETMApproxNPCChordalSize      "NPC_ChordalSize"
#define PEXETMApproxDCChordalSize       "DC_ChordalSize"
#define PEXETMCurveApproxWCChordalDev   "WCS_ChordalDev"
#define PEXETMCurveApproxNPCChordalDev  "NPC_ChordalDev"
#define PEXETMCurveApproxDCChordalDev   "DC_ChordalDev"
#define PEXETMSurfaceApproxWCPlanarDev  "WCS_PlanarDev"
#define PEXETMSurfaceApproxNPCPlanarDev "NPC_PlanarDev"
#define PEXETMSurfaceApproxDCPlanarDev  "DC_PlanarDev"
#define PEXETMApproxWCRelative          "WCS_Relative"
#define PEXETMApproxNPCRelative         "NPC_Relative"
#define PEXETMApproxDCRelative          "DC_Relative"

/* reflection models */
#define PEXReflectionNone               1
#define PEXReflectionAmbient            2
#define PEXReflectionDiffuse            3
#define PEXReflectionSpecular           4

/* reflection model mnemonics */
#define PEXETMReflectionNone            "NoShading"
#define PEXETMReflectionAmbient         "Ambient"
#define PEXETMReflectionDiffuse         "Diffuse"
#define PEXETMReflectionSpecular        "Specular"

/* surface interpolation methods */
#define PEXSurfaceInterpNone            1
#define PEXSurfaceInterpColor           2
#define PEXSurfaceInterpDotProduct      3
#define PEXSurfaceInterpNormal          4

/* surface interpolation method mnemonics */
#define PEXETMSurfaceInterpNone         "None"
#define PEXETMSurfaceInterpColor        "Color"
#define PEXETMSurfaceInterpDotProduct   "DotProduct"
#define PEXETMSurfaceInterpNormal       "Normal"

/* model clip operators */
#define PEXModelClipReplace             1
#define PEXModelClipIntersection        2

/* model clip operator mnemonics */
#define PEXETMModelClipReplace          "Replace"
#define PEXETMModelClipIntersection     "Intersection"

/* light types */
#define PEXLightAmbient                 1
#define PEXLightWCVector                2
#define PEXLightWCPoint                 3
#define PEXLightWCSpot                  4

/* light type mnemonics */
#define PEXETMLightAmbient              "Ambient"
#define PEXETMLightWCVector             "WCS_Vector"
#define PEXETMLightWCPoint              "WCS_Point"
#define PEXETMLightWCSpot               "WCS_Spot"

/* color types */
#define PEXColorTypeIndexed             0
#define PEXColorTypeRGB                 1
#define PEXColorTypeCIE                 2
#define PEXColorTypeHSV                 3
#define PEXColorTypeHLS                 4
#define PEXColorTypeRGB8                5
#define PEXColorTypeRGB16               6

/* color type mnemonics */
#define PEXETMColorTypeIndexed          "Indexed"
#define PEXETMColorTypeRGB              "RGBFloat"
#define PEXETMColorTypeCIE              "CIEFloat"
#define PEXETMColorTypeHSV              "HSVFloat"
#define PEXETMColorTypeHLS              "HLSFloat"
#define PEXETMColorTypeRGB8             "RGBInt8"
#define PEXETMColorTypeRGB16            "RGBInt16"

/* float formats */
#define PEXIEEE_754_32                  1
#define PEXDEC_F_Floating               2
#define PEXIEEE_754_64                  3
#define PEXDEC_D_Floating               4

/* float format mnemonics */
#define PEXETMIEEE_754_32               "IEEE_754_32"
#define PEXETMDEC_F_Floating            "DEC_F_Floating"
#define PEXETMIEEE_754_64               "IEEE_754_64"
#define PEXETMDEC_D_Floating            "DEC_D_Floating"

/* HLHSR modes */
#define PEXHLHSROff                     1
#define PEXHLHSRZBuffer                 2
#define PEXHLHSRPainters                3
#define PEXHLHSRScanline                4
#define PEXHLHSRHiddenLineOnly          5
#define PEXHLHSRZBufferID               6

/* HLHSR mode mnemonics */
#define PEXETMHLHSROff                  "Off"
#define PEXETMHLHSRZBuffer              "ZBuffer"
#define PEXETMHLHSRPainters             "Painters"
#define PEXETMHLHSRScanline             "Scanline"
#define PEXETMHLHSRHiddenLineOnly       "HiddenLineOnly"
#define PEXETMHLHSRZBufferID            "ZBufferId"

/* prompt echo types */
#define PEXEchoPrimitive                1
#define PEXEchoStructure                2
#define PEXEchoNetwork                  3

/* prompt echo type mnemonics */
#define PEXETMEchoPrimitive             "EchoPrimitive"
#define PEXETMEchoStructure             "EchoStructure"
#define PEXETMEchoNetwork               "EchoNetwork"

/* display update methods */
#define PEXVisualizeEach                1
#define PEXVisualizeEasy                2
#define PEXVisualizeNone                3
#define PEXSimulateSome                 4
#define PEXVisualizeWhenever            5

/* display update method mnemonics */
#define PEXETMVisualizeEach             "VisualizeEach"
#define PEXETMVisualizeEasy             "VisualizeEasy"
#define PEXETMVisualizeNone             "VisualizeNone"
#define PEXETMSimulateSome              "SimulateSome"
#define PEXETMVisualizeWhenever         "VisualizeWhenever"

/* color approximation types */
#define PEXColorSpace                   1
#define PEXColorRange                   2

/* color approximation type mnemonics */
#define PEXETMColorSpace                "ColorSpace"
#define PEXETMColorRange                "ColorRange"

/* color approximation models */
#define PEXColorApproxRGB               1
#define PEXColorApproxCIE               2
#define PEXColorApproxHSV               3
#define PEXColorApproxHLS               4
#define PEXColorApproxYIQ               5

/* color approximation model mnemonics */
#define PEXETMColorApproxRGB            "RGB"
#define PEXETMColorApproxCIE            "CIE"
#define PEXETMColorApproxHSV            "HSV"
#define PEXETMColorApproxHLS            "HLS"
#define PEXETMColorApproxYIQ            "YIQ"

/* rendering color models */
#define PEXRenderingColorModelImpDep    0
#define PEXRenderingColorModelRGB       1
#define PEXRenderingColorModelCIE       2
#define PEXRenderingColorModelHSV       3
#define PEXRenderingColorModelHLS       4

/* rendering color model mnemonics */
     /* PEXETMRenderingColorModelImpDep is an implementation-dependent string */
#define PEXETMRenderingColorModelRGB    "RGB"
#define PEXETMRenderingColorModelCIE    "CIE"
#define PEXETMRenderingColorModelHSV    "HSV"
#define PEXETMRenderingColorModelHLS    "HLS"

/* parametric surface characteristics */
#define PEXPSCNone                      1
#define PEXPSCImpDep                    2
#define PEXPSCIsoCurves                 3
#define PEXPSCMCLevelCurves             4
#define PEXPSCWCLevelCurves             5

/* parametric surface characteristic mnemonics */
#define PEXETMPSCNone                   "None"
     /* PEXETMPSCImpDep                 is an implementation-dependent string */
#define PEXETMPSCIsoCurves              "IsoparametricCurves"
#define PEXETMPSCMCLevelCurves          "MC_LevelCurves"
#define PEXETMPSCWCLevelCurves          "WC_LevelCurves"

/* standard escape identifiers */
#define PEXEscapeSetEchoColor           1

/* standard escape identifier mnemonics */
#define PEXETMEscapeSetEchoColor        "SetEchoColor"

/* pick one methods */
#define PEXPickLast                     1
#define PEXPickClosestZ                 2
#define PEXPickVisibleAny               3
#define PEXPickVisibleClosest           4

/* pick one method mnemonics */
#define PEXETMPickLast                  "Last"
#define PEXETMPickClosestZ              "ClosestZ"
#define PEXETMPickVisibleAny            "VisibleAny"
#define PEXETMPickVisibleClosest        "VisibleClosest"

/* pick all methods */
#define PEXPickAllAll                   1
#define PEXPickAllVisible               2

/* pick all method mnemonics */
#define PEXETMPickAllAll                "All"
#define PEXETMPickAllVisible            "Visible"


/*
 * implementation dependent constants
 */
#define PEXIDDitheringSupported         1
#define PEXIDMaxEdgeWidth               2
#define PEXIDMaxLineWidth               3
#define PEXIDMaxMarkerSize              4
#define PEXIDMaxModelClipPlanes         5
#define PEXIDMaxNameSetNames            6
#define PEXIDMaxNonAmbientLights        7
#define PEXIDMaxNURBOrder               8
#define PEXIDMaxTrimCurveOrder          9
#define PEXIDMinEdgeWidth               10
#define PEXIDMinLineWidth               11
#define PEXIDMinMarkerSize              12
#define PEXIDNominalEdgeWidth           13
#define PEXIDNominalLineWidth           14
#define PEXIDNominalMarkerSize          15
#define PEXIDNumSupportedEdgeWidths     16
#define PEXIDNumSupportedLineWidths     17
#define PEXIDNumSupportedMarkerSizes    18
#define PEXIDBestColorApprox            19
#define PEXIDTransparencySupported      20
#define PEXIDDoubleBufferingSupported   21
#define PEXIDChromaticityRedU           22
#define PEXIDChromaticityRedV           23
#define PEXIDLuminanceRed               24
#define PEXIDChromaticityGreenU         25
#define PEXIDChromaticityGreenV         26
#define PEXIDLuminanceGreen             27
#define PEXIDChromaticityBlueU          28
#define PEXIDChromaticityBlueV          29
#define PEXIDLuminanceBlue              30
#define PEXIDChromaticityWhiteU         31
#define PEXIDChromaticityWhiteV         32
#define PEXIDLuminanceWhite             33
#define PEXIDMaxHitsEventSupported      34

/* values for PEXIDBestColorApprox */
#define PEXColorApproxAnyValues         0
#define PEXColorApproxPowersOf2         1

/*
 * match rendering targets
 */
#define PEXAnyDrawable                  0
#define PEXWindowDrawable               1
#define PEXPixmapDrawable               2
#define PEXBufferDrawable               3


/*
 * output primitive and attribute values
 */

/* shape hints */
#define PEXShapeComplex                 0
#define PEXShapeNonConvex               1
#define PEXShapeConvex                  2
#define PEXShapeUnknown                 3

/* contour hints */
#define PEXContourDisjoint              0
#define PEXContourNested                1
#define PEXContourIntersecting          2
#define PEXContourUnknown               3

/* facet and vertex attributes bitmask */
#define PEXGANone                       0
#define PEXGAColor                      (1L<<0)
#define PEXGANormal                     (1L<<1)
#define PEXGAEdges                      (1L<<2)

/* flags (e.g., switches, visibility, and edges) */
#define PEXOff                          0
#define PEXOn                           1

/* HLHSR identifier for PEXHLHSRZBufferID HLHSR mode */
#define PEXHLHSRIDDisable               0
#define PEXHLHSRIDEnable                1

/* text path direction */
#define PEXPathRight                    0
#define PEXPathLeft                     1
#define PEXPathUp                       2
#define PEXPathDown                     3

/* text vertical alignment */
#define PEXVAlignNormal                 0
#define PEXVAlignTop                    1
#define PEXVAlignCap                    2
#define PEXVAlignHalf                   3
#define PEXVAlignBase                   4
#define PEXVAlignBottom                 5

/* text horizontal alignment */
#define PEXHAlignNormal                 0
#define PEXHAlignLeft                   1
#define PEXHAlignCenter                 2
#define PEXHAlignRight                  3

/* text precision */
#define PEXStringPrecision              0
#define PEXCharPrecision                1
#define PEXStrokePrecision              2

/* character set width */
#define PEXCSByte                       0
#define PEXCSShort                      1
#define PEXCSLong                       2

/* curve and surface types */
#define PEXRational                     0
#define PEXNonRational                  1

/* clipping */
#define PEXClipXY                       (1L<<0)
#define PEXClipBack                     (1L<<1)
#define PEXClipFront                    (1L<<2)
#define PEXClippingAll                  (PEXClipXY | PEXClipBack | PEXClipFront)

#define PEXClip                         0
#define PEXNoClip                       1

/* cull modes */
#define PEXNone                         0
#define PEXBackFaces                    1
#define PEXFrontFaces                   2

/* local transform composition types */
#define PEXPreConcatenate               0
#define PEXPostConcatenate              1
#define PEXReplace                      2

/* isoparametric curves placement types */
#define PEXUniformPlacement             0
#define PEXNonUniformPlacement          1

/* aspect source flags (ASFs) */
#define PEXBundled                      0
#define PEXIndividual                   1

/* mask values for setting ASFs */
#define PEXASFMarkerType                (1L<<0)
#define PEXASFMarkerScale               (1L<<1)
#define PEXASFMarkerColor               (1L<<2)
#define PEXASFTextFontIndex             (1L<<3)
#define PEXASFTextPrec                  (1L<<4)
#define PEXASFCharExpansion             (1L<<5)
#define PEXASFCharSpacing               (1L<<6)
#define PEXASFTextColor                 (1L<<7)
#define PEXASFLineType                  (1L<<8)
#define PEXASFLineWidth                 (1L<<9)
#define PEXASFLineColor                 (1L<<10)
#define PEXASFCurveApprox               (1L<<11)
#define PEXASFPolylineInterp            (1L<<12)
#define PEXASFInteriorStyle             (1L<<13)
#define PEXASFInteriorStyleIndex        (1L<<14)
#define PEXASFSurfaceColor              (1L<<15)
#define PEXASFSurfaceInterp             (1L<<16)
#define PEXASFReflectionModel           (1L<<17)
#define PEXASFReflectionAttr            (1L<<18)
#define PEXASFBFInteriorStyle           (1L<<19)
#define PEXASFBFInteriorStyleIndex      (1L<<20)
#define PEXASFBFSurfaceColor            (1L<<21)
#define PEXASFBFSurfaceInterp           (1L<<22)
#define PEXASFBFReflectionModel         (1L<<23)
#define PEXASFBFReflectionAttr          (1L<<24)
#define PEXASFSurfaceApprox             (1L<<25)
#define PEXASFSurfaceEdges              (1L<<26)
#define PEXASFSurfaceEdgeType           (1L<<27)
#define PEXASFSurfaceEdgeWidth          (1L<<28)
#define PEXASFSurfaceEdgeColor          (1L<<29)


/*
 * resource identifier value when resource is referenced, but freed
 */
#define PEXAlreadyFreed                 1


/*
 * pipeline context
 */

/* pipeline context attributes bitmask shift values */
#define PEXPCMarkerType                 0
#define PEXPCMarkerScale                1
#define PEXPCMarkerColor                2
#define PEXPCMarkerBundleIndex          3
#define PEXPCTextFont                   4
#define PEXPCTextPrecision              5
#define PEXPCCharExpansion              6
#define PEXPCCharSpacing                7
#define PEXPCTextColor                  8
#define PEXPCCharHeight                 9
#define PEXPCCharUpVector               10
#define PEXPCTextPath                   11
#define PEXPCTextAlignment              12
#define PEXPCATextHeight                13
#define PEXPCATextUpVector              14
#define PEXPCATextPath                  15
#define PEXPCATextAlignment             16
#define PEXPCATextStyle                 17
#define PEXPCTextBundleIndex            18
#define PEXPCLineType                   19
#define PEXPCLineWidth                  20
#define PEXPCLineColor                  21
#define PEXPCCurveApprox                22
#define PEXPCPolylineInterp             23
#define PEXPCLineBundleIndex            24
#define PEXPCInteriorStyle              25
#define PEXPCInteriorStyleIndex         26
#define PEXPCSurfaceColor               27
#define PEXPCReflectionAttr             28
#define PEXPCReflectionModel            29
#define PEXPCSurfaceInterp              30
#define PEXPCBFInteriorStyle            31

#define PEXPCBFInteriorStyleIndex       32
#define PEXPCBFSurfaceColor             33
#define PEXPCBFReflectionAttr           34
#define PEXPCBFReflectionModel          35
#define PEXPCBFSurfaceInterp            36
#define PEXPCSurfaceApprox              37
#define PEXPCCullingMode                38
#define PEXPCDistinguishFlag            39
#define PEXPCPatternSize                40
#define PEXPCPatternRefPoint            41
#define PEXPCPatternRefVec1             42
#define PEXPCPatternRefVec2             43
#define PEXPCInteriorBundleIndex        44
#define PEXPCSurfaceEdgeFlag            45
#define PEXPCSurfaceEdgeType            46
#define PEXPCSurfaceEdgeWidth           47
#define PEXPCSurfaceEdgeColor           48
#define PEXPCEdgeBundleIndex            49
#define PEXPCLocalTransform             50
#define PEXPCGlobalTransform            51
#define PEXPCModelClip                  52
#define PEXPCModelClipVolume            53
#define PEXPCViewIndex                  54
#define PEXPCLightState                 55
#define PEXPCDepthCueIndex              56
#define PEXPCASFValues                  57
#define PEXPCPickID                     58
#define PEXPCHLHSRIdentifier            59
#define PEXPCNameSet                    60
#define PEXPCColorApproxIndex           61
#define PEXPCRenderingColorModel        62
#define PEXPCParaSurfCharacteristics    63
#define PEXPCMaxShift                   63


/*
 * renderer and renderer picking
 */

/* renderer state */
#define PEXIdle                         0
#define PEXRendering                    1
#define PEXPicking                      2

/* renderer dynamics */
#define PEXDynamic                      0
#define PEXNotDynamic                   1

/* renderer echo modes */
#define PEXNoEcho                       0
#define PEXEcho                         1
#define PEXUnEcho                       2

/* renderer attributes bitmask */
#define PEXRAPipelineContext            (1L<<0)
#define PEXRACurrentPath                (1L<<1)
#define PEXRAMarkerBundle               (1L<<2)
#define PEXRATextBundle                 (1L<<3)
#define PEXRALineBundle                 (1L<<4)
#define PEXRAInteriorBundle             (1L<<5)
#define PEXRAEdgeBundle                 (1L<<6)
#define PEXRAViewTable                  (1L<<7)
#define PEXRAColorTable                 (1L<<8)
#define PEXRADepthCueTable              (1L<<9)
#define PEXRALightTable                 (1L<<10)
#define PEXRAColorApproxTable           (1L<<11)
#define PEXRAPatternTable               (1L<<12)
#define PEXRATextFontTable              (1L<<13)
#define PEXRAHighlightIncl              (1L<<14)
#define PEXRAHighlightExcl              (1L<<15)
#define PEXRAInvisibilityIncl           (1L<<16)
#define PEXRAInvisibilityExcl           (1L<<17)
#define PEXRARendererState              (1L<<18)
#define PEXRAHLHSRMode                  (1L<<19)
#define PEXRANPCSubVolume               (1L<<20)
#define PEXRAViewport                   (1L<<21)
#define PEXRAClipList                   (1L<<22)
#define PEXRAPickIncl                   (1L<<23)
#define PEXRAPickExcl                   (1L<<24)
#define PEXRAPickStartPath              (1L<<25)
#define PEXRABackgroundColor            (1L<<26)
#define PEXRAClearImage                 (1L<<27)
#define PEXRAClearZ                     (1L<<28)
#define PEXRAEchoMode                   (1L<<29)
#define PEXRAMaxShift                   29

/* renderer dynamics bitmask */
/*      tables                */
#define PEXRDTMarkerBundle              (1L<<0)
#define PEXRDTTextBundle                (1L<<1)
#define PEXRDTLineBundle                (1L<<2)
#define PEXRDTInteriorBundle            (1L<<3)
#define PEXRDTEdgeBundle                (1L<<4)
#define PEXRDTViewTable                 (1L<<5)
#define PEXRDTColorTable                (1L<<6)
#define PEXRDTDepthCueTable             (1L<<7)
#define PEXRDTLightTable                (1L<<8)
#define PEXRDTColorApproxTable          (1L<<9)
#define PEXRDTPatternTable              (1L<<10)
#define PEXRDTTextFontTable             (1L<<11)
#define PEXRDTMarkerBundleContents      (1L<<16)
#define PEXRDTTextBundleContents        (1L<<17)
#define PEXRDTLineBundleContents        (1L<<18)
#define PEXRDTInteriorBundleContents    (1L<<19)
#define PEXRDTEdgeBundleContents        (1L<<20)
#define PEXRDTViewTableContents         (1L<<21)
#define PEXRDTColorTableContents        (1L<<22)
#define PEXRDTDepthCueTableContents     (1L<<23)
#define PEXRDTLightTableContents        (1L<<24)
#define PEXRDTColorApproxContents       (1L<<25)
#define PEXRDTPatternTableContents      (1L<<26)
#define PEXRDTTextFontTableContents     (1L<<27)
/*      name sets              */
#define PEXRDNHighlightNameSet          (1L<<0)
#define PEXRDNInvisibilityNameSet       (1L<<1)
#define PEXRDNPickNameSet               (1L<<2)
#define PEXRDNHighlightNameSetContents  (1L<<16)
#define PEXRDNInvisibilityNameSetContents (1L<<17)
#define PEXRDNPickNameSetContents       (1L<<18)
/*      attributes            */
#define PEXRDAHLHSRMode                 (1L<<0)
#define PEXRDANPCSubVolume              (1L<<1)
#define PEXRDAViewport                  (1L<<2)
#define PEXRDAClipList                  (1L<<3)
#define PEXRDAEchoMode                  (1L<<4)

/* renderer pick status */
#define PEXNoPick                       0
#define PEXPick                         1
#define PEXAbortedPick                  2


/*
 * name set
 */

/* name set changes */
#define PEXNSAdd                        0
#define PEXNSRemove                     1
#define PEXNSReplace                    2


/*
 * look up table
 */

/* table types */
#define PEXLUTLineBundle                1
#define PEXLUTMarkerBundle              2
#define PEXLUTTextBundle                3
#define PEXLUTInteriorBundle            4
#define PEXLUTEdgeBundle                5
#define PEXLUTPattern                   6
#define PEXLUTTextFont                  7
#define PEXLUTColor                     8
#define PEXLUTView                      9
#define PEXLUTLight                     10
#define PEXLUTDepthCue                  11
#define PEXLUTColorApprox               12

/* status in PEXGetTableEntry */
#define PEXDefaultEntry                 0
#define PEXDefinedEntry                 1

/* return type in PEXGetTableEntry and PEXGetTableEntries */
#define PEXSetValue                     0
#define PEXRealizedValue                1


/*
 * structure
 */

/* structure editing mode */
#define PEXStructureInsert              0
#define PEXStructureReplace             1 

/* whence in structure element ranges */
#define PEXBeginning                    0
#define PEXCurrent                      1
#define PEXEnd                          2

/* structure info */
#define PEXElementPtr                   (1L<<0)
#define PEXNumElements                  (1L<<1)
#define PEXLengthStructure              (1L<<2)
#define PEXHasRefs                      (1L<<3)
#define PEXEditMode                     (1L<<4)

/* structures in network */
#define PEXAll                          0
#define PEXOrphans                      1

/* ancestors and descendents*/
#define PEXTopPart                      0
#define PEXBottomPart                   1

/* element search */
#define PEXForward                      0
#define PEXBackward                     1

/* element info bitmask */
#define PEXElementType                  (1L<<0)
#define PEXElementSize                  (1L<<1)
#define PEXElementData                  (1L<<2)


/*
 * search context
 */

/* element search status */
#define PEXNotFound                     1
#define PEXFound                        2

/* search context attributes bitmask */
#define PEXSCPosition                   (1L<<0)
#define PEXSCDistance                   (1L<<1)
#define PEXSCCeiling                    (1L<<2)
#define PEXSCModelClipFlag              (1L<<3)
#define PEXSCStartPath                  (1L<<4)
#define PEXSCNormalList                 (1L<<5)
#define PEXSCInvertedList               (1L<<6)
#define PEXSCMaxShift                   6


/*
 * PHIGS workstation
 */

/* workstation update state */
#define PEXNotPending                   0
#define PEXPending                      1

/* workstation visual state */
#define PEXCorrect                      0
#define PEXDeferred                     1
#define PEXSimulated                    2

/* workstation display state */
#define PEXEmpty                        0
#define PEXNotEmpty                     1

/* workstation buffer mode */
#define PEXSingleBuffered               0
#define PEXDoubleBuffered               1

/* workstation dynamics */
#define PEXIMM                          0
#define PEXIRG                          1
#define PEXCBS                          2

/* workstation structure posting priorities */
#define PEXHigher                       0
#define PEXLower                        1

/* workstation attributes bitmask shift values */
#define PEXPWDisplayUpdate              0
#define PEXPWVisualState                1
#define PEXPWDisplaySurface             2
#define PEXPWViewUpdate                 3
#define PEXPWDefinedViews               4
#define PEXPWWorkstationUpdate          5
#define PEXPWReqNPCSubVolume            6
#define PEXPWCurNPCSubVolume            7
#define PEXPWReqViewport                8
#define PEXPWCurViewport                9
#define PEXPWHLHSRUpdate                10
#define PEXPWReqHLHSRMode               11
#define PEXPWCurHLHSRMode               12
#define PEXPWDrawable                   13
#define PEXPWMarkerBundle               14
#define PEXPWTextBundle                 15
#define PEXPWLineBundle                 16
#define PEXPWInteriorBundle             17
#define PEXPWEdgeBundle                 18
#define PEXPWColorTable                 19
#define PEXPWDepthCueTable              20
#define PEXPWLightTable                 21
#define PEXPWColorApproxTable           22
#define PEXPWPatternTable               23
#define PEXPWTextFontTable              24
#define PEXPWHighlightIncl              25
#define PEXPWHighlightExcl              26
#define PEXPWInvisibilityIncl           27
#define PEXPWInvisibilityExcl           28
#define PEXPWPostedStructures           29
#define PEXPWNumPriorities              30
#define PEXPWBufferUpdate               31

#define PEXPWReqBufferMode              32
#define PEXPWCurBufferMode              33
#define PEXPWMaxShift                   33

/* values for indices to returned workstation dynamics */
#define PEXPWDViewRep                   0
#define PEXPWDMarkerBundle              1
#define PEXPWDTextBundle                2
#define PEXPWDLineBundle                3
#define PEXPWDInteriorBundle            4
#define PEXPWDEdgeBundle                5
#define PEXPWDColorTable                6
#define PEXPWDPatternTable              7
#define PEXPWDWorkstationTransform      8
#define PEXPWDHighlightFilter           9
#define PEXPWDInvisibilityFilter        10
#define PEXPWDHLHSRMode                 11
#define PEXPWDStructureModify           12
#define PEXPWDPostStructure             13
#define PEXPWDUnpostStructure           14
#define PEXPWDDeleteStructure           15
#define PEXPWDReferenceModify           16
#define PEXPWDBufferModify              17
#define PEXPWDLightTable                18
#define PEXPWDDepthCueTable             19
#define PEXPWDColorApproxTable          20


/*
 * workstation picking
 */

/* workstation pick status */
     /* PEXNoPick                       defined the same as for renderer pick */
     /* PEXPick                         defined the same as for renderer pick */

/* pick echo modes */
     /* PEXNoEcho                       defined the same as for renderer echo */
     /* PEXEcho                         defined the same as for renderer echo */

/* pick path order */
#define PEXTopFirst                     0
#define PEXBottomFirst                  1

/* pick all, more hits flag */
#define PEXMoreHits                     0
#define PEXNoMoreHits                   1
#define PEXMayBeMoreHits                2

/* workstation pick device attributes bitmask */
#define PEXPDPickStatus                 (1L<<0)
#define PEXPDPickPath                   (1L<<1)
#define PEXPDPickPathOrder              (1L<<2)
#define PEXPDPickIncl                   (1L<<3)
#define PEXPDPickExcl                   (1L<<4)
#define PEXPDPickDataRec                (1L<<5)
#define PEXPDPromptEchoType             (1L<<6)
#define PEXPDEchoVolume                 (1L<<7)
#define PEXPDEchoSwitch                 (1L<<8)
#define PEXPDMaxShift                   8

/* workstation pick measure attributes bitmask */
#define PEXPMStatus                     (1L<<0)
#define PEXPMPath                       (1L<<1)
#define PEXPMMaxShift                   1

/*
 * events
 */
#define PEXMaxHitsReached               0
#define PEXMaxEvent                     0


/*
 * errors
 */
#define BadPEXColorType                 0
#define BadPEXRendererState             1
#define BadPEXFloatingPointFormat       2
#define BadPEXLabel                     3
#define BadPEXLookupTable               4
#define BadPEXNameSet                   5
#define BadPEXPath                      6
#define BadPEXFont                      7
#define BadPEXWorkstation               8
#define BadPEXPickMeasure               9
#define BadPEXPipelineContext           10
#define BadPEXRenderer                  11
#define BadPEXSearchContext             12
#define BadPEXStructure                 13
#define BadPEXOutputCommand             14
#define PEXMaxError                     14


/*
 * protocol output command codes
 */
#define PEXOCAll                        0
#define PEXOCMarkerType                 1
#define PEXOCMarkerScale                2
#define PEXOCMarkerColorIndex           3
#define PEXOCMarkerColor                4
#define PEXOCMarkerBundleIndex          5
#define PEXOCTextFontIndex              6
#define PEXOCTextPrecision              7
#define PEXOCCharExpansion              8
#define PEXOCCharSpacing                9
#define PEXOCTextColorIndex             10
#define PEXOCTextColor                  11
#define PEXOCCharHeight                 12
#define PEXOCCharUpVector               13
#define PEXOCTextPath                   14
#define PEXOCTextAlignment              15
#define PEXOCATextHeight                16
#define PEXOCATextUpVector              17
#define PEXOCATextPath                  18
#define PEXOCATextAlignment             19
#define PEXOCATextStyle                 20
#define PEXOCTextBundleIndex            21
#define PEXOCLineType                   22
#define PEXOCLineWidth                  23
#define PEXOCLineColorIndex             24
#define PEXOCLineColor                  25
#define PEXOCCurveApprox                26
#define PEXOCPolylineInterpMethod       27
#define PEXOCLineBundleIndex            28
#define PEXOCInteriorStyle              29
#define PEXOCInteriorStyleIndex         30
#define PEXOCSurfaceColorIndex          31
#define PEXOCSurfaceColor               32
#define PEXOCReflectionAttributes       33
#define PEXOCReflectionModel            34
#define PEXOCSurfaceInterpMethod        35
#define PEXOCBFInteriorStyle            36
#define PEXOCBFInteriorStyleIndex       37
#define PEXOCBFSurfaceColorIndex        38
#define PEXOCBFSurfaceColor             39
#define PEXOCBFReflectionAttributes     40
#define PEXOCBFReflectionModel          41
#define PEXOCBFSurfaceInterpMethod      42
#define PEXOCSurfaceApprox              43
#define PEXOCFacetCullingMode           44
#define PEXOCFacetDistinguishFlag       45
#define PEXOCPatternSize                46
#define PEXOCPatternAttributes2D        47
#define PEXOCPatternAttributes          48
#define PEXOCInteriorBundleIndex        49
#define PEXOCSurfaceEdgeFlag            50
#define PEXOCSurfaceEdgeType            51
#define PEXOCSurfaceEdgeWidth           52
#define PEXOCSurfaceEdgeColorIndex      53
#define PEXOCSurfaceEdgeColor           54
#define PEXOCEdgeBundleIndex            55
#define PEXOCIndividualASF              56
#define PEXOCLocalTransform             57
#define PEXOCLocalTransform2D           58
#define PEXOCGlobalTransform            59
#define PEXOCGlobalTransform2D          60
#define PEXOCModelClipFlag              61
#define PEXOCModelClipVolume            62
#define PEXOCModelClipVolume2D          63
#define PEXOCRestoreModelClipVolume     64
#define PEXOCViewIndex                  65
#define PEXOCLightSourceState           66
#define PEXOCDepthCueIndex              67
#define PEXOCPickID                     68
#define PEXOCHLHSRID                    69
#define PEXOCColorApproxIndex           70
#define PEXOCRenderingColorModel        71
#define PEXOCParaSurfCharacteristics    72
#define PEXOCAddToNameSet               73
#define PEXOCRemoveFromNameSet          74
#define PEXOCExecuteStructure           75
#define PEXOCLabel                      76
#define PEXOCApplicationData            77
#define PEXOCGSE                        78
#define PEXOCMarkers                    79
#define PEXOCMarkers2D                  80
#define PEXOCText                       81
#define PEXOCText2D                     82
#define PEXOCAnnotationText             83
#define PEXOCAnnotationText2D           84
#define PEXOCPolyline                   85
#define PEXOCPolyline2D                 86
#define PEXOCPolylineSetWithData        87
#define PEXOCNURBCurve                  88
#define PEXOCFillArea                   89
#define PEXOCFillArea2D                 90
#define PEXOCFillAreaWithData           91
#define PEXOCFillAreaSet                92
#define PEXOCFillAreaSet2D              93
#define PEXOCFillAreaSetWithData        94
#define PEXOCTriangleStrip              95
#define PEXOCQuadrilateralMesh          96
#define PEXOCSetOfFillAreaSets          97
#define PEXOCNURBSurface                98
#define PEXOCCellArray                  99
#define PEXOCCellArray2D                100
#define PEXOCExtendedCellArray          101
#define PEXOCGDP                        102
#define PEXOCGDP2D                      103
#define PEXOCNoop                       104
#define PEXOCNil                        0xffff


/*
 * protocol request codes
 */
#define PEXRCGetExtensionInfo           1
#define PEXRCGetEnumTypeInfo            2
#define PEXRCGetImpDepConstants         3
#define PEXRCCreateLookupTable          4
#define PEXRCCopyLookupTable            5
#define PEXRCFreeLookupTable            6
#define PEXRCGetTableInfo               7
#define PEXRCGetPredefinedEntries       8
#define PEXRCGetDefinedIndices          9
#define PEXRCGetTableEntry              10
#define PEXRCGetTableEntries            11
#define PEXRCSetTableEntries            12
#define PEXRCDeleteTableEntries         13
#define PEXRCCreatePipelineContext      14
#define PEXRCCopyPipelineContext        15
#define PEXRCFreePipelineContext        16
#define PEXRCGetPipelineContext         17
#define PEXRCChangePipelineContext      18
#define PEXRCCreateRenderer             19
#define PEXRCFreeRenderer               20
#define PEXRCChangeRenderer             21
#define PEXRCGetRendererAttributes      22
#define PEXRCGetRendererDynamics        23
#define PEXRCBeginRendering             24
#define PEXRCEndRendering               25
#define PEXRCBeginStructure             26
#define PEXRCEndStructure               27
#define PEXRCRenderOutputCommands       28
#define PEXRCRenderNetwork              29
#define PEXRCCreateStructure            30
#define PEXRCCopyStructure              31
#define PEXRCDestroyStructures          32
#define PEXRCGetStructureInfo           33
#define PEXRCGetElementInfo             34
#define PEXRCGetStructuresInNetwork     35
#define PEXRCGetAncestors               36
#define PEXRCGetDescendants             37
#define PEXRCFetchElements              38
#define PEXRCSetEditingMode             39      
#define PEXRCSetElementPointer          40
#define PEXRCSetElementPointerAtLabel   41
#define PEXRCElementSearch              42
#define PEXRCStoreElements              43
#define PEXRCDeleteElements             44
#define PEXRCDeleteElementsToLabel      45
#define PEXRCDeleteBetweenLabels        46
#define PEXRCCopyElements               47
#define PEXRCChangeStructureRefs        48
#define PEXRCCreateNameSet              49
#define PEXRCCopyNameSet                50
#define PEXRCFreeNameSet                51
#define PEXRCGetNameSet                 52
#define PEXRCChangeNameSet              53
#define PEXRCCreateSearchContext        54
#define PEXRCCopySearchContext          55
#define PEXRCFreeSearchContext          56
#define PEXRCGetSearchContext           57
#define PEXRCChangeSearchContext        58
#define PEXRCSearchNetwork              59
#define PEXRCCreateWorkstation          60
#define PEXRCFreeWorkstation            61
#define PEXRCGetWorkstationAttributes   62
#define PEXRCGetWorkstationDynamics     63
#define PEXRCGetWorkstationViewRep      64
#define PEXRCRedrawAllStructures        65
#define PEXRCUpdateWorkstation          66
#define PEXRCRedrawClipRegion           67
#define PEXRCExecuteDeferredActions     68
#define PEXRCSetWorkstationViewPriority 69
#define PEXRCSetWorkstationDisplayUpdateMode 70
#define PEXRCMapDCtoWC                  71
#define PEXRCMapWCtoDC                  72
#define PEXRCSetWorkstationViewRep      73
#define PEXRCSetWorkstationWindow       74
#define PEXRCSetWorkstationViewport     75
#define PEXRCSetWorkstationHLHSRMode    76
#define PEXRCSetWorkstationBufferMode   77
#define PEXRCPostStructure              78
#define PEXRCUnpostStructure            79
#define PEXRCUnpostAllStructures        80
#define PEXRCGetWorkstationPostings     81
#define PEXRCGetPickDevice              82
#define PEXRCChangePickDevice           83
#define PEXRCCreatePickMeasure          84
#define PEXRCFreePickMeasure            85
#define PEXRCGetPickMeasure             86
#define PEXRCUpdatePickMeasure          87
#define PEXRCLoadFont                   88
#define PEXRCUnloadFont                 89
#define PEXRCQueryFont                  90
#define PEXRCListFonts                  91
#define PEXRCListFontsWithInfo          92
#define PEXRCQueryTextExtents           93
#define PEXRCMatchRenderingTargets      94
#define PEXRCEscape                     95
#define PEXRCEscapeWithReply            96
#define PEXRCRenderElements             97
#define PEXRCAccumulateState            98
#define PEXRCBeginPickOne               99
#define PEXRCEndPickOne                 100
#define PEXRCPickOne                    101
#define PEXRCBeginPickAll               102
#define PEXRCEndPickAll                 103
#define PEXRCPickAll                    104

#endif /* PEX_H */
