#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/
/* $Header: Xproto.h,v 10.11 86/04/22 15:09:54 jg Rel $ */

/* Definitions for the X window system usually hidden by library interface */

#define XFROMUSER
#define X_TCP_LI_PORT	5800		/* add display number */
#define X_TCP_BI_PORT	5900		/* add display number */

#if defined(XFROMUSER) || (!defined(XFROMINIT) && defined(sun))
#define X_UNIX_PATH	"/tmp/X"	/* concat display number */
#else
#define X_UNIX_PATH	"/dev/X"	/* concat display number */
#endif

/* Request structure */

#ifdef BIGSHORTS
#define psizeof(x) ( 2*sizeof(x) / sizeof(short) )
typedef struct _short_p {
	short left:16, right:16;
} short_p;
typedef struct _ushort_p {
	unsigned short left:16, right:16;
} ushort_p;
#else
#define psizeof sizeof
#endif

typedef struct _XReq {
	unsigned char code;
	unsigned char func;
	unsigned short mask B16;
	Window windowId;
	union {
	    long l[4];
#ifdef BIGSHORTS
	    short_p s[4];
	    ushort_p u[4];
#else
	    short s[8];
	    unsigned short u[8];
#endif BIGSHORTS
	    char b[16];
	} param;
} XReq;

/* Reply structure */

typedef struct _XRep {
	long code;
	union {
	    long l[5];
#ifdef BIGSHORTS
	    short_p s[5];
	    ushort_p u[5];
#else
	    short s[10];
	    unsigned short u[10];
#endif BIGSHORTS
	    char b[20];
	} param;
} XRep;

#ifdef BIGSHORTS
#define params0 param.s[0].left
#define params1 param.s[0].right
#define params2 param.s[1].left
#define params3 param.s[1].right
#define params4 param.s[2].left
#define params5 param.s[2].right
#define params6 param.s[3].left
#define params7 param.s[3].right
#define params8 param.s[4].left
#define params9 param.s[4].right
#define paramu0 param.u[0].left
#define paramu1 param.u[0].right
#define paramu2 param.u[1].left
#define paramu3 param.u[1].right
#define paramu4 param.u[2].left
#define paramu5 param.u[2].right
#define paramu6 param.u[3].left
#define paramu7 param.u[3].right
#define paramu8 param.u[4].left
#define paramu9 param.u[4].right
#else
#define params0 param.s[0]
#define params1 param.s[1]
#define params2 param.s[2]
#define params3 param.s[3]
#define params4 param.s[4]
#define params5 param.s[5]
#define params6 param.s[6]
#define params7 param.s[7]
#define params8 param.s[8]
#define params9 param.s[9]
#define paramu0 param.u[0]
#define paramu1 param.u[1]
#define paramu2 param.u[2]
#define paramu3 param.u[3]
#define paramu4 param.u[4]
#define paramu5 param.u[5]
#define paramu6 param.u[6]
#define paramu7 param.u[7]
#define paramu8 param.u[8]
#define paramu9 param.u[9]
#endif BIGSHORTS

/* Reply codes */

#define X_Reply		0		/* Normal reply */
#define X_Error		-1		/* Error */

/* Request codes */

#define X_CreateWindow		1
#define X_CreateTransparency	2
#define X_DestroyWindow		3
#define X_DestroySubwindows	4
#define X_MapWindow		5
#define X_MapSubwindows		6
#define X_UnmapWindow		7
#define X_UnmapSubwindows	8
#define X_UnmapTransparent	9
#define X_RaiseWindow		10
#define X_LowerWindow		11
#define X_CircWindowUp		12
#define X_MoveWindow		13
#define X_ChangeWindow		14
#define X_ConfigureWindow	15
#define X_ChangeBackground	16
#define X_ChangeBorder		17
#define X_TileMode		18
#define X_ClipMode		19
#define X_QueryWindow		20
#define X_StoreName		21
#define X_FetchName		22
#define X_SetIconWindow		23
#define X_SetResizeHint		24
#define X_GetResizeHint		25
#define X_DefineCursor	 	26
#define X_SelectInput		27
#define X_GrabMouse		28
#define X_GrabButton		29
#define X_QueryMouse		30
#define X_InterpretLocator	31
#define X_WarpMouse		32
#define X_FocusKeyboard		33
#define X_CircWindowDown	34
#define X_QueryTree		35
#define X_Clear			40
#define X_PixFill		41
#define X_TileFill		42
#define X_PixmapPut		43
#define X_PixmapBitsPut		44
#define X_BitmapBitsPut		45
#define X_CopyArea		46
#define X_Text			47
#define X_TextMask		48
#define X_Line			49
#define X_Draw			50
#define X_DrawFilled		51
#define X_PixmapSave		52
#define X_PixmapGet		53
#define X_StippleFill		54
#define X_SetUp			80
#define X_UngrabMouse		81
#define X_UngrabButton		82
#define X_GetColor		83
#define X_GetColorCells		84
#define X_FreeColors		85
#define X_StoreColors		86
#define X_QueryColor		87
#define X_GetFont		88
#define X_FreeFont		89
#define X_QueryFont		90
#define X_CharWidths		91
#define X_StringWidth		92
#define X_FontWidths		93
#define X_StoreBitmap		94
#define X_FreeBitmap		95
#define X_CharBitmap		96
#define X_StorePixmap		97
#define X_FreePixmap		98
#define X_MakePixmap		99
#define X_QueryShape		100
#define X_StoreCursor		101
#define X_FreeCursor		102
#define X_MouseControl		103
#define X_FeepControl		104
#define X_Feep			105
#define X_ShiftLock		106
#define X_KeyClick		107
#define X_AutoRepeat		108
#define X_ScreenSaver		109
#define X_StoreBytes		110
#define X_FetchBytes		111
#define X_RotateCuts		112
#define X_AddHost		113
#define X_RemoveHost		114
#define X_GetHosts		115
#define X_GrabServer		116
#define X_UngrabServer		117
#define X_LookupColor		118
#define X_AppendBytes		119
