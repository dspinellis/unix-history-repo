/*
 * This is a header file describing the interface via int 10H to the
 * video subsystem.
 */

#define	BIOS_VIDEO	0x10

typedef enum {
    SetMode = 0,
    SetCursorType,
    SetCursorPosition,
    ReadCursorPosition,
    ReadLightPenPosition,
    SelectActiveDisplayPage,
    ScrollActivePageUp,
    ScrollActivePageDown,
    ReadAttribute_Character,
    WriteAttribute_Character,
    WriteCharacterOnly,
    SetColorPalette,
    WriteDot,
    ReadDot,
    WriteTeletypeToActivePage,
    CurrentVideoState,
    Reserved16,
    Reserved17,
    Reserved18,
    WriteString
} VideoOperationsType;

typedef enum {
    bw_40x25 = 0,
    color_40x25,
    bw_80x25,
    color_80x25,
    color_320x200,
    bw_320x200,
    bw_640x200,
    internal_bw_80x25
} VideoModeType;
