/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: erro.h,v 2.2 84/07/11 15:14:16 guido Exp $ */

/*
 * B editor -- User (error) messages collected together.
 */

#ifndef _ERROR
#define _ERROR(name, message) extern char name[]
#endif

_ERROR(COPY_EMPTY, "Empty copy buffer");
_ERROR(DEL_REM, "The remains wouldn't fit");
_ERROR(EDIT_BAD, "Trouble reading your unit, see last line. Hit break if you don't want this");
_ERROR(EDIT_TABS, "Spaces and tabs mixed for indentation; check your program layout");
_ERROR(EXIT_HOLES, "There are still holes left.  Please fill or delete these first.");
_ERROR(GOTO_BAD, "Sorry -- bad reply from terminal for cursor sense");
_ERROR(GOTO_NO, "Sorry -- your terminal does not support the control-G command");
_ERROR(GOTO_OUT, "The cursor isn't pointing at a part of the buffer");
_ERROR(GOTO_REC, "You can't use control-G in recording mode (it wouldn't work in playback)");
_ERROR(GOTO_TAH, "Type-ahead lost");
_ERROR(GOTO_TO, "Cursor sense time-out");
_ERROR(INS_BAD, "Cannot insert '%c'");
_ERROR(PLB_NOK, "No keystrokes recorded");
_ERROR(REC_OK, "Keystrokes recorded, use control-P to play back");
_ERROR(REDO_OLD, "This redo brought you to an older version.  Type backspace to undo");
