; This is the set of low-level links to AT functions.
; (c) Copyright 1981 Lars Ericson 
; 	Linking functions to AT Package for LISP. 		

; ATConnect "i_connectno"
(cfasl 'atlinker.o '_LATConnect 'ATConnect "integer-function" "-lipc -lxat -lc -lcmu")

; ATCloseConnection 
(getaddress '_CloseConnection 'ATCloseConnection "integer-function")

; ATSetBounds
(getaddress '_LATSetBounds 'ATSetBounds "function")

; ATGetBounds
(getaddress '_LATGetBounds 'ATGetBounds "function")

; ATSetTextPos
(getaddress '_LATSetTextPos 'ATSetTextPos "function")

; ATGetTextPos 
(array ATGetTextPos-textx fixnum 1)
(array ATGetTextPos-texty fixnum 1)
(getaddress '_GetTextPos '%ATGetTextPos "integer-function")

; ATTextHome
(getaddress '_TextHome 'ATTextHome "integer-function")
; ATSetGraphicsPos
(getaddress '_LATSetGraphicsPos 'ATSetGraphicsPos "function")
; ATGetGraphicsPos
(array ATGetGraphicsPos-textx fixnum 1)
(array ATGetGraphicsPos-texty fixnum 1)
(getaddress '_GetGraphicsPos '%ATGetGraphicsPos "integer-function")

; %ATSelectFont
(getaddress '_LATSelectFont '%ATSelectFont "function")
; ATGetFont
(array ATGetFontResult fixnum 1)
(getaddress '_GetFont '%ATGetFont "integer-function")

; ATGetFontHeight
(array ATGetFntHeightResult fixnum 1)
(getaddress '_GetFntHeight '%ATGetFntHeight "integer-function")

; ATGetFontDescent
(array ATGetFntDescentResult fixnum 1)
(getaddress '_GetFntDescent '%ATGetFntDescent "integer-function")

; ATGetFntWidth
(array ATGetFntWidthResult fixnum 1)
(getaddress '_GetFntWidth '%ATGetFntWidth "integer-function")

; ATGetCharWidth
(array ATGetCharWidthResult fixnum 1)
(getaddress '_GetCharWidth '%ATGetCharWidth "integer-function")

; ATSelectCaret
(getaddress '_LATSelectCaret '%ATSelectCaret "function")
; ATSetLeftMargin
(getaddress '_LATSetLeftMargin '%ATSetLeftMargin "function")
; ATGetLeftMargin
(array ATGetLeftMarginResult fixnum 1)
(getaddress '_GetLeftMargin '%ATGetLeftMargin "integer-function")

; ATSetTabSpacing
(getaddress '_LATSetTabSpacing '%ATSetTabSpacing "function")
; ATGetTabSpacing
(getaddress '_LATGetTabSpacing '%ATGetTabSpacing "function")
; ATSetScrolling
(getaddress '_LATSetScrolling '%ATSetScrolling "function")
; %ATSelectBrush
(getaddress '_LATSelectBrush '%ATSelectBrush "function")
; %ATGetBrush
(array %ATGetBrushResult fixnum 1)
(getaddress '_GetBrush '%ATGetBrush "integer-function")

; %ATSelectInk
(getaddress '_LATSelectInk '%ATSelectInk "function")
; %ATGetInk
(array %ATGetInkResult fixnum 1)
(getaddress '_GetInk '%ATGetInk "integer-function")

; %ATSetInkData
(getaddress '_LATSetInkData '%ATSetInkData "function")
; %ATSetMode
(getaddress '_LATSetMode '%ATSetMode "function")
; %ATGetMode
(array %ATGetModeResult fixnum 1)
(getaddress '_GetMode '%ATGetMode "integer-function")

; ATPutChar
(getaddress '_LATPutChar 'ATPutChar "function")
; ATPutString
(getaddress '_LATPutString 'ATPutString "function")
; ATCenterString
(getaddress '_LATCenterString 'ATCenterString "function")
; ATRightJustify
(getaddress '_LATRightJustify 'ATRightJustify "function")
; %ATPutPoint
(getaddress '_LATPutPoint '%ATPutPoint "function")
; %ATPutLine
(getaddress '_LATPutLine '%ATPutLine "function")
; %ATPutCurve
(getaddress '_LATPutCurve '%ATPutCurve "function")
; %ATPutData
(getaddress '_LATPutData '%ATPutData "function")
; %ATPutRectangle
(getaddress '_LATPutRectangle '%ATPutRectangle "function")
; %ATCopyRectangle
(getaddress '_LATCopyRectangle '%ATCopyRectangle "function")
; %ATCopyMaskedRectangle -- NIY
; (getaddress '_LATCopyMaskedRectangle '%ATCopyMaskedRectangle "function")
; ATAllocateExtendedScreen
(getaddress '_LATAllocateExtendedScreen 'ATAllocateExtendedScreen "function")
; %ATSelCursor
(getaddress '_LATSelCursor '%ATSelCursor "function")
; %ATGetCursor
(array %ATGetCursorResult fixnum 1)
(getaddress '_GetCursor '%ATGetCursor "integer-function")

; %ATSetCurData
(getaddress '_LATSetCurData '%ATSetCurData "function")
; %ATSetCurPos
(getaddress '_LATSetCurPos '%ATSetCurPos "function")
; %ATGetCurPos
(array %ATGetCurPosResultX fixnum 1)
(array %ATGetCurPosResultY fixnum 1)
(getaddress '_GetCurPos '%ATGetCurPos "integer-function")

; %ATSetTracking
(getaddress '_LATSetTracking '%ATSetTracking "function")
; ATEventAlloc
(getaddress '_LATEventAlloc 'ATEventAlloc "function")
; ATEventFree
(getaddress '_LATEventFree 'ATEventFree "function")
; %ATGetEventType
(getaddress '_LATGetEventType '%ATGetEventType "function")
; ATGetEventWindow
(getaddress '_LATGetEventWindow 'ATGetEventWindow "function")
; ATGetEventTime -- Not Implemented Yet.
; (getaddress '_LATGetEventTime 'ATGetEventTime "function")
; ATGetEventX
(getaddress '_LATGetEventX 'ATGetEventX "function")
; ATGetEventY
(getaddress '_LATGetEventY 'ATGetEventY "function")
; ATGetEventBlankTop
(getaddress '_LATGetEventBlankTop 'ATGetEventBlankTop "function")
; ATGetEventBlankMiddle
(getaddress '_LATGetEventBlankMiddle 'ATGetEventBlankMiddle "function")
; ATGetEventBlankBottom
(getaddress '_LATGetEventBlankBottom 'ATGetEventBlankBottom "function")
; ATGetEventShiftRight
(getaddress '_LATGetEventShiftRight 'ATGetEventShiftRight "function")
; ATGetEventControl
(getaddress '_LATGetEventControl 'ATGetEventControl "function")
; ATGetEventShiftLeft
(getaddress '_LATGetEventShiftLeft 'ATGetEventShiftLeft "function")
; ATGetEventLock
(getaddress '_LATGetEventLock 'ATGetEventLock "function")
; ATGetEventMouseMiddle
(getaddress '_LATGetEventMouseMiddle 'ATGetEventMouseMiddle "function")
; ATGetEventMouseRight
(getaddress '_LATGetEventMouseRight 'ATGetEventMouseRight "function")
; ATGetEventMouseLeft
(getaddress '_LATGetEventMouseLeft 'ATGetEventMouseLeft "function")
; ATGetEventReported
(getaddress '_LATGetEventReported 'ATGetEventReported "function")
; ATGetEventInked
(getaddress '_LATGetEventInked 'ATGetEventInked "function")
; ATGetEventGoingDown
(getaddress '_LATGetEventGoingDown 'ATGetEventGoingDown "function")
; ATGetEventKeyIndex
(getaddress '_LATGetEventKeyIndex 'ATGetEventKeyIndex "function")
; ATGetEventCode
(getaddress '_LATGetEventCode 'ATGetEventCode "function")
; ATGetEventHeader
(getaddress '_LATGetEventHeader 'ATGetEventHeader "function")
; ATGetEventMoreToCome
(getaddress '_LATGetEventMoreToCome 'ATGetEventMoreToCome "function")
; ATGetEventMinX
(getaddress '_LATGetEventMinX 'ATGetEventMinX "function")
; ATGetEventMaxX
(getaddress '_LATGetEventMaxX 'ATGetEventMaxX "function")
; ATGetEventMinY
(getaddress '_LATGetEventMinY 'ATGetEventMinY "function")
; ATGetEventMaxY
(getaddress '_LATGetEventMaxY 'ATGetEventMaxY "function")
; ATGetEventMovement
(getaddress '_LATGetEventMovement 'ATGetEventMovement "function")
; ATGetEventLeftX
(getaddress '_LATGetEventLeftX 'ATGetEventLeftX "function")
; ATGetEventBottomY
(getaddress '_LATGetEventBottomY 'ATGetEventBottomY "function")
; ATGetEventWidth
(getaddress '_LATGetEventWidth 'ATGetEventWidth "function")
; ATGetEventHeight
(getaddress '_LATGetEventHeight 'ATGetEventHeight "function")
; %ATGetEvent
(getaddress '_LATGetEvent '%ATGetEvent "function")
; ATSetIntervalLength
(getaddress '_LATSetIntervalLength 'ATSetIntervalLength "function")
; ATStartTimer
(getaddress '_LATStartTimer 'ATStartTimer "function")
; %ATSetTrigger
(getaddress '_LATSetTrigger '%ATSetTrigger "function")
; ATSetEventTimer
(getaddress '_LATSetEventTimer 'ATSetEventTimer "function")
; ATNotifyBounds
(getaddress '_LATNotifyBounds 'ATNotifyBounds "function")
; %ATTransmitEnable
(getaddress '_LATTransmitEnable '%ATTransmitEnable "function")
; ATWindowProbe
(getaddress '_WindowProbe 'ATWindowProbe "integer-function")
; %ATCreateWindow
(getaddress '_LATCreateWindow '%ATCreateWindow "function")
; ATSetWindow
(getaddress '_LATSetWindow 'ATSetWindow "function")
; ATGetWindow
(array ATGetWindowResult fixnum 1)
(getaddress '_GetWindow '%ATGetWindow "integer-function")

; %ATWindowHeader
(getaddress '_LATWindowHeader '%ATWindowHeader "function")
; ATDeleteWindow
(getaddress '_DeleteWindow 'ATDeleteWindow "integer-function")
; ATCreateConcept
(getaddress '_LATCreateConcept 'ATCreateConcept "function")
; ATSetClearLine
(getaddress '_LATSetClearLine 'ATSetClearLine "function")
