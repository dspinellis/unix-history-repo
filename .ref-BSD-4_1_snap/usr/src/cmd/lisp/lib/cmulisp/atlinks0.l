
(dv %AT-Cursor-AList
    ((SCROLLTRIANGLE . 1)
     (SCROLLDOWN . 2)
     (HOURGLASS . 3)
     (SCROLLUP . 4)
     (DOUBLEARROW . 5)
     (MARKCIRCLE . 6)
     (GRID . 7)
     (CARET . 8)
     (OSARROW . 9)
     (QUESTIONMARK . 10)
     (NULLCURSOR . 11)
     (HEAVYARROW . 12)
     (TRIDENT . 13)
     (BLOCK . 14)
     (POINT . 15)
     (HEAVYPOINT . 16)
     (UNDERBAR . 17)
     (MAXBICURSOR . 17)
     (DEFAULT . -1)))

(dv %AT-CursorIndex-AList
    ((1 . SCROLLTRIANGLE)
     (2 . SCROLLDOWN)
     (3 . HOURGLASS)
     (4 . SCROLLUP)
     (5 . DOUBLEARROW)
     (6 . MARKCIRCLE)
     (7 . GRID)
     (8 . CARET)
     (9 . OSARROW)
     (10 . QUESTIONMARK)
     (11 . NULLCURSOR)
     (12 . HEAVYARROW)
     (13 . TRIDENT)
     (14 . BLOCK)
     (15 . POINT)
     (16 . HEAVYPOINT)
     (17 . UNDERBAR)
     (17 . MAXBICURSOR)
     (-1 . DEFAULT)))

(dv %AT-Font-AList
    ((HELV10 . 1)
     (HELV10B . 2)
     (HELV18 . 3)
     (ASM6 . 4)
     (GACHA10 . 5)
     (MAXBIFONT . 5)
     (DEFAULT . -1)))

(def ATCopyRectangle
  (lambda (left right bottom top destleft destbottom inkpattern inkmode)
    (%ATCopyRectangle left
                      right
                      bottom
                      top
                      destleft
                      destbottom
                      (cdr (assq inkpattern %AT-Ink-AList))
                      (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATCreateWindow
  (lambda (leftx bottomy width height minwidth minheight headheight)
    (%ATCreateWindow leftx bottomy width height minwidth minheight headheight)))

(def ATGetBrush
  (lambda nil
    (cdr (assq (%ATGetBrush) %AT-CursorIndex-AList))))

(def ATGetCursor
  (lambda nil
    (cdr (assq (%ATGetCursor) %AT-CursorIndex-AList))))

(def ATGetEvent
  (lambda (event wait)
    (cond (wait (%ATGetEvent event 1)) ((%ATGetEvent event 0)))))

(def ATGetEventType
  (lambda (event)
    (cdr (assq (%ATGetEventType event) %AT-EventTypeIndex-AList))))

(def ATGetFont
  (lambda nil
    (cdr (assq (%ATGetFont) %AT-FontIndex-AList))))

(def ATGetMode
  (lambda nil
    (cdr (assq (%ATGetMode) %AT-InkModeIndex-AList))))

(def ATPutData
  (lambda (left right bottom top dataarray arraylen inkmode)
    (%ATPutData left
                right
                bottom
                top
                dataarray
                arraylen
                (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATPutPoint
  (lambda (x y brush inkpattern inkmode)
    (%ATPutPoint x
                 y
                 (cdr (assq brush %AT-Cursor-AList))
                 (cdr (assq inkpattern %AT-Ink-AList))
                 (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATPutRectangle
  (lambda (left right bottom top inkpattern inkmode)
    (%ATPutRectangle left
                     right
                     bottom
                     top
                     (cdr (assq inkpattern %AT-Ink-AList))
                     (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATSelCursor
  (lambda (cursor)
    (%ATSelCursor (cdr (assq cursor %AT-Cursor-AList)))))

(def ATSelectBrush
  (lambda (cursor)
    ((lambda (cursor-index) (%ATSelectBrush cursor-index))
     (cdr (assq cursor %AT-Cursor-AList)))))

(def ATSelectCaret
  (lambda (cursor millisec_rate)
    ((lambda (cursor_index) (%ATSelectCaret cursor_index millisec_rate))
     (cdr (assq cursor %AT-Cursor-AList)))))

(def ATSelectFont
  (lambda (font)
    ((lambda (fontindex) (%ATSelectFont fontindex))
     (cdr (assq font %AT-Font-AList)))))

(def ATSelectInk
  (lambda (ink)
    ((lambda (ink-index) (%ATSelectInk ink-index))
     (cdr (assq ink %AT-Ink-AList)))))

(def ATSetCurData
  (lambda (x y xoff yoff oldindex newindex)
    (%ATSetCurData x
                   y
                   xoff
                   yoff
                   (cdr (assq oldindex %AT-Cursor-AList))
                   (cdr (assq newindex %AT-Cursor-AList)))))

(def ATSetCurPos
  (lambda (x y bool)
    (cond (bool (%ATSetCurPos x y 1)) ((%ATSetCurPos x y 0)))))

(def ATSetInkData
  (lambda (x y oldindex)
    (cdr (assq (%ATSetInkData x y oldindex)))))

(def ATSetMode
  (lambda (inkmode)
    ((lambda (mode-index) (%ATSetMode mode-index))
     (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATSetScrolling
  (lambda (bool)
    (cond (bool (%ATSetScrolling 1)) ((%ATSetScrolling 0)))))

(def ATSetTracking
  (lambda (bool)
    (cond (bool (%ATSetTracking 1)) ((%ATSetTracking 0)))))

(def ATSetTrigger
  (lambda (type start key down enable)
    (%ATSetTrigger (cdr (assq type %AT-TriggerType-AList))
                   (cond (start 1) (0))
                   (cond ((cdr (assq key %AT-Key-AList))) (key))
                   (cond (down 1) (0))
                   (cond (enable 1) (0)))))

(def ATTransmitEnable
  (lambda (type enable)
    (%ATTransmitEnable (cdr (assq type %AT-EventType-AList))
                       (cond (enable 1) (0)))))

(def ATWindowHeader
  (lambda (bool)
    (%ATWindowHeader (cond (bool 1) (0)))))

(dv DispXMax 639)

(dv DispXMin 0)

(dv DispYMax 807)

(dv DispYMin 0)

(dv FrameYMax 1615)

(dv %AT-Ink-AList
    ((WHITE . 0)
     (BLACK . 1)
     (LIGHTGRAY . 2)
     (GRAY . 3)
     (DARKGRAY . 4)
     (CHECK2 . 5)
     (CHECK4 . 6)
     (VERT1 . 7)
     (VERT2 . 8)
     (VERT4 . 9)
     (HORIZ1 . 10)
     (HORIZ2 . 11)
     (LEFTDIAG2 . 12)
     (LEFTCHECK4 . 13)
     (RIGHTDIAG2 . 14)
     (RIGHTCHECK4 . 15)
     (MAXGRAY . 15)
     (HERRINGBONE . 16)
     (HORIZ4 . 17)
     (MAXBIINK . 17)))

(dv %AT-InkIndex-AList
    ((0 . WHITE)
     (1 . BLACK)
     (2 . LIGHTGRAY)
     (3 . GRAY)
     (4 . DARKGRAY)
     (5 . CHECK2)
     (6 . CHECK4)
     (7 . VERT1)
     (8 . VERT2)
     (9 . VERT4)
     (10 . HORIZ1)
     (11 . HORIZ2)
     (12 . LEFTDIAG2)
     (13 . LEFTCHECK4)
     (14 . RIGHTDIAG2)
     (15 . RIGHTCHECK4)
     (15 . MAXGRAY)
     (16 . HERRINGBONE)
     (17 . HORIZ4)
     (17 . MAXBIINK)))

(dv %AT-FontIndex-AList
    ((1 . HELV10)
     (2 . HELV10B)
     (3 . HELV18)
     (4 . ASM6)
     (5 . GACHA10)
     (5 . MAXBIFONT)
     (-1 . DEFAULT)))

(dv %AT-InkMode-AList
    ((REPLACE . 0) (PAINT . 1) (INVERT . 2) (ERASE . 3) (DEFAULT . -1)))

(def ATPutLine
  (lambda (endx endy brush inkpattern inkmode)
    (%ATPutLine endx
                endy
                (cdr (assq brush %AT-Cursor-AList))
                (cdr (assq inkpattern %AT-Ink-AList))
                (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATPutCurve
  (lambda (fdx fdy fddx fddy numpts inkpattern inkmode)
    (%ATPutCurve fdx
                 fdy
                 fddx
                 fddy
                 numpts
                 (cdr (assq inkpattern %AT-Ink-AList))
                 (cdr (assq inkmode %AT-InkMode-AList)))))

(def ATCopyMaskedRectangle
  (lambda (left right bottom top destleft destbottom inkpattern inkmode)
    (%ATCopyRectangle left
                      right
                      bottom
                      top
                      destleft
                      destbottom
                      (cdr (assq inkpattern %AT-Ink-AList))
                      (cdr (assq inkmode %AT-InkMode-AList)))))

(dv %AT-EventType-AList
    ((KEY . 1)
     (REPORT . 2)
     (BOUNDS . 3)
     (TIMEOUT . 4)
     (EVENTTIMEOUT . 5)
     (WINDOWBOUNDS . 6)
     (EXPOSED . 7)
     (INK . 8)
     (NULLS . 9)
     (CHARS . 10)
     (POSITION . 11)
     (DESELECT . 12)
     (PROBE . 13)))

(dv %AT-TriggerType-AList ((TIMER . 1) (INKING . 2) (REPORTING . 3)))

(dv %AT-Key-AList
    ((IMOUSELEFT . 77)
     (IMOUSEMIDDLE . 79)
     (IMOUSERIGHT . 78)
     (IBLANKBOTTOM . 61)
     (IBLANKMIDDLE . 30)
     (IBLANKTOP . 31)
     (ICONTROL . 36)
     (ISHIFTLEFT . 41)
     (ISHIFTRIGHT . 60)
     (ILOCK . 56)
     (ANYCHAR . 80)
     (ANYKEY . 81)
     (NOW . 82)))

(dv %AT-EventTypeIndex-AList
    ((1 . KEY)
     (2 . REPORT)
     (3 . BOUNDS)
     (4 . TIMEOUT)
     (5 . EVENTTIMEOUT)
     (6 . WINDOWBOUNDS)
     (7 . EXPOSED)
     (8 . INK)
     (9 . NULLS)
     (10 . CHARS)
     (11 . POSITION)
     (12 . DESELECT)
     (13 . PROBE)))
