(setsyntax "#" 2)
(setq playfns
      (quote
       (MyMenu MainProgram
               MakeWindow
               Get-A-String
               GetMouseKey
               YesNo
               Printable
               DoMB)))

(setq MyMenu
      (quote
       (|Window Hackery| (|Play with Window| . MakeWindow)
                         (|Draw Lines| . DrawLines)
                         (|Save Data| . SaveData)
                         (|| . $I$)
                         (Quit . $R$))))

(def MainProgram
     (lambda nil
      (setq Event (ATEventAlloc))
      (ATConnect 2)
      (ATCreateWindow -5 20 600 800 600 800 17)
      (ATTransmitEnable 'NULLS nil)
      (ATTransmitEnable 'INK nil)
      (ATTransmitEnable 'BOUNDS nil)
      (ATSetTrigger 'REPORTING nil 'NOW nil nil)
      (ATSetTrigger 'INKING nil 'NOW nil nil)
      (ATWindowHeader nil)
      (ATTextHome)
      (setq screen (ATGetWindow))
      (ATSelectCaret 'CARET 50)
      (ATSelectFont 'HELV10B)
      (makemenu 'MyMenu 5 300 400)
      (setq saved nil)
      (activate)
      (ATCloseConnection)
      (exit)))

(def MakeWindow
     (lambda nil
      (prog (oldwindow ans lrx ury)
            (setq oldwindow (ATGetWindow))
            (cond
             ((and (boundp 'playwindow) playwindow)
              (ATSetWindow playwindow)
              (ATDeleteWindow)
              (setq playwindow nil)
              (ATSetWindow oldwindow)
              (return)))
            (ATSetWindow screen)
       nexy (ATPutChar 12)
            (ATPutString
             (quote
              |Click Mouse Where you want Lower Left Hand Corner of Window|))
            (GetMouseKey Event)
            (setq x (ATGetEventX Event))
            (setq y (ATGetEventY Event))
            (ATPutPoint x y 'MARKCIRCLE 'BLACK 'REPLACE)
            (ATPutString (concat crlf 'OK?))
            (cond
             ((not (YesNo Event))
              (ATPutPoint x
                          y
                          'MARKCIRCLE
                          'WHITE
                          'REPLACE)
              (ATPutString (concat '|...try again| crlf))
              (go nexy)))
            (ATPutString (concat '|...Good| crlf))
       nelr (ATPutString '|Click Mouse for Lower Right Hand Corner|)
            (GetMouseKey Event)
            (setq lrx (ATGetEventX Event))
            (cond
             ((not (< x lrx))
              (ATPutString
               (concat '|...Too much to the left, try again| crlf))
              (go nelr)))
            (ATPutRectangle x lrx y y 'BLACK 'REPLACE)
            (ATPutString (concat crlf 'OK?))
            (cond
             ((not (YesNo Event))
              (ATPutRectangle x lrx y y 'WHITE 'REPLACE)
              (ATPutString (concat '|...try again| crlf))
              (go nelr)))
            (ATPutString (concat '|...Good| crlf))
       neur (ATPutString '|Click Mouse for Upper Right Hand Corner|)
            (GetMouseKey Event)
            (setq ury (ATGetEventY Event))
            (cond
             ((not (< y ury))
              (ATPutString (concat '|...Too low, try again| crlf))
              (go neur)))
            (ATPutRectangle lrx lrx y ury 'BLACK 'REPLACE)
            (ATPutString (concat crlf 'OK?))
            (cond
             ((not (YesNo Event))
              (ATPutRectangle lrx lrx y ury 'WHITE 'REPLACE)
              (ATPutString (concat '|...try again| crlf))
              (go neur)))
            (ATPutString (concat '|...Good| crlf))
            (ATPutRectangle x x y ury 'BLACK 'REPLACE)
            (ATPutRectangle x lrx ury ury 'BLACK 'REPLACE)
            (ATPutString '|Still OK?|)
            (cond
             ((not (YesNo Event))
              (ATPutRectangle x lrx y y 'WHITE 'REPLACE)
              (ATPutRectangle lrx lrx y ury 'WHITE 'REPLACE)
              (ATPutRectangle x x y ury 'WHITE 'REPLACE)
              (ATPutRectangle x lrx ury ury 'WHITE 'REPLACE)
              (ATPutPoint x
                          y
                          'MARKCIRCLE
                          'WHITE
                          'REPLACE)
              (ATPutString
               (concat '|...back to the old drawing board!| crlf))
              (go nexy)))
            (ATPutString (concat '|...Good| crlf))
            (setq length (- ury y))
            (setq width (- lrx x))
       newh (ATPutString '|Click Mouse Button for Header Height|)
            (GetMouseKey Event)
            (setq headerheight (- (ATGetEventY Event) ury))
            (cond
             ((not (plusp headerheight))
              (ATPutString (concat '|...too low, try again| crlf))
              (go newh)))
            (ATPutString (concat '|...Good| crlf))
            (ATPutRectangle lrx
                            lrx
                            ury
                            (+ ury headerheight)
                            'BLACK
                            'REPLACE)
            (ATPutRectangle x
                            lrx
                            (+ ury headerheight)
                            (+ ury headerheight)
                            'BLACK
                            'REPLACE)
            (ATPutRectangle x
                            x
                            ury
                            (+ ury headerheight)
                            'BLACK
                            'REPLACE)
            (ATPutString 'OK?)
            (cond
             ((not (YesNo Event))
              (ATPutRectangle lrx
                              lrx
                              ury
                              (+ ury headerheight)
                              'WHITE
                              'REPLACE)
              (ATPutRectangle x
                              lrx
                              (+ ury headerheight)
                              (+ ury headerheight)
                              'WHITE
                              'REPLACE)
              (ATPutRectangle x
                              x
                              ury
                              (+ ury headerheight)
                              'WHITE
                              'REPLACE)
              (ATPutString (concat '|...try again| crlf))
              (go newh)))
            (ATPutRectangle x lrx y y 'WHITE 'REPLACE)
            (ATPutRectangle lrx lrx y ury 'WHITE 'REPLACE)
            (ATPutRectangle x x y ury 'WHITE 'REPLACE)
            (ATPutRectangle x lrx ury ury 'WHITE 'REPLACE)
            (ATPutRectangle lrx
                            lrx
                            ury
                            (+ ury headerheight)
                            'WHITE
                            'REPLACE)
            (ATPutRectangle x
                            lrx
                            (+ ury headerheight)
                            (+ ury headerheight)
                            'WHITE
                            'REPLACE)
            (ATPutRectangle x
                            x
                            ury
                            (+ ury headerheight)
                            'WHITE
                            'REPLACE)
            (ATPutString (concat '|...Good| crlf))
            (ATPutPoint x y 'MARKCIRCLE 'WHITE 'REPLACE)
            (ATTextHome)
            (ATCreateWindow x y width length width length headerheight)
            (setq playwindow (ATGetWindow))
            (ATSetWindow oldwindow))))

(def Get-A-String
     (lambda nil
      (prog (temp evtype evcode lastchar xpos ypos charwidth fnth fntd event)
       more-chars(setq event (ATEventAlloc))
            (ATGetEvent event t)
            (setq evtype (ATGetEventType event))
            (cond ((not (equal evtype 'KEY)) (go more-chars)))
            (setq evcode (ATGetEventCode event))
            (cond
             ((equal 13 evcode)
              (ATEventFree event)
              (return (implode (nreverse temp)))))
            (cond
             ((or (equal evcode 8) (equal evcode 127))
              (cond ((null temp) (go more-chars)))
              (setq lastchar (car temp))
              (setq temp (cdr temp))
              (setq xpos
                    (car (ATGetTextPos))
                    ypos
                    (cdr (ATGetTextPos))
                    charwidth
                    (ATGetCharWidth lastchar))
              (ATSetTextPos (- xpos charwidth) ypos)
              (ATPutRectangle (- xpos charwidth)
                              xpos
                              (- ypos
                                 (or fntd (setq fntd (ATGetFntDescent))))
                              (+ ypos
                                 (or fnth (setq fnth (ATGetFntHeight))))
                              'WHITE
                              'REPLACE)))
            (cond ((not (Printable event)) (go more-chars)))
            (setq temp (cons evcode temp))
            (ATPutChar evcode)
            (go more-chars))))

(def GetMouseKey
     (lambda (event)
      (prog nil
       more (ATGetEvent Event t)
            (cond ((not (equal 'KEY (ATGetEventType event)))
                   (go more))
                  ((not (< 131 (ATGetEventCode event) 135)) (go more))))))

(def YesNo
     (lambda (event)
      (prog (ans1 ans2)
            (setq ans1 (Get-A-String))
            (setq ans2 (getchar ans1 1))
            (cond ((not
                    (or (equal ans2 'Y)
                        (equal ans2 'y)
                        (equal '|| ans1)))
                   (return nil))
                  (t (return t))))))

(def Printable (lambda (event) (< 31 (ATGetEventCode event) 127)))

(def DoMB
     (lambda (event minx defaultx maxx miny defaulty maxy)
      (prog (ansx ansy)
            (cond ((equal (ATGetEventMouseLeft event) 1)
                   (setq ansx defaultx ansy defaulty))
                  ((equal (ATGetEventMouseMiddle event) 1)
                   (setq ansx (ATGetEventX event) ansy (ATGetEventY event))
                   (cond
                    ((not (< (|1-| minx))
                          (t (go hairy)
                             nil
                             (return (cons ansx (list ansy)))
                             hairy
                             X
                             (ATPutString
                              (concat '|Enter an X Value: (|
                                      minx
                                      '| to |
                                      maxx
                                      '|) [|
                                      defaultx
                                      '|] ?|))
                             (setq ansx (Get-A-String))
                             (cond ((not (numberp ansx))
                                    (ATPutString
                                     (concat (quote
                                              |<-- is not a number, try again|)
                                             crlf))
                                    (go X))
                                   ((not (< (|1-| minx) ansx (|1+| maxx)))
                                    (ATPutString
                                     (concat (quote
                                              |<-- is not in range, try again|)
                                             crlf))
                                    (go X)))
                             (ATPutString crlf)
                             Y
                             (ATPutString
                              (concat '|Enter a Y Value: (|
                                      miny
                                      '| to |
                                      maxy
                                      '|) [|
                                      defaulty
                                      '|] ?|))
                             (setq ansy (Get-A-String))
                             (cond ((not (numberp ansy))
                                    (ATPutString
                                     (concat (quote
                                              |<-- is not a number, try again|)
                                             crlf))
                                    (go Y))
                                   ((not (< (|1-| miny) ansy (|1+| maxy)))
                                    (ATPutString
                                     (concat (quote
                                              |<-- is not in range, try again|)
                                             crlf))
                                    (go Y)))
                             (return (cons ansx (list ansy))))))))))))



(dv menufns
    (makemenu activate
              refresh
              killmenu
              massacremenus
              ATSoakUpEvents
              clear
              cls
              crlf))

(def makemenu
  (lambda (firstmenu length xpos ypos)
    (prog (oldwindow tall breadth header newwindow menu-item oldfont)
          (or (boundp '%MenuMap) (setq %MenuMap nil))
          (setq oldwindow (ATGetWindow))
          (setq oldfont (ATGetFont))
          (ATSelectFont 'ASM6)
          (setq tall (* (ATGetFntHeight) length))
          (or (boundp 'ASM6ht) (setq ASM6ht (ATGetFntHeight)))
          (setq breadth (+ (* (ATGetFntWidth) 20) 4))
          (or (boundp 'ASM6wd) (setq ASM6wd breadth))
          (ATSelectFont 'HELV10B)
          (setq header (+ (ATGetFntHeight) 4))
          (ATSelectFont oldfont)
          (ATCreateWindow xpos ypos breadth tall breadth tall header)
          (ATTransmitEnable 'NULLS nil)
          (ATTransmitEnable 'INK nil)
          (ATTransmitEnable 'BOUNDS nil)
          (ATSetTrigger 'REPORTING nil 'NOW nil nil)
          (ATSetTrigger 'INKING nil 'NOW nil nil)
          (setq newwindow (ATGetWindow))
          (setq %MenuMap
                (nconc %MenuMap (list (list newwindow firstmenu length))))
          (setq menu-item (eval firstmenu))
          (refresh t menu-item)
          (refresh nil menu-item)
          (ATSetWindow oldwindow)
          (return newwindow)
    )
  )
)

(def activate
  (lambda nil
    (prog (oldwindow hdr win# evtype menu# itemcnt menu y menuln line# item# thingtodo newmenu)
          (cond ((not (boundp '%MenuMap))
                      (princ '|Error no Menus exist!|)
                      (terpri)
                      (return)
                )
                ((null %MenuMap)
                       (princ '|Error no Menus exist!|)
                       (terpri)
                       (return)
                )
          )
          (setq event (ATEventAlloc))
          (ATSoakUpEvents event)
     loop (ATGetEvent event t)
          (setq evtype (ATGetEventType event))
          (setq win# (ATGetEventWindow event))
          (setq menu# (assoc win# %MenuMap))
          (cond ((not menu#)
                      (go loop)
                )
          )
          (setq menu (eval (cadr menu#)))
          (cond ((equal evtype 'EXPOSED)
                        (cond ((not (equal 0 (ATGetEventMoreToCome event)))
                                    (go loop)
                              )
                        )
                        (setq oldwindow (ATGetWindow))
                        (ATSetWindow win#)
                        (cond ((equal 0 (ATGetEventHeader event))
                                      (refresh nil menu)
                              )
                              (t  (refresh t menu)
                              )
                        )
                )
                ((not (equal evtype 'KEY))
                      (go loop)
                )
          )
          (cond ((> 126 (ATGetEventCode event))
                    (go loop)
                )
          )
          (setq y (ATGetEventY event))
          (setq menuln (|1-| (length menu)))
          (setq line# (/ y ASM6ht))
          (setq item# (- (caddr menu#) line#))
          (cond ((< item# 1)
                    (go loop)
                    (*** clicked in the header)
                )
          )
          (cond ((> item# menuln)
                    (go loop)
                    (*** below last line used)
                )
          )
          (setq oldwindow (ATGetWindow))
          (ATSetWindow win#)
          (setq thingtodo (cdr (nth item# menu)))
          (cond ((equal '$I$ thingtodo)
                        (go loop)
                )
                ((equal '$R$ thingtodo)
                        (return)
                )
                ((getd thingtodo)
                       (ATPutRectangle 0
                                       ASM6wd
                                       (* line# ASM6ht)
                                       (* (|1+| line#) ASM6ht)
                                       'BLACK
                                       'INVERT)
                       (apply (getd thingtodo) nil)
                       (ATPutRectangle 0
                                       ASM6wd
                                       (* line# ASM6ht)
                                       (* (|1+| line#) ASM6ht)
                                       'BLACK
                                       'INVERT)
                )
                ((boundp thingtodo)
                         (setq newmenu
                               (list (car menu#) thingtodo (caddr menu#)))
                         (setq %MenuMap (delete menu# %MenuMap))
                         (setq %MenuMap (nconc %MenuMap (list newmenu)))
                         (refresh t (eval thingtodo))
                         (refresh nil (eval thingtodo))
                )
          )
          (ATSetWindow oldwindow)
          (go loop)
    )
  )
)

(def refresh
  (lambda (header menu)
    (prog (len)
          (cond (header  (ATWindowHeader t)
                         (ATSelectFont 'HELV10B)
                         (ATTextHome)
                         (clear)
                         (ATCenterString (car menu) (/ ASM6wd 2))
                         (ATWindowHeader nil)
                )
                (t  (ATWindowHeader nil)
                    (setq len (length menu))
                    (ATSelectFont 'ASM6)
                    (ATTextHome)
                    (clear)
                    (cond ((> len 2)
                              (for x
                                   =
                                   1
                                   to
                                   (- len 2)
                                   (ATPutString
                                    (concat (car (nth x menu)) crlf)))
                          )
                    )
                    (ATPutString (car (nth (|1-| len) menu)))
                )
          )
    )
  )
)

(def killmenu
  (lambda (menunumber)
    (prog (menudescr)
          (or (boundp '%MenuMap) (return nil))
          (setq menudescr (assoc menunumber %MenuMap))
          (cond ((null menudescr)
                       (return nil)
                )
          )
          (setq %MenuMap (delete menudescr %MenuMap))
          (ATSetWindow menunumber)
          (ATDeleteWindow)
          (return t)
    )
  )
)

(def massacremenus
  (lambda nil
    (prog nil
          (or (boundp '%MenuMap) (return nil))
          (for-each x %MenuMap (ATSetWindow (car x)) (ATDeleteWindow))
          (return t)
    )
  )
)

(def ATSoakUpEvents
  (lambda (event)
    (prog (count success)
          (setq count 0)
     loop (setq success (ATGetEvent event nil))
          (cond ((equal 1 success)
                        (setq count (|1+| count))
                        (go loop)
                )
          )
          (return count)
    )
  )
)

(def clear
  (lambda nil
    (ATPutString cls)
  )
)

(dv cls ||)

(dv crlf |
|)
