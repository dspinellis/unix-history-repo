


EMACS(1)            UNIX Programmer's Manual             EMACS(1)



NNAAMMEE
     emacs - GNU project Emacs

SSYYNNOOPPSSIISS
     eemmaaccss [file ...]

DDEESSCCRRIIPPTTIIOONN
     _G_N_U _E_m_a_c_s is a new version of _E_m_a_c_s, written by the author
     of the original (PDP-10) _E_m_a_c_s, Richard Stallman. Its user
     functionality encompasses everything other _E_m_a_c_s editors do,
     and it is easily extensible since its editing commands are
     written in Lisp.

     _E_m_a_c_s has an extensive interactive help facility, but the
     facility assumes that you know how to manipulate _E_m_a_c_s win-
     dows and buffers.  CTRL-h (backspace or CTRL-h) enters the
     Help facility.  Help Tutorial (CTRL-h t) requests an
     interactive tutorial which can teach beginners the fundamen-
     tals of _E_m_a_c_s in a few minutes.  Help Apropos (CTRL-h a)
     helps you find a command given its functionality, Help Char-
     acter (CTRL-h c) describes a given character's effect, and
     Help Function (CTRL-h f) describes a given Lisp function
     specified by name.

     _E_m_a_c_s'_s Undo can undo several steps of modification to your
     buffers, so it is easy to recover from editing mistakes.

     _G_N_U _E_m_a_c_s'_s many special packages handle mail reading
     (RMail) and sending (Mail), outline editing (Outline), com-
     piling (Compile), running subshells within _E_m_a_c_s windows
     (Shell), running a Lisp read-eval-print loop (Lisp-
     Interaction-Mode), and automated psychotherapy (Doctor).

     There is an extensive reference manual, but users of other
     Emacses should have little trouble adapting even without a
     copy.  Users new to _E_m_a_c_s will be able to use basic features
     fairly rapidly by studying the tutorial and using the self-
     documentation features.

     Emacs Options

     The following options are of general interest:

     ffiillee    Edit _f_i_l_e.

     ++nnuummbbeerr Go to the line specified by _n_u_m_b_e_r (do not insert a
             space between the "+" sign and the number).

     --dd ddiissppllaayynnaammee
             Create the _E_m_a_c_s window on the display specified by
             _d_i_s_p_l_a_y_n_a_m_e. This must be the first argument listed
             in the command line.



Printed 6/29/90         1987 September 1                        1






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



     --qq      Do not load an init file.

     --uu uusseerr Load _u_s_e_r'_s init file.

     --tt ffiillee Use specified _f_i_l_e as the terminal instead of using
             stdin/stdout.  This must be the first argument
             specified in the command line.

     The following options are lisp-oriented (these options are
     processed in the order encountered):

     --ff ffuunnccttiioonn
             Execute the lisp function _f_u_n_c_t_i_o_n.

     --ll ffiillee Load the lisp code in the file _f_i_l_e.

     The following options are useful when running _E_m_a_c_s as a
     batch editor:

     --bbaattcchh ccoommmmaannddffiillee
             Edit in batch mode using the commands found in _c_o_m_-
             _m_a_n_d_f_i_l_e. The editor will send messages to stdout.
             This option must be the first in the argument list.

     --kkiillll   Exit _E_m_a_c_s while in batch mode.

     _U_s_i_n_g _E_m_a_c_s _w_i_t_h _X

     _E_m_a_c_s has been tailored to work well with the X window sys-
     tem.  To enable this feature, you must define the macro
     HHAAVVEE__XX__WWIINNDDOOWWSS in the file _s_r_c/_c_o_n_f_i_g._h before compiling
     _E_m_a_c_s. If you run _E_m_a_c_s from under X windows, it will create
     its own X window to display in.  You will probably want to
     start the editor as a background process so that you can
     continue using your original window.  To use the optional X
     Menu features, define also the macro HHAAVVEE__XX__MMEENNUU.. This macro
     is separate from HHAAVVEE__XX__WWIINNDDOOWWSS because the Menu facility of
     X does not work on all the systems that support X.  _E_m_a_c_s
     can be started with the following X switches:

     --rr      Display the _E_m_a_c_s window in inverse video.

     --ii      Use the "kitchen sink" bitmap icon when iconifying
             the _E_m_a_c_s window.

     --ffoonntt ffoonntt
             Set the _E_m_a_c_s window's font to that specified by
             _f_o_n_t. You will find the various _X fonts in the
             /_u_s_r/_n_e_w/_l_i_b/_X/_f_o_n_t directory.  Note that _E_m_a_c_s will
             only accept fixed width fonts.  These include the
             _6_x_1_0._o_n_x, _6_x_1_3._o_n_x, _6_x_1_3_p._o_n_x, _8_x_1_3._o_n_x, and
             _9_x_1_5._o_n_x fonts.  The other fixed width fonts are



Printed 6/29/90         1987 September 1                        2






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



             specified by the _f_x character sequence that comes
             before the ._o_n_x extension.

             When you specify a font, do not include the ._o_n_x
             extension.  Be sure to put a space between the -_f_o_n_t
             switch and the font specification argument.

     --bb ppiixxeellss
             Set the _E_m_a_c_s window's border width to the number of
             pixels specified by _p_i_x_e_l_s.

     --iibb ppiixxeellss
             Set the window's internal border width to the number
             of pixels specified by _p_i_x_e_l_s. Defaults to one pixel
             of padding on each side of the window.

     --ww ==[[WWIIDDTTHH]][[xxHHEEIIGGHHTT]][[{{++--}}XXOOFFFF[[{{++--}}YYOOFFFF]]]]
             Set the _E_m_a_c_s window's width, height, and position
             on the screen.  The []'s denote optional arguments,
             the {}'s surround alternatives.  WIDTH and HEIGHT
             are in number of characters, XOFF and YOFF are in
             pixels.  WIDTH defaults to 80, HEIGHT to 24, XOFF
             and YOFF to 1.  If you don't give XOFF and/or YOFF,
             then you must use the mouse to create the window.
             If you give XOFF and/or YOFF, then a WIDTHxHEIGHT
             window will automatically be creating without inter-
             vention.  XOFF and YOFF specify deltas from a corner
             of the screen to the corresponding corner of the
             window, as follows:

             +XOFF+YOFF     upper left to upper left
             -XOFF+YOFF     upper right to upper right
             +XOFF-YOFF     lower left to lower left
             -XOFF-YOFF     lower right to lower right

     --ffgg ccoolloorr
             On color displays, sets the color of the text.

     --bbgg ccoolloorr
             On color displays, sets the color of the window's
             background.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a
             list of valid _c_o_l_o_r names.

     --bbdd ccoolloorr
             On color displays, sets the color of the window's
             border.See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a list of
             valid _c_o_l_o_r names.

     --ccrr ccoolloorr
             On color displays, sets the color of the window's
             text cursor.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a
             list of valid _c_o_l_o_r names.



Printed 6/29/90         1987 September 1                        3






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



     --mmss ccoolloorr
             On color displays, sets the color of the window's
             mouse cursor.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a
             list of valid _c_o_l_o_r names.

     --dd ddiissppllaayynnaammee
             Create the _E_m_a_c_s window on the display specified by
             _d_i_s_p_l_a_y_n_a_m_e. Must be the first option specified in
             the command line.  --nnww Tells _E_m_a_c_s not to use its
             special interface to X.  If you use this switch when
             invoking _E_m_a_c_s from an _x_t_e_r_m window, display is done
             in the _x_t_e_r_m window.  This must be the first option
             specified in the command line.

     You can set _X default values for your _E_m_a_c_s windows in your
     ._X_d_e_f_a_u_l_t_s file.  Use the following format:

          emacs.keyword:value

     where _v_a_l_u_e specifies the default value of _k_e_y_w_o_r_d. _E_m_a_c_s
     lets you set default values for the following keywords:

     BBooddyyFFoonntt
             Sets the window's text font.

     RReevveerrsseeVViiddeeoo
             If _R_e_v_e_r_s_e_V_i_d_e_o'_s value is set to _o_n, the window
             will be displayed in inverse video.

     BBiittMMaappIIccoonn
             If _B_i_t_M_a_p_I_c_o_n'_s value is set to _o_n, the window will
             iconify into the "kitchen sink."

     BBoorrddeerrWWiiddtthh
             Sets the window's border width in pixels.

     FFoorreeggrroouunndd
             For color displays, sets the window's text color.
             See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a list of valid
             _c_o_l_o_r names.

     BBaacckkggrroouunndd
             For color displays, sets the window's background
             color.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a list of
             valid _c_o_l_o_r names.

     BBoorrddeerr  For color displays, sets the color of the window's
             border.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a list of
             valid _c_o_l_o_r names.

     CCuurrssoorr  For color displays, sets the color of the window's
             text cursor.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a



Printed 6/29/90         1987 September 1                        4






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



             list of valid _c_o_l_o_r names.

     MMoouussee   For color displays, sets the color of the window's
             mouse cursor.  See the file /_u_s_r/_l_i_b/_r_g_b._t_x_t for a
             list of valid _c_o_l_o_r names.

     If you try to set color values while using a black and white
     display, the window's characteristics will default as fol-
     lows: the foreground color will be set to black, the back-
     ground color will be set to white, the border color will be
     set to grey, and the text and mouse cursors will be set to
     black.


     _U_s_i_n_g _t_h_e _M_o_u_s_e

     The following lists the key bindings for the mouse cursor
     when used in an _E_m_a_c_s window.


     MOUSE BUTTON             FUNCTION
     left                     set mark
     middle                   set cursor
     right                    select (Emacs) window
     SHIFT-middle             put text into X cut buffer (cut text)
     SHIFT-right              paste text
     CTRL-middle              cut text and kill it
     CTRL-right               select this window, then split it into
                              two windows
     CTRL-SHIFT-left          X buffer menu--hold the buttons and keys
                              down, wait for menu to appear, select
                              buffer, and release.  Move mouse out of
                              menu and release to cancel.
     CTRL-SHIFT-middle        X help menu--pop up index card menu for
                              Emacs help.
     CTRL-SHIFT-right         Select window with mouse, and delete all
                              other windows.  Same as typing
                              CTRL-x 1.


MMAANNUUAALLSS
     You can order printed copies of the  GNU  Emacs  Manual  for
     $15.00/copy  postpaid  from  the  Free  Software Foundation,
     which develops  GNU  software  (contact  them  for  quantity
     prices on the manual).  Their address is:
         Free Software Foundation
         675 Mass Ave.
         Cambridge, MA 02139
     Your local Emacs maintainer might also  have  copies  avail-
     able.   As  with  all  software  and  publications from FSF,
     everyone is permitted to make and distribute copies  of  the
     Emacs manual.  The TeX source to the manual is also included



Printed 6/29/90         1987 September 1                        5






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



     in the Emacs source distribution.

FFIILLEESS
     /usr/local/emacs/src - C source files and object files

     /usr/local/emacs/lisp - Lisp source files and compiled files
     that define most editing commands.  Some are preloaded; oth-
     ers are autoloaded from this directory when used.

     /usr/local/emacs/man  -  sources  for  the  Emacs  reference
     manual.

     /usr/local/emacs/etc - various programs that are  used  with
     GNU Emacs, and some files of information.

     /usr/local/emacs/etc/DOC.*  -  contains  the   documentation
     strings for the Lisp primitives and preloaded Lisp functions
     of GNU Emacs.  They are stored here to reduce  the  size  of
     Emacs proper.

     /usr/local/emacs/etc/DIFF discusses  GNU  Emacs  vs.  Twenex
     Emacs;
     /usr/local/emacs/etc/CCADIFF discusses  GNU  Emacs  vs.  CCA
     Emacs;
     /usr/local/emacs/etc/GOSDIFF discusses GNU Emacs vs. Gosling
     Emacs.
     /usr/local/emacs/etc/SERVICE lists people  offering  various
     services  to assist users of GNU Emacs, including education,
     troubleshooting, porting and customization.
     These files also have information useful to  anyone  wishing
     to  write  programs  in  the  Emacs Lisp extension language,
     which has not yet been fully documented.

     /usr/local/emacs/info - files  for  the  Info  documentation
     browser  (a  subsystem of Emacs) to refer to.  Currently not
     much of Unix is documented here, but the  complete  text  of
     the  Emacs reference manual is included in a convenient tree
     structured form.

     /usr/local/emacs/lock - holds lock files that are  made  for
     all  files  being modified in Emacs, to prevent simultaneous
     modification of one file by two users.

     /usr/local/emacs/cpp - the  GNU  cpp,  needed  for  building
     Emacs  on  certain  versions  of Unix where the standard cpp
     cannot handle long names for macros.

     /usr/local/emacs/shortnames  -  facilities  for  translating
     long  names  to  short  names in C code, needed for building
     Emacs on certain versions of Unix where the C compiler  can-
     not handle long names for functions or variables.




Printed 6/29/90         1987 September 1                        6






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



BBUUGGSS
     There is a mailing  list,  bug-gnu-emacs@prep.ai.mit.edu  on
     the    internet   (ucbvax!prep.ai.mit.edu!bug-gnu-emacs   on
     UUCPnet), for reporting Emacs bugs and  fixes.   But  before
     reporting  something as a bug, please try to be sure that it
     really is a bug, not  a  misunderstanding  or  a  deliberate
     feature.   We  ask you to read the section ``Reporting Emacs
     Bugs'' near the end of the reference manual (or Info system)
     for hints on how and when to report bugs.  Also, include the
     version number of the Emacs you are  running  in  _e_v_e_r_y  bug
     report that you send in.

     Do not expect a personal answer to a bug report.   The  pur-
     pose  of reporting bugs is to get them fixed for everyone in
     the next release, if  possible.   For  personal  assistance,
     look  in  the  SERVICE file (see above) for a list of people
     who offer it.

     Please do not send anything but bug reports to this  mailing
     list.   Send  requests  to  be added to mailing lists to the
     special list info-gnu-emacs-request@prep.ai.mit.edu (or  the
     corresponding  UUCP  address).   For  more information about
     Emacs      mailing      lists,      see       the       file
     /usr/local/emacs/etc/MAILINGLISTS.  Bugs tend actually to be
     fixed if they can be isolated, so it is in your interest  to
     report  them  in  such  a way that they can be easily repro-
     duced.

     Bugs that I know about are: shell will not  work  with  pro-
     grams running in Raw mode on some Unix versions.

UUNNRREESSTTRRIICCTTIIOONNSS
     _E_m_a_c_s is free; anyone may redistribute copies  of  _E_m_a_c_s  to
     anyone  under  the  terms stated in the _E_m_a_c_s General Public
     License, a copy of which accompanies each copy of _E_m_a_c_s  and
     which also appears in the reference manual.

     Copies of _E_m_a_c_s may sometimes be received packaged with dis-
     tributions  of Unix systems, but it is never included in the
     scope of any license covering those systems.  Such inclusion
     violates  the  terms on which distribution is permitted.  In
     fact, the primary purpose of the General Public  License  is
     to  prohibit anyone from attaching any other restrictions to
     redistribution of _E_m_a_c_s.

     Richard Stallman encourages you to improve and extend _E_m_a_c_s,
     and  urges  that  you  contribute your extensions to the GNU
     library.  Eventually GNU (Gnu's Not Unix) will be a complete
     replacement for Berkeley Unix.  Everyone will be able to use
     the GNU system for free.





Printed 6/29/90         1987 September 1                        7






EMACS(1)            UNIX Programmer's Manual             EMACS(1)



AAUUTTHHOORRSS
     _E_m_a_c_s was written by Richard Stallman and the Free  Software
     Foundation.  Joachim Martillo and Robert Krawitz added the X
     features.



















































Printed 6/29/90         1987 September 1                        8



