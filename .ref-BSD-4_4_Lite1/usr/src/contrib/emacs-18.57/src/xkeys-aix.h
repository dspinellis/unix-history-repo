/*  This file is included by x11term.c.  It provides a workable keymapping */
/*  for X windows under AIX.  						   */


/*-------------------------------------------------------------------------*/
/*---------------------------- normal -------------------------------------*/

#if 0
 XRebindKeysym(XXdisplay, XK_BackSpace, 0, 0, "\177", 1);
#endif

 XRebindKeysym(XXdisplay, XK_F1, 0, 0, "\033[001q", 6);
 XRebindKeysym(XXdisplay, XK_F2, 0, 0, "\033[002q", 6);
 XRebindKeysym(XXdisplay, XK_F3, 0, 0, "\033[003q", 6);
 XRebindKeysym(XXdisplay, XK_F4, 0, 0, "\033[004q", 6);
 XRebindKeysym(XXdisplay, XK_F5, 0, 0, "\033[005q", 6);
 XRebindKeysym(XXdisplay, XK_F6, 0, 0, "\033[006q", 6);
 XRebindKeysym(XXdisplay, XK_F7, 0, 0, "\033[007q", 6);
 XRebindKeysym(XXdisplay, XK_F8, 0, 0, "\033[008q", 6);
 XRebindKeysym(XXdisplay, XK_F9, 0, 0, "\033[009q", 6);
 XRebindKeysym(XXdisplay, XK_F10, 0, 0, "\033[010q", 6);
 XRebindKeysym(XXdisplay, XK_F11, 0, 0, "\033[011q", 6);
 XRebindKeysym(XXdisplay, XK_F12, 0, 0, "\033[012q", 6);

 XRebindKeysym(XXdisplay, XK_Print, 0, 0, "\033[209q", 6);
 XRebindKeysym(XXdisplay, XK_Cancel, 0, 0, "\033[213q", 6);
 XRebindKeysym(XXdisplay, XK_Pause, 0, 0, "\033[217q", 6);

 XRebindKeysym(XXdisplay, XK_Insert, 0, 0, "\033[139q", 6);
 XRebindKeysym(XXdisplay, XK_Home, 0, 0, "\033[H", 3);
 XRebindKeysym(XXdisplay, XK_Prior, 0, 0, "\033[150q", 6);

#if 0
 XRebindKeysym(XXdisplay, XK_Delete, 0, 0, "\033[P", 3);
#endif

 XRebindKeysym(XXdisplay, XK_End, 0, 0, "\033[146q", 6);
 XRebindKeysym(XXdisplay, XK_Next, 0, 0, "\033[154q", 6);

 XRebindKeysym(XXdisplay, XK_Up, 0, 0, "\033[A", 3);
 XRebindKeysym(XXdisplay, XK_Left, 0, 0, "\033[D", 3);
 XRebindKeysym(XXdisplay, XK_Down, 0, 0, "\033[B", 3);
 XRebindKeysym(XXdisplay, XK_Right, 0, 0, "\033[C", 3);

 XRebindKeysym(XXdisplay, XK_Execute, 0, 0, "\033[114q", 6);

/*-------------------------------------------------------------------------*/
/*---------------------------- shift --------------------------------------*/
#if 0
 XRebindKeysym(XXdisplay, XK_BackSpace, XMOD_Shift, 1, "\177", 1);
#endif

 XRebindKeysym(XXdisplay, XK_F1, XMOD_Shift, 1, "\033[013q", 6);
 XRebindKeysym(XXdisplay, XK_F2, XMOD_Shift, 1, "\033[014q", 6);
 XRebindKeysym(XXdisplay, XK_F3, XMOD_Shift, 1, "\033[015q", 6);
 XRebindKeysym(XXdisplay, XK_F4, XMOD_Shift, 1, "\033[016q", 6);
 XRebindKeysym(XXdisplay, XK_F5, XMOD_Shift, 1, "\033[017q", 6);
 XRebindKeysym(XXdisplay, XK_F6, XMOD_Shift, 1, "\033[018q", 6);
 XRebindKeysym(XXdisplay, XK_F7, XMOD_Shift, 1, "\033[019q", 6);
 XRebindKeysym(XXdisplay, XK_F8, XMOD_Shift, 1, "\033[020q", 6);
 XRebindKeysym(XXdisplay, XK_F9, XMOD_Shift, 1, "\033[021q", 6);
 XRebindKeysym(XXdisplay, XK_F10, XMOD_Shift, 1, "\033[022q", 6);
 XRebindKeysym(XXdisplay, XK_F11, XMOD_Shift, 1, "\033[023q", 6);
 XRebindKeysym(XXdisplay, XK_F12, XMOD_Shift, 1, "\033[024q", 6);

 XRebindKeysym(XXdisplay, XK_Print, XMOD_Shift, 1, "\033[210q", 6);
 XRebindKeysym(XXdisplay, XK_Cancel, XMOD_Shift, 1, "\033[214q", 6);
 XRebindKeysym(XXdisplay, XK_Pause, XMOD_Shift, 1, "\033[218q", 6);

 XRebindKeysym(XXdisplay, XK_Home, XMOD_Shift, 1, "\033[143q", 6);
 XRebindKeysym(XXdisplay, XK_Prior, XMOD_Shift, 1, "\033[151q", 6);
 XRebindKeysym(XXdisplay, XK_End, XMOD_Shift, 1, "\033[147q", 6);
 XRebindKeysym(XXdisplay, XK_Next, XMOD_Shift, 1, "\033[155q", 6);

 XRebindKeysym(XXdisplay, XK_Up, XMOD_Shift, 1, "\033[161q", 6);
 XRebindKeysym(XXdisplay, XK_Left, XMOD_Shift, 1, "\033[158q", 6);
 XRebindKeysym(XXdisplay, XK_Down, XMOD_Shift, 1, "\033[164q", 6);
 XRebindKeysym(XXdisplay, XK_Right, XMOD_Shift, 1, "\033[167q", 6);

/*-------------------------------------------------------------------------*/
/*---------------------------- control ------------------------------------*/
#if 0
 XRebindKeysym(XXdisplay, XK_BackSpace, XMOD_Ctrl, 1, "\177", 1);
#endif

 XRebindKeysym(XXdisplay, XK_minus, XMOD_Ctrl, 1, "\037", 1);
 XRebindKeysym(XXdisplay, XK_2, XMOD_Ctrl, 1, "\000", 1);
 XRebindKeysym(XXdisplay, XK_space, XMOD_Ctrl, 1, "\000", 1);
 
 XRebindKeysym(XXdisplay, XK_F1, XMOD_Ctrl, 1, "\033[025q", 6);
 XRebindKeysym(XXdisplay, XK_F2, XMOD_Ctrl, 1, "\033[026q", 6);
 XRebindKeysym(XXdisplay, XK_F3, XMOD_Ctrl, 1, "\033[027q", 6);
 XRebindKeysym(XXdisplay, XK_F4, XMOD_Ctrl, 1, "\033[028q", 6);
 XRebindKeysym(XXdisplay, XK_F5, XMOD_Ctrl, 1, "\033[029q", 6);
 XRebindKeysym(XXdisplay, XK_F6, XMOD_Ctrl, 1, "\033[030q", 6);
 XRebindKeysym(XXdisplay, XK_F7, XMOD_Ctrl, 1, "\033[031q", 6);
 XRebindKeysym(XXdisplay, XK_F8, XMOD_Ctrl, 1, "\033[032q", 6);
 XRebindKeysym(XXdisplay, XK_F9, XMOD_Ctrl, 1, "\033[033q", 6);
 XRebindKeysym(XXdisplay, XK_F10, XMOD_Ctrl, 1, "\033[034q", 6);
 XRebindKeysym(XXdisplay, XK_F11, XMOD_Ctrl, 1, "\033[035q", 6);
 XRebindKeysym(XXdisplay, XK_F12, XMOD_Ctrl, 1, "\033[036q", 6);

 XRebindKeysym(XXdisplay, XK_Print, XMOD_Ctrl, 1, "\033[211q", 6);
 XRebindKeysym(XXdisplay, XK_Cancel, XMOD_Ctrl, 1, "\033[215q", 6);

 XRebindKeysym(XXdisplay, XK_Insert, XMOD_Ctrl, 1, "\033[140q", 6);
 XRebindKeysym(XXdisplay, XK_Home, XMOD_Ctrl, 1, "\033[144q", 6);
 XRebindKeysym(XXdisplay, XK_Prior, XMOD_Ctrl, 1, "\033[152q", 6);

#if 0
 XRebindKeysym(XXdisplay, XK_Delete, XMOD_Ctrl, 1, "\033[142q", 6);
#endif

 XRebindKeysym(XXdisplay, XK_End, XMOD_Ctrl, 1, "\033[148q", 6);
 XRebindKeysym(XXdisplay, XK_Next, XMOD_Ctrl, 1, "\033[156q", 6);

 XRebindKeysym(XXdisplay, XK_Up, XMOD_Ctrl, 1, "\033[162q", 6);
 XRebindKeysym(XXdisplay, XK_Left, XMOD_Ctrl, 1, "\033[159q", 6);
 XRebindKeysym(XXdisplay, XK_Down, XMOD_Ctrl, 1, "\033[165q", 6);
 XRebindKeysym(XXdisplay, XK_Right, XMOD_Ctrl, 1, "\033[168q", 6);

/*-------------------------------------------------------------------------*/
/*---------------------------- alternate ----------------------------------*/
#if 0
 XRebindKeysym(XXdisplay, XK_BackSpace, XMOD_Alt, 1, "\177", 1);
#endif

 XRebindKeysym(XXdisplay, XK_F1, XMOD_Alt, 1, "\033[037q", 6);
 XRebindKeysym(XXdisplay, XK_F2, XMOD_Alt, 1, "\033[038q", 6);
 XRebindKeysym(XXdisplay, XK_F3, XMOD_Alt, 1, "\033[039q", 6);
 XRebindKeysym(XXdisplay, XK_F4, XMOD_Alt, 1, "\033[040q", 6);
 XRebindKeysym(XXdisplay, XK_F5, XMOD_Alt, 1, "\033[041q", 6);
 XRebindKeysym(XXdisplay, XK_F6, XMOD_Alt, 1, "\033[042q", 6);
 XRebindKeysym(XXdisplay, XK_F7, XMOD_Alt, 1, "\033[043q", 6);
 XRebindKeysym(XXdisplay, XK_F8, XMOD_Alt, 1, "\033[044q", 6);
 XRebindKeysym(XXdisplay, XK_F9, XMOD_Alt, 1, "\033[045q", 6);
 XRebindKeysym(XXdisplay, XK_F10, XMOD_Alt, 1, "\033[046q", 6);
 XRebindKeysym(XXdisplay, XK_F11, XMOD_Alt, 1, "\033[047q", 6);
 XRebindKeysym(XXdisplay, XK_F12, XMOD_Alt, 1, "\033[048q", 6);

 XRebindKeysym(XXdisplay, XK_Print, XMOD_Alt, 1, "\033[212q", 6);
 XRebindKeysym(XXdisplay, XK_Cancel, XMOD_Alt, 1, "\033[216q", 6);

 XRebindKeysym(XXdisplay, XK_Insert, XMOD_Alt, 1, "\033[141q", 6);
 XRebindKeysym(XXdisplay, XK_Home, XMOD_Alt, 1, "\033[145q", 6);
 XRebindKeysym(XXdisplay, XK_Prior, XMOD_Alt, 1, "\033[153q", 6);

#if 0
 XRebindKeysym(XXdisplay, XK_Delete, XMOD_Alt, 1, "\033[M", 3);
#endif

 XRebindKeysym(XXdisplay, XK_End, XMOD_Alt, 1, "\033[149q", 6);
 XRebindKeysym(XXdisplay, XK_Next, XMOD_Alt, 1, "\033[157q", 6);

 XRebindKeysym(XXdisplay, XK_Up, XMOD_Alt, 1, "\033[163q", 6);
 XRebindKeysym(XXdisplay, XK_Left, XMOD_Alt, 1, "\033[160q", 6);
 XRebindKeysym(XXdisplay, XK_Down, XMOD_Alt, 1, "\033[166q", 6);
 XRebindKeysym(XXdisplay, XK_Right, XMOD_Alt, 1, "\033[169q", 6);
