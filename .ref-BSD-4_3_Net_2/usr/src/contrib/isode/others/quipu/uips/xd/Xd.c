/* $Header: /f/osi/others/quipu/uips/xd/RCS/Xd.c,v 7.2 91/02/22 09:32:39 mrose Interim $ */
#ifndef lint
	static char *rcsid = "$Id: Xd.c,v 7.2 91/02/22 09:32:39 mrose Interim $";
#endif
/*
 $Log:	Xd.c,v $
 * Revision 7.2  91/02/22  09:32:39  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/17  11:50:17  mrose
 * sync
 * 
 * Revision 7.0  90/06/12  13:10:46  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:32  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:02  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:00  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/09  15:57:25  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:43  emsrssn
 * Initial revision
 * 
 * 
*/

#include <stdio.h>
#include "quipu/util.h"
#include "sequence.h"
#include "dirtitle.h"

#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Clock.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Logo.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Text.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>

#include <X11/Xaw/Cardinals.h>
#include <X11/cursorfont.h>

#define ASIZELIMIT 50   /* used for result list size and directory limits */

#define MAXLOOKBACKLENGTH 10
#define MAXLENGTH 255
#define MAXRESULTLISTLENGTH ASIZELIMIT
#define NOHISTORY "You've not been anywhere yet"

extern char base_path[];
extern char friendly_base_path[];
extern char goto_path[];
extern char mvalue[];
extern int filt_num;
extern unsigned int typeindx;
extern char *filtvalue[];
extern char *filttype[];
extern char dua_help_dir[];
extern int *av_typeindx;
extern D_seq showseq, dnseq;
extern int entry_number;

int element_number = 0;
int asizelimit = ASIZELIMIT;

void add_to_lookback(), xprint(), CreatePopupHelp(), Change_Help();

static void Call_Change_Help();
static void Quit(), create_genform(), create_inputform();
static void create_viewlist(), create_commandform(), create_outputform();
static void PopupList(), PopupTypelist(), Quit_Popup();
static void PopupHelp(), PopdownHelp(), Change_Type(), Change_Search_Area();
static void Search(), List(), widen_search();

static Boolean lookback_open = False; /* is there a look back popup? */
static Boolean help_popped_up= False; /* is the help window up? */

String result_list[MAXRESULTLISTLENGTH];
String lookback_list[MAXLOOKBACKLENGTH];
Widget toplevel;
XtAppContext app_con;

static XtActionsRec buttonActionsTable[] = {
  {"Call_Change_Help", (XtActionProc) Call_Change_Help},
};





void
init_widgets ()
{
  Arg args[1];
  Widget outer;
  int count;

  outer = XtCreateManagedWidget("outer", panedWidgetClass, toplevel,
                                  args, 0);

  XtAppAddActions(app_con, buttonActionsTable, XtNumber(buttonActionsTable));

  create_genform(outer);
  create_inputform(outer);
  create_outputform(outer);
  create_commandform(outer);
  create_viewlist(outer);

  for(count=0; count < MAXRESULTLISTLENGTH; count++) {
    result_list[count] = XtMalloc(MAXLENGTH);
  }

  for(count=0; count < MAXLOOKBACKLENGTH; count++) {
    lookback_list[count] = XtMalloc(MAXLENGTH);
  }
  /* add text to stop widget assuming daft size */
  (void) strcpy(lookback_list[0], NOHISTORY );  
    
}

free_memory()
{
  int count;

  for(count=0; count < MAXRESULTLISTLENGTH; count++) {
    XtFree(result_list[count]);
  }

  for(count=0; count < MAXLOOKBACKLENGTH; count++) {
    XtFree(lookback_list[count]);
  }
}


void
Loop()
{
  Arg args[5];
  Pixmap title_pix;
  String title_name = "outer.genform.title";
  Widget title_widg = XtNameToWidget(toplevel,title_name);

  XtRealizeWidget(toplevel);
  XFlush(XtDisplay(toplevel));    /* this ensures that all requests are sent */
                                  /* without this line, linking with cc means */
				  /* that the requests are not all sent */

  title_pix =  XCreateBitmapFromData(XtDisplay(title_widg),
                 XtWindow(title_widg), stef_bits, stef_width,
                 stef_height);
  XtSetArg(args[0], XtNbitmap, title_pix);
  XtSetValues(title_widg, args, 1);

  XtAppMainLoop(app_con);
}


/*ARGSUSED*/
static void
Call_Change_Help(w, event, params, num_params)
Widget w;
XEvent *event;
String *params;
int num_params;
{
  Change_Help(params[0]);
}


static void
create_viewlist(parent)
Widget parent;
{
  Widget view_list, list_area;
  Arg args[10];

  view_list = XtCreateManagedWidget("view_list", viewportWidgetClass, parent,
                                  args, 0);

  XtSetArg(args[0], XtNlist, result_list);
  XtSetArg(args[1], XtNdefaultColumns, (int) 1);
  XtSetArg(args[2], XtNverticalList, True );
  list_area = XtCreateManagedWidget("list_area", listWidgetClass,
                                         view_list, args, 3);
  XtAddCallback(list_area, XtNcallback, Change_Search_Area, (XtPointer) 0 );

}



static void
create_outputform(parent)
Widget parent;
{
  Arg args[10];
  static XawTextSelectType sarray[] = {
    XawselectLine,
    XawselectPosition,
    XawselectAll,
    XawselectNull
  };

  XtSetArg(args[0], XtNselectTypes, sarray);
  (void) XtCreateManagedWidget("result_area", asciiTextWidgetClass,
                                parent, args, 1);

}



static void
create_commandform(parent)
Widget parent;
{
  Arg args[1];      /* used to pass zero args...to be consistent for lint */
  Widget form, widen_but;
  Widget search_but, list_but, lookback_but;


  form = XtCreateManagedWidget("commandform", formWidgetClass, parent,
                               args, 0 );

  search_but = XtCreateManagedWidget("search_but", commandWidgetClass,
                               form, args, 0 );
  XtAddCallback(search_but, XtNcallback, Search, (XtPointer)0 );

 
  list_but = XtCreateManagedWidget("list_but", commandWidgetClass,
                               form, args, 0 );
  XtAddCallback(list_but,  XtNcallback, List, (XtPointer)0 );


  lookback_but = XtCreateManagedWidget("lookback_but", commandWidgetClass,
                               form, args, 0);
  XtAddCallback(lookback_but, XtNcallback, PopupList, (XtPointer)0 );


  widen_but = XtCreateManagedWidget("widen_but", commandWidgetClass, form,
                               args, 0 );
  XtAddCallback(widen_but, XtNcallback, widen_search, (XtPointer)0 );
}



  
void
Searchpress()
{
  XEvent search_event;
  Widget search_but = XtNameToWidget(toplevel, "outer.commandform.search_but");

  search_event.type = ButtonPress;
  search_event.xbutton.button = Button1;
  search_event.xany.display = XtDisplay(search_but);
  search_event.xany.window = XtWindow(search_but);

  XSendEvent(XtDisplay(search_but), XtWindow(search_but), True, 
	     ButtonPressMask | ButtonReleaseMask, &search_event);

  search_event.type = ButtonRelease;
  XSendEvent(XtDisplay(search_but), XtWindow(search_but), True,
             ButtonPressMask | ButtonReleaseMask, &search_event);
}



static Widget
create_type_menu(parent)
Widget parent;
{
  Arg args[1];    /* used to pass zero args...to be consistent for lint */
  Widget menu, button;
  int n;

  menu = XtCreatePopupShell("menu", simpleMenuWidgetClass,
                                parent, args, 0 );

  n = 0;
  while (av_typeindx[n] != -1) {
    button = XtCreateManagedWidget((String) filttype[av_typeindx[n]],
                                   smeBSBObjectClass,
                                   menu, args, 0);
    XtAddCallback(button, XtNcallback, Change_Type, (XtPointer) av_typeindx[n]);
    n++;
  }
  return menu;
}

  

static void
create_inputform(parent)
Widget parent;
{
  Widget form, area_form, for_form, type_form;
  Widget type_but;
  Widget search_for;

  Arg args[10];


  static XtActionsRec actionsTable[] = {
    {"Searchpress", (XtActionProc) Searchpress},
  };

  static char defaultTranslations[] =
    "<Key>Return: Searchpress() \n\
     Ctrl<Key>M:  Searchpress() \n\
     Ctrl<Key>O:  Searchpress()";

  XtTranslations trans_table;

  form = XtCreateManagedWidget("inputform", formWidgetClass, parent,
                               args, 0 );

  area_form = XtCreateManagedWidget("area_form", formWidgetClass, form,
                               args, 0 );

  (void) XtCreateManagedWidget("search_area_label", 
                               labelWidgetClass, area_form, args, 0);


  XtSetArg(args[0], XtNstring, friendly_base_path);
  XtSetArg(args[1], XtNwidth, (Dimension) 300);
  (void) XtCreateManagedWidget("search_area", asciiTextWidgetClass,
				      area_form, args, 2);

  for_form = XtCreateManagedWidget("for_form", formWidgetClass, form,
                               args, 0);

  (void) XtCreateManagedWidget("search_for_label", labelWidgetClass,
				     for_form, args, 0);

  search_for = XtCreateManagedWidget("search_for", asciiTextWidgetClass,
                                     for_form, args, 0);
  
  XtAppAddActions(app_con, actionsTable, XtNumber(actionsTable));
  trans_table = XtParseTranslationTable(defaultTranslations);
  XtOverrideTranslations(search_for, trans_table);

  type_form = XtCreateManagedWidget("type_form", formWidgetClass, form,
                               args, 0 );

  type_but = XtCreateManagedWidget("type_but", menuButtonWidgetClass,
                                     type_form, args, 0);

  (void) XtCreateManagedWidget("search_type", asciiTextWidgetClass,
                                     type_form, args, 0);

  (void) create_type_menu(type_but);
}  




  
static void
create_genform(parent)
Widget parent;
{
  Widget form, quit_but, help_but;
  Arg args[10];

  form = XtCreateManagedWidget("genform", formWidgetClass, parent,
			       args, 0 );

  XtSetArg(args[0], XtNheight, stef_height);
  XtSetArg(args[1], XtNwidth, stef_width);
  (void) XtCreateManagedWidget("title", labelWidgetClass,form,
                                   args, 2);

  quit_but = XtCreateManagedWidget("quit_but", commandWidgetClass, form,
				   args, 0);
  XtAddCallback(quit_but, XtNcallback, Quit, (XtPointer) toplevel);
			   

  help_but = XtCreateManagedWidget("help_but", commandWidgetClass, form, 
				   args, 0);
  CreatePopupHelp(help_but);
  XtAddCallback(help_but, XtNcallback, PopupHelp, (XtPointer)0 );
}



/*ARGSUSED*/  
static void
Quit(widget, closure, callData)
Widget widget;
XtPointer closure, callData;
{
  XtDestroyWidget((Widget) closure);
  exit(0);
}


void
CreatePopupHelp(button)
Widget  button;
{
  Widget      popup, form, quit_but;
  Arg args[5];
  FILE *file;
  char fullname[MAXLENGTH];
  char filename[MAXLENGTH];

  (void) strcpy(filename, "xd.help");

  popup = XtCreatePopupShell("popup", transientShellWidgetClass, button,
                               args, 0);

  form = XtCreateManagedWidget("form", formWidgetClass, popup,
                                   args, 0);

  quit_but = XtCreateManagedWidget("quit_but", commandWidgetClass, form,
                                   args, 0);
  XtAddCallback(quit_but, XtNcallback, PopdownHelp, (XtPointer)0 );

  (void) strcpy(fullname, dua_help_dir);
  (void) strcat(fullname, filename);
  if (!(file = fopen(fullname, "r"))) {
    (void) strcpy(fullname, "./xd/helpdir/");
    (void) strcat(fullname, filename);
    if (!(file = fopen(fullname, "r"))) (void) printf("Helpfile not found");
  }

  if(file != NULL) {
    (void) fclose(file);
    XtSetArg(args[0], XtNstring, fullname );
  }
  (void) XtCreateManagedWidget("help_text", asciiTextWidgetClass, form,
                               args, 1);
}


/*ARGSUSED*/
static void
PopupHelp(button, client_data, call_data)
Widget  button;
XtPointer client_data, call_data;
{
  Widget pop_widg = XtNameToWidget(button, "popup");
  Arg         args[5];
  Position    x, y;
  Cardinal    n;


  XtTranslateCoords(button, (Position) 0, (Position) 0,
                    &x, &y);

  n = 0;
  XtSetArg(args[n], XtNx, x);           n++;
  XtSetArg(args[n], XtNy, y-245);       n++;
  XtSetValues(pop_widg, args, n);

  help_popped_up = TRUE;
  XtPopup(pop_widg, XtGrabNone);

}

/*ARGSUSED*/
static void
PopdownHelp(button, client_data, call_data)
Widget  button;
XtPointer client_data, call_data;
{
  Widget pop_widg = XtParent( XtParent(button) );

  help_popped_up = FALSE;
  XtPopdown(pop_widg);
}

void
Change_Help(filename)
String filename;
{
  Arg args[5];
  FILE *file;
  char *fullname;
  static char * old_pointer = 0;

  /* this gets around a problem  mine or athena widgets ? */
  /* does usestringinplace=false work for type = file */
  /*  when using asciitext widgets? */

  if(help_popped_up){
    fullname = XtMalloc(MAXLENGTH * sizeof(char));
    if(old_pointer != 0) {
      XtFree((char *) old_pointer);
      old_pointer = fullname;
    }

    (void) strcpy(fullname, dua_help_dir);
    (void) strcat(fullname, filename);
    if (!(file = fopen(fullname, "r"))) {
      (void) strcpy(fullname, "./Xd/helpdir/");
      (void) strcat(fullname, filename);
      if (!(file = fopen(fullname, "r"))) (void) fprintf(stderr, "Helpfile not found\n");
    }

    if(file != NULL) {
      (void) fclose(file);
      XtSetArg(args[0], XtNstring, fullname );
      XtSetValues( XtNameToWidget(toplevel,
               "outer.genform.help_but.popup.form.help_text"), args, 1);
    }
    XFlush(XtDisplay(toplevel));
  }
}


/*ARGSUSED*/
static void
PopupList(button, client_data, call_data)
Widget  button;
XtPointer client_data, call_data;
{
  Arg         args[5];
  Widget      popup, form, list_places, quit_but;
  Position    x, y;
  Dimension   width, height;
  Cardinal    n;
  String widget_name = "outer.commandform.lookback_but.popup";


  if(lookback_open == True) {
    XtPopup(XtNameToWidget(toplevel, widget_name), XtGrabNone);
    return;
  }


  n = 0;
  XtSetArg(args[0], XtNwidth, &width); n++;
  XtSetArg(args[1], XtNheight, &height); n++;
  XtGetValues(button, args, n);
  XtTranslateCoords(button, (Position) (width / 2), (Position) (height / 2),
                    &x, &y);

  n = 0;
  XtSetArg(args[n], XtNx, x);                         n++;
  XtSetArg(args[n], XtNy, y);                         n++;

  popup = XtCreatePopupShell("popup", transientShellWidgetClass, button,
                               args, n);
  
  form = XtCreateManagedWidget("form", formWidgetClass, popup,
                                   args, 0);

  XtSetArg(args[0], XtNlabel, (String) "Quit");
  quit_but = XtCreateManagedWidget("quit_but", commandWidgetClass, form,
                                   args, 1);
  XtAddCallback(quit_but, XtNcallback, Quit_Popup, (XtPointer) form);

  XtSetArg(args[0], XtNlist, lookback_list);
  XtSetArg(args[1], XtNfromVert, quit_but);
  XtSetArg(args[2], XtNverticalList, True );
  XtSetArg(args[3], XtNnumberStrings, MAXLOOKBACKLENGTH );
  list_places = XtCreateManagedWidget("list_places", listWidgetClass, form,
                               args, 4);
  XtAddCallback(list_places, XtNcallback, Change_Search_Area, (XtPointer)0 );

  lookback_open = True;

  XtPopup(popup, XtGrabNone);
}




void
Add_To_Results(add_this)
String add_this;
{
  Arg args[10];
  String str;
  char new_str[2000];
  Widget result_widg = XtNameToWidget(toplevel, "outer.result_area");

  XtSetArg(args[0], XtNstring, &str);
  XtGetValues(result_widg, args, ONE);

  if(*str == '\0')
    (void) strcpy(new_str, add_this);
  else
    (void) sprintf((char *)new_str,"%s\n%s", str, add_this);

  XtSetArg(args[0], XtNstring, new_str);
  XtSetValues(result_widg, args, 1);
}



void Clear_Results()
{
  Arg args[10];
  Widget result_widg = XtNameToWidget(toplevel, "outer.result_area");

  XtSetArg(args[0], XtNstring, "\0");
  XtSetValues(result_widg, args, 1);
}



Switch_Off_Result_Update()
{
  Widget result_widg = XtNameToWidget(toplevel, "outer.result_area");
  XawTextDisableRedisplay(result_widg);
}


Switch_On_Result_Update()
{
  Widget result_widg = XtNameToWidget(toplevel, "outer.result_area");
  XawTextEnableRedisplay(result_widg);
}



/*ARGSUSED*/
static void
Search(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  Arg args[10];
  int n, numb;
  String str;
  String string;
  Widget list_widg = XtNameToWidget(toplevel, "outer.view_list.list_area");
  Widget widg = XtNameToWidget(toplevel, 
			      "outer.inputform.for_form.search_for");

  Clear_Results();
  XtSetArg(args[0], XtNstring, &str);
  XtGetValues(widg, args, ONE);
  (void) strcpy(mvalue, str);
  srch_start();
  xprint("Search completed");
  showseq = dnseq;
  element_number = entry_number;

  numb = element_number;
  if(numb >= MAXRESULTLISTLENGTH)
    numb = MAXRESULTLISTLENGTH-1;

  for (n = 0; n < numb; n++) {
    string = get_from_seq(n+1, showseq);
    (void) strcpy(result_list[n], string);
  }

  for(; n<MAXRESULTLISTLENGTH; n++) {
    (void) strcpy(result_list[n], "");
  }

  XawListChange(list_widg, result_list, MAXRESULTLISTLENGTH, 0, True);
}



/*ARGSUSED*/
static void
List(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  String string;
  int n, numb;
  Arg args[10];
  Widget list_widg = XtNameToWidget(toplevel, "outer.view_list.list_area");
  Widget top_widg= XtNameToWidget(toplevel, "outer");
  Display *this_display = XtDisplay(top_widg);
  Cursor time_cur = XCreateFontCursor(this_display, XC_watch);
  Cursor normal_cur;

  Clear_Results();
  (void) strcpy(mvalue, "");

  XtSetArg(args[0], XtNcursor, &normal_cur );
  XtGetValues(top_widg, args, 1);

  XtSetArg(args[0], XtNcursor, time_cur );
  XtSetValues(top_widg, args, 1);
  XFlush(this_display);


  srch_start();
  set_default_type();
  showseq = dnseq;
  element_number = entry_number;

  numb = element_number;
  if(numb >= MAXRESULTLISTLENGTH)
    numb = MAXRESULTLISTLENGTH-1;

  for (n = 0; n < numb; n++) {
    string = get_from_seq(n+1, showseq);
    (void) strcpy(result_list[n], string);
  }

  for(; n<MAXRESULTLISTLENGTH; n++) {
    (void) strcpy(result_list[n], "");
  }

  XtSetArg(args[0], XtNcursor, normal_cur);
  XtSetValues(top_widg, args, 1);

  XawListChange(list_widg, result_list, MAXRESULTLISTLENGTH, 0, True);
}



/*ARGSUSED*/
static void
Change_Type(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
  typeindx = (unsigned int) client_data;
  Set_Search_Type(filttype[(int) client_data]);
}



Set_Search_Type(string)
String string;
{
  Arg         args[5];
  Widget search_w = XtNameToWidget(toplevel, "outer.inputform.type_form.search_type");
  Widget typebut_w = XtNameToWidget(toplevel,
                                    "outer.inputform.type_form.type_but");
  Widget typemenu_w = XtNameToWidget(toplevel,
                                    "outer.inputform.type_form.type_but.menu");

  XtDestroyWidget(typemenu_w);
  (void) create_type_menu(typebut_w);

  XtSetArg(args[0], XtNstring, string);
  XtSetValues( search_w, args, 1);
}


/*ARGSUSED*/
static void
Change_Search_Area(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{

  XawListReturnStruct *item = (XawListReturnStruct*)call_data;

  Clear_Results();

  if((strlen(item->string) != 0) && (strcmp(item->string, NOHISTORY))){
    Set_Search_Area(item->string);
    rd_start();
    add_to_lookback(base_path);
  }

  XawListUnhighlight(w);
}



Set_Search_Area(string)
String string;
{
  Arg   args[10];
  String widget_name = "outer.inputform.area_form.search_area";
  Widget search_w = XtNameToWidget(toplevel, widget_name);

  XtSetArg(args[0], XtNstring, string);
  XtSetValues(search_w, args, 1);

  (void) strcpy(base_path, string);
  set_default_type();
}

/*ARGSUSED*/
static void
Quit_Popup(widget, client_data, call_data)
Widget  widget;
XtPointer client_data, call_data;
{
    Widget popup = XtParent((Widget) client_data);

    if(lookback_open == True)
      if(XtNameToWidget(toplevel,
         "outer.commandform.lookback_but.popup.form.quit_but") == widget)
             lookback_open = False; 

    XtDestroyWidget(popup);
}


void
xprint(str)
String str;
{
  Add_To_Results(str);  
}

/*ARGSUSED*/
static void
widen_search()
{
  Arg   args[10];
  String widget_name = "outer.inputform.area_form.search_area";
  Widget search_w = XtNameToWidget(toplevel, widget_name);

  widen();
  XtSetArg(args[0], XtNstring, base_path);
  XtSetValues(search_w, args, 1);
}
 

void
add_to_lookback(addthis)
String addthis;
{
  int count;
  String widget_name = "outer.commandform.lookback_but.popup.form.list_places";
  Widget list_w;

  for(count=0; count<MAXLOOKBACKLENGTH; count++ ) 
    if(!strcmp(lookback_list[count], addthis))
      return;    /* no duplicates thankyou! */

  for(count=0; count<MAXLOOKBACKLENGTH-1; ) {
    (void) strcpy(lookback_list[count], lookback_list[count+1]);
    count++;
  }
  (void) strcpy(lookback_list[count++], addthis);
  
  if(lookback_open == True) {
    list_w = XtNameToWidget(toplevel,widget_name);
    XawListChange(list_w, lookback_list, MAXLOOKBACKLENGTH, 0, True);
  }
 
  XFlush(XtDisplay(toplevel));
}
