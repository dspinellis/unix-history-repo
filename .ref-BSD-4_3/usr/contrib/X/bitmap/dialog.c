#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */
#include <X/Xlib.h>

#ifndef lint
static char *rcsid_dialog_c = "$Header: dialog.c,v 10.4 86/02/01 15:18:29 tony Rel $";
#endif

extern int foreground;
extern int background;
extern Pixmap backmap;
extern Pixmap border;
extern int borderwidth;
extern int invertplane;
extern int mousepix;

#define NULL 0
#define MIN_BETWEEN_COMMANDS 10
#define BETWEEN_LINES 10
#define TOP_MARGIN 10
#define BOTTOM_MARGIN 10
#define MSG_RIGHT_MARGIN 7
#define MSG_LEFT_MARGIN 7
#define COMMAND_WIDTH_FUDGE 8

#define max(a,b) ((a > b) ? a : b)

#include "../cursors/cross.cursor"
#include "../cursors/cross_mask.cursor"
static Cursor cross_cursor;

static struct dialog_data {
  Window w;
  Font font;
  int font_height;
  char *msg1, *msg2;
  int msg1_length, msg2_length;
  struct command_data *command_info;
  };

static struct command_data {
  Window window;
  char *name;
  int name_length;
  int name_width;  /* in pixels */
  int x_offset;
  };

int dialog (w, font, font_height,
    msg1, msg2, command_names, n_commands, input_handler)
  Window w;
  Font font;
  int font_height;
  char *msg1, *msg2;
  char **command_names;
  int n_commands;
  int (*input_handler) ();
  {
  struct dialog_data data;
  static int initialized = 0;  
  int msg1_width = XQueryWidth (msg1, font);
  int msg2_width = XQueryWidth (msg2, font);
  int command_width = 0;
  int result;
  register int i;
  
  if (!initialized) {
    Initialize ();
    initialized = 1;
    }

  data.font = font;
  data.font_height = font_height;
  data.msg1 = msg1;
  data.msg2 = msg2;
  data.msg1_length = strlen (msg1);
  data.msg2_length = strlen (msg2);
  data.command_info = (struct command_data *) malloc
    (n_commands*sizeof (struct command_data));

  for (i=0;i<n_commands;i++) {
    data.command_info[i].name = command_names[i];
    data.command_info[i].name_length = strlen (command_names[i]);
    data.command_info[i].name_width = XQueryWidth (command_names[i], font);
    if (data.command_info[i].name_width > command_width)
      command_width = data.command_info[i].name_width;
    }
  command_width += COMMAND_WIDTH_FUDGE;

  {
  int between_commands;
  {
  int height =
    3*font_height + 2*BETWEEN_LINES + TOP_MARGIN + BOTTOM_MARGIN;
  int width = max (msg1_width, msg2_width)
    + MSG_LEFT_MARGIN + MSG_RIGHT_MARGIN;
  int min_width =
    n_commands*command_width + (n_commands+1)*MIN_BETWEEN_COMMANDS;
  int x, y;
  DeterminePlace (w, &x, &y);
  width = max (width, min_width);
  between_commands = 
     (width - n_commands*command_width)/(n_commands+1);
  data.w = XCreateWindow (RootWindow, x, y, width, height,
    borderwidth, border, backmap);
  }

  {
  int xx = between_commands;
  OpaqueFrame *frames = (OpaqueFrame *)malloc(n_commands*sizeof(OpaqueFrame));
  for (i=0;i<n_commands;i++) {
    register OpaqueFrame *frame = &frames[i];
    register struct command_data *command = &data.command_info[i];
    command->x_offset = (command_width - command->name_width)/2;
    frame->x = xx;
    frame->y = TOP_MARGIN + 2*(font_height+BETWEEN_LINES);
    frame->width = command_width;
    frame->height = font_height;
    frame->bdrwidth = 1;
    frame->border = border;
    frame->background = backmap;
    xx += (between_commands + command_width);
    }
  XCreateWindows (data.w, frames, n_commands);
  for (i=0;i<n_commands;i++)
    data.command_info[i].window = frames[i].self;
  free (frames);
  }}
  
  XSelectInput (data.w,
    ButtonPressed | ButtonReleased | ExposeWindow | LeaveWindow);
  
  XDefineCursor (data.w, cross_cursor);
  XMapWindow (data.w);
  XMapSubwindows (data.w);
  
  while (1) {
    struct command_data *command = NULL;
    XEvent event;
    XNextEvent (&event);
    if (event.window != data.w) {
      (*input_handler) (&event);
      continue;  /* back around the loop */
      }
    if (event.subwindow == 0) {
      ProcessDialogWindowEvent (&data, &event);
      continue;
      }
    for (i=0;i<n_commands;i++)
      if (event.subwindow == data.command_info[i].window) {
	command = &data.command_info[i];
	break;
	}
    if (command == NULL)
      continue;  /* really shouldn't happen, but what can you do? */
    result = ProcessCommandEvent (&data, command, &event);
    if (result >= 0)
      break;
    }
  
  XDestroyWindow (data.w);

  free (data.command_info);
  return (result);
  }   /* end of dialog procedure */


Initialize ()
  {
  cross_cursor = XCreateCursor (cross_width, cross_height, cross_bits,
    cross_mask_bits, cross_x_hot, cross_y_hot,
    mousepix, background, GXcopy);
  }


/* ProcessCommandEvent returns -1 unless a command was actually invoked,
in which case it returns the command number. */

static int ProcessCommandEvent (data, command, event)
  struct dialog_data *data;
  struct command_data *command;
  XEvent *event;
  {
  static struct command_data *button_down_command = NULL;
  
  switch (event->type) {
    
    case ExposeWindow:
      XTextMask (command->window, command->x_offset,
        0, command->name, command->name_length, data->font, foreground);
      break;

    case ButtonPressed:
      if (button_down_command != NULL)
        break;  /* must be second button press; ignore it */
      button_down_command = command;
      InvertCommand (data, command);
      break;

    case LeaveWindow:
      if (command == button_down_command) {
	InvertCommand (data, command);
	button_down_command = NULL;
	}
      break;

    case ButtonReleased:
      if (command == button_down_command) {
	button_down_command = NULL;
        return (command - data->command_info);
        }
      break;
   
    }

  return (-1);
  }


static ProcessDialogWindowEvent (data, event)
  struct dialog_data *data;
  XEvent *event;
  {
  if (event->type == ExposeWindow) {
    XTextMask (
      data->w, MSG_LEFT_MARGIN,
      TOP_MARGIN, data->msg1, data->msg1_length, data->font, foreground);
    XTextMask (
      data->w, MSG_LEFT_MARGIN,
        TOP_MARGIN + data->font_height + BETWEEN_LINES,
	data->msg2, data->msg2_length, data->font, foreground);
    }
  }


static InvertCommand (data, command)
  struct dialog_data *data;
  struct command_data *command;
  {
  XPixFill (command->window, 0, 0, 400, data->font_height, 1, NULL,
	   GXinvert, invertplane);
  }


static DeterminePlace (w, px, py)
  Window w;
  int *px, *py;
  {
  WindowInfo info;
  XQueryWindow (w, &info);
  /* max (0,...) is to make sure dialog window is on screen, even
     if "parent" window is partially off screen (negative x or y) */
  *px = max (0, info.x + 10);
  *py = max (0, info.y + 10);
  }

