/*
 * @(#)help.c	1.2	%G%
 *
 * Routines to provide help screens for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include "gremlin.h"
#include "icondata.h"

/* imports from main.c */

extern struct pixwin *pix_pw;
extern struct rect pix_size;
extern struct pixrect *scratch_pr;
extern struct pixfont *text_pf;
extern pix_fd;
extern menu_fd;
extern text_fd;
extern tool_fd;

/* imports from sun.c */

extern get_any_button();

/* locals */

static help_x, help_y;		/* current position on help screen */

#define help_dy  (text_pf->pf_defaultsize.y)
#define HELP_LEFT 8
#define HELP_SKIP 16


/*
 * Display a help screen consisting of a pixrect, a title and some text.
 * The current picture is saved in the scratch pixrect before displaying
 * the help screen.  After the user presses a mouse button, the picture
 * is restored and processing continues.
 */
static
help_screen(icon_pr, title, text)
struct pixrect *icon_pr;
char *title, *text;
{
    /* save current display */
    pw_read(scratch_pr, 0, 0, pix_size.r_width, pix_size.r_height, PIX_SRC,
			pix_pw, 0, 0);
    
    /* clear picture subwindow before help screen display */
    pw_writebackground(pix_pw, 0, 0, 2000, 2000, PIX_SRC);

    help_x = HELP_LEFT;
    help_y = 8;

    /* display icon if not a NULL pointer */
    if (icon_pr != NULL) {
	pw_write(pix_pw, help_x, help_y, icon_pr->pr_size.x, icon_pr->pr_size.y,
			 PIX_SRC, icon_pr, 0, 0);
	help_x += icon_pr->pr_size.x + HELP_SKIP;
	help_message(title);
	help_y += icon_pr->pr_size.y + HELP_SKIP;
    }
    else {
	help_message(title);
	help_y += help_dy + HELP_SKIP;
    }

    help_x = HELP_LEFT;
    help_message(text);

    help_y += HELP_SKIP;
    help_x = HELP_LEFT;
    help_message("Press a mouse button to continue.");

    /* wait for mouse button in any subwindow or tool border */
    get_any_button();

    /* restore picture subwindow */
    pw_write(pix_pw, 0, 0, pix_size.r_width, pix_size.r_height, PIX_SRC,
			scratch_pr, 0, 0);
}


/*
 * A simple text display tool for help messages.
 * Text is displayed at the current help location (help_x, help_y).
 * Newlines within the text string are handled appropriately.
 * Maximum line length (between newlines) is 80 characters, although 
 * only about 64 can be displayed on the default Gremlin screen.
 */
static
help_message(text)
char *text;
{
    char buf[80];
    register i;

    while (*text != '\0') {
	i = 0;

	while ((*text != '\n') && (*text != '\0'))
	    buf[i++] = *text++;
	buf[i] = '\0';

	if (*text == '\n')
	    text++;

	pw_text(pix_pw, help_x, help_y + TEXT_BASELINE + 2, PIX_SRC, 
							    text_pf, buf);
	help_y += help_dy;
    }
}


static char help_HELP[] = "\
The three subwindows in Gremlin are used for text entry,\n\
menu display and command selection, and picture display.\n\
In addition, the normal tool manager pop-up menu is available\n\
from the tool borders.\n\
\n\
In the menu subwindow, the left mouse button is used to\n\
invoke commands after selecting the appropriate icon, the\n\
middle mouse button is used to effect the same command on\n\
only the current set (where appropriate), and the right mouse\n\
button provides a help screen for the icon.\n\
\n\
In the picture subwindow, the left mouse button is used to\n\
lay down points, the middle mouse button erases points in the\n\
opposite order from which they were layed down, and the right\n\
mouse button provides a help screen.\n\
\n\
In the text subwindow, command arguments (when required)\n\
are entered from the keyboard.  Arguments must be entered\n\
before the command is selected.  Simple editing commands\n\
(backspace, line and word delete) can be used to modify the\n\
argument.  Again, the right mouse button provides a help\n\
screen display.";

help()
{
    help_screen(&question_pr, "Help ('?')", help_HELP);
}


static char justify_HELP[] = "\
Select text justification by moving marker to one of nine\n\
positioning points within the JUSTIFY icon and pressing the\n\
left mouse button.\n\
\n\
Modify justification of text in the current set by moving the\n\
marker as above and then pressing the middle mouse button.\n\
\n\
When text is displayed in the current set, its justification\n\
mode is indicated by a small dot.";

justify_help()
{
    help_screen(&justify_pr, "Text Justification", justify_HELP);
}


static char size1_HELP[] = "\n\
Set the default font size to one with the left mouse button.\n\
\n\
Modify text in the current set to size one with the middle\n\
mouse button.";

size1_help()
{
    help_screen(&size1_pr, "Set Text Size One", size1_HELP);
}


static char roman_HELP[] = "\
Set the default font to Roman with the left mouse button.\n\
\n\
Modify text in the current set to Roman font with the middle\n\
mouse button.";

roman_help()
{
    help_screen(&roman_pr, "Set Roman Text Font", roman_HELP);
}


static char scale_HELP[] = "\
Scaling uses three points to define a transformation.\n\
The current set is scaled by the ratio of the distances\n\
between the first and second and the first and third points.";

scale_help()
{
    help_screen(&scale_pr, "Scale Current Set ('s')", scale_HELP);
}


static char move_HELP[] = "\
Translation uses two points to define a transformation.\n\
The current set is translated through the relative distance\n\
between the two points.";

move_help()
{
    help_screen(&move_pr, "Translate Current Set ('t')", move_HELP);
}


static char hmirror_HELP[] = "\
Mirroring uses one point to define a transformation.\n\
The current set is reflected about the horizontal line\n\
containing the point.";

hmirror_help()
{
    help_screen(&hmirror_pr, "Horizontal Mirror", hmirror_HELP);
}


static char vmirror_HELP[] = "\
Mirroring uses one point to define a transformation.\n\
The current set is reflected about the vertical line\n\
containing the point.";

vmirror_help()
{
    help_screen(&vmirror_pr, "Vertical Mirror", vmirror_HELP);
}


static char include_HELP[] = "\
The current set is selected by points.  Using the left mouse\n\
button, the current set will include ONLY those elements near\n\
the points.  With the middle mouse button, those elements near\n\
the points will be ADDED to the current set.";

include_help()
{
    help_screen(&include_pr, "Define Current Set ('d')", include_HELP);
}


static char put_HELP[] = "\
The current set is copied into the specified set buffer for\n\
possible later retrieval.  An optional positioning point may\n\
be specified for use in positioning the set when it is later\n\
copied into a picture.  With no positioning point specified,\n\
a point is selected from among the reference points of the\n\
current set.";

put1_help()
{
    help_screen(&put1_pr, "Save Current Set in Buffer One ('1')", put_HELP);
}


put2_help()
{
    help_screen(&put2_pr, "Save Current Set in Buffer Two ('2')", put_HELP);
}


put3_help()
{
    help_screen(&put3_pr, "Save Current Set in Buffer Three ('3')", put_HELP);
}


put4_help()
{
    help_screen(&put4_pr, "Save Current Set in Buffer Four ('4')", put_HELP);
}


static char horizontal_HELP[] = "\
Horizontal adjustment forces each point laid down to lie on\n\
a horizontal line from the previous point.  The left mouse\n\
button toggles this drawing mode.";

horizontal_help()
{
    help_screen(&horizontal_pr, "Horizontal Adjustment", horizontal_HELP);
}


static char vertical_HELP[] = "\
Vertical adjustment forces each point laid down to lie on a\n\
vertical line from the previous point.  The left mouse button\n\
toggles this drawing mode.";

vertical_help()
{
    help_screen(&vertical_pr, "Vertical Adjustment", vertical_HELP);
}


static char stipple_HELP[] = "\
Select the stipple pattern used for drawing polygons with\n\
the left mouse button.\n\
\n\
Modify polygons in the current set to the specified stipple\n\
pattern with the middle mouse button.";

stipple1_help()
{
    help_screen(&white_pr, "Set Stipple Pattern One", stipple_HELP);
}


stipple2_help()
{
    help_screen(&gray_pr, "Set Stipple Pattern Two", stipple_HELP);
}


stipple3_help()
{
    help_screen(&_50_pr, "Set Stipple Pattern Three", stipple_HELP);
}


stipple4_help()
{
    help_screen(&black_pr, "Set Stipple Pattern Four", stipple_HELP);
}


stipple5_help()
{
    help_screen(&stipple5_pr, "Set Stipple Pattern Five", stipple_HELP);
}


stipple6_help()
{
    help_screen(&stipple6_pr, "Set Stipple Pattern Six", stipple_HELP);
}


stipple7_help()
{
    help_screen(&stipple7_pr, "Set Stipple Pattern Seven", stipple_HELP);
}


stipple8_help()
{
    help_screen(&stipple8_pr, "Set Stipple Pattern Eight", stipple_HELP);
}


static char size2_HELP[] = "\n\
Set the default font size to two with the left mouse button.\n\
\n\
Modify text in the current set to size two with the middle\n\
mouse button.";

size2_help()
{
    help_screen(&size2_pr, "Set Text Size Two", size2_HELP);
}


static char italics_HELP[] = "\
Set the default font to Italics with the left mouse button.\n\
\n\
Modify text in the current set to Italics font with the middle\n\
mouse button.";

italics_help()
{
    help_screen(&italics_pr, "Set Italics Text Font", italics_HELP);
}


static char copy_HELP[] = "\
Copying uses two or more points.  A copy of the current set\n\
is made and translated by a relative distance between the\n\
first and each additional point.  The last copy becomes\n\
the new current set.";

copy_help()
{
    help_screen(&copy_pr, "Copy Current Set ('c')", copy_HELP);
}


static char erase_HELP[] = "\
The current set is erased.  See also the undo command.";

erase_help()
{
    help_screen(&erase_pr, "Erase Current Set ('e')", erase_HELP);
}


static char movepoint_HELP[] = "\
This command uses one or more points.  The element of the\n\
current set which contains the point closest to the first of\n\
these points is redrawn with that point replaced by the\n\
remaining points, or deleted if there is only one point.";

movepoint_help()
{
    help_screen(&movepoint_pr, "Move Point", movepoint_HELP);
}


static char rotate_HELP[] = "\
Three points are used to define a rotation.  The rotation is\n\
performed relative to the first point, through an angle formed\n\
by the lines between points one and two and points one and\n\
three, respectively.";

rotate_help()
{
    help_screen(&rotate_pr, "Rotate Current Set ('r')", rotate_HELP);
}


static char filecabinet_HELP[] = "\
This icon produces a pop-up menu to select commands for\n\
manipulating files: Edit, Path, Read, Write and Save Set.\n\
With each command, parameters should be specified in the\n\
text subwindow before invocation.\n\
\n\
The Edit command causes a new file to be opened for editing.\n\
Picture sets saved in the buffers are preserved across edits.\n\
The file name will be displayed in the Gremlin tool border.\n\
\n\
The Path command is used to set the directory search path\n\
for the Edit and Read commands.  Directory names should be\n\
separated by colons and may include the ~ notation.  If a file\n\
cannot be found using any of the paths, a final check will be\n\
made in the Gremlin library, /usr/local/lib/gremlin.\n\
\n\
The Read command is used to add elements from the specified\n\
file into the current picture.  The new elements become the\n\
current set.  A point may be specified to position the file\n\
in the picture.\n\
\n\
The Write command saves the entire picture in a file.  If no\n\
file name is specified in the text subwindow, the current Edit\n\
file name is used.  An optional point may be specified to aid\n\
in Reading the picture later.\n\
\n\
The Save Set command is similar to the Write command with two\n\
exceptions: only the current set is written, and a file name\n\
must be specified.";

filecabinet_help()
{
    help_screen(&filecabinet_pr, "File Commands", filecabinet_HELP);
}


static char boxinc_HELP[] = "\
Two points must be placed that define a rectangular area\n\
(the endpoints of the diagonal of the rectangle).\n\
\n\
With the left mouse button, all elements contained within the\n\
the rectangle become the current set.\n\
\n\
With the middle mouse button, those same elements are ADDED\n\
to the current set.";

boxinc_help()
{
    help_screen(&boxinc_pr, "Select Area for Current Set ('f')", boxinc_HELP);
}


static char manhattan_HELP[] = "\
Manhattan adjustment forces each point laid down to be either\n\
directly horizontal or vertical with respect to the previous\n\
point, whichever it is closer to.  The left mouse button\n\
toggles this drawing mode.";

manhattan_help()
{
    help_screen(&horvert_pr, "Manhattan Adjustment ('z')", manhattan_HELP);
}


static char gravity_HELP[] = "\
This command toggles each time it is selected.  When on,\n\
gravity forces a point to coincide with the nearest existing\n\
point or reference point.  It will only take affect, however,\n\
if the point is near enough to an element to be gravitiated\n\
to it.";

gravity_help()
{
    help_screen(&gravity_pr, "Set Gravity ('g')", gravity_HELP);
}


static char size3_HELP[] = "\n\
Set the default font size to three with the left mouse button.\n\
\n\
Modify text in the current set to size three with the middle\n\
mouse button.";

size3_help()
{
    help_screen(&size3_pr, "Set Text Size Three", size3_HELP);
}


static char bold_HELP[] = "\
Set the default font to Bold with the left mouse button.\n\
\n\
Modify text in the current set to Bold font with the middle\n\
mouse button.";

bold_help()
{
    help_screen(&bold_pr, "Set Bold Text Font", bold_HELP);
}


static char brush_HELP[] = "\
The left mouse button sets the current brush style.\n\
\n\
The middle mouse button modifies all elements in the current\n\
set (except text) to the selected brush style.";

brush1_help()
{
    help_screen(&dotted_pr, "Set Dotted Line Style", brush_HELP);
}


brush2_help()
{
    help_screen(&broken_pr, "Set Broken Line Style", brush_HELP);
}


brush3_help()
{
    help_screen(&thick_pr, "Set Thick Line Style", brush_HELP);
}


brush4_help()
{
    help_screen(&dashed_pr, "Set Dashed Line Style", brush_HELP);
}


brush5_help()
{
    help_screen(&narrow_pr, "Set Narrow Line Style", brush_HELP);
}


brush6_help()
{
    help_screen(&medium_pr, "Set Medium Line Style", brush_HELP);
}


static char arrow_HELP[] = "\
This command requires two points.  The first point indicates\n\
the tip of the arrow.  The second point indicates the\n\
direction from which the arrow points.";

arrow_help()
{
    help_screen(&arrow_pr, "Draw Arrowhead ('w')", arrow_HELP);
}


static char text_HELP[] = "\
Text is positioned using one or two points.  If two points\n\
are used, the text is positioned relative to their locus.\n\
The text specified in the text subwindow is displayed using\n\
the current font, size and justification.\n\
\n\
See the text subwindow help display for an explanation of\n\
quick text entry.";

text_help()
{
    help_screen(&text_pr, "Display Text", text_HELP);
}


static char misc_HELP[] = "\
This command invokes a pop-up menu of infrequently used\n\
commands: Clear Points, Show Points, Gripe (rarely used)\n\
and Point.\n\
\n\
Clear Points will clear all positioning points and reference\n\
points from the display.\n\
\n\
Show Points will display the reference points of those\n\
elements in the current set.\n\
\n\
Gripe displays a message indicating the mail address of\n\
the current Gremlin maintainer.\n\
\n\
Point can be used to lay down a point at a specific location.\n\
The coordinates of the point must first be entered in the\n\
text subwindow.";

misc_help()
{
    help_screen(&misc_pr, "Miscellaneous Commands", misc_HELP);
}


static char get_HELP[] = "\
This command retrieves a set from the specified buffer and\n\
copies it into the picture.  At least one point must be\n\
specified, indicating the position(s) in the picture where\n\
the set is to be copied.";

get1_help()
{
    help_screen(&get1_pr, "Add Buffer One To Picture", get_HELP);
}


get3_help()
{
    help_screen(&get3_pr, "Add Buffer Three To Picture", get_HELP);
}


get2_help()
{
    help_screen(&get2_pr, "Add Buffer Two To Picture", get_HELP);
}


get4_help()
{
    help_screen(&get4_pr, "Add Buffer Four To Picture", get_HELP);
}


static char linestyle_HELP[] = "\
This command toggles symbolic line display.  When highlighted,\n\
this icon indicates that elements will be drawn using their\n\
true line style (or brush).  When not highlighted, all\n\
elements are drawn using the narrow brush, decreasing display\n\
times.";

linestyle_help()
{
    help_screen(&linestyle_pr, "Toggle Line Style Mode", linestyle_HELP);
}


static char align_HELP[] = "\
This command forces points to be aligned on pixel boundaries\n\
as specified in the icon.  Alignment occurs in powers of two.\n\
To select the next alignment value use the left mouse button,\n\
and to select the previous value use the middle mouse button.\n\
\n\
The precedence of point positioning modifiers is as follows:\n\
gravity will override alignment, and adjustment (vertical,\n\
horizontal or manhattan) will be applied to the point to which\n\
the point has been gravitated.";

align_help()
{
    help_screen(&align_pr, "Set Point Alignment", align_HELP);
}


static char size4_HELP[] = "\n\
Set the default font size to four with the left mouse button.\n\
\n\
Modify text in the current set to size four with the middle\n\
mouse button.";

size4_help()
{
    help_screen(&size4_pr, "Set Text Size Four", size4_HELP);
}


static char special_HELP[] = "\
Set the default font to Special with the left mouse button.\n\
\n\
Modify text in the current set to Special font with the middle\n\
mouse button.";

special_help()
{
    help_screen(&special_pr, "Set Special Text Font", special_HELP);
}


static char arc_HELP[] = "\
This command requires two points to draw a full circle or\n\
three points to draw an arc.  The first point determines the\n\
center of a circle.  The second is a point on the circle,\n\
thus defining the radius.  An optional third point determines\n\
a counter-clockwise angle from the second point which is the\n\
extent of the arc.";

arc_help()
{
    help_screen(&arc_pr, "Draw Circle or Arc ('a')", arc_HELP);
}


static char curve_HELP[] = "\
A curve is determined by a number of points distributed along\n\
its trajectory.  Two points yield a straight line.  If the\n\
first and last points of a spline are the same, a smooth\n\
closed figure will be drawn.  Curves are drawn with the left\n\
mouse button using the current brush style.\n\
\n\
With the middle mouse button, vectors and polygons in the\n\
current set are modified to become curves.";

curve_help()
{
    help_screen(&curve_pr, "Draw Curve ('b')", curve_HELP);
}


static char vector_HELP[] = "\
With the left mouse button, a line is drawn connecting each\n\
of the points layed down in order.  The current brush (narrow,\n\
dotted, etc) is used.\n\
\n\
With the middle mouse button, curves and polygons in the\n\
current set are modified to become vectors.";

vector_help()
{
    help_screen(&vector_pr, "Draw Vector ('v')", vector_HELP);
}


static char box_HELP[] = "\
Two points are used to define the endpoints of the diagonal\n\
of a rectangle.  A box is drawn in the current brush which\n\
forms the rectangle.";

box_help()
{
    help_screen(&box_pr, "Draw Box ('x')", box_HELP);
}


static char grid_HELP[] = "\
This command toggles the display of a grid used to aid in\n\
laying down points.  The grid is displayed on 32 pixel\n\
boundaries.";

grid_help()
{
    help_screen(&grid_pr, "Toggle Grid Display ('q')", grid_HELP);
}


static char littlepoint_HELP[] = "\
This command toggles the style of points displayed.  When the\n\
icon is highlighted, points are indicated by a small circle\n\
and are numbered from zero.  When the icon is unhighlighted,\n\
points are displayed as a small diamond.";

littlepoint_help()
{
    help_screen(&littlepoint_pr, "Toggle Point Style", littlepoint_HELP);
}


static char undo_HELP[] = "\
This command undoes the last command which modified the\n\
picture contents.";

undo_help()
{
    help_screen(&undo_pr, "Undo Last Command", undo_HELP);
}


static char pan_HELP[] = "\
With the left mouse button this command requires one point.\n\
The entire picture and current set are translated such that\n\
the specified point is located at the center of the display.\n\
\n\
With the middle mouse button no point is required.  The\n\
picture is translated such that its absolute center is\n\
brought to the center of the display.";

pan_help()
{
    help_screen(&pan_pr, "Panning", pan_HELP);
}


static char polygon_HELP[] = "\
Polygons can be drawn either bordered or unbordered.  At\n\
least three points are required to draw a polygon.  If the\n\
first and the last points are not the same, that line\n\
segment will be added automatically.  With the left mouse\n\
button, a filled polygon will be added to the display in the\n\
current stipple style.  If a border is to be drawn, this\n\
will be added in the current line style.\n\
\n\
The middle mouse button is used to modify curves, vectors and\n\
other polygons in the current set to be polygons of the\n\
selected type (bordered or unbordered).";

bpolygon_help()
{
    help_screen(&bpolygon_pr, "Fill Bordered Polygon", polygon_HELP);
}


polygon_help()
{
    help_screen(&polygon_pr, "Fill Polygon", polygon_HELP);
}


static char textsw_HELP[] = "\
Command arguments (when required) are entered here from the\n\
keyboard.  Arguments must be entered before the command is\n\
selected.  Simple editing commands (backspace, line and word\n\
delete) can be used to modify the argument.  The middle mouse\n\
button is used to display the previous text string.\n\
\n\
The quick form of the TEXT command is invoked by pressing\n\
RETURN after entering a string to be displayed in the picture.\n\
The string is displayed at the LAST point layed down (using\n\
the current justification mode), and this point is removed\n\
from the display.";

textsw_help()
{
    help_screen(NULL, "Text Subwindow Help", textsw_HELP);
}


static char pixsw_HELP[] = "\
The left mouse button is used to lay down points.  The middle\n\
mouse button erases points in the opposite order from which\n\
they were layed down.";

pixsw_help()
{
    help_screen(NULL, "Picture Subwindow Help", pixsw_HELP);
}


menusw_help()
{
    /* keeps getting invoked by mistake - not useful anyway */
}
