.NH 2
Display Coordinate System
.XS
Display Coordinate system
.XE
.PP
.IN "Definitions" "Coordinate System"
.IN "Definitions" "X Axis"
.IN "Definitions" "Y Axis"
As with most bitmap display systems, the coordinate system is
different from a cartesian coordinate system.
X and y coordinate systems are measured in pixels from
the UPPER LEFT corner of the screen or window.
.PP
This means that while the X coordinate behaves as one might expect,
the Y coordinate increases DOWN the screen.
.PP
.IN "Definitions" "Window Size"
When you specify the size of a window in X, it is the size of the
usable pixels inside the border.
.PP
X always measures window size in pixels EXCLUSIVE of borders, from the
upper left hand corner.
This is the actual usable size of the window.
.PP
If you query the root window, you can determine the absolute size of the 
display.
