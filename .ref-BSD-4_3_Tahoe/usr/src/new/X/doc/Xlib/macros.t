.de FD
.LP
.KS
.TA .5i 3i
.ta .5i 3i
.sp 2
.nf
..
.de FN
.fi
.sp 1
.KE
.LP
..
.de IN		\" send an index entry to the stderr
.tm \\n%\t\\$1\t\\$2
..
.de C{
.KS
.nf
.D
.\"
.\"	choose appropriate monospace font
.\"	the imagen conditional, 480,
.\"	may be changed to L if LB is too
.\"	heavy for your eyes...
.\"
.ie "\\*(.T"480" .ft L
.el .ie "\\*(.T"300" .ft L
.el .ie "\\*(.T"202" .ft PO
.el .ie "\\*(.T"aps" .ft CW
.el .ft R
.ps \\n(PS
.ie \\n(VS>40 .vs \\n(VSu
.el .vs \\n(VSp
..
.de C}
.DE
.R
..

