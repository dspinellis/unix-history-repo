.PP
RCS markers like "$Revision: 2.3 $" etc., can be pretty-printed (i.e.,
without the leading keyword and the $-signs) as follows.

In n/troff, define the following macro:
.nf

        .de VL
        \\$2
        ..

The call
        .VL $Revision: 1.2 $
.fi
picks out the number (actually, the value field of any RCS marker).
In all manual pages, I recommend the use an identification
section instead of an author section, which looks something like this:
.nf

        .SH IDENTIFICATION
        Author: Walter F. Tichy,
        Purdue University, West Lafayette, IN, 47907.
        .sp 0
        Revision Number:
        .VL $Revision: 3.0 $
        ; Release Date:
        .VL $Date: 82/11/27 11:43:39 $
        \&.
        .sp 0
        Copyright \(co 1982 by Walter F. Tichy.


.fi
One could use the same trick with C-macros, but, unfortunately, these macros
want commas separating the arguments. I can only offer sscanf instead: 
.nf

        char * getkeyval(s)
        char * s;
        {       static char keyval[100];
                sscanf(s,"%*s%s",keyval);
                return keyval;
        }

An example use of getkeyval() is the following greeting message: 

       printf("Program version %s\n",getkeyval("$Revision 1.2 $"));

.fi
There is no option in RCS that strips off the keywords, for a good reason:
If the keyword is stripped off, it becomes impossible to update the
keyword value automatically.

There is no way to suppress the keyword expansion, either. If you 
absolutely need a keyword in RCS format unexpanded, piece it together from 
two strings in C, or imbed the null-character \& in n/troff. 
