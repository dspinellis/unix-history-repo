#! /bin/sh
#
#	local for okeeffe.berkeley.edu, running nntp to/from ucbvax.berkeley.edu
#
rm -f Makefile
cp Makefile.dst Makefile
chmod u+w Makefile
ed Makefile  <<'EOF'
g/^#BSD4_3 /s///
g/^#V7 /s///
g/^#NNTP /s///
g/^#RESOLVE /d
g/^#USG /d
g/^#EXCELAN /d
g/=fork/d
g/^#VMS /d
g/^#BSD4_2 /d
g/^#BSD4_1 /d
g/^all:/s/$(OTHERS)//
g/#NOTVMS/s/#NOTVMS//
g/-DDBM/s/-DDBM//
g/-ldbm/s/-ldbm//
g/#NOTVMS/s/#NOTVMS.*//
g/NNTPSRC/s,NNTPSRC,/usr/src/new/nntp,
/^UUXFLAGS/s/-r -z/-r -z -n -gd/
/^LIBDIR/s;/usr/lib/news;/usr/new/lib/news;
/^BINDIR/s;/usr/bin;/usr/new;
w
q
EOF
rm -f defs.h
cp defs.dist defs.h
chmod u+w defs.h
ed defs.h << 'EOF'
g/NNTPSRC/s,NNTPSRC,/usr/src/new/nntp,
g/SERVERFILE/s,SERVERFILE,/usr/new/lib/news/server,
/N_UMASK/s/000/002/
/DFTXMIT/s/-z/-z -gd/
/UXMIT/s/-z/-z -gd/
/INTERNET/s;/\* ;;
/GHNAME/s;/\* ;;
/DOXREFS/s;/\* ;;
/BSD4_2/s;/\* ;;
/SENDMAIL/s;/\* ;;
/MYORG/s/Frobozz.*Louis/CSRG, UC Berkeley/
/ROOTID/s/10/0/
/MYDOMAIN/s/\.UUCP/.Berkeley.EDU/
/MKDIRSUB/s;/\* ;;
w
q
EOF
echo "Be sure to make a login for usenet and create the group news"
echo "Also, be sure to alter Makefile and defs.h to reflect the"
echo "user name, id number, group name and group id number in the"
echo "places documented."
echo "Finally, please set your organization name in defs.h"
echo "Look for #define MYORG for the string to alter."
echo "Good Luck."
