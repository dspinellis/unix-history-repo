$!
$! this command procedure installs libg++ on a VMS system.  It is assumed that
$! GNU-C and GNU-C++ have already been installed.  It is also assumed that the
$! sharable library will be used.  In order to run this you will need to have
$! write access to GNU_CC:[000000] and SYS$LIBRARY:
$!
$! first build the actual library.  This will copy the result to sys$library
$@make_libgxx
$!
$! now copy the entry point to right place.
$!
$copy libgxx_shr_main.obj GNU_CC:[000000]
$!
$! next install the files for container classes.
$copy genclass.* GNU_CC:[000000]*.*
$!
$! now install the options files required for linking.
$ copy options_shr.opt GNU_CC:[000000]*.*
$! copy options.opt GNU_CC:[000000]*.*   !only required to link non-shared lib
$!
$! now install the command files required to link a user program to the library
$!
$copy cxshare.com GNU_CC:[000000]*.*
$!copy cxlink.com GNU_CC:[000000]*.*  !used to link to non-shared library.
$!
$write sys$output "The installation is complete"
