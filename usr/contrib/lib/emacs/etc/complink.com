$ ! Compile and link those programs in [etc] that are usable on VMS.
$
$ old = f$environment("default")
$ set default emacs_library:[etc]
$ if f$search("test-distrib.c") .nes. "" then goto version44
$
$ cc test_distrib.c
$ link test_distrib.obj,vmslink.opt/opt
$ run test_distrib.exe
$ cc /debug make_docfile.c
$ link make_docfile.obj,vmslink.opt/opt
$ cc /debug digest_doc.c
$ link digest_doc.obj,vmslink.opt/opt
$ cc /debug sorted_doc.c
$ cc /debug qsort.c
$ link sorted_doc.obj,qsort.obj,vmslink.opt/opt
$ cc /debug etags_vmslib.c
$ cc /debug etags.c
$ link etags.obj,etags_vmslib.obj,vmslink.opt/opt
$ goto finish
$
$version44:
$ cc test-distrib.c
$ link test-distrib.obj,vmslink.opt/opt
$ run test-distrib.exe
$ cc /debug make-docfile.c
$ link make-docfile.obj,vmslink.opt/opt
$ cc /debug digest-doc.c
$ link digest-doc.obj,vmslink.opt/opt
$ cc /debug sorted-doc.c
$ cc /debug qsort.c
$ link sorted-doc.obj,qsort.obj,vmslink.opt/opt
$ cc /debug etags-vmslib.c
$ cc /debug etags.c
$ link etags.obj,etags-vmslib.obj,vmslink.opt/opt
$
$finish:
$ cc /debug yow.c
$ link yow.obj,vmslink.opt/opt
$
$ set default 'old'
