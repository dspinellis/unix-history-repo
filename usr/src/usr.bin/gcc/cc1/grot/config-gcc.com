$ !
$ !	Set up to compile GCC on VMS
$ !
$ echo = "write sys$output"
$ !
$ if f$search("config.h") .nes. "" then delete config.h.*
$ copy [.config]xm-vms.h []config.h
$ echo "Linked `config.h' to `[.config]xm-vms.h'.
$ !
$ if f$search("tm.h") .nes. "" then delete tm.h.*
$ copy [.config]tm-vms.h []tm.h
$ echo "Linked `tm.h' to `[.config]tm-vms.h'.
$ !
$ if f$search("md.") .nes. "" then delete md..*
$ copy [.config]vax.md []md.
$ echo "Linked `md' to `[.config]vax.md'.
$ !
$ if f$search("aux-output.c") .nes. "" then delete aux-output.c.*
$ copy [.config]out-vax.c []aux-output.c
$ echo "Linked `aux-output.c' to `[.config]out-vax.c'.
$ !
$ if f$search("config.status") .nes. "" then delete config.status.*
$ open/write file config.status
$ write file "Links are now set up for use with a vax running VMS."
$ close file
$ type config.status
