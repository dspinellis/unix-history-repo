#
#	gemdos/xbios/bios interface on the atari
#
#  ++jrb	bammi@cadence.com
#

# camel book pp204
sub enum {
    local($_) = @_;
    local(@specs) = split(/,/);
    local($val);
    for(@specs) {
        if(/=/) {
	    $val = eval $_;
        } else {
	    eval $_ . ' = ++$val';
	}
    }
}

# these must match the defines in atarist.c

&enum(<<'EOL');
$_trap_1_w=1, $_trap_1_ww, $_trap_1_wl, $_trap_1_wlw, $_trap_1_www,
$_trap_1_wll, $_trap_1_wwll, $_trap_1_wlww, $_trap_1_wwlll, $_trap_13_w,
$_trap_13_ww, $_trap_13_wl, $_trap_13_www, $_trap_13_wwl, $_trap_13_wwlwww,
$_trap_14_w, $_trap_14_ww, $_trap_14_wl, $_trap_14_www, $_trap_14_wwl,
$_trap_14_wwll, $_trap_14_wllw, $_trap_14_wlll, $_trap_14_wwwl,
$_trap_14_wwwwl, $_trap_14_wllww, $_trap_14_wwwwwww, $_trap_14_wllwwwww,
$_trap_14_wllwwwwlw, $_trap_14_wllwwwwwlw
EOL

sub Pterm0 {
  syscall($_trap_1_w, 0x00);
}
sub Cconin {
  syscall($_trap_1_w, 0x01);
}
sub Cconout {
  syscall($_trap_1_ww, 0x02, @_);
}
sub Cauxin {
  syscall($_trap_1_w, 0x03);
}
sub Cauxout {
  syscall($_trap_1_ww, 0x04, @_);
}
sub Cprnout {
  syscall($_trap_1_ww, 0x05, @_);
}
sub Crawio {
  syscall($_trap_1_ww, 0x06, @_);
}
sub Crawcin {
  syscall($_trap_1_w, 0x07);
}
sub Cnecin {
  syscall($_trap_1_w, 0x08);
}
sub Cconws {
  syscall($_trap_1_wl, 0x09, @_);
}
sub Cconrs {
  syscall($_trap_1_wl, 0x0A, @_);
}
sub Cconis {
  syscall($_trap_1_w, 0x0B);
}
sub Dsetdrv {
  syscall($_trap_1_ww, 0x0E, @_);
}
sub Cconos {
  syscall($_trap_1_w, 0x10);
}
sub Cprnos {
  syscall($_trap_1_w, 0x11);
}
sub Cauxis {
  syscall($_trap_1_w, 0x12);
}
sub Cauxos {
  syscall($_trap_1_w, 0x13);
}
sub Dgetdrv {
  syscall($_trap_1_w, 0x19);
}
sub Fsetdta {
  syscall($_trap_1_wl, 0x1A, @_);
}
sub Super {
  syscall($_trap_1_wl, 0x20, @_);
}
sub Tgetdate {
  syscall($_trap_1_w, 0x2A);
}
sub Tsetdate {
  syscall($_trap_1_ww, 0x2B, @_);
}
sub Tgettime {
  syscall($_trap_1_w, 0x2C);
}
sub Tsettime {
  syscall($_trap_1_ww, 0x2D, @_);
}
sub Fgetdta {
  syscall($_trap_1_w, 0x2F);
}
sub Sversion {
  syscall($_trap_1_w, 0x30);
}
sub Ptermres {
  syscall($_trap_1_wlw, 0x31, @_);
}
sub Dfree {
  syscall($_trap_1_wlw, 0x36, @_);
}
sub Dcreate {
  syscall($_trap_1_wl, 0x39, @_);
}
sub Ddelete {
  syscall($_trap_1_wl, 0x3A, @_);
}
sub Dsetpath {
  syscall($_trap_1_wl, 0x3B, @_);
}
sub Fcreate {
  syscall($_trap_1_wlw, 0x3C, @_);
}
sub Fopen {
  syscall($_trap_1_wlw, 0x3D, @_);
}
sub Fclose {
  syscall($_trap_1_ww, 0x3E, @_);
}
sub Fread {
  syscall($_trap_1_wwll, 0x3F, @_);
}
sub Fwrite {
  syscall($_trap_1_wwll, 0x40, @_);
}
sub Fdelete {
  syscall($_trap_1_wl, 0x41, @_);
}
sub Fseek {
  syscall($_trap_1_wlww, 0x42, @_);
}
sub Fattrib {
  syscall($_trap_1_wlww, 0x43, @_);
}
sub Fdup {
  syscall($_trap_1_ww, 0x45, @_);
}
sub Fforce {
  syscall($_trap_1_www, 0x46, @_);
}
sub Dgetpath {
  syscall($_trap_1_wlw, 0x47, @_);
}
sub Malloc {
  syscall($_trap_1_wl, 0x48, @_);
}
sub Mfree {
  syscall($_trap_1_wl, 0x49, @_);
}
sub Mshrink {
  syscall($_trap_1_wwll, 0x4A, @_);
}
sub Pexec {
  syscall($_trap_1_wwlll, 0x4B, @_);
}
sub Pterm {
  syscall($_trap_1_ww, 0x4C, @_);
}
sub Fsfirst {
  syscall($_trap_1_wlw, 0x4E, @_);
}
sub Fsnext {
  syscall($_trap_1_w, 0x4F);
}
sub Frename {
  syscall($_trap_1_wwll, 0x56, @_);
}
sub Fdatime {
  syscall($_trap_1_wlww, 0x57, @_);
}
sub Getmpb {
  syscall($_trap_13_wl, 0x00, @_);
}
sub Bconstat {
  syscall($_trap_13_ww, 0x01, @_);
}
sub Bconin {
  syscall($_trap_13_ww, 0x02, @_);
}
sub Bconout {
  syscall($_trap_13_www, 0x03, @_);
}
sub Rwabs {
  syscall($_trap_13_wwlwww, 0x04, @_);
}
sub Setexc {
  syscall($_trap_13_wwl, 0x05, @_);
}
sub Tickcal {
  syscall($_trap_13_w, 0x06);
}
sub Getbpb {
  syscall($_trap_13_ww, 0x07, @_);
}
sub Bcostat {
  syscall($_trap_13_ww, 0x08, @_);
}
sub Mediach {
  syscall($_trap_13_ww, 0x09, @_);
}
sub Drvmap {
  syscall($_trap_13_w, 0x0A);
}
sub Kbshift {
  syscall($_trap_13_ww, 0x0B, @_);
}
sub Getshift {
  &Kbshift(-1);
}
sub Initmous {
  syscall($_trap_14_wwll, 0x00, @_);
}
sub Ssbrk {
  syscall($_trap_14_ww, 0x01, @_);
}
sub Physbase {
  syscall($_trap_14_w, 0x02);
}
sub Logbase {
  syscall($_trap_14_w, 0x03);
}
sub Getrez {
  syscall($_trap_14_w, 0x04);
}
sub Setscreen {
  syscall($_trap_14_wllw, 0x05, @_);
}
sub Setpallete {
  syscall($_trap_14_wl, 0x06, @_);
}
sub Setcolor {
  syscall($_trap_14_www, 0x07, @_);
}
sub Floprd {
  syscall($_trap_14_wllwwwww, 0x08, @_);
}
sub Flopwr {
  syscall($_trap_14_wllwwwww, 0x09, @_);
}
sub Flopfmt {
  syscall($_trap_14_wllwwwwwlw, 0x0A, @_);
}
sub Midiws {
  syscall($_trap_14_wwl, 0x0C, @_);
}
sub Mfpint {
  syscall($_trap_14_wwl, 0x0D, @_);
}
sub Iorec {
  syscall($_trap_14_ww, 0x0E, @_);
}
sub Rsconf {
  syscall($_trap_14_wwwwwww, 0x0F, @_);
}
sub Keytbl {
  syscall($_trap_14_wlll, 0x10, @_);
}
sub Random {
  syscall($_trap_14_w, 0x11);
}
sub Protobt {
  syscall($_trap_14_wllww, 0x12, @_);
}
sub Flopver {
  syscall($_trap_14_wllwwwww, 0x13, @_);
}
sub Scrdmp {
  syscall($_trap_14_w, 0x14);
}
sub Cursconf {
  syscall($_trap_14_www, 0x15, @_);
}
sub Settime {
  syscall($_trap_14_wl, 0x16, @_);
}
sub Gettime {
  syscall($_trap_14_w, 0x17);
}
sub Bioskeys {
  syscall($_trap_14_w, 0x18);
}
sub Ikbdws {
  syscall($_trap_14_wwl, 0x19, @_);
}
sub Jdisint {
  syscall($_trap_14_ww, 0x1A, @_);
}
sub Jenabint {
  syscall($_trap_14_ww, 0x1B, @_);
}
sub Giaccess {
  syscall($_trap_14_www, 0x1C, @_);
}
sub Offgibit {
  syscall($_trap_14_ww, 0x1D, @_);
}
sub Ongibit {
  syscall($_trap_14_ww, 0x1E, @_);
}
sub Xbtimer {
  syscall($_trap_14_wwwwl, 0x1E, @_);
}
sub Dosound {
  syscall($_trap_14_wl, 0x20, @_);
}
sub Setprt {
  syscall($_trap_14_ww, 0x21, @_);
}
sub Kbdvbase {
  syscall($_trap_14_w, 0x22);
}
sub Kbrate {
  syscall($_trap_14_www, 0x23, @_);
}
sub Prtblk {
  syscall($_trap_14_wl, 0x24, @_);
}
sub Vsync {
  syscall($_trap_14_w, 0x25);
}
sub Supexec {
  syscall($_trap_14_wl, 0x26, @_);
}
sub Blitmode {
  syscall($_trap_14_ww, 0x40, @_);
}
sub Mxalloc {
  syscall($_trap_1_wlw, 0x44, @_);
}
sub Maddalt {
  syscall($_trap_1_wll, 0x14, @_);
}
sub Setpalette {
  syscall($_trap_14_wl, 0x06, @_);
}
sub EsetShift {
  syscall($_trap_14_ww, 80, @_);
}
sub EgetShift {
  syscall($_trap_14_w, 81);
}
sub EsetBank {
  syscall($_trap_14_ww, 82, @_);
}
sub EsetColor {
  syscall($_trap_14_www, 83, @_);
}
sub EsetPalette {
  syscall($_trap_14_wwwl, 84, @_);
}
sub EgetPalette {
  syscall($_trap_14_wwwl, 85, @_);
}
sub EsetGray {
  syscall($_trap_14_ww, 86, @_);
}
sub EsetSmear {
  syscall($_trap_14_ww, 87, @_);
}
sub Bconmap {
  syscall($_trap_14_ww, 0x2b, @_);
}
sub Bconctl {
  syscall($_trap_14_wwl, 0x2d, @_);
}

1;
