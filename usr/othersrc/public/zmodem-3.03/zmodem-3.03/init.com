$define lnk$library  sys$library:vaxcrtl.olb
$sz :== $dua7:[802873]sz.exe
$rz :== $dua7:[802873]rz.exe
$crc :== $dua7:[802873]crc.exe
$crcb :== $dua7:[802873]crcb.exe
