# Simpleminded conditional-processor awk script
# to permit use of a single .y source file for C and Objective C.
# If objc=1, the ifobjc conditionals succeed.
# If objc=0, the ifc conditionals succeed.
/^ifobjc$/,/^end ifobjc$/ \
  { if (objc != 0 && $0 != "ifobjc" && $0 != "end ifobjc") print; next }
/^ifc$/,/^end ifc$/       \
  { if (objc == 0 && $0 != "ifc" && $0 != "end ifc") print; next }

{ print }
