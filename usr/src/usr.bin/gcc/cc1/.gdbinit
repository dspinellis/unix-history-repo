define pr
set debug_rtx ($)
end

document pr
Print the full structure of the rtx that is $.
Works only when an inferior is executing.
end

define pt
set debug_tree ($)
end

document pt
Print the full structure of the tree that is $.
Works only when an inferior is executing.
end

define ptc
output (enum tree_code) $.common.code
echo \n
end

document ptc
Print the tree-code of the tree node that is $.
end

define pdn
output $.decl.name->identifier.pointer
echo \n
end

document pdn
Print the name of the decl-node that is $.
end

define prc
output (enum rtx_code) $.code
echo \ (
output $.mode
echo )\n
end

document prc
Print the rtx-code and machine mode of the rtx that is $.
end

define pi
print $.fld[0].rtx@7
end

document pi
Print the fields of an instruction that is $.
end

# Don't let abort actually run, as it will make
# stdio stop working and therefore the `pr' command below as well.
b abort
