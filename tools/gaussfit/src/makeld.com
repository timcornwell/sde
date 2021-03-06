$!  This procedure compiles Gauss
$!
$! on warning then exit
$ define LNK$LIBRARY SYS$LIBRARY:VAXCRTL.OLB
$!                      
$ if f$search("Arithops.obj") .eqs. "" then write SYS$OUTPUT "arithops"
$ if f$search("Arithops.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug ArithOps.c
$ if f$search("Array.obj") .eqs. "" then write SYS$OUTPUT "array"
$ if f$search("Array.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug Array.c
$ if f$search("Datum.obj") .eqs. "" then write SYS$OUTPUT "Datum"
$ if f$search("Datum.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug Datum.c
$ if f$search("Declare.obj") .eqs. "" then write SYS$OUTPUT "Declare"
$ if f$search("Declare.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug Declare.c
$ if f$search("errhandle.obj") .eqs. "" then write SYS$OUTPUT "errhandle "
$ if f$search("Errhandle.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug ErrHandle.c
$ if f$search("export.obj") .eqs. "" then write SYS$OUTPUT "export"
$ if f$search("export.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug export.c
$ if f$search("files.obj") .eqs. "" then write SYS$OUTPUT "files "
$ if f$search("Files.obj") .eqs. "" then cc /define=USEANSI /LIST - 
    /show=(symbols,expansion)  /noopt/debug Files.c
$ if f$search("functs.obj") .eqs. "" then write SYS$OUTPUT "functs "
$ if f$search("Functs.obj") .eqs. "" then cc /define=USEANSI /LIST - 
       /show=(symbols,expansion)  /noopt/debug Functs.c 
$ if f$search("gauss.obj") .eqs. "" then write SYS$OUTPUT "gauss "
$ if f$search("Gauss.obj") .eqs. "" then cc /define=USEANSI /LIST - 
       /show=(symbols,expansion)  /noopt/debug Gauss.c
$ if f$search("house.obj") .eqs. "" then write SYS$OUTPUT "house "
$   if f$search("House.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug House.c
$ if f$search("SymbolTable.obj") .eqs. "" then write SYS$OUTPUT "symboltable"
$ if f$search("SymbolTable.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug SymbolTable.c
$ if f$search("logicops.obj") .eqs. "" then write SYS$OUTPUT "logicops "
$ if f$search("LogicOps.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug LogicOps.c
$ if f$search("matrixman.obj") .eqs. "" then write SYS$OUTPUT "matrixman "
$ if f$search("MatrixMan.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug MatrixMan.c
$ if f$search("opcodes.obj") .eqs. "" then write SYS$OUTPUT "opcodes "
$ if f$search("Opcodes.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug Opcodes.c
$ if f$search("quicksort.obj") .eqs. "" then write SYS$OUTPUT "quicksort "
$ if f$search("Quicksort.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug Quicksort.c
$ if f$search("robust.obj") .eqs. "" then write SYS$OUTPUT "robust "
$ if f$search("Robust.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion,include,intermediate)  /noopt/debug Robust.c
$ if f$search("simplefits.obj") .eqs. "" then write SYS$OUTPUT "simplefits "
$ if f$search("SimpleFits.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug SimpleFits.c
$ if f$search("simplemidas2.obj") .eqs. "" then write SYS$OUTPUT "simplemidas2 "
$ if f$search("SimpleMidas2.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug SimpleMidas2.c
$ if f$search("gfitmain.obj") .eqs. "" then write SYS$OUTPUT "gfitmain "
$ if f$search("gfitmain.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug gfitmain.c
$ if f$search("symtab.obj") .eqs. "" then write SYS$OUTPUT "symtab "
$ if f$search("symtab.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
      /show=(symbols,expansion)  /noopt/debug symtab.c
$ if f$search("minsum.obj") .eqs. "" then write SYS$OUTPUT "minsum "
$ if f$search("minsum.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug minsum.c
$ if f$search("machine.obj") .eqs. "" then write SYS$OUTPUT "machine"
$ if f$search("Machine.obj") .eqs. "" then cc /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug Machine.c
$ if f$search("compile.obj") .eqs. "" then write SYS$OUTPUT "compile"
$ if f$search("compile.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug compile.c
$ if f$search("ytab.obj") .eqs. "" then write SYS$OUTPUT "ytab"
$ if f$search("ytab.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug ytab.c
$ if f$search("util.obj") .eqs. "" then write SYS$OUTPUT "util"
$ if f$search("util.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug util.c
$ if f$search("gfitmain.obj") .eqs. "" then write SYS$OUTPUT "gfitmain"
$ if f$search("gfitmain.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug gfitmain.c
$ if f$search("minsum.obj") .eqs. "" then write SYS$OUTPUT "minsum"
$ if f$search("minsum.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug minsum.c
$ if f$search("ytab.obj") .eqs. "" then write SYS$OUTPUT "ytab"
$ if f$search("ytab.obj") .eqs. "" then cc  /define=USEANSI /LIST - 
     /show=(symbols,expansion)  /noopt/debug ytab.c
$ write sys$output "NOW LINKING"
$link/debug  Gauss.obj + MatrixMan.obj + ErrHandle.obj + Functs.obj + -
       ArithOps.obj + SymTab.obj +  House.obj  + -
       LogicOps.obj + Machine.obj + Opcodes.obj  + Robust.obj + -
       SimpleFits.obj + SimpleMidas2.obj + export.obj; +-
       compile.obj  + ytab.obj + gfitmain.obj + -
       array.obj +  datum.obj + declare.obj + symboltable.obj + -
       minsum.obj +  util.obj  + files.obj + Quicksort.obj 
$!
$if f$search("gauss.exe") .eqs. ""then write sys$output "No executable produced"
$
