
enum {
	fnSqrtFn,		
	fnSinFn,		
	fnCosFn,		
	fnLogFn,		
	fnLog10Fn,				
	fnExpFn,				
	fnTanFn,				
	fnArcSinFn,				
	fnArcCosFn,				
	fnArcTanFn,				
	fnAbsFn,
	fnImport,
	fnExport,
	fnExportConstr,
	fnExport2,
	fnTriang,
	fnNumFns
};

typedef struct {
	int fnindex;
	opfunptr func;
	char *name;
} BuiltIn;

BuiltIn builtins[fnNumFns] = {
	fnSqrtFn,			(opfunptr)SqrtFn,			"sqrt",		
	fnSinFn,			(opfunptr)SinFn,			"sin",		
	fnCosFn,			(opfunptr)CosFn,			"cos",		
	fnLogFn,			(opfunptr)LogFn,			"log",		
	fnLog10Fn,			(opfunptr)Log10Fn,			"log10",				
	fnExpFn,			(opfunptr)ExpFn,			"exp",				
	fnTanFn,			(opfunptr)TanFn,			"tan",				
	fnArcSinFn,			(opfunptr)ArcSinFn,			"asin",				
	fnArcCosFn,			(opfunptr)ArcCosFn,			"acos",				
	fnArcTanFn,			(opfunptr)ArcTanFn,			"atan",				
	fnAbsFn,			(opfunptr)AbsFn,			"abs",		
	fnImport,			(opfunptr)import,			"import",
	fnExport,			(opfunptr)export,			"export",
	fnExportConstr,		(opfunptr)exportconstraint,	"exportconstraint",
	fnExport2,			(opfunptr)export2,			"export2",
	fnTriang,			(opfunptr)triangularize,	"triangularize"
};
