#define DEBUG

#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b) traceIfFalse a b
#define TRACE_ERROR(a) traceError a
#define FROM_BUILT_IN_DATA(m, a) case fromBuiltinData a of { Nothing -> TRACE_ERROR(m); Just x -> x }
#define DataConstraint(a) FromData a
#else
#define TRACE_IF_FALSE(a,b) b
#define TRACE_ERROR(a) error ()
#define FROM_BUILT_IN_DATA(m, a) unsafeFromBuiltinData a
#define DataConstraint(a) UnsafeFromData a
#endif
