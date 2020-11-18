@library[lisp]
@begin document

@heading Primops


@subheading Primitive Lisp Functions


@defun eq x y
@end defun

@subsubheading Fixnum Arithmetic


@defun zerop number
@end defun

@defun minusp number
@end defun

@defun plusp number
@end defun

@defun = number1 number2
@end defun

@defun < n1 n2
@end defun

@defun > n1 n2
@end defun

@defun >= n1 n2
@end defun

@defun <= n1 n2
@end defun

@defun 2-arg-logand n1 n2
@end defun

@defun 2-arg-logxor n1 n2
@end defun

@defun 2-arg-logior n1 n2
@end defun

@defun 2-arg-+ n1 n2
@end defun

@defun 2-arg-- n1 n2
@end defun

@defun 1+ n
@end defun

@defun 1- n

@end defun

@defun ash n nbits
*** this doesn't work yet ***
@end defun

@defun byte size position
@end defun

@subheading Hardware Primops

@defun hw:nop
@end defun

@defun hw:write-map value
@end defun

@defun hw:write-gc-ram value
@end defun

@defun hw:write-transporter-ram value
@end defun

@defun hw:ch-topen
@end defun

@defun hw:ch-tcall
@end defun

@defun hw:dispatch pc
@end defun

@defun hw:jump fcn
@end defun

@subsubheading Untyped Arithmetic


@defun hw:dpb value byte-spec word
@end defun

@defun hw:dpb-xor value byte-spec word
@end defun

@defun hw:dpb-unboxed value byte-spec word
@end defun

@defun hw:ldb from byte-spec into
@end defun

@defun hw:32logbitp index n
@end defun

@defun hw:32= n1 n2
@end defun

@defun hw:32>= n1 n2
@end defun

@defun hw:32logand n1 n2
@end defun

@defun hw:32logxor n1 n2
@end defun

@defun hw:32logior n1 n2
@end defun

@defun hw:32-1+ n
@end defun

@defun hw:32-2+ n
@end defun

@defun hw:32-4+ n
@end defun

@defun hw:32+ n1 n2
@end defun

@defun hw:32- n1 n2
@end defun

@defun hw:24= n1 n2
@end defun

@defun hw:24+ n1 n2

@subheading Compiler Internal Primops

@defun Y ???
@end defun

@defun conditional test-primop &rest args
@end defun

@defun test ???
@end defun

@defun true? value
@end defun

@defun setq-lexical var value
@end defun

@defun setq-special symbol value
@end defun

@defun special-ref symbol
@end defun

@defun function-ref symbol
@end defun

@defun optional-setup first-label nargs supplied-p-vars &rest init-labels
@end defun

@defun noop
@end defun

@defun open-frame call
@end defun

@defun read-functional-source fsource
@end defun

@defun write-functional-dest fdest value
@end defun

@defun funcall-internal f
@end defun


@heading Hardware Macros

@defmac hw:unboxed-constant n
@end defmac

@subheading Functional Sources

@defmac hw:read-processor-status
Read the PROCESSOR-STATUS register.
@end defmac

@defmac hw:read-processor-control
Read the PROCESSOR-CONTROL register.
@end defmac

@defmac hw:read-open-active-return
Read the OPEN-ACTIVE-RETURN register.
@end defmac

@defmac hw:read-return-pc-return-dest
Read the RETURN-PC-RETURN-DEST register
@end defmac

@defmac hw:read-call-sp-hp
Read the CALL-SP-HP register.
@end defmac

@defmac hw:read-gc-ram
Read the GC-RAM.
@end defmac

@defmac hw:read-map
Read the MEMORY-MAP.
@end defmac

@defmac hw:read-memory-control
Read the MEMORY-CONTROL register.
@end defmac

@defmac hw:read-memory-status
@end defmac

@defmac hw:read-trap-pc
Read the TRAP-PC.
@end defmac

@defmac hw:read-vma
Read the VMA.
@end defmac

@defmac hw:read-md
Read the MD.
@end defmac

@defmac hw:trap-off
Turn off the global trap flag and return its value
in the low bit (the other bits are garbage).
@end defmac


@subheading Functional Destinations


@defmac hw:datatype-ram-write-pulse
@end defmac

@defmac hw:write-processor-control
Write the PROCESSOR-CONTROL register.
@end defmac

@defmac hw:write-open-active-return
Write the OPEN-ACTIVE-RETURN register.
@end defmac

@defmac hw:write-call-stack
Write the CALL-STACK.
@end defmac

@defmac hw:write-call-sp-hp
Write the CALL-SP-HP.
@end defmac

@defmac hw:write-memory-control
Write the MEMORY-CONTROL
@end defmac

@defmac hw:write-microsecond-clock
Write the MICROSECOND-CLOCK
@end defmac

@defmac hw:write-statistics-counter
Write the STATISTICS-COUNTER
@end defmac

@subsubheading The MD


@defmac hw:write
Write the MD
@end defmac

@defmac hw:md-start-write-no-gc-trap
@end defmac

@defmac hw:md-start-write
@end defmac


@subsubheading The VMA


@defmac hw:write-vma
Write the VMA
@end defmac

@subsubheading VMA-START-WRITE

@defmac hw:vma-start-write-no-gc-trap
@end defmac

@defmac hw:vma-start-write
@end defmac

@subsubheading VMA-START-READ


@defmac hw:vma-start-read-no-transport
@end defmac

@defmac hw:vma-start-read
@end defmac

@defmac hw:vma-start-read-visible-evcp
@end defmac

@defmac hw:vma-start-read-will-write
@end defmac

@defmac hw:vma-start-read-cdr-no-transport
@end defmac

@defmac hw:vma-start-read-cdr
@end defmac

@defmac hw:vma-start-read-cdr-visible-evcp
@end defmac

@defmac hw:vma-start-read-cdr-will-write

@end defmac

@defmac hw:vma-start-read-early-no-transport
@end defmac

@defmac hw:vma-start-read-early
@end defmac

@defmac hw:vma-start-read-early-visible-evcp
@end defmac

@defmac hw:vma-start-read-early-will-write
@end defmac

@defmac hw:vma-start-read-early-cdr-no-transport
@end defmac

@defmac hw:vma-start-read-early-cdr
@end defmac

@defmac hw:vma-start-read-early-cdr-visible-evcp
@end defmac

@defmac hw:vma-start-read-early-cdr-will-write
@end defmac


@end[document]
