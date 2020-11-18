
set-up-boot-progress
        ((m-b) a-disk-run-light)
        ;;get to beginning of line four lines above run bars
        ((m-b) sub m-b (a-constant (eval (+ 32. 32. 32. 32. 12.))))
        ((a-boot-progress-base) m-b)

        ;;draw four whole lines worth of zeros starting one line
        ;;above a-boot-progress-base
        ((md) setz)
        ((m-c) (a-constant (* 32. 4)))
        ((m-d) sub m-b (a-constant 32.))

clear-boot-progess-field
        ((vma-start-write) m-d)
        (check-page-write-map-reload-only)
        ((m-d) add m-d (a-constant 1))
        ((m-c) sub m-c (a-constant 1))
        (jump-not-equal m-c a-zero clear-boot-progress-field)

        ;;now draw a line with a bit for every 128. pages
        ((md) seto)
        ((m-d) a-boot-progress-base)
        ((m-d) add m-d (a-constant 32.))
        ((vma-start-read) (a-constant (+ 400 %sys-com-valid-size)))
        (illop-if-page-fault)
        ((m-c) ldb (byte 20. 12.) md)           ;divide by 128 * 32
draw-boot-progress-baseline
        ((vma-start-write) m-d)
        (check-page-write-map-reload-only)
        ((m-d) add m-d (a-constant 1))
        ((m-c) sub m-c (a-constant 1))
        (jump-not-equal m-c a-zero draw-boot-progress-baseline)

        (popj)

update-boot-progress
        ((m-tem) a-number-of-pages-copied)
        ((m-tem) ldb (byte 20. 12.) m-tem)
        ((vma) a-boot-progress-base)
        ((md) seto)
draw-boot-progress-full-words
        ((vma-start-write) vma)
        (check-page-write-map-reload-only)
        ((vma) add vma (a-constant 1))
        ((m-tem) sub m-tem (a-constant 1))
        (jump-not-equal m-tem a-zero draw-boot-progress-full-words)

        ((m-tem) a-number-of-pages-copied)
        ((m-tem) ldb (byte 5 7) m-tem)          ;sort of remainder by 32.
#+lambda((m-tem) sub m-tem (a-constant 1))
        ((oa-mod-low) oal-bytl-1 m-tem)
        ((md) dpb m-minus-one (byte 0 0) a-zero)
        ((vma-start-write) vma)
        (check-page-write-map-reload-only)
        (popj)
