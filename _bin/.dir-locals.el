;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil .
      ((eval . (dolist (extra-path '("/usr/lib/llvm-2.9/include"))
                 (add-to-list 'ffap-c-path extra-path)))))
 (nil (ac-clang-flags "-x c++" "-I/usr/lib/llvm-2.9/include" "-D_GNU_SOURCE" "-D__STDC_LIMIT_MACROS" "-D__STDC_CONSTANT_MACROS"))
 )
