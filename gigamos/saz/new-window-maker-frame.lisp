
(DEFFLAVOR new-WINDOW-MAKER-frame
           ((*frame* nil))
           (
            tv:select-mixin
            tv:inferiors-not-in-select-menu-mixin
            tv:bordered-constraint-frame-with-shared-io-buffer
            tv:window tv:process-mixin
            )
  (:default-init-plist
    :label nil
    :save-bits T
    :panes
    '((title-pane tv:window
                  :label nil :blinker-p nil :save-bits t
                  :reverse-video-p t
                  :deexposed-typeout-action :permit)
      (menu-pane tv:command-menu :item-list ("menu"))
      (instrument-pane tv:window :label nil :blinker-p nil :save-bits t)
      (documentation-pane tv:window :label nil :blinker-p nil :save-bits t)
      (graphics-pane graphic-window :label nil :blinker-p nil
                     :save-bits t :deexposed-typeout-action :permit)
      (window-properties-pane zwei:zmail-choose-variable-values-pane :label "Window Properties" :blinker-p nil :save-bits t))
    :constraints
    '((main . ((title-and-instrument-and-menu-pane graphics-and-window-properties-pane)
               ((title-and-instrument-and-menu-pane :horizontal (0.1)
                                                    (title-pane instrument-pane menu-pane)
                                                    ((title-pane 65. :characters))
                                                    ((menu-pane .55))
                                                    ((instrument-pane :even))))
               ((graphics-and-window-properties-pane :horizontal (0.9)
                                                     (graphics-pane window-properties-pane)
                                                     ((graphics-pane .733))
                                                     ((window-properties-pane :even))))))))
  :INITTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES
  :SETTABLE-INSTANCE-VARIABLES
  (:special-instance-variables *frame*))
