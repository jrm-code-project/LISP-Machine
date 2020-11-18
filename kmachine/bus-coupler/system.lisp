;;; This defines the bus coupler system.

(fs:set-logical-pathname-host
  "BC"
  :physical-host "JB"
  :translations
  '(("bus-coupler;" "<k.bus-coupler>")))

(defsystem bus-coupler
  (:name "Bus Coupler")
  (:pathname-default "bc:bus-coupler;")
  (:module bc  ("cd" "md" "mbc"))
  (:compile-load bc))
