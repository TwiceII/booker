(ns booker.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [booker.core-test]
   [booker.common-test]))

(enable-console-print!)

(doo-tests 'booker.core-test
           'booker.common-test)
