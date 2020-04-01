(ns allpa.runner
  (:require
    [doo.runner :refer-macros [doo-tests]]
    [allpa.test-core]))

(doo-tests 'allpa.test-core)
