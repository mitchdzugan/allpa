(ns ^:figwheel-no-load allpa.dev
  (:require
    [allpa.test-page :as test-page]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(test-page/init!)
