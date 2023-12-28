(ns ^:dev/once dojo.dev
  (:require [dojo.core :as core]))


(defn ^:dev/after-load re-render []
  (core/mount-root))

(core/init!)
