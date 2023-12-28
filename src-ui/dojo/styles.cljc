(ns dojo.styles
  (:require
   [stylo.core :refer [c]]))

(def h1 (c [:text-xl] [:pt 4] [:pb 1] [:mb 2] :border-b))

(def h2 (c [:text-lg] [:pt 2] [:pb 0] [:mb 1]))

(def link (c [:text :blue-600] [:hover :border-b [:text :blue-700]]))

(def btn-c (c :border :rounded [:px 3] [:py 0.5] [:bg :blue-600] [:text :white]
              [:hover [:bg :blue-700] ]))

(def gbtn-c (c :border :rounded [:px 3] [:py 0.5] [:bg :green-600] [:text :white]
               [:hover [:bg :green-700] ]))

(def obtn-c (c :border :rounded [:px 3] [:py 0.5] [:bg :orange-600] [:text :white]
               [:hover [:bg :orange-700] ]))
