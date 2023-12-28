(ns dojo.core-test
  (:require [dojo.test :as dt]
            [dojo.core]
            [clojure.test :as t]))


(t/deftest test-dojo

  (dt/read-ns 'dojo)
  (dt/read-ns 'xtdb)
  (dt/read-ns 'demo)


  (dt/truncate)



  (dt/save
        {:id "doc-1"
         :resourceType "DocumentReference"
         :period {:start "2023-10-10"}
         :type {:coding [{:display "Tilt table study" :system "loinc" :code "46213-5"}]}
         :content [{:attachment {:contentType "pdf" :url "..."}}]})


  (dt/save
        {:id "doc-1"
         :resourceType "DocumentReference"
         :period {:start "2023-10-10"}
         :type {:coding [{:display "Holter monitor study" :system "loinc" :code "18754-2"}]}
         :content [{:attachment {:contentType "pdf" :url "..."}}]})

  (dt/save
        {:resourceType "Observation",
         :id "body-temperature",
         :status "final",
         :category [{:coding [{:system "http://terminology.hl7.org/CodeSystem/observation-category", :code "vital-signs", :display "Vital Signs"}]}],
         :code
         {:coding
          [{:system "http://loinc.org", :code "8310-5", :display "Body temperature"}],
          :text "Body Temperature"},
         :effectiveDateTime "2023-12-27T14:57",
         :valueQuantity
         {:value 37.1,
          :unit "°C",
          :system "http://unitsofmeasure.org",
          :code "Cel"}})

  (dt/match-op
   'dojo/timeline {}
   {:result
    [{:type "DocumentReference",
      :date "2023-10-10",
      :display "Tilt table study"}
     {:type "Observation",
      :date "2023-12-27",
      :display "Body Temperature"}]})


  )
