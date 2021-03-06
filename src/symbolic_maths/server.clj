(ns symbolic_maths.server
  (:require [noir.server :as server]))

(server/load-views "src/symbolic_maths/views/")

;;(defpage "/" []
  ;;"hello")

(defn -main [& m]
  (println "inside 'server.clj'")
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8080"))]
    (server/start port {:mode mode
                        :ns 'test-noir})))

