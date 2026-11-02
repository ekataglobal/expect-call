(defproject whitepages/expect-call "0.4.0" ;; NOTE: no longer published
  :description "A Clojure library for no-fuss function mocking"
  :url "https://github.com/whitepages/expect-call"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/core.match "1.1.1"]]
  :profiles {:dev {:dependencies [[clj-kondo/clj-kondo "2026.10.26"]]
                   :test-paths ["test-hooks"]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.3"]]}
             :1.11 {:dependencies [[org.clojure/clojure "1.11.4"]]}
             :1.12 {:dependencies [[org.clojure/clojure "1.12.6"]]}})
