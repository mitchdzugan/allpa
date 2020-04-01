(defproject allpa "0.1.0"
            :description "FIXME: write description"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[org.clojure/clojure "1.10.0" :scope "provided"]
   [org.clojure/clojurescript "1.10.597" :scope "provided"]]

  :plugins
  [[lein-cljsbuild "1.1.7"]
   [lein-figwheel "0.5.19"]
   [cider/cider-nrepl "0.21.1"]
   [lein-doo "0.1.10"]]

  :clojurescript? true
  :jar-exclusions [#"\.swp|\.swo|\.DS_Store"]
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :profiles
  {:dev
   {:dependencies
    [[ring-server "0.5.0"]
     [ring-webjars "0.2.0"]
     [ring "1.8.0"]
     [ring/ring-defaults "0.3.2"]
     [metosin/reitit "0.3.10"]
     [metosin/reitit-ring "0.3.10"]
     [hiccup "1.0.5"]
     [nrepl "0.6.0"]
     [binaryage/devtools "0.9.11"]
     [cider/piggieback "0.4.2"]
     [figwheel-sidecar "0.5.19"]
     [reagent "0.9.0-rc4"]]

    :source-paths ["src" "env/dev/clj" "env/dev/cljs"]
    :resource-paths ["resources" "env/dev/resources" "target/cljsbuild"]

    :figwheel
    {:server-port      3450
     :nrepl-port       7000
     :nrepl-middleware [cider.piggieback/wrap-cljs-repl
                        cider.nrepl/cider-middleware]
     :css-dirs         ["resources/public/css" "env/dev/resources/public/css"]
     :ring-handler     allpa.server/app}
    :cljsbuild
    {:builds
     {:app
      {:source-paths ["src" "env/dev/cljs"]
       :figwheel     {:on-jsload "allpa.test-page/mount-root"}
       :compiler     {:main          allpa.dev
                      :asset-path    "/js/out"
                      :output-to     "target/cljsbuild/public/js/app.js"
                      :output-dir    "target/cljsbuild/public/js/out"
                      :source-map-timestamp true
                      :source-map    true
                      :optimizations :none
                      :pretty-print  true}}}}}
   :test-cljs
   {:cljsbuild
    {:builds
     {:test-cljs
      {:source-paths ["src" "test"]
       :compiler     {:main          allpa.runner
                      :output-to     "target/test/core.js"
                      :target        :nodejs
                      :optimizations :none
                      :source-map    true
                      :pretty-print  true}}}}
    :doo {:build "test-cljs"}}}
    :aliases
    {"test-cljs"
     ["do"
      ["clean"]
      ["with-profile" "test-cljs" "doo" "node" "once"]]
     "test-cljs-watch"
     ["do"
      ["clean"]
      ["with-profile" "test-cljs" "doo" "node"]]})
