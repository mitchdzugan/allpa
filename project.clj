(defproject allpa "0.1.9"
  :description "FIXME: write description"
  :url "https://github.com/mitchdzugan/allpa"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies
  [[org.clojure/clojure "1.10.1" :scope "provided"]
   [org.clojure/clojurescript "1.10.597" :scope "provided"]]

  :plugins
  [[lein-cljsbuild "1.1.7"]
   [lein-figwheel "0.5.19"]
   [cider/cider-nrepl "0.21.1"]
   [lein-doo "0.1.10"]
   [lein-cloverage "1.0.13"]
   [lein-shell "0.5.0"]
   [lein-ancient "0.6.15"]
   [lein-changelog "0.3.2"]]
  :deploy-repositories [["releases" :clojars]]
  :release-tasks [["shell" "git" "diff" "--exit-code"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["changelog" "release"]
                  ["update-readme-version"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["vcs" "push"]]

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
     [metosin/reitit "0.4.2"]
     [metosin/reitit-ring "0.4.2"]
     [hiccup "1.0.5"]
     [nrepl "0.7.0"]
     [binaryage/devtools "1.0.0"]
     [cider/piggieback "0.4.2"]
     [figwheel-sidecar "0.5.19"]
     [reagent "0.10.0"]]

    :source-paths ["src" "env/dev/clj" "env/dev/cljs"]
    :resource-paths ["resources" "env/dev/resources" "target/cljsbuild"]

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
  :aliases {"update-readme-version" ["shell"
                                     "sed"
                                     "-i"
                                     "s/\\\\[allpa \"[0-9.]*\"\\\\]/[allpa \"${:version}\"]/"
                                     "README.md"]
            "test-cljs"
            ["do"
             ["clean"]
             ["with-profile" "test-cljs" "doo" "node" "once"]]
            "test-cljs-watch"
            ["do"
             ["clean"]
             ["with-profile" "test-cljs" "doo" "node"]]})
