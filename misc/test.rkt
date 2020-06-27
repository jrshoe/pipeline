#lang racket
(require "logger.rkt")

(pipeline
 (stage
  "setup"
  (steps (setup "python"))
  (steps (env "RUBY_VER" "3"))
  (steps (env "python" "2.6"))
  (steps
   (env "JAVA_VER" "12"))
  (steps (env "foo" "/bar")))
 (stage
  "before_install"
  (steps
   (sh "sudo apt-get update")
   (sh
    "sudo apt-get install -y -q mydependency")))
 (stage
  "install"
  (steps
   (sh "./bin/buildout")))
 (stage
  "after_install"
  (steps))
 (stage
  "excute_script"
  (steps "./bin/test")))