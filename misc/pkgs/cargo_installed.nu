let ctoml = $nu.home-path + "/.cargo/.crates.toml"
open $ctoml | get v1 | columns | each {|e| split row " " | get 0} | str join "\n"
