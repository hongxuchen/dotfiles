//usr/bin/env go run "$0" "$@"; exit "$?"

package main

import (
    "fmt"
    "os"
)

func main() {
    fmt.Println("Hello", os.Args[1])
    os.Exit(42)
}
