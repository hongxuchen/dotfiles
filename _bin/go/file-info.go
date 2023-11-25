/// 2>/dev/null ; gorun "$0" "$@" ; exit $?

package main

import (
    "fmt"
    "os"
    "net/http"
)

func GetFileContentType(out *os.File) (string, error) {
    buf := make([]byte, 512)
    _, err := out.Read(buf)
    if err != nil {
        return "", err
    }
    contentType := http.DetectContentType(buf)
    return contentType, nil
}

func main() {

    if len(os.Args) <= 1 {
        fmt.Printf("usage: %s message\n", os.Args[0])
        os.Exit(1)
    }
    fname := os.Args[1]
    info, err := os.Lstat(fname)
    if err != nil {
        fmt.Fprintln(os.Stderr, "Error: ", err)
        os.Exit(1)
    }
    mode := info.Mode()

    f, err := os.Open(fname)
    if err != nil {
        fmt.Fprintln(os.Stderr, "Error: ", err)
        os.Exit(1)
    }
    defer f.Close()

    contentType, err := GetFileContentType(f)
    if err != nil {
        fmt.Fprintln(os.Stderr, "Error: ", err)
        os.Exit(1)
    }

    fmt.Printf("%s:\t%s\n %s\n", fname, contentType, mode)
    os.Exit(0)
}
