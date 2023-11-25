#!/usr/bin/env python3

import argparse
import sys

import fitz
from prettyformatter import pprint
from rich.console import Console

console = Console()


def main():
    parser = argparse.ArgumentParser(
        description="pdf utility",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "-m", "--metadata", action="store_true", required=False, help="get metadata",
    )
    parser.add_argument(
        "-t", "--toc", action="store_true", required=False, help="get toc",
    )
    parser.add_argument(
        "-c", "--content", action="store_true", required=False, help="get text",
    )
    parser.add_argument(
        "file", metavar="FILE", action="store", help="The pdf file to be analyzed",
    )
    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(1)
    args = parser.parse_args()
    if args.file is None:
        print("pdf file not specified", file=sys.stderr)
        sys.exit(1)
    doc: fitz.Document = fitz.open(args.file)
    if args.metadata:
        console.print("=== metadata ===", style="yellow")
        pprint(doc.metadata)
    if args.toc:
        toc: list[str] = doc.get_toc(simple=False)
        if len(toc) == 0:
            console.print("\n=== Empty TOC ===", style="yellow")
        else:
            console.print("=== table of content ===", style="yellow")
            pprint(toc)
    if args.content:
        console.print("\n === Content ===", style="yellow")
        for idx, page in enumerate(iter(doc)):
            console.print(f"\n === Page {idx+1} ===", style="cyan")
            print(page.get_text())


if __name__ == "__main__":
    main()
