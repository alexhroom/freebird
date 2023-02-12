"""Main app for freebird parser."""

import src.parse
import argparse
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(
        description="Lightweight literate programming format."
    )

    parser.add_argument(
        "path", type=str, help="The path to the freebird document or directory."
    )
    parser.add_argument(
        "--recursive, -r",
        type=bool,
        default=False,
        help="If true, runs on a directory rather than a single file.",
    )

    args = parser.parse_args()
    path = Path(args.path)

    header = src.parse.read_header(path)

    src.parse.weave(path, header["weave"], header["begin_src"], header["end_src"])
    src.parse.tangle(path, header["tangle"])


if __name__ == "__main__":
    main()
