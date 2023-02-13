"""Algorithms for parsing freebird documents."""

import re
import pathlib

import toml


def read_header(filepath: pathlib.Path) -> dict:
    """
    Reads freebird header into a dict.

    Parameters
    ----------
    filepath: pathlib.Path
        A filepath to the freebird document.

    Returns
    -------
    dict:
        a dict corresponding to the TOML in the header.

    """
    with open(filepath, encoding="utf-8") as file:
        pattern = re.compile(r"FREEBIRD-->\n(.*)<--", flags=re.M | re.S)
        header_text = re.match(pattern, file.read())
        return toml.loads(header_text.group(1))


def tangle(filepath: pathlib.Path, output_filetype: str):
    """
    Tangles a freebird document into source code.

    Parameters
    ----------
    file: pathlib.Path
        The path to the file object to be parsed.
    output_filetype: str
        The file extension of the output file.
    """
    output_filename = filepath.with_suffix(output_filetype)

    with open(filepath, "r", encoding="utf-8") as file:
        with open(output_filename, "w", encoding="utf-8") as output:
            pattern = re.compile(
                r"(?:>\* |> )(.*\n+)|(?:>BEGIN\n)([\S\s]*\n)(?:>END\n)"
            )
            tangled_text = re.findall(pattern, file.read())
            # flatten capture group tuples down into just strings and
            # remove final newline so end of file doesn't have 2 newlines
            tangled_text = [x[0] if x[1] == "" else x[1] for x in tangled_text]
            tangled_text[-1] = tangled_text[-1].strip("\n")
            output.write("".join(tangled_text) + "\n")


def weave(filepath: pathlib.Path, output_filetype: str, begin_src: str, end_src: str):
    """
    Weaves a freebird document into documentation.

    Parameters
    ----------
    file: pathlib.Path
        The path to the file object to be parsed.
    output_filetype: str
        The file extension of the output file.
    begin_src: str
        The documentation type's marker for beginning source code blocks.
    end_src: str
        The documentation type's marker for ending source code blocks.
    """
    output_filename = filepath.with_suffix(output_filetype)

    with open(filepath, encoding="utf-8") as file:
        with open(output_filename, "w", encoding="utf-8") as output:
            file_contents = file.read()

            replacements = [
                (r"FREEBIRD-->\n[\S\s]*<--\n", ""),  # remove FREEBIRD header
                # remove hidden code lines at end of hidden code block
                (r">\* .*\n", ""),
                # remove hidden code lines at start of hidden code block
                (r"\n>\* .*", ""),
                # remove remaining hidden code lines
                (r"\n>\* .*", ""),
                # replace multiline >BEGIN command with src syntax
                (r">BEGIN\n", f"{begin_src}\n"),
                # replace multiline >END command with src syntax
                (r">END\n", f"{end_src}\n"),
                # surround single-line code with src syntax
                (r"\n\n> (.*)\n\n", rf"\n\n{begin_src}\n\g<1>\n{end_src}\n\n"),
                # replace start of multi-line > blocks with src syntax
                (r"\n\n> ", f"\n\n{begin_src}\n"),
                # replace end of multi-line > blocks with src syntax
                (r"> (.*)\n\n", rf"\g<1>\n{end_src}\n"),
                # remove any remaining Bird arrows
                (r"> ", ""),
            ]

            for pattern, replacement in replacements:
                file_contents = re.sub(pattern, replacement, file_contents)

            output.write(file_contents)
