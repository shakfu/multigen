#!python
# type: ignore
# To generate "include/stc/priv/utf8_tab.c" file.

import numpy as np  # type: ignore[import-not-found]
import pandas as pd  # type: ignore[import-untyped]

_UNICODE_DIR = "https://www.unicode.org/Public/15.0.0/ucd"


def read_unidata(casetype="lowcase", category="Lu", bitrange=16):
    """Read Unicode data from UnicodeData.txt.

    Args:
        casetype: Case type to filter ('lowcase', 'upcase', or 'titlecase').
        category: Unicode category to filter (e.g., 'Lu' for uppercase letters).
        bitrange: Bit range to filter (16 for BMP, >16 for supplementary planes).

    Returns:
        DataFrame with Unicode character data.
    """
    df = pd.read_csv(
        _UNICODE_DIR + "/UnicodeData.txt",
        sep=";",
        converters={0: lambda x: int(x, base=16)},
        names=[
            "code",
            "name",
            "category",
            "canclass",
            "bidircat",
            "chrdecomp",
            "decdig",
            "digval",
            "numval",
            "mirrored",
            "uc1name",
            "comment",
            "upcase",
            "lowcase",
            "titlecase",
        ],
        usecols=["code", "name", "category", "bidircat", "upcase", "lowcase", "titlecase"],
    )
    if bitrange == 16:
        df = df[df["code"] < (1 << 16)]
    else:
        df = df[df["code"] >= (1 << 16)]

    if category:
        df = df[df["category"] == category]
    df = df.replace(np.nan, "0")
    for k in ["upcase", "lowcase", "titlecase"]:
        df[k] = df[k].apply(int, base=16)

    if casetype:  # 'lowcase', 'upcase', 'titlecase'
        df = df[df[casetype] != 0]  # remove mappings to 0
    return df


def read_casefold(bitrange):
    """Read case folding data from CaseFolding.txt.

    Args:
        bitrange: Bit range to filter (16 for BMP, >16 for supplementary planes).

    Returns:
        DataFrame with case folding data.
    """
    df = pd.read_csv(
        _UNICODE_DIR + "/CaseFolding.txt",
        engine="python",
        sep="; #? ?",
        comment="#",
        converters={0: lambda x: int(x, base=16)},
        names=["code", "status", "lowcase", "name"],
    )  # comment => 'name'
    if bitrange == 16:
        df = df[df["code"] < (1 << 16)]
    else:
        df = df[df["code"] >= (1 << 16)]

    df = df[df.status.isin(["S", "C"])]
    df["lowcase"] = df["lowcase"].apply(int, base=16)
    return df


def make_caselist(df, casetype):
    """Create a list of case mappings from DataFrame.

    Args:
        df: DataFrame with Unicode data.
        casetype: Case type column to extract ('lowcase', 'upcase', or 'titlecase').

    Returns:
        List of tuples (code, case_mapping, name).
    """
    caselist = []
    for _idx, row in df.iterrows():
        caselist.append((row["code"], row[casetype], row["name"]))
    return caselist


def make_table(caselist):
    """Create a compact table from case mapping list.

    Args:
        caselist: List of tuples (code, case_mapping, name).

    Returns:
        List of table entries with range compression.
    """
    prev_a, prev_b = 0, 0
    diff_a, diff_b = 0, 0
    prev_offs = 0
    n_1 = len(caselist) - 1

    table = []
    for j in range(0, len(caselist)):
        a, b, name = caselist[j]
        offset = b - a

        if abs(diff_a) > 2 or a - prev_a != diff_a or b - prev_b != diff_b or prev_offs != offset:
            if j > 0:  # and start_a not in [0xAB70, 0x13F8]: # BUG in CaseFolding.txt V14
                table.append([start_a, prev_a, prev_b, start_name])  # type: ignore[has-type]
            if j < n_1:
                diff_a = caselist[j + 1][0] - a
                diff_b = caselist[j + 1][1] - b
                start_a = a
                start_name = name

        prev_a, prev_b = a, b
        prev_offs = offset

    table.append((start_a, a, b, start_name))  # type: ignore[arg-type]
    return table


def print_table(name, table, style=1, bitrange=16):
    """Print a case mapping table (currently unused, but kept for reference).

    Args:
        name: Table name.
        table: Table data.
        style: Print style (1 or 2).
        bitrange: Bit range for filtering.
    """
    for a, b, c, _t in table:
        if style == 1:  # first char with name
            b - a + 1 if abs(c - b) != 1 else (b - a) / 2 + 1
        elif style == 2:  # all chars
            n = 0
            for _k in range(a, b + 1, 2 if c - b == 1 else 1):
                n += 1
                if n % 17 == 0:
                    pass


def print_index_table(name, indtab):
    """Print an index table (currently unused, but kept for reference).

    Args:
        name: Table name.
        indtab: Index table data.
    """
    for _i in range(len(indtab)):
        pass


def compile_table(casetype="lowcase", category=None, bitrange=16):
    """Compile a complete case mapping table from Unicode data.

    Args:
        casetype: Case type ('lowcase', 'upcase', or 'titlecase').
        category: Unicode category filter, or None for case folding.
        bitrange: Bit range for filtering (16 for BMP).

    Returns:
        Compiled case mapping table.
    """
    if category:
        df = read_unidata(casetype, category, bitrange)
    else:
        df = read_casefold(bitrange)
    caselist = make_caselist(df, casetype)
    table = make_table(caselist)
    return table


def main():
    """Generate UTF-8 case mapping tables from Unicode data."""
    bitrange = 16

    casemappings = compile_table("lowcase", None, bitrange)  # CaseFolding.txt
    upcase = compile_table("lowcase", "Lu", bitrange)  # UnicodeData.txt uppercase
    lowcase = compile_table("upcase", "Ll", bitrange)  # UnicodeData.txt lowercase

    len(casemappings)

    # add additional Lu => Ll mappings from UnicodeData.txt
    # create upcase_ind: lower => upper index list sorted by mapped lowercase values:
    upcase_ind = []
    for v in upcase:
        try:
            upcase_ind.append(casemappings.index(v))
        except ValueError:
            upcase_ind.append(len(casemappings))
            casemappings.append(v)

    # add additional Ll => Lu mappings from UnicodeData.txt
    # create lowcase_ind: upper => lower index list sorted by uppercase values:
    lowcase_ind = []
    for u in lowcase:
        v = (u[2] - (u[1] - u[0]), u[2], u[1], "")
        try:
            j = next(i for i, x in enumerate(casemappings) if x[0] == v[0] and x[1] == v[1] and x[2] == v[2])
            lowcase_ind.append(j)
        except StopIteration:
            lowcase_ind.append(len(casemappings))
            casemappings.append(v)

    print_table("casemappings", casemappings, style=1, bitrange=bitrange)

    # upcase => low
    upcase_ind.sort(key=lambda i: casemappings[i][0])
    print_index_table("upcase_ind", upcase_ind)

    # lowcase => up. add "missing" SHARP S caused by https://www.unicode.org/policies/stability_policy.html#Case_Pair
    if bitrange == 16:
        lowcase_ind.append(next(i for i, x in enumerate(casemappings) if x[0] == ord("ẞ")))
    lowcase_ind.sort(key=lambda i: casemappings[i][2] - (casemappings[i][1] - casemappings[i][0]))
    print_index_table("lowcase_ind", lowcase_ind)


########### main:

if __name__ == "__main__":
    main()
