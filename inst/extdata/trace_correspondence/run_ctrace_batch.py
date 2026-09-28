#!/usr/bin/env python3
# =============================================================================
# run_ctrace_batch.py -- drives the original C implementation of TRACE
# =============================================================================
# For each target/competitor/control triple in pairs_24.csv, runs the
# repaired original C TRACE (see ctrace_bugfix.patch and the README) with
# the classic p86 parameter file and the full 213-word slex lexicon,
# probing the maximum word-token activation of the target, competitor,
# and control after every cycle (90 cycles) via the added `wmax` command.
# Output: ctrace_trajectories.csv (item, class, words, role, cycle, act).
#
# Requires: the original TRACE C distribution (McClelland & Elman, 1986;
# 2005-modified copy), patched with ctrace_bugfix.patch and built with
# `make trace`, run from the distribution directory containing p.1/ and
# slex. The distribution itself is not redistributed here; see README.
# =============================================================================
import csv, subprocess, sys

PAIRS = "pairs_24.csv"
OUT   = "ctrace_trajectories.csv"
NC    = 90

rows = []
for tr in csv.DictReader(open(PAIRS)):
    k, t, c, u = int(tr["item"]), tr["target"], tr["comp"], tr["ctrl"]
    cmds = ["nreps", "1", f"test -{t}-"]
    for _ in range(NC):
        cmds += ["cycle", f"wmax {t}", f"wmax {c}", f"wmax {u}"]
    cmds.append("quit")
    p = subprocess.run(["./trace", "-p", "p.1/p86", "-l", "slex"],
                       input="\n".join(cmds) + "\n",
                       capture_output=True, text=True, timeout=600)
    for ln in p.stdout.splitlines():
        if "WMAX" in ln:
            f = ln.split("WMAX", 1)[1].split()
            role = "targ" if f[1] == t else ("comp" if f[1] == c else "ctrl")
            rows.append([k, tr["class"], t, c, u, role, int(f[0]), f[2]])
    print(k, tr["class"], t, "done", file=sys.stderr)

with open(OUT, "w", newline="") as fh:
    w = csv.writer(fh)
    w.writerow(["item","class","target","comp","ctrl","role","cycle","act"])
    w.writerows(rows)
print("wrote", OUT, len(rows), "rows", file=sys.stderr)
