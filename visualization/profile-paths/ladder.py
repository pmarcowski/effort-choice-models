# -*- coding: utf-8 -*-

"""
The Effort Ladder

A sequence of choices along an effort scale in percent of maximum voluntary
contraction (% MVC). Choice d offers the same outcome at two neighboring
efforts, level d − 1 or level d: the down branch takes the easier option, the
up branch the harder one. Every decision maker faces the same pair at choice d
whatever it chose before, so each choice reads its value function's slope
between two neighboring efforts. Taking the higher value, monotonic profiles
climb or descend throughout and nonmonotonic ones turn where their value
function turns; sampling from the softmax rule, a path also wavers wherever
its value function is flat.

Effort enters the model as a proportion of the ladder's maximum, E = level /
maximum, as the paper normalizes stairs by the most it used (270); with the
default maximum of 100% MVC, E is simply % MVC / 100.

With equal steps the tree recombines into a lattice: after c choices a
decision maker sits at node (c, k), where k counts harder minus easier
choices, so every choice moves k by one.
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "GPL 3.0"

from dataclasses import dataclass, field

NOMINAL = 1.0  # nominal value x of the outcome offered at every choice


class Ladder:
    """Effort levels in % MVC, evenly spaced from 0 to the maximum."""

    def __init__(self, choices=20, max_mvc=100.0):
        if choices < 1:
            raise ValueError("need at least one choice")
        if max_mvc <= 0:
            raise ValueError("the maximum effort must be positive")
        self.n = int(choices)
        self.max_mvc = float(max_mvc)
        self.step = self.max_mvc / self.n
        self.levels = [i * self.step for i in range(self.n + 1)]

    def effort(self, i):
        """Effort of level i as a proportion of the ladder's maximum."""
        return i / self.n

    def label(self, i):
        """Level i in % MVC, as text: 35, or 37.5 or 33.33 when steps are fractional."""
        return f"{round(self.levels[i], 2):g}"

    @staticmethod
    def options(d):
        """Level indices (easier, harder) offered at choice d, counted from 1."""
        return d - 1, d

    def lattice_edges(self):
        """Every edge of the lattice of possible paths: (choice, k_from, k_to)."""
        return [
            (d, k, k + step)
            for d in range(1, self.n + 1)
            for k in range(-(d - 1), d, 2)
            for step in (-1, 1)
        ]


@dataclass
class Walk:
    """One decision maker's sequence of choices along the ladder."""

    profile: object
    sampled: bool = False  # choices drawn from the softmax rule, not the higher value
    choices: list = field(default_factory=list)  # True = the harder option
    p_up: list = field(default_factory=list)  # softmax probability of the harder option
    ks: list = field(default_factory=lambda: [0])  # lattice row after each choice
    taken: list = field(default_factory=list)  # level taken at each choice
    values: list = field(default_factory=list)  # subjective value of each outcome taken

    @property
    def key(self):
        return self.profile.key

    def harder(self, upto=None):
        """Times the harder option was chosen, over the first `upto` choices."""
        return sum(self.choices[:upto])

    def value(self, upto=None):
        """Subjective value of the outcomes collected over the first `upto` choices."""
        return sum(self.values[:upto])

    def reversals(self):
        """Columns where the path reverses, as (column, 'peak' or 'trough')."""
        return [
            (c, "peak" if self.choices[c - 1] else "trough")
            for c in range(1, len(self.choices))
            if self.choices[c - 1] != self.choices[c]
        ]

    def turn(self):
        """Where the path turns, as (column, 'peak' or 'trough'), or None.

        Taking the higher value, a path reverses at most once, where its value
        function turns. A sampled path also wavers a step or two wherever its
        value function is flat, so its turn is its highest peak or lowest trough,
        if that lies at least three rows beyond both ends of the path; held over
        several columns, it is placed at the middle one.
        """
        margin = 3 if self.sampled else 1
        ends = (self.ks[0], self.ks[-1])
        rise = max(self.ks) - max(ends)
        fall = min(ends) - min(self.ks)
        if max(rise, fall) < margin:
            return None
        kind, row = ("peak", max(self.ks)) if rise >= fall else ("trough", min(self.ks))
        cols = [c for c, k in enumerate(self.ks) if k == row]
        return cols[len(cols) // 2], kind


def walk(ladder, profile, rng=None):
    """Walk the ladder; sample choices with `rng`, or take the higher value."""
    w = Walk(profile=profile, sampled=rng is not None)
    for d in range(1, ladder.n + 1):
        easier, harder = ladder.options(d)
        sv_easier = profile.value(NOMINAL, ladder.effort(easier))
        sv_harder = profile.value(NOMINAL, ladder.effort(harder))
        p = profile.p_up(sv_easier, sv_harder)
        up = (rng.random() < p) if rng is not None else (sv_harder > sv_easier)
        level = harder if up else easier
        w.choices.append(up)
        w.p_up.append(p)
        w.ks.append(w.ks[-1] + (1 if up else -1))
        w.taken.append(level)
        w.values.append(profile.value(NOMINAL, ladder.effort(level)))
    return w


def walk_all(ladder, profiles, rng=None):
    return [walk(ladder, p, rng) for p in profiles]


def results_order(walks):
    """Walks from the top of the lattice to the bottom, ties in profile order."""
    return sorted(walks, key=lambda w: -w.ks[-1])


def lanes(walks):
    """Lanes that keep decision makers who ever share an edge side by side.

    Returns each decision maker's lane, 0 at the top (None for one that shares
    no edge, which runs on its route), and the number of lanes. Walking the
    results order, each takes the highest lane below every earlier one it
    shares an edge with, so on shared stretches the higher-placed path runs
    above. Two lanes usually do; more are added only when paths need them.
    """
    order = results_order(walks)
    shares = {
        (a.key, b.key)
        for a in walks for b in walks
        if a.key != b.key and any(
            a.ks[c - 1] == b.ks[c - 1] and a.ks[c] == b.ks[c] for c in range(1, len(a.ks)))
    }
    for count in range(2, len(walks) + 1):
        lane = {}
        for w in order:
            mates = [v for v in order if (w.key, v.key) in shares]
            if not mates:
                lane[w.key] = None
                continue
            above = [lane[v.key] for v in mates if lane.get(v.key) is not None]
            free = [i for i in range(count) if not above or i > max(above)]
            if not free:
                break
            lane[w.key] = free[0]
        else:
            return lane, count
    return {w.key: i for i, w in enumerate(order)}, len(walks)


def path_value(ladder, choices, profile, upto=None):
    """Subjective value of the outcomes along a path, to a given decision maker."""
    total = 0.0
    for d, up in enumerate(choices[:upto], start=1):
        easier, harder = ladder.options(d)
        total += profile.value(NOMINAL, ladder.effort(harder if up else easier))
    return total


def cross_valuation(ladder, walks, upto=None):
    """How each decision maker (row) values each decision maker's path (column),
    over the first `upto` choices."""
    return {
        (valuer.key, owner.key): path_value(ladder, owner.choices, valuer.profile, upto)
        for valuer in walks
        for owner in walks
    }


def own_path_best(matrix, walks):
    """Whether every decision maker values its own path at least as much as any other."""
    return all(
        matrix[(w.key, w.key)] >= max(matrix[(w.key, v.key)] for v in walks) - 1e-12
        for w in walks
    )


def run_length(choices):
    """A path as runs of harder and easier choices: ↑8 ↓12."""
    runs = []
    for up in choices:
        if runs and runs[-1][0] == up:
            runs[-1][1] += 1
        else:
            runs.append([up, 1])
    return " ".join(f"{'↑' if up else '↓'}{n}" for up, n in runs)


def dry_run_text(ladder, walks):
    """Profiles, results and cross-valuation as plain text, for --dry-run."""
    rows = results_order(walks)
    rule = "sampled from the softmax rule" if rows[0].sampled else "the option of higher value"
    out = [
        f"{ladder.n} choices, 0–{ladder.label(ladder.n)}% MVC in steps of "
        f"{round(ladder.step, 4):g}%, nominal value {NOMINAL:g} per outcome; each choice {rule}",
        "",
        f"{'decision maker':24s} {'parameters':44s} source",
    ]
    out += [f"{w.profile.name:24s} {w.profile.params_text():44s} {w.profile.source}" for w in rows]
    out += ["", f"{'decision maker':24s} {'harder':>6s} {'turned at':>14s} {'value':>8s}   path"]
    for w in rows:
        turn = w.turn()
        turned = f"{turn[1]} {ladder.label(turn[0])}%" if turn else "—"
        out.append(f"{w.profile.name:24s} {w.harder():6d} {turned:>14s} {w.value():8.2f}   {run_length(w.choices)}")
    out += ["", "each path (column), valued by each decision maker (row)"]
    out.append(" " * 8 + "".join(f"{w.profile.label:>8s}" for w in rows))
    matrix = cross_valuation(ladder, walks)
    for r in rows:
        out.append(f"{r.profile.label:8s}" + "".join(f"{matrix[(r.key, c.key)]:8.2f}" for c in rows))
    out += ["", "P(harder) at each choice"]
    out += [f"{w.profile.label:8s}" + " ".join(f"{p:.2f}" for p in w.p_up) for w in rows]
    return "\n".join(out)
