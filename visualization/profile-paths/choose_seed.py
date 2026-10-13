# -*- coding: utf-8 -*-

"""
Default Seed

Picks the draw of sampled choices that the animation shows by default. A
draw gives a profile its shape when the sampled path opens with the same
choice, and turns the same way (at a peak, at a trough, or not at all), as
the path that always takes the higher value. Among the draws that give every
profile its shape and whose paths fit two lanes, the default is the most
typical: the closest to the average draw in each path's reversals and end
row, and in where each turn falls, measured in standard deviations over all
draws.

Usage:
    python choose_seed.py                            # the designed profiles: dual_power.SEED
    python choose_seed.py --seeds 10000
    python choose_seed.py --profiles figure1 --choices 16
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "GPL 3.0"

import argparse
import random
import statistics
import sys

from dual_power import PROFILE_SETS, SEED
from ladder import Ladder, cross_valuation, lanes, own_path_best, results_order, run_length, walk_all


def parse_args(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[1],
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--profiles", choices=list(PROFILE_SETS), default="designed",
                   help="parameter set (default designed)")
    p.add_argument("--choices", type=int, default=20, help="number of choices (default 20)")
    p.add_argument("--max-mvc", type=float, default=100.0,
                   help="highest effort on the ladder, in %% MVC (default 100)")
    p.add_argument("--tau", type=float, default=None, help="choice sensitivity for every profile, overriding the set's")
    p.add_argument("--seeds", type=int, default=3000, help="draws to consider: seeds 0 to N - 1 (default 3000)")
    p.add_argument("--top", type=int, default=5, help="most typical draws to list (default 5)")
    return p.parse_args(argv)


def shape(walk):
    """How a path opens and turns: (harder first?, 'peak', 'trough' or None)."""
    turn = walk.turn()
    return walk.choices[0], turn[1] if turn else None


def features(walks, turning):
    """What typicality is measured on: each path's reversals and end row, and
    the column of the turn for the profiles in `turning`."""
    out = {}
    for w in walks:
        out[f"{w.key} reversals"] = len(w.reversals())
        out[f"{w.key} end"] = w.ks[-1]
        if w.key in turning:
            turn = w.turn()
            out[f"{w.key} turn"] = turn[0] if turn else None
    return out


def main(argv=None):
    args = parse_args(argv)
    ladder = Ladder(args.choices, args.max_mvc)
    profiles = PROFILE_SETS[args.profiles]
    if args.tau is not None:
        profiles = [p.with_tau(args.tau) for p in profiles]

    expected = {w.key: shape(w) for w in walk_all(ladder, profiles)}
    turning = {key for key, (_, kind) in expected.items() if kind}
    draws = {s: walk_all(ladder, profiles, random.Random(s)) for s in range(args.seeds)}
    measures = {s: features(walks, turning) for s, walks in draws.items()}

    names = list(next(iter(measures.values())))
    scale = {}
    for name in names:
        values = [m[name] for m in measures.values() if m[name] is not None]
        scale[name] = statistics.mean(values), statistics.pstdev(values) or 1.0

    def distance(seed):
        return sum(((measures[seed][name] - mean) / sd) ** 2 for name, (mean, sd) in scale.items())

    shaped = [
        s for s, walks in draws.items()
        if all(shape(w) == expected[w.key] for w in walks) and lanes(walks)[1] <= 2
    ]
    own = sum(own_path_best(cross_valuation(ladder, walks), walks) for walks in draws.values())

    print(f"{args.profiles} profiles, {ladder.n} choices, seeds 0 to {args.seeds - 1}")
    for key, (up, kind) in expected.items():
        print(f"  {key:4s} opens {'harder' if up else 'easier'}, turns {f'at a {kind}' if kind else 'not at all'}")
    print(f"{len(shaped)} draws ({len(shaped) / args.seeds:.0%}) give every profile its shape and fit two lanes")
    print(f"each decision maker values its own path most in {own / args.seeds:.0%} of all draws")
    if not shaped:
        sys.exit("no draw gives every profile its shape; try more --seeds or a higher --tau")

    ranked = sorted(shaped, key=distance)
    print(f"\nmost typical, by distance from the average draw (sum of squared z over {len(names)} measures):")
    for s in ranked[: args.top]:
        paths = "   ".join(f"{w.profile.label} {run_length(w.choices)}" for w in results_order(draws[s]))
        print(f"  seed {s:5d}  {distance(s):5.2f}   {paths}")
    best = ranked[0]
    note = "" if args.profiles != "designed" else (
        "  (dual_power.SEED)" if best == SEED else f"  (dual_power.SEED is {SEED})")
    print(f"\ndefault: seed {best}{note}")


if __name__ == "__main__":
    main()
