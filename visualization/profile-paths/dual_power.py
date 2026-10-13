# -*- coding: utf-8 -*-

"""
Dual-Power Model and Preference Profiles

The DPOWER value function, its softmax choice rule, and three sets of the four
effort preference profiles: a set designed for the animation (the default),
the theoretical examples of Figure 1, and the participant estimates of
Figure 5b. Parameter values and their provenance live here, so the animation
and any dry run draw from one source.
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "GPL 3.0"

import math
from dataclasses import dataclass, replace


def logistic(z):
    """1 / (1 + e^-z), without overflow at large |z|."""
    if z >= 0:
        return 1 / (1 + math.exp(-z))
    ez = math.exp(z)
    return ez / (1 + ez)


@dataclass(frozen=True)
class Profile:
    """One decision maker: a DPOWER parameter set with a name and a color."""

    key: str  # short label: D, I, D-I, I-D
    name: str
    delta1: float  # steepness of the positive system
    gamma1: float  # curvature of the positive system
    delta2: float  # steepness of the negative system
    gamma2: float  # curvature of the negative system
    omega: float  # weight of the positive system, in (0, 1)
    tau: float  # softmax temperature (choice sensitivity)
    color: str
    source: str = ""

    def multiplier(self, effort):
        """sv(x, E) / x = 1 + ω δ₁ E^γ₁ − (1 − ω) δ₂ E^γ₂, at effort E in [0, 1]."""
        positive = self.omega * self.delta1 * effort**self.gamma1
        negative = (1 - self.omega) * self.delta2 * effort**self.gamma2
        return 1 + positive - negative

    def value(self, nominal, effort):
        """Subjective value sv(x, E) of an outcome of nominal value x at effort E."""
        return nominal * self.multiplier(effort)

    def p_up(self, sv_down, sv_up):
        """Probability of choosing the harder option under the softmax rule."""
        return logistic(self.tau * (sv_up - sv_down))

    def with_tau(self, tau):
        return replace(self, tau=tau)

    @property
    def label(self):
        """Short label for display, with an en dash: I–D."""
        return self.key.replace("-", "–")

    def params_text(self):
        return (
            f"δ₁ {self.delta1:g}  γ₁ {self.gamma1:g}  δ₂ {self.delta2:g}  "
            f"γ₂ {self.gamma2:g}  ω {self.omega:g}  τ {self.tau:g}"
        )


# The visualizer's profile hues (dual-power/app.py PROFILE_COLORS), lifted in
# lightness for an off-black canvas. Same hue per profile in both tools.
COLORS = {
    "D": "#E0564E",  # from #A73030
    "I": "#35C168",  # from #15983D
    "D-I": "#4A8FE6",  # from #0C5BB0
    "I-D": "#B48BEE",  # from #9C6ADE
}

NAMES = {
    "D": "Decreasing",
    "I": "Increasing",
    "D-I": "Decreasing–Increasing",
    "I-D": "Increasing–Decreasing",
}

# Choice sensitivity for the designed and Figure 1 profiles, which have no
# fitted τ of their own. Choices are sampled from the softmax rule, and
# neighboring efforts 5% MVC apart differ in value by only a few percent of the
# outcome, so τ sets how far a path strays from its profile. At τ = 60 (between
# the 75th and 90th percentiles of the fitted DPOWER τ, 33 and 89) a linear
# profile takes its preferred option 95% of the time; at the median, 13, it
# would take it 66% of the time and the paths would be mostly noise.
TAU = 60.0

# The draw shown by default, as picked by choose_seed.py. Of the first 3,000
# seeds, 94% give every designed profile its shape (the expected first choice,
# a peak for Increasing–Decreasing and a trough for Decreasing–Increasing, no
# turn for the other two) in paths that fit two lanes. Of those, seed 214 is
# the most typical by each path's reversals and end point and where the two
# turns fall.
SEED = 214


def _profile(key, delta1, gamma1, delta2, gamma2, omega, tau, source):
    return Profile(key, NAMES[key], delta1, gamma1, delta2, gamma2, omega, tau, COLORS[key], source)


# Designed for the effort ladder, as (δ₁, γ₁, δ₂, γ₂, ω). All four share
# δ₁ = δ₂ = 2 and τ, so they differ only in system weight and curvature, and
# all value an effortless outcome at its nominal value. Sampled choices waver
# where a value function is flat, and each profile is flat somewhere else:
#   Increasing               ω = 0.75, concave systems (γ = 0.5): f = 1 + √E,
#                            eager at first, less and less sure that more effort
#                            adds value (as the Figure 5b participant)
#   Decreasing               ω = 0.25, linear systems: f = 1 − E, a steady
#                            aversion with an occasional slip anywhere
#   Increasing–Decreasing    ω = 0.5, the concave benefit of Increasing with a
#                            parabolic cost (γ₂ = 2): f = 1 + √E − E², peak at
#                            40% MVC, unsure on either side of it
#   Decreasing–Increasing    ω = 0.5, a concave cost (γ₂ = 0.5) and a steep late
#                            benefit (γ₁ = 10, as in Figure 1): f = 1 + E¹⁰ − √E,
#                            trough at 73% MVC
# Every first choice is clear-cut (Decreasing's slips 5% of the time), so the
# four open as two pairs, and the two turns are well apart.
DESIGNED = [
    _profile("D", 2, 1, 2, 1, 0.25, TAU, "designed"),
    _profile("I", 2, 0.5, 2, 0.5, 0.75, TAU, "designed"),
    _profile("D-I", 2, 10, 2, 0.5, 0.5, TAU, "designed"),
    _profile("I-D", 2, 0.5, 2, 2, 0.5, TAU, "designed"),
]

# Figure 1 of the paper, parameters as printed in its caption (δ₁, γ₁, δ₂, γ₂, ω).
FIGURE1 = [
    _profile("D", 2, 2, 2, 2, 0.2, TAU, "Figure 1 caption"),
    _profile("I", 2, 2, 2, 2, 0.8, TAU, "Figure 1 caption"),
    _profile("D-I", 3, 10, 3, 3, 0.5, TAU, "Figure 1 caption"),
    _profile("I-D", 4, 1.5, 3, 3, 0.5, TAU, "Figure 1 caption"),
]

# Figure 5b of the paper: the four participants whose value functions it shows
# (model_inference.R, val_examples0), with their full DPOWER estimates from
# output/models/processed/fits.txt (par1 δ₁, par2 δ₂, par3 γ₁, par4 γ₂, par5 ω, par6 τ).
FIGURE5 = [
    _profile(
        "D",
        1e-07, 9.58238321280104e-07, 4.55965962172557, 3.30622906093306,
        9.99999999994061e-08, 13.5749663870219, "WS3HIHI2",
    ),
    _profile(
        "I",
        12.5950045756027, 0.469362176193688, 0.604310345089228, 0.4693860209009,
        0.146227196007375, 4.05832077963768, "SM20HIHI1",
    ),
    _profile(
        "D-I",
        0.873270601453098, 21.4773674197367, 15.3276882307347, 0.16631505602521,
        0.959264773091844, 11.5251664077037, "MT8HIRI1",
    ),
    _profile(
        "I-D",
        3.49256065353286, 3.25889617376038, 2.43446970669793, 25.0809414669555,
        0.382990984152843, 13.4967273333077, "WS8HIRI1",
    ),
]

PROFILE_SETS = {"designed": DESIGNED, "figure1": FIGURE1, "figure5": FIGURE5}
