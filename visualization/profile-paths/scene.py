# -*- coding: utf-8 -*-

"""
Scene: Layout, Timeline, Camera, and Drawing

Everything visual lives here. The canvas is a matplotlib figure addressed in
design units (a 1920×1080 composition, origin bottom-left), so stroke widths,
dot radii, braid offsets and text sit exactly where the design says at any
16:9 output size.

The left of the frame holds the lattice of choices over an effort axis in
% MVC, drawn so that every cell is an equilateral triangle; the camera starts
on the first fork and zooms out about the root as the decision makers
advance. Every step is labeled with the effort taken, for as long as the zoom
leaves the label room. The right half holds a summary that stays put: what
this is and the model behind it, the results so far, each decision maker's
value function beside how it values every path, and the source.
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "GPL 3.0"

import bisect
import copy
import math

import numpy as np
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
from matplotlib import patheffects  # noqa: E402
from matplotlib.collections import LineCollection  # noqa: E402
from matplotlib.colors import to_rgb  # noqa: E402
from matplotlib.font_manager import FontProperties  # noqa: E402
from matplotlib.patches import Circle, Rectangle  # noqa: E402
from matplotlib.textpath import TextPath  # noqa: E402

from ladder import NOMINAL, cross_valuation, lanes, own_path_best, results_order  # noqa: E402

# --- Style tokens -----------------------------------------------------------

BG = "#0E0F12"
INK = "#F2F1EC"
SUBTLE = "#8A8E96"  # labels, headers, the source
BODY = "#B4B8BF"  # running text: the lede and the definitions
STEP = "#C9CBD0"  # effort taken at each step
LATTICE = (1.0, 1.0, 1.0, 0.15)  # every possible path
OFFER = (1.0, 1.0, 1.0, 0.38)  # the two options of the choice at hand
AXIS = (1.0, 1.0, 1.0, 0.32)
RULE = (1.0, 1.0, 1.0, 0.16)

FONT_UI = ["Segoe UI", "Inter", "Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"]
FONT_DISPLAY = ["Segoe UI", "Inter Display", "Inter", "Helvetica Neue", "Arial", "DejaVu Sans"]

# Sizes in design units: pixels of the 1920×1080 composition.
PATH_W = 5.0  # a decision maker's path
LATTICE_W = 1.3
OFFER_W = 2.0
AXIS_W = 1.4
BRAID_GAP = 16.0  # between decision makers on the same path: two dots side by side
DOT_R = 7.0  # decision maker
HALO_R = 22.0
ROOT_R = 5.5
RING_R = 13.0  # marks a turn
RING_W = 2.6
MATRIX_LEFT = 280.0  # each path valued by each decision maker, from the panel's left edge
MATRIX_PITCH = 100.0  # between its columns
LABEL_GAP = 5.0  # from a label's glyphs to the edge of its own path
LABEL_PAD = 6.0  # between the glyphs of two labels
TEXT_HALO = 4.0  # background stroke behind labels on the lattice
SLOPE = math.tan(math.radians(30))  # edges at 30°: every lattice cell is equilateral

# Type in points; at the design resolution 1 pt is 1.39 units.
TITLE_PT = 30
LEDE_PT = 14
EQUATION_PT = 14.5
NOTE_PT = 13  # definitions and asides
KICKER_PT = 11.5  # section labels, in tracked capitals
HEAD_PT = 12  # column heads and plot ticks
ROW_PT = 16
MATRIX_PT = 15
TICK_PT = 14
STEP_PT = 12
ANNOTATION_PT = 14
CITATION_PT = 11

# Layout in design units.
MARGIN = 150  # outer margin, left and right
TOP = 70  # top edge to the top of the lattice and the panel
LATTICE_BOTTOM = 210  # bottom of the lattice, above the effort axis
AXIS_Y = 170
TICK_LABEL_Y = 150
BOTTOM_ROW_Y = 92  # baseline of the axis title and the citation
GUTTER = 150  # lattice to panel, at the least
PANEL_MIN = 700

TITLE = "DPOWER Choice Model"
LEDE = (
    "A decision model that weighs the cost of effort against the value it adds.",
    "Four decision makers make the same {choices} between neighboring efforts.",
)
CITATION = "Marcowski, Bia\u0142aszek & Winkielman (2025)  \u00b7  J Exp Psychol Gen"

HAIR = "\u200a"  # hair space


def smoothstep(u):
    u = min(1.0, max(0.0, u))
    return u * u * (3 - 2 * u)


def ease_in_out(u):
    u = min(1.0, max(0.0, u))
    return 0.5 - 0.5 * math.cos(math.pi * u)


def lerp(a, b, u):
    return a + (b - a) * u


def mix(c0, c1, u):
    """A color between two, as RGB."""
    return tuple(lerp(a, b, u) for a, b in zip(to_rgb(c0), to_rgb(c1)))


def number(v):
    """Two decimals, with a true minus sign."""
    return f"{v:.2f}".replace("-", "\u2212")


def tracked(text):
    """Small capitals with a hair of letter spacing."""
    return " ".join(HAIR.join(word) for word in text.upper().split(" "))


def resolve_fonts(candidates, fallback):
    """The first installed family from `candidates`, with matplotlib's bundled
    `fallback` behind it for any glyph the first lacks. Resolving once keeps
    matplotlib from searching, and warning, on every text artist."""
    from matplotlib import font_manager

    for name in candidates:
        try:
            font_manager.findfont(font_manager.FontProperties(family=name), fallback_to_default=False)
        except ValueError:
            continue
        return [name, fallback] if name != fallback else [fallback]
    return [fallback]


# --- Timeline ---------------------------------------------------------------


class Timeline:
    """When each choice happens.

    Choices keep a brisk pace. The first, every choice that reveals a turn, and
    the choice just before one run slower, so the moments where paths part get
    time to read. `speed` scales the whole piece.
    """

    def __init__(self, n, events, speed=1.0, intro=1.5, first=2.4, base=0.8, lead=1.15,
                 event=2.2, outro=1.4, reveal=1.2, hold=4.5):
        s = 1.0 / speed
        self.n = n
        self.scale = s
        self.start = [0.0] * (n + 1)
        self.dur = [0.0] * (n + 1)
        t = intro * s
        for d in range(1, n + 1):
            if d == 1:
                dur = first
            elif d in events:
                dur = event
            elif d + 1 in events:
                dur = lead
            else:
                dur = base
            self.start[d], self.dur[d] = t, dur * s
            t += dur * s
        self.intro_end = intro * s
        self.walk_end = t
        self.outro_end = t + outro * s
        self.reveal_end = self.outro_end + reveal * s
        self.total = self.reveal_end + hold * s

    def current(self, t):
        """The choice under way at time t: 0 before the walk, n + 1 after it."""
        if t < self.start[1]:
            return 0
        if t >= self.walk_end:
            return self.n + 1
        return bisect.bisect_right(self.start, t, 1, self.n + 1) - 1

    def local(self, d, t):
        return (t - self.start[d]) / self.dur[d]

    def stages(self, d, t):
        """(unfold, deliberate, travel) of choice d at time t, each in [0, 1]."""
        if d < 1 or d > self.n:
            return (1.0, 0.0, 1.0) if d > self.n else (0.0, 0.0, 0.0)
        u = self.local(d, t)
        if u <= 0:
            return 0.0, 0.0, 0.0
        if u >= 1:
            return 1.0, 0.0, 1.0
        unfold = smoothstep(u / 0.30)
        deliberate = smoothstep((u - 0.04) / 0.16) * (1 - smoothstep((u - 0.34) / 0.16))
        travel = ease_in_out((u - 0.34) / 0.62)
        return unfold, deliberate, travel

    def travel(self, d, t):
        return self.stages(d, t)[2]

    def pace(self, t):
        """Choices completed at an even pace through each choice: the camera's clock,
        free of the stop and go of the decision makers' steps."""
        d = self.current(t)
        if d == 0:
            return 0.0
        if d > self.n:
            return float(self.n)
        return d - 1 + min(1.0, max(0.0, self.local(d, t)))

    def progress(self, t):
        """Choices completed, fractionally: 2.5 is halfway through the third."""
        d = self.current(t)
        if d == 0:
            return 0.0
        if d > self.n:
            return float(self.n)
        return d - 1 + self.travel(d, t)

    def completed(self, t):
        """Whole choices completed by time t."""
        return int(math.floor(self.progress(t) + 1e-9))


# --- Canvas -----------------------------------------------------------------

DESIGN_W, DESIGN_H = 1920, 1080


class Canvas:
    """A matplotlib figure addressed in design units.

    The layout is composed for a 1920×1080 canvas; other output sizes render the
    same composition scaled, by changing the figure's dpi rather than every
    coordinate. The design space keeps the output's aspect ratio.
    """

    def __init__(self, width, height):
        k = min(width / DESIGN_W, height / DESIGN_H)
        self.w, self.h = width / k, height / k  # design units
        self.width, self.height = width, height  # output pixels
        base_dpi = 100.0
        self.fig = plt.figure(figsize=(self.w / base_dpi, self.h / base_dpi), dpi=base_dpi * k, facecolor=BG)
        self.ax = self.fig.add_axes([0, 0, 1, 1])
        self.ax.set_facecolor(BG)
        self.px2pt = 72.0 / base_dpi  # design units to points
        self.font_ui = resolve_fonts(FONT_UI, "DejaVu Sans")
        self.font_display = resolve_fonts(FONT_DISPLAY, "DejaVu Sans")
        self._typeset_math(self.font_ui[0])
        self._ink = {}

    @staticmethod
    def _typeset_math(family):
        """Set equations in the interface font, so they read as one with the text."""
        if family == "DejaVu Sans":
            matplotlib.rcParams["mathtext.fontset"] = "dejavusans"
            return
        matplotlib.rcParams["mathtext.fontset"] = "custom"
        matplotlib.rcParams["mathtext.rm"] = family
        matplotlib.rcParams["mathtext.sf"] = family
        matplotlib.rcParams["mathtext.it"] = f"{family}:italic"
        matplotlib.rcParams["mathtext.bf"] = f"{family}:bold"
        matplotlib.rcParams["mathtext.cal"] = family
        matplotlib.rcParams["mathtext.tt"] = family

    def begin(self):
        self.ax.cla()
        self.ax.set_xlim(0, self.w)
        self.ax.set_ylim(0, self.h)
        self.ax.axis("off")

    def line(self, pts, color, width, z=1, alpha=None, dash=None):
        """A polyline; `dash` is (on, off) in design units for a dashed one."""
        xs, ys = zip(*pts)
        style = (0, tuple(v * self.px2pt / (width * self.px2pt) for v in dash)) if dash else "solid"
        self.ax.plot(xs, ys, color=color, alpha=alpha, lw=width * self.px2pt, linestyle=style,
                     solid_capstyle="round", solid_joinstyle="round", dash_capstyle="butt", zorder=z)

    def lines(self, segments, color, width, z=1, alpha=None):
        if segments:
            self.ax.add_collection(LineCollection(
                segments, colors=[color], linewidths=width * self.px2pt, alpha=alpha,
                capstyle="round", zorder=z,
            ))

    def glow_line(self, pts, color, width, z=5, alpha=1.0):
        for k, a in ((4.2, 0.07), (2.3, 0.16), (1.0, 1.0)):
            self.line(pts, color, width * k, z=z, alpha=a * alpha)

    def circle(self, center, radius, color, z=8, alpha=1.0):
        self.ax.add_patch(Circle(center, radius, facecolor=color, edgecolor="none", alpha=alpha, zorder=z))

    def ring(self, center, radius, color, width, z=8, alpha=1.0):
        self.ax.add_patch(Circle(center, radius, facecolor="none", edgecolor=color,
                                 linewidth=width * self.px2pt, alpha=alpha, zorder=z))

    def rect(self, x0, y0, x1, y1, color, z=8, alpha=1.0):
        self.ax.add_patch(Rectangle((x0, y0), x1 - x0, y1 - y0, facecolor=color, edgecolor="none",
                                    alpha=alpha, zorder=z))

    def text(self, x, y, s, size=12, color=INK, alpha=1.0, ha="left", va="baseline",
             display=False, weight="normal", z=20, halo=0.0):
        if alpha <= 0:
            return
        effects = [patheffects.withStroke(linewidth=halo * self.px2pt, foreground=BG)] if halo else None
        self.ax.text(x, y, s, fontsize=size, color=color, alpha=min(1.0, alpha), ha=ha, va=va,
                     family=self.font_display if display else self.font_ui, weight=weight, zorder=z,
                     path_effects=effects)

    def ink(self, s, size, display=False):
        """Extents of the glyphs of `s` set at `size` points, from the text origin, in design units."""
        key = (s, size, display)
        if key not in self._ink:
            em = size / self.px2pt
            family = self.font_display if display else self.font_ui
            e = TextPath((0, 0), s, size=em, prop=FontProperties(family=family)).get_extents()
            self._ink[key] = (e.x0, e.y0, e.x1, e.y1)
        return self._ink[key]

    def rgb(self):
        self.fig.canvas.draw()
        buf = np.asarray(self.fig.canvas.buffer_rgba())[:, :, :3]
        if buf.shape[0] != self.height or buf.shape[1] != self.width:  # dpi rounding
            from PIL import Image

            buf = np.asarray(Image.fromarray(buf).resize((self.width, self.height), Image.LANCZOS))
        return np.ascontiguousarray(buf)

    def close(self):
        plt.close(self.fig)


# --- View -------------------------------------------------------------------


class View:
    """The lattice at a zoom: `depth` columns fill the lattice area, about the root."""

    def __init__(self, apex_x, mid_y, width, depth):
        self.apex_x, self.mid_y = apex_x, mid_y
        self.sx = width / depth  # one column
        self.sy = self.sx * SLOPE  # one step of k

    def x(self, c):
        return self.apex_x + c * self.sx

    def node(self, c, k):
        return self.apex_x + c * self.sx, self.mid_y + k * self.sy


# --- Labels on the lattice --------------------------------------------------


class Tag:
    """A label on the lattice that keeps one place at every zoom.

    The middle of its glyphs sits at lattice point (cx, cy), shifted by a fixed
    offset in design units, so it travels with what it labels and keeps its
    size. Below the column width `sx_min` it would touch something, so it fades
    out on the way there; a label with room at every zoom has `sx_min` 0.
    """

    def __init__(self, text, size, ink, color, z, cx, cy, ox, oy, shown_sx, choice=None):
        x0, y0, x1, y1 = ink
        self.text, self.size, self.color, self.z = text, size, color, z
        self.w, self.h = x1 - x0, y1 - y0
        self.origin = (-x0 - self.w / 2, -y0 - self.h / 2)  # glyph middle to text origin
        self.cx, self.cy, self.ox, self.oy = cx, cy, ox, oy
        self.shown_sx = shown_sx  # column width when the label first shows
        self.sx_min = 0.0
        self.choice = choice

    def center(self, view):
        return view.apex_x + self.cx * view.sx + self.ox, view.mid_y + self.cy * view.sy + self.oy

    def boxes(self, apex_x, mid_y, sxs):
        """Glyph box at each column width in `sxs`: x0, y0, x1, y1 arrays."""
        x = apex_x + self.cx * sxs + self.ox
        y = mid_y + self.cy * sxs * SLOPE + self.oy
        return x - self.w / 2, y - self.h / 2, x + self.w / 2, y + self.h / 2

    def room(self, sx):
        if self.sx_min <= 0:
            return 1.0
        return smoothstep((sx - self.sx_min) / (0.12 * self.sx_min))


def box_distance(px, py, x0, y0, x1, y1):
    """Distance from points to boxes, one box per row: (rows × points)."""
    dx = np.maximum(np.maximum(x0[:, None] - px, 0.0), px - x1[:, None])
    dy = np.maximum(np.maximum(y0[:, None] - py, 0.0), py - y1[:, None])
    return np.hypot(dx, dy)


# --- Scene ------------------------------------------------------------------

CAMERA = [(s, math.exp(-0.5 * (s / 0.35) ** 2)) for s in np.arange(-1.05, 1.06, 0.05)]  # Gaussian, σ 0.35 s
CAMERA = [(s, w / sum(w for _, w in CAMERA)) for s, w in CAMERA]
SETTLED = 0.06  # columns of room past the last once the walk is over: the final zoom
BRAID_RES = 16  # samples per edge along a path
ROOT_EASE = 2.5  # edges over which decision makers ease from the root stack into their lanes
ZOOMS = 240  # column widths at which label placement is checked


class Scene:
    """Draws the piece at any time t."""

    def __init__(self, ladder, walks, width=1920, height=1080, lattice=True, show_p=False, speed=1.0):
        self.ladder = ladder
        self.walks = walks
        self.by_key = {w.key: w for w in walks}
        self.order = results_order(walks)  # top of the lattice to the bottom
        self.show_p = show_p
        self.canvas = Canvas(width, height)
        self.turns = self._turns()
        self.timeline = Timeline(ladder.n, {reveal for _, _, reveal in self.turns.values()}, speed=speed)
        self.edges = ladder.lattice_edges() if lattice else []
        self._layout()
        self._braid()
        self._place_labels()
        self._functions()

    # -- layout

    def _layout(self):
        W, H = self.canvas.w, self.canvas.h
        top = H - TOP
        height = top - LATTICE_BOTTOM
        width = height * math.cos(math.radians(30))  # equilateral: the right edge is the height
        widest = W - 2 * MARGIN - GUTTER - PANEL_MIN
        if width > widest:  # a narrow frame keeps the panel readable
            width = widest
        self.apex_x = MARGIN
        self.lattice_w = width
        self.mid_y = LATTICE_BOTTOM + (top - LATTICE_BOTTOM) / 2
        self.panel_x0 = max(MARGIN + width + GUTTER, W / 2 + GUTTER / 2)  # the summary takes the right half
        self.panel_x1 = W - MARGIN
        self.top = top
        # label every column, or every 2nd, 4th... so labels at full zoom-out stay apart
        final_sx = width / self.ladder.n
        self.major_every = next(j for j in (1, 2, 4, 5, 10, 20, 25, 50, 100) if j * final_sx >= 64)
        inks = [self.canvas.ink(self.ladder.label(col), TICK_PT) for col in range(self.ladder.n + 1)]
        self.tick_half = [(x1 - x0) / 2 for x0, _, x1, _ in inks]  # half the width of each effort label

    def _functions(self):
        """Each decision maker's value function, sv(x, E) over the ladder's efforts
        in multiples of the nominal value x, and one scale for all: whole steps
        that take in every curve and x itself."""
        self.f_e = np.linspace(0.0, 1.0, 161)
        self.f_v = {w.key: np.array([w.profile.multiplier(e) for e in self.f_e]) for w in self.walks}
        lo = min(1.0, *(v.min() for v in self.f_v.values()))
        hi = max(1.0, *(v.max() for v in self.f_v.values()))
        step = next(k for k in (0.5, 1, 2, 5, 10, 20, 50) if (hi - lo) / k <= 3)
        first, last = math.floor(lo / step + 1e-9), math.ceil(hi / step - 1e-9)
        self.f_axis = first * step, last * step

        def multiple(v):
            if v == 0:
                return "0"
            return (r"$x$" if v > 0 else r"$-x$") if abs(v) == 1 else rf"${v:g}x$"

        values = [k * step for k in range(first, last + 1)]
        self.f_ticks = [(v, multiple(v)) for v in values]
        if 1.0 not in values:  # x on its own: labeled where it has room
            room = min(abs(v - 1.0) for v in values) >= 0.3 * step
            self.f_ticks.append((1.0, multiple(1.0) if room else None))

    def _turns(self):
        """Each path's turn as (column, 'peak' or 'trough', revealing choice): the
        choice that leaves the turning row for the last time, when the turn is plain."""
        turns = {}
        for w in self.walks:
            turn = w.turn()
            if turn:
                col, kind = turn
                last = max(c for c, k in enumerate(w.ks) if k == w.ks[col])
                turns[w.key] = (col, kind, last + 1)
        return turns

    def _braid(self):
        """Each decision maker's lattice row and vertical offset along its path.

        A decision maker keeps one lane for the whole walk, so its path is its
        lattice route shifted by a constant: stretches shared with others run side
        by side and every turn stays crisp. Only out of the root, where all four
        start stacked, do the offsets ease into their lanes.
        """
        n = self.ladder.n
        lane_offsets = self._lanes()
        self.grid_p = np.arange(n * BRAID_RES + 1) / BRAID_RES
        ease = np.array([smoothstep(p / ROOT_EASE) for p in self.grid_p])
        self.grid_k, self.grid_off = {}, {}
        for w in self.walks:
            stacked = self._offset(w, self.order)
            lane = lane_offsets[w.key]
            self.grid_off[w.key] = lane + (stacked - lane) * (1 - ease)
            rows = []
            for p in self.grid_p:
                c = min(n, max(1, math.ceil(p - 1e-9)))  # the edge holding p
                rows.append(lerp(w.ks[c - 1], w.ks[c], p - (c - 1)))
            self.grid_k[w.key] = np.array(rows)

    def _lanes(self):
        """Each decision maker's offset from its route: its lane (ladder.lanes),
        with the lanes centered on the route."""
        lane, count = lanes(self.walks)
        return {
            w.key: 0.0 if lane[w.key] is None else ((count - 1) / 2 - lane[w.key]) * BRAID_GAP
            for w in self.walks
        }

    def _point(self, view, w, p):
        """Screen position of a decision maker at path position p (edges walked)."""
        k = np.interp(p, self.grid_p, self.grid_k[w.key])
        off = np.interp(p, self.grid_p, self.grid_off[w.key])
        return view.apex_x + p * view.sx, view.mid_y + k * view.sy + off

    def _path_points(self, view, w, p_end):
        """A decision maker's path from the root to position p_end."""
        i = int(math.floor(p_end * BRAID_RES + 1e-9))
        xs = view.apex_x + self.grid_p[: i + 1] * view.sx
        ys = view.mid_y + self.grid_k[w.key][: i + 1] * view.sy + self.grid_off[w.key][: i + 1]
        pts = list(zip(xs, ys))
        if p_end > self.grid_p[i] + 1e-9:
            pts.append(self._point(view, w, p_end))
        return pts

    @staticmethod
    def _offset(w, mates):
        j = next(i for i, v in enumerate(mates) if v.key == w.key)
        return ((len(mates) - 1) / 2 - j) * BRAID_GAP

    # -- labels

    def _place_labels(self):
        """Set every label on the lattice where it reads clearly.

        Each step is labeled with the effort taken, beside the middle of its edge,
        once however many decision makers share the edge; each turn with its kind
        and effort, beside its ring. A label has room where it is clear of every
        path, ring, dot and earlier label at every zoom from the one where it
        first shows to the final one. Steps go in order, each on the outer side
        of its path if that has room, else the inner side. Turns go first, at the
        first place around the ring with room, unless that would cost a step its
        room; then they go last, and give way as the steps arrive. A label with
        no room anywhere takes the place that keeps it longest and fades out as
        the camera pulls back to where it would touch; one that would touch on
        arrival is left out.
        """
        n = self.ladder.n
        self.final_sx = self.lattice_w / (n + SETTLED)
        self.zooms = np.geomspace(self.final_sx * 0.995, self.lattice_w, ZOOMS)
        self._obstacles()
        steps, turns = self._step_candidates(), self._turn_candidates()
        late = set()
        best = self._place(steps, turns, late)
        for key in turns:
            trial = self._place(steps, turns, late | {key})
            if trial[2] < best[2]:
                late, best = late | {key}, trial
        self.step_tags, self.turn_tags, _ = best

    def _step_candidates(self):
        """Per edge taken, in order of the choices and from the top: where its label may go."""
        n = self.ladder.n
        groups = {}
        for w in self.order:  # top to bottom, so shared edges list their mates in lane order
            for d in range(1, n + 1):
                groups.setdefault((d, w.ks[d - 1], w.ks[d]), []).append(w)
        steps = []
        for (d, k0, k1), mates in sorted(groups.items(), key=lambda item: (item[0][0], -item[0][1] - item[0][2])):
            up = k1 > k0
            text = self.ladder.label(d if up else d - 1)
            ink = self.canvas.ink(text, STEP_PT)
            width, height = ink[2] - ink[0], ink[3] - ink[1]
            row = (k0 + k1) / 2
            lanes = [float(np.interp(d - 0.5, self.grid_p, self.grid_off[w.key])) for w in mates]
            others = [(v.ks[d - 1] + v.ks[d]) / 2 for v in self.walks if v not in mates]
            above = [r - row for r in others if r > row]
            below = [row - r for r in others if r < row]
            outer_up = not above or (below and min(above) >= min(below))
            sides = [(-0.5 if up else 0.5, math.sqrt(3) / 2), (0.5 if up else -0.5, -math.sqrt(3) / 2)]
            if not outer_up:
                sides.reverse()
            shown = max(self.final_sx, self.lattice_w / (d + 0.5))  # the last shows as the camera settles
            candidates = []
            for nx, ny in sides:
                lane = max(lanes) if ny > 0 else min(lanes)
                reach = abs(nx) * width / 2 + abs(ny) * height / 2 + PATH_W / 2 + LABEL_GAP
                candidates.append(Tag(text, STEP_PT, ink, STEP, 11, d - 0.5, row, nx * reach,
                                      lane + ny * reach, shown, choice=d))
            steps.append(candidates)
        return steps

    def _turn_candidates(self):
        """Per turn, in order of revealing: where its label may go around the ring."""
        turns = {}
        for key, (col, kind, reveal) in sorted(self.turns.items(), key=lambda item: item[1][2]):
            w = self.by_key[key]
            text = f"{kind}  ·  {self.ladder.label(col)}%"
            ink = self.canvas.ink(text, ANNOTATION_PT)
            width, height = ink[2] - ink[0], ink[3] - ink[1]
            outside = 1 if kind == "peak" else -1  # above a peak, below a trough, else the other way
            turns[key] = [
                Tag(text, ANNOTATION_PT, ink, w.profile.color, 12, col, w.ks[col], ox,
                    side * (RING_R + 7 + height / 2), self.lattice_w / max(1.0, reveal - 0.2))
                for side in (outside, -outside)
                for ox in (-(RING_R + 5 + width / 2), RING_R + 5 + width / 2, 0.0)
            ]
        return turns

    def _place(self, steps, turns, late):
        """Labels placed with the turns in `late` after the steps: (step tags, turn
        tags, steps without room at every zoom)."""
        placed, step_tags, turn_tags, short = [], [], {}, 0

        def place_turns(keys):
            for key in keys:
                tag = self._settle(turns[key], placed)
                if tag:
                    turn_tags[key] = tag
                    placed.append(tag)

        place_turns([key for key in turns if key not in late])
        for candidates in steps:
            tag = self._settle(candidates, placed)
            short += tag is None or tag.sx_min > 0
            if tag:
                step_tags.append(tag)
                placed.append(tag)
        place_turns([key for key in turns if key in late])
        return step_tags, turn_tags, short

    def _obstacles(self):
        """Path samples, rings and dots, at every column width in `self.zooms`."""
        sxs = self.zooms
        n = self.ladder.n
        xs, ys = [], []
        for w in self.walks:
            xs.append(self.apex_x + np.outer(sxs, self.grid_p))
            ys.append(self.mid_y + np.outer(sxs * SLOPE, self.grid_k[w.key]) + self.grid_off[w.key])
        self.obstacle_paths = np.concatenate(xs, axis=1), np.concatenate(ys, axis=1)
        circles = []
        for key, (col, _, _) in self.turns.items():
            k = self.by_key[key].ks[col]
            circles.append((self.apex_x + col * sxs, self.mid_y + k * sxs * SLOPE, RING_R + RING_W / 2 + 2))
        for w in self.walks:
            end = self.mid_y + w.ks[n] * sxs * SLOPE + self.grid_off[w.key][-1]
            circles.append((self.apex_x + n * sxs, end, DOT_R + 3))
        circles.append((np.full_like(sxs, self.apex_x), np.full_like(sxs, self.mid_y), ROOT_R + 3))
        self.obstacle_circles = circles

    def _touches(self, tag, placed):
        """At which column widths in `self.zooms` the label would touch something."""
        sxs = self.zooms
        x0, y0, x1, y1 = tag.boxes(self.apex_x, self.mid_y, sxs)
        px, py = self.obstacle_paths
        hit = (box_distance(px, py, x0, y0, x1, y1) < PATH_W / 2 + LABEL_GAP - 0.75).any(axis=1)
        for cx, cy, r in self.obstacle_circles:
            hit |= box_distance(cx[:, None], cy[:, None], x0, y0, x1, y1)[:, 0] < r
        for other in placed:
            shown = (sxs > other.sx_min) & (sxs <= other.shown_sx)
            ox0, oy0, ox1, oy1 = other.boxes(self.apex_x, self.mid_y, sxs)
            hit |= shown & (x0 < ox1 + LABEL_PAD) & (ox0 < x1 + LABEL_PAD) \
                & (y0 < oy1 + LABEL_PAD) & (oy0 < y1 + LABEL_PAD)
        hit |= (x0 < self.apex_x - 70) | (x1 > self.panel_x0 - 24) | (y0 < AXIS_Y + 14) | (y1 > self.canvas.h - 14)
        return hit & (sxs <= tag.shown_sx)

    def _settle(self, candidates, placed):
        """A copy of the first candidate with room at every zoom; else of the one
        with room the longest, set to fade before it would touch; else None."""
        best, best_sx = None, math.inf
        for tag in candidates:
            hit = self._touches(tag, placed)
            if not hit.any():
                return copy.copy(tag)
            sx = self.zooms[hit].max() * 1.03
            if sx < best_sx:
                best, best_sx = tag, sx
        if best is None or best_sx >= best.shown_sx * 0.97:
            return None
        chosen = copy.copy(best)
        chosen.sx_min = best_sx
        return chosen

    # -- time

    def depth(self, t):
        """Columns in view: the choices made so far at the camera's even pace,
        smoothed, plus a column of room ahead that closes once the walk is over."""
        tl = self.timeline
        progress = sum(w * tl.pace(t + s * tl.scale) for s, w in CAMERA)
        settle = smoothstep((t - tl.walk_end) / (tl.outro_end - tl.walk_end))
        return max(1.0, progress + lerp(1.0, SETTLED, settle))

    def lattice_unfold(self, c, t):
        """Column c of the lattice unfolds as the decision makers travel to the one before."""
        tl = self.timeline
        if c < 1:
            return 1.0
        if c == 1:
            return smoothstep((t / tl.intro_end - 0.4) / 0.55)
        return tl.travel(c - 1, t)

    def window(self, t):
        """The two efforts on offer, as a window [p, p + 1] that glides along with the
        decision makers, and how visible it is: in as the walk begins, out over the
        last step."""
        tl, n = self.timeline, self.ladder.n
        p = tl.progress(t)
        shown = self.first_choice(t) * (1 - smoothstep(p - (n - 1)))
        return min(p, n - 1), shown

    @staticmethod
    def in_window(col, p):
        """How fully column `col` sits in the window [p, p + 1]: 1 inside, fading over a column."""
        return max(0.0, 1 - max(0.0, p - col, col - (p + 1)))

    def intro(self, t):
        """The opening reveal, one curve shared by everything that stays put: the
        summary, the axis, the root and the decision makers."""
        tl = self.timeline
        return smoothstep((t - 0.1 * tl.scale) / (0.9 * tl.scale))

    def first_choice(self, t):
        """How far the first choice has come in: its options, the window on the
        efforts on offer, and their names all arrive together."""
        return smoothstep(self.timeline.local(1, t) / 0.3)

    # -- drawing

    def draw(self, t):
        self.canvas.begin()
        view = View(self.apex_x, self.mid_y, self.lattice_w, self.depth(t))
        self._draw_lattice(view, t)
        self._draw_offers(view, t)
        self._draw_paths(view, t)
        self._draw_turns(view, t)
        self._draw_steps(view, t)
        self._draw_first_fork(view, t)
        self._draw_walkers(view, t)
        self._draw_axis(view, t)
        self._draw_panel(t)
        return self.canvas.rgb()

    def _draw_lattice(self, view, t):
        segments = []
        for d, k0, k1 in self.edges:
            g = smoothstep(self.lattice_unfold(d, t))
            if g <= 0:
                continue
            x0, y0 = view.node(d - 1, k0)
            x1, y1 = view.node(d, k1)
            segments.append([(x0, y0), (lerp(x0, x1, g), lerp(y0, y1, g))])
        self.canvas.lines(segments, LATTICE, LATTICE_W, z=1)

    def _draw_offers(self, view, t):
        """The choice at hand: from every node a decision maker stands on, the two
        edges on offer brighten in place while it is weighed, and dim as the
        decision makers set off along the ones they take."""
        tl = self.timeline
        d = tl.current(t)
        if not 1 <= d <= self.ladder.n:
            return
        a = smoothstep(tl.local(d, t) / 0.30) * (1 - smoothstep(tl.travel(d, t) / 0.8))
        if a <= 0:
            return
        segments = [
            [view.node(d - 1, k), view.node(d, k + step)]
            for k in sorted({w.ks[d - 1] for w in self.walks})
            for step in (-1, 1)
        ]
        self.canvas.lines(segments, OFFER[:3], OFFER_W, z=3, alpha=OFFER[3] * a)

    def _draw_paths(self, view, t):
        p = self.timeline.progress(t)
        if p <= 0:
            return
        for w in self.walks:
            self.canvas.glow_line(self._path_points(view, w, p), w.profile.color, PATH_W, z=5)

    def _draw_turns(self, view, t):
        """A ring at each turn, labeled while there is room. If any label has to give
        way before the end, the rest follow as the walk ends, so the final frame
        treats every turn alike; the results still give where each path turned."""
        tl = self.timeline
        all_stay = len(self.turn_tags) == len(self.turns) and all(
            tag.sx_min <= 0 for tag in self.turn_tags.values())
        stay = 1.0 if all_stay else 1 - smoothstep((t - tl.walk_end) / (tl.outro_end - tl.walk_end))
        for key, (col, _, reveal) in self.turns.items():
            w = self.by_key[key]
            a = smoothstep(tl.travel(reveal, t) / 0.5)
            if a <= 0:
                continue
            self.canvas.ring(view.node(col, w.ks[col]), RING_R, w.profile.color, RING_W, z=7, alpha=a)
            tag = self.turn_tags.get(key)
            if tag:
                self._draw_tag(view, tag, a * tag.room(view.sx) * stay)

    def _draw_steps(self, view, t):
        """The effort taken at each step, as the decision makers take it."""
        tl = self.timeline
        for tag in self.step_tags:
            a = smoothstep((tl.travel(tag.choice, t) - 0.4) / 0.6) * tag.room(view.sx)
            self._draw_tag(view, tag, a)

    def _draw_tag(self, view, tag, alpha):
        if alpha <= 0.005:
            return
        x, y = tag.center(view)
        self.canvas.text(x + tag.origin[0], y + tag.origin[1], tag.text, size=tag.size, color=tag.color,
                         alpha=alpha, z=tag.z, halo=TEXT_HALO)

    def _draw_first_fork(self, view, t):
        """Name the two options once, inside the first fork beside each edge, where
        the step labels will not go, then let them go."""
        tl = self.timeline
        if self.ladder.n < 3:
            return
        a = self.first_choice(t) * (1 - smoothstep(tl.local(3, t)))
        if a <= 0:
            return
        size = ANNOTATION_PT - 1
        for k, name in ((1, "harder"), (-1, "easier")):
            ink = self.canvas.ink(name, size)
            width, height = ink[2] - ink[0], ink[3] - ink[1]
            nx, ny = 0.5, -k * math.sqrt(3) / 2  # toward the inside of the fork
            taking = [w for w in self.walks if w.ks[1] == k]
            offs = [float(np.interp(0.5, self.grid_p, self.grid_off[w.key])) for w in taking] or [0.0]
            lane = min(offs) if k > 0 else max(offs)
            reach = nx * width / 2 + abs(ny) * height / 2 + PATH_W / 2 + LABEL_GAP + 4
            tag = Tag(name, size, ink, SUBTLE, 12, 0.5, k / 2, nx * reach, lane + ny * reach, math.inf)
            self._draw_tag(view, tag, a)

    def _draw_walkers(self, view, t):
        """The decision makers. While a choice is weighed, each halo swells once,
        as far as its decision maker is torn: not at all when one option is near
        certain, most when the two are close to even."""
        tl = self.timeline
        a = self.intro(t)
        x, y = view.node(0, 0)
        self.canvas.circle((x, y), ROOT_R, INK, z=9, alpha=a)
        d = tl.current(t)
        weighing = 1 <= d <= self.ladder.n
        hump = math.sin(math.pi * min(1.0, max(0.0, tl.local(d, t) / 0.5))) if weighing else 0.0
        deliberate = tl.stages(d, t)[1] if weighing else 0.0
        p = tl.progress(t)
        for w in self.walks:
            px, py = self._point(view, w, p)
            torn = 1 - abs(2 * w.p_up[d - 1] - 1) if weighing else 0.0
            swell = torn * hump
            self.canvas.circle((px, py), HALO_R * (1 + 0.28 * swell), w.profile.color, z=14,
                               alpha=(0.16 + 0.18 * swell) * a)
            self.canvas.circle((px, py), DOT_R, w.profile.color, z=15, alpha=a)
            if self.show_p and deliberate > 0:
                self.canvas.text(px + HALO_R + 6, py, f"{w.p_up[d - 1]:.2f}", size=ANNOTATION_PT - 2,
                                 color=w.profile.color, alpha=a * deliberate, va="center", z=16, halo=TEXT_HALO)

    def _draw_axis(self, view, t):
        c, tl = self.canvas, self.timeline
        a_in = self.intro(t)
        unfolded = [self.lattice_unfold(col, t) for col in range(self.ladder.n + 1)]
        front = max((col - 1 + u for col, u in enumerate(unfolded) if col > 0 and u > 0), default=0.0)
        c.line([(view.x(0), AXIS_Y), (view.x(front), AXIS_Y)], AXIS, AXIS_W, z=2, alpha=AXIS[3] * a_in)
        minor = smoothstep((view.sx - 80) / 30)
        edge = self.apex_x + self.lattice_w  # ticks of columns still beyond the frame wait for it
        p, shown = self.window(t)
        offered = [shown * self.in_window(col, p) for col in range(self.ladder.n + 1)]  # the two efforts on offer
        named, keep = self._tick_room(view.sx, offered)
        for col, u in enumerate(unfolded):
            if u <= 0:
                continue
            lead = a_in if col == 0 else u
            base = 1.0 if col % self.major_every == 0 else minor
            ease = smoothstep((edge + 30 - view.x(col)) / 30)
            a = lead * max(base, offered[col]) * ease
            if a <= 0:
                continue
            x = view.x(col)
            c.line([(x, AXIS_Y), (x, AXIS_Y - 8)], AXIS, AXIS_W, z=2, alpha=AXIS[3] * a)
            c.text(x, TICK_LABEL_Y, self.ladder.label(col), size=TICK_PT, color=mix(SUBTLE, INK, named[col]),
                   alpha=lead * max(base, named[col]) * ease * keep[col], ha="center", va="top")
        if shown > 0:
            c.line([(view.x(p), AXIS_Y), (view.x(p + 1), AXIS_Y)], INK, 3.6, z=3, alpha=shown)
        for key, (col, _, reveal) in self.turns.items():
            a = smoothstep(tl.travel(reveal, t) / 0.5)
            if a > 0:
                x = view.x(col)
                c.line([(x, AXIS_Y + 3), (x, AXIS_Y + 14)], self.by_key[key].profile.color, 3.4, z=4, alpha=a)
        c.text(self.apex_x, BOTTOM_ROW_Y, tracked("effort, % MVC"), size=KICKER_PT, color=SUBTLE,
               alpha=a_in)

    def _tick_room(self, sx, offered):
        """Room for the effort labels on the axis at column width sx. The efforts on
        offer are named while each has room beside the next, and any other label
        they would touch gives way to them. Returns, per column, how far its label
        is named as on offer and the share of its alpha it keeps."""
        half, n = self.tick_half, self.ladder.n

        def clear(i, j):  # 0 with less than half the pad between the two labels' glyphs, 1 with all of it
            return smoothstep(2 * (abs(j - i) * sx - half[i] - half[j]) / LABEL_PAD - 1)

        fit = 1.0
        for col in range(n):
            pair = smoothstep(min(offered[col], offered[col + 1]) / 0.25)  # in full once both are a quarter in
            if pair > 0:
                fit *= 1 - pair * (1 - clear(col, col + 1))
        named = [o * fit for o in offered]
        keep = [1.0] * (n + 1)
        for j, on in enumerate(named):
            if on > 0:
                for i in range(n + 1):
                    if i != j:
                        keep[i] *= 1 - on * (1 - clear(i, j))
        return named, keep

    # -- panel

    def _draw_panel(self, t):
        """The summary, top to bottom: what this is and the model behind it, the
        results so far, the value functions beside how each decision maker values
        every path, and the source. Its top and bottom lines are the lattice's."""
        c, tl = self.canvas, self.timeline
        x0, x1 = self.panel_x0, self.panel_x1
        a_head = a_body = self.intro(t)
        done = tl.completed(t)

        y = self.top - c.ink("D", TITLE_PT, display=True)[3]  # capitals hang from the top line
        c.text(x0, y, TITLE, size=TITLE_PT, display=True, alpha=a_head)
        y -= 42
        n = self.ladder.n
        choices = f"{n} choices" if n != 1 else "1 choice"
        for line in LEDE:
            c.text(x0, y, line.format(choices=choices), size=LEDE_PT, color=BODY, alpha=a_head)
            y -= 25
        y = self._draw_definitions(a_head, x0, x1, y - 21)
        y = self._draw_results(t, done, a_body, x0, x1, y - 58)

        # value functions beside each path valued by each decision maker: the plot is
        # square and spans the table, from the top of its first line to its last row
        y -= 62
        spanner, _, rows = self._table_rows(y)
        table_top = spanner + c.ink("path of", HEAD_PT)[3]
        self._draw_functions(t, a_body, x0, y, table_top, rows[-1])
        self._draw_matrix(t, done, a_body, x0 + MATRIX_LEFT, x1, y)
        c.text(x0, BOTTOM_ROW_Y, CITATION, size=CITATION_PT, color=SUBTLE, alpha=a_head)

    def _draw_definitions(self, a, x0, x1, y):
        """The model and how a choice is made, then the terms of the results;
        returns the last baseline."""
        c, ladder = self.canvas, self.ladder
        text_x = x0 + 104
        c.text(x0, y, "model", size=NOTE_PT, alpha=a)
        c.text(text_x, y,
               r"$sv(x, E) = x\,[1 + \omega\,\delta_1 E^{\gamma_1} - (1 - \omega)\,\delta_2 E^{\gamma_2}]$"
               rf"$,\quad E = \%\,\mathrm{{MVC}}\,/\,{round(ladder.max_mvc, 4):g}$",
               size=EQUATION_PT, alpha=a)
        y -= 34
        c.text(x0, y, "choice", size=NOTE_PT, alpha=a)
        if self.walks[0].sampled:
            taus = {w.profile.tau for w in self.walks}
            tau = rf"\tau = {taus.pop():g}" if len(taus) == 1 else r"\tau\ \mathrm{fitted}"
            c.text(text_x, y, r"$P(\mathrm{harder}) = 1\,/\,(1 + e^{-\tau\,\Delta sv})$" rf"$,\quad {tau}$",
                   size=EQUATION_PT, alpha=a)
            c.text(x1, y, r"sampled;  $\Delta sv$ = harder $-$ easier", size=NOTE_PT, color=BODY, alpha=a,
                   ha="right")
        else:
            c.text(text_x, y, r"the option of higher $sv(x, E)$", size=EQUATION_PT, alpha=a)
        y -= 38
        terms = (
            ("harder", "times the harder option, up in the tree, was chosen"),
            ("turned at", "effort at the path’s highest peak or lowest trough"),
            ("value", rf"$\Sigma\,sv(x, E)$ over the outcomes collected, each of nominal value $x = {NOMINAL:g}$"),
        )
        for i, (term, note) in enumerate(terms):
            if i:
                y -= 28
            c.text(x0, y, term, size=NOTE_PT, alpha=a)
            c.text(text_x, y, note, size=NOTE_PT, color=BODY, alpha=a)
        return y

    def _draw_results(self, t, done, a, x0, x1, y):
        """The results so far, one row per decision maker, top of the lattice
        first; returns the last row's baseline."""
        c, tl, ladder = self.canvas, self.timeline, self.ladder
        c.text(x0, y, tracked("Results"), size=KICKER_PT, color=SUBTLE, alpha=a)
        c.text(x1, y, f"outcomes collected: {done} each", size=NOTE_PT, color=SUBTLE, alpha=a, ha="right")
        col_harder, col_turned, col_value = x1 - 250, x1 - 120, x1
        y -= 38
        for label, x in (("harder", col_harder), ("turned at", col_turned), ("value", col_value)):
            c.text(x, y, label, size=HEAD_PT, color=SUBTLE, alpha=a, ha="right")
        c.line([(x0, y - 14), (x1, y - 14)], RULE, 1.0, z=20, alpha=a)
        for i, w in enumerate(self.order):
            yy = y - 51 - 44 * i
            c.circle((x0 + 7, yy + 7), 6.5, w.profile.color, z=21, alpha=a)
            c.text(x0 + 28, yy, w.profile.name, size=ROW_PT, alpha=a)
            c.text(col_harder, yy, f"{w.harder(done)}", size=ROW_PT, alpha=a, ha="right")
            turn = self.turns.get(w.key)
            seen = turn is not None and tl.travel(turn[2], t) > 0.5
            c.text(col_turned, yy, f"{ladder.label(turn[0])}%" if seen else "—", size=ROW_PT,
                   color=INK if seen else SUBTLE, alpha=a, ha="right")
            c.text(col_value, yy, number(w.value(done)) if done else "—", size=ROW_PT,
                   color=INK if done else SUBTLE, alpha=a, ha="right")
        return yy

    def _table_rows(self, y):
        """Baselines of the cross-valuation table under a section title at y: the
        'path of' spanner, the column labels, and one row per decision maker."""
        spanner = y - 38
        header = spanner - 42
        return spanner, header, [header - 38 - 33 * i for i in range(len(self.order))]

    def _draw_functions(self, t, a, x0, y, top, bottom):
        """Every decision maker's value function on one square plot, open at the
        top: sv over effort in multiples of x, a dashed line at x itself, the band
        of the choice at hand, and a ring where each path turned once the turn is
        revealed."""
        c, tl, ladder, n = self.canvas, self.timeline, self.ladder, self.ladder.n
        lo, hi = self.f_axis
        side = top - bottom
        widest = max(self.canvas.ink(label, HEAD_PT)[2] for _, label in self.f_ticks if label)
        left = x0 + widest + 11
        right = left + side

        def at(e, v):
            return left + e * side, bottom + (v - lo) / (hi - lo) * side

        c.text(x0, y, tracked("Value functions"), size=KICKER_PT, color=SUBTLE, alpha=a)
        c.line([at(0.0, 1.0), at(1.0, 1.0)], AXIS, 1.0, z=20, alpha=a, dash=(4, 4))
        c.line([(left, top), (left, bottom), (right, bottom)], AXIS, AXIS_W, z=20, alpha=a)
        for v, label in self.f_ticks:
            yy = at(0.0, v)[1]
            c.line([(left - 5, yy), (left, yy)], AXIS, AXIS_W, z=20, alpha=a)
            if label:
                c.text(left - 9, yy, label, size=HEAD_PT, color=SUBTLE, alpha=a, ha="right", va="center")
        for e in (0.0, 0.5, 1.0):  # none, half and all of the ladder's maximum
            xx = at(e, lo)[0]
            c.line([(xx, bottom), (xx, bottom - 5)], AXIS, AXIS_W, z=20, alpha=a)
            c.text(xx, bottom - 9, f"{round(e * ladder.max_mvc, 2):g}", size=HEAD_PT, color=SUBTLE, alpha=a,
                   ha="center", va="top")
        c.text((left + right) / 2, bottom - 31, "% MVC", size=HEAD_PT, color=SUBTLE, alpha=a, ha="center",
               va="top")
        p, shown = self.window(t)
        if shown > 0:
            c.rect(at(p / n, lo)[0], bottom, at((p + 1) / n, lo)[0], top, INK, z=20, alpha=0.14 * shown * a)
        for w in reversed(self.order):
            pts = [at(e, v) for e, v in zip(self.f_e, self.f_v[w.key])]
            c.line(pts, w.profile.color, 2.6, z=21, alpha=a)
            turn = self.turns.get(w.key)
            if turn:
                revealed = smoothstep(tl.travel(turn[2], t) / 0.5)
                if revealed > 0:
                    e = turn[0] / n
                    c.ring(at(e, w.profile.multiplier(e)), 6.5, w.profile.color, 2.2, z=22, alpha=revealed * a)

    def _draw_matrix(self, t, done, a, x0, x1, y):
        """How each decision maker values every path so far, its own path in bold,
        and, once the walk is over, whether each values its own path most."""
        c, tl = self.canvas, self.timeline
        c.text(x0, y, tracked("Each path, valued by each decision maker"), size=KICKER_PT, color=SUBTLE,
               alpha=a)
        matrix = cross_valuation(self.ladder, self.walks, done)
        cols = [x1 - MATRIX_PITCH * (len(self.order) - 1 - j) for j in range(len(self.order))]
        block_left = cols[0] - 66
        spanner, header, rows = self._table_rows(y)
        c.text((block_left + x1) / 2, spanner, "path of", size=HEAD_PT, color=SUBTLE, alpha=a, ha="center")
        c.line([(block_left, spanner - 14), (x1, spanner - 14)], RULE, 1.0, z=20, alpha=a)
        c.text(x0, header, "valued by", size=HEAD_PT, color=SUBTLE, alpha=a)
        for x, w in zip(cols, self.order):
            c.text(x, header, w.profile.label, size=MATRIX_PT, color=w.profile.color, alpha=a, ha="right")
        for yy, valuer in zip(rows, self.order):
            c.text(x0, yy, valuer.profile.label, size=MATRIX_PT, color=valuer.profile.color, alpha=a)
            for x, owner in zip(cols, self.order):
                own = owner.key == valuer.key
                text = number(matrix[(valuer.key, owner.key)]) if done else "—"
                c.text(x, yy, text, size=MATRIX_PT, color=INK if own and done else SUBTLE,
                       weight="bold" if own and done else "normal", alpha=a, ha="right")
        if done and own_path_best(matrix, self.walks):
            a_note = a * smoothstep((t - tl.walk_end) / (0.8 * tl.scale))
            c.text(x1, rows[-1] - 33, "each values its own path most", size=NOTE_PT, color=SUBTLE, alpha=a_note,
                   ha="right")

    def close(self):
        self.canvas.close()
