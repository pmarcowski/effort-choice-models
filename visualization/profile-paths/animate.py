# -*- coding: utf-8 -*-

"""
Profile Paths Animation

Renders four decision makers, one per effort preference profile of the
Dual-Power Model, making the same sequence of choices between neighboring
efforts in % MVC, each choice sampled from the model's softmax rule, and
parting where their value functions disagree, beside live results that end
on how each decision maker values every path.

Usage:
    python animate.py                        # profile-paths.mp4, the designed profiles
    python animate.py --seed 7               # another draw of the sampled choices
    python animate.py --deterministic        # take the option of higher value instead
    python animate.py --profiles figure1     # the published Figure 1 examples
    python animate.py --profiles figure5     # the Figure 5b participants, with fitted τ
    python animate.py --choices 16 --max-mvc 80
    python animate.py --dry-run              # print parameters and results, render nothing
    python animate.py --still 12 --out frame.png
    python animate.py --gif profile-paths.gif
"""

__author__ = "Przemyslaw Marcowski"
__email__ = "p.marcowski@gmail.com"
__license__ = "GPL 3.0"

import argparse
import os
import random
import subprocess
import sys
import time

from PIL import Image

from dual_power import PROFILE_SETS, SEED
from ladder import Ladder, dry_run_text, walk_all
from scene import Scene

HERE = os.path.dirname(os.path.abspath(__file__))


def parse_args(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[1],
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--profiles", choices=list(PROFILE_SETS), default="designed",
                   help="parameter set: designed for this animation (default), the published "
                        "Figure 1 examples, or the Figure 5b participants")
    p.add_argument("--choices", type=int, default=20, help="number of choices (default 20)")
    p.add_argument("--max-mvc", type=float, default=100.0,
                   help="highest effort on the ladder, in %% MVC (default 100)")
    p.add_argument("--tau", type=float, default=None, help="choice sensitivity for every profile, overriding the set's")
    p.add_argument("--seed", type=int, default=SEED, help=f"seed for the sampled choices (default {SEED})")
    p.add_argument("--deterministic", action="store_true",
                   help="take the option of higher value at every choice instead of sampling")
    p.add_argument("--speed", type=float, default=1.0, help="pace multiplier (default 1)")
    p.add_argument("--no-lattice", action="store_true", help="hide the faint lattice of all possible paths")
    p.add_argument("--show-p", action="store_true",
                   help="print P(harder) beside each decision maker while it weighs a choice")
    p.add_argument("--size", default="1920x1080", help="frame size, WxH, composed for 16:9 (default 1920x1080)")
    p.add_argument("--fps", type=int, default=60)
    p.add_argument("--out", default=os.path.join(HERE, "profile-paths.mp4"),
                   help="output video (.mp4), or image (.png) with --still")
    p.add_argument("--gif", default=None, help="also write a GIF preview (15 fps)")
    p.add_argument("--gif-width", type=int, default=640, help="width of the GIF preview in pixels (default 640)")
    p.add_argument("--still", type=float, default=None, help="render one frame at this time (s) as PNG")
    p.add_argument("--dry-run", action="store_true", help="print parameters and results, and exit")
    return p.parse_args(argv)


def build(args):
    ladder = Ladder(args.choices, args.max_mvc)
    profiles = PROFILE_SETS[args.profiles]
    if args.tau is not None:
        profiles = [p.with_tau(args.tau) for p in profiles]
    rng = None if args.deterministic else random.Random(args.seed)
    return ladder, walk_all(ladder, profiles, rng)


def write_video(scene, path, fps):
    import imageio_ffmpeg

    w, h = scene.canvas.width, scene.canvas.height
    n = int(round(scene.timeline.total * fps))
    writer = imageio_ffmpeg.write_frames(path, size=(w, h), fps=fps, quality=9,
                                         macro_block_size=1, ffmpeg_log_level="error")
    writer.send(None)
    t0 = time.time()
    for i in range(n):
        writer.send(scene.draw(i / fps).tobytes())
        if i % max(1, n // 10) == 0 or i == n - 1:
            print(f"  {i + 1:5d}/{n} frames  {time.time() - t0:5.0f}s", file=sys.stderr)
    writer.close()


def write_gif(video, path, width):
    """A looping preview at 15 fps, encoded from the video with a palette fitted to it."""
    import imageio_ffmpeg

    graph = (f"fps=15,scale={width}:-1:flags=lanczos,split[a][b];"
             "[a]palettegen=max_colors=128:stats_mode=diff[p];"
             "[b][p]paletteuse=dither=bayer:bayer_scale=3:diff_mode=rectangle")
    subprocess.run([imageio_ffmpeg.get_ffmpeg_exe(), "-loglevel", "error", "-y", "-i", video,
                    "-vf", graph, path], check=True)


def main(argv=None):
    args = parse_args(argv)
    ladder, walks = build(args)
    if args.dry_run:
        print(dry_run_text(ladder, walks))
        return
    width, height = (int(v) for v in args.size.lower().split("x"))
    scene = Scene(ladder, walks, width=width, height=height, lattice=not args.no_lattice,
                  show_p=args.show_p, speed=args.speed)
    try:
        if args.still is not None:
            out = args.out if args.out.lower().endswith(".png") else os.path.splitext(args.out)[0] + ".png"
            Image.fromarray(scene.draw(args.still)).save(out)
            print(out)
            return
        print(f"rendering {scene.timeline.total:.1f}s at {args.fps} fps, {width}x{height}", file=sys.stderr)
        write_video(scene, args.out, args.fps)
        print(args.out)
        if args.gif:
            write_gif(args.out, args.gif, args.gif_width)
            print(args.gif)
    finally:
        scene.close()


if __name__ == "__main__":
    main()
