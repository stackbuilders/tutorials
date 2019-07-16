---
title: Video Generation with Python
published: 2019-07-17
tags: Python, video, multimedia, custom media
language: python
author-name: Sebastian Arias
twitter-profile: sebasarias95
github-profile: larox
description: Python is a language that can be use for everything, generating multimedia content like a video is a good and fun way to get started in Python.
---


When you hear about Python, you might have in mind the powerful interpreted
programming language used for data science, artificial intelligence and for
web apps with [Flask][flask] or [Django][django]. However, Python is not only
capable of achieving all of this, but here in Stack Builders we’ve also used
it to create and render a fully customized video with animations, text and
draw vectors using Python’s [Moviepy][moviepy] and [Gizeh][gizeh] libraries.

## The Challenge

Creating a customized video might sound easy, especially if tools for video
editing are an option. Creating one or maybe a couple of them might even
sound fun, but if you need to generate hundreds or even thousands of
customized videos daily, this does not sound like a good idea. Imagine
Facebook manually creating millions of videos for friendship anniversaries,
birthdays and marketing campaigns for all their users. That would require an
extreme amount of time to do, create delays in the delivery of the videos,
and take away from an ideal user experience.

Producing a video programmatically might not be a task developers come across
daily. Having a firm understanding of what the final video should look like
is vital for accomplishing this task.

## Facing the challenge

Here is where software development comes in handy and helps automate tasks to
make our lives easier once again. Having a toolbelt filled with tools like
Python gives us the upper hand in this challenge. This open source
tutorial/contribution will explain how to work with multiple video segments
using instances from Gizeh and Moviepy together.

## What is Moviepy?

You might have heard of [FFMPEG][ffmpeg] or [ImageMagick][imagemagick] for
image and video edition in a programmatically way. [MoviePy][moviepy] is a
Python module for video editing (Python wrapper for FFMPEG). It provides
functions for cutting, concatenations, title insertions, video compositing,
video processing, and the creation of custom effects. It can read and write
common video and audio formats and be run on any platform with Python 2.7 or
3+.

## What is Gizeh?

[Gizeh][gizeh] is a library for drawing vectors in Python. It implements some
classes on top of [cairocffi][cairo] to make it more intuitive.

## What do we need?

For this tutorial, Python 3 is required and will be using pipenv. All the
modules can be installed from PyPI (Python Package Index) using `pip`. For
further instructions check this file [here][readme].

## Getting Started

This sample code will help to understand some basic concepts about Python, Moviepy
and Gizeh in a fun way.

### Importing the libraries

```python
import Moviepy.editor as mpy
import Gizeh as gz
from math import pi
```
In this piece of code we are telling Python to add the libraries that are
going to be used in this tutorial to keep it simple. We are adding all of the
MoviePy tools inside its *editor* module and renaming it to mpy to keep it
short.

In Gizeh's case, we are adding it completely and renaming it to `gz`.

And finally we import `pi` from `math` python module to make some
calculations easier in the code.

### Writing text

```python
BLUE = (59/255, 89/255, 152/255)

def render_text(t):
    surface = gz.Surface(640, 60, bg_color=(1, 1, 1))
```

The `render_text` function will set up all that Gizeh needs to draw what we want.
This function is important to make the process easy to understand, keeping
Gizeh code apart from Moviepy code. This function receives as a parameter the time
`t` that will be explained later on.

You might have noticed that `BLUE` is a RGB color. Gizeh works
with RGB but on a scale from `0` to `1`, so white is `(1, 1, 1)`
equivalent to `(255, 255, 255)`. In the case of `BLUE` we explicitly divide
the Stack Builders blue `(59, 89, 152)` to match [Gizeh's format][gizeh_rgb].

Gizeh likes to works like an artist; it needs a canvas to draw all of the things
we specify in our code. So we need to create a `surface` with `width`, `height`
and `background color`. In this case our surface will be `width=640`,
`height=60` and `background color = (1, 1, 1)`.

``` Python
text = gz.text("Let's build together", fontfamily="Charter",
                fontsize=30, fontweight='bold', fill=BLUE, xy=(320, 40))
```

With the surface ready, we can start drawing things in Gizeh. To draw the
text in our surface, we will be using `gz.text`. This function receives as a
first parameter the text to render. The other parameters are used to style
the text (key parameters that are self explanatory). `xy` receives
coordinates for positioning the text inside the surface. In this case
`xy = (320, 40)`.

``` Python
text.draw(surface)
return surface.get_npimage()
```

Now with the `text` ready we can `draw` it in the `surface` created
earlier on. To accomplish this we just need to tell the `text` on which surface it
needs to be drawn.

To get the surface with all of the content drawn on it, we use
`surface.get_npimage()`. This returns a **numpy array** with all of the drawings
on it. This array can be used with Moviepy.

``` Python
text = mpy.VideoClip(render_text, duration=10)
```

It's time to use Moviepy! Now, using `VideoClip` we are able to create an
instance of a video in memory. With the `render_text` function returning the
text in a **numpy array**, we can convert the array into a Moviepy clip. The
`mpy.VideoClip` function receives as required parameters: what we want to
render or the *frame* that will be converted into a video clip and the
duration it should have. Do you remember the `t` parameter in the
`render_text` function? Internally `mpy.VideoClip` saves the frame of the
clip corresponding to the time `t` in the *frame*. The resulting video
clip will automatically size to match our previous canvas (`640x60`), so
there is no need to specify it.

### Adding an Image File

Now that we have the text ready, we can add some images to the video to make
it more attractive to the viewers. For this we will be using the following code:

``` Python
SB_logo_path = './assets/StackBuildersLogo.jpg'
sb_logo = mpy.ImageClip(SB_logo_path).\
    set_position(('center', 0)).\
    resize(width=200)
```

The `mpy.ImageClip` function receives as a parameter the image to render. In
this case the image path is set in the variable `SB_logo_path`. Using
`ImageClip` attributes, it's possible to position the image using x, y
coordinates. In this case, it's possible to mix keywords like `center`,
`bottom` and `top` with specific coordinates.

Finally, the `resize` attribute reduces the size of the image keeping its
ratio aspect using the key arguments `width` or `height` like in the snippet.
It is possible to set a custom size explicitly using a tuple with the new
dimensions `(width, height)`.

### Adding Vector Drawings

With the text and image ready there is just one thing left to implement:
Gizeh drawings.

Gizeh is capable of drawing multiple elements like lines, circles,
rectangles, etc. However, in this case we are going to use `gz.star` that is
going to draw a custom star for us. (Just because this video deserves 5 Gizeh
stars)

``` Python
def draw_stars(t):
    surface = gz.Surface(640, 120, bg_color=(1, 1, 1))
```

As for the text, we first start by setting the surface we are going to use to draw
the stars.

``` Python
for i in range(5):
    star = gz.star(nbranches=5, radius=120*0.2,
                   xy=[100*(i+1), 50], fill=(0, 1, 0),
                   angle=t*pi)
    star.draw(surface)
```
This is where the fun begins! As we said before, this video deserves 5 stars,
and since drawing them one-by-one could be very repetitive, we are going to
use a loop. The `gz.star` element has as a first parameter `nbranches` that
represents the number of star tips we want to draw. Then the `radius`
determines the size of the stars (imagine a circle surrounding the star). The
`xy` parameter works exactly as in `gz.text`. In this case we are just
calculating the `x` coordinate depending on the *for* loop step to position
each star dynamically and using the `fill` parameter our star will be green. To
animate the star we are using the `angle` parameter that gives us the change
of rotating the star on its own axis. This attribute uses radians, so to
rotate the stars we are multiplying `pi` (180 degrees) by the time in the
video.

At the end of each loop step, a star is drawn on the surface we previously
set. This works in the same way as in `gz.text`.

``` Python
return surface.get_npimage()
```

Finally, a **numpy array** is returned.


``` Python
stars = mpy.VideoClip(draw_stars, duration=10)
```

Now, using `mpy.VideoClip`, we have all of the stars inside a clip. (Like we
did with the `text`.)

### Generating The Video Clip

``` Python
WHITE = (255, 255, 255)
VIDEO_SIZE = (640, 480)
```

Before generating and writing the complete video, we first need to create some
variable to keep the code easy to understand. The first one is `WHITE`. It is
going to be the background color. Unlike Gizeh, Moviepy uses the traditional
RGB scale (0 - 255).

The second one is `VIDEO_SIZE`, we will be using a standard of `640x480`

```Python
video = mpy.CompositeVideoClip(
    [
        sb_logo,
        text.set_position(
            ('center', sb_logo.size[1])),
        stars.set_position(
            ('center', sb_logo.size[1] + text.size[1])
        )
    ],
    size=VIDEO_SIZE).\
    on_color(
        color=WHITE,
        col_opacity=1).set_duration(10)
```

Using `mpy.CompositeVideoClip` we can generate a `VideoClip` made up of other
clips displayed together. The first parameter is an array of the clips to be
used. In the case of `text` and `star` clips, the positions are set here
directly to use the `sb_logo` attribute `size` that provides a tuple of the
clip dimensions `(width, height)`. This helps to calculate in an easier way
the `y-position` of each clip.

The second parameter in `CompositeVideoClip` is the size of the final clip.
In this case `(640x480)`.


The `on_color` attribute helps to set the background color of the `VideoClip`
to `color=WHITE`. The `on_color` attribute has some extra parameters to
change `size`, `position` and `color opacity`, but for now we don't need
them.

And last but not least, the video duration is set to 10 seconds using
`set_duration` attribute.

```Python
video.write_videofile('video_with_python.mp4', fps=10)
```

Finally, the `write_videofile` attribute writes in the disk the `VideoClip`
result of the `CompositeVideoClip` stored in memory. Its first parameter is
the `video file name`, then we define the `fps` rate. The `write_videofile`
has multiple parameters but we just need to set these two for now.

Now if we run the code using `python python_video.py`, we will
have a 10 seconds video like this:

<div style="text-align:center">
<img src="https://github.com/stackbuilders/tutorials/blob/tutorials/tutorials/python/python-video-generation/images/video.gif" width=500 /> </br>
**Figure 1** - Video generated by our code
</div>

This might seem simple, but its our first video generated by code! If you
want to dig deeper into the code, [here][link-to-the-code] is the link of the
final version of the project

## Conclusions

Creating a video is a fun task and doing it using code is even better!

This tutorial proves that you don't need to be a Python expert to generate a
video using code from scratch. This is one of the advantages of Python as a
language. It's useful beyond tasks like web development, data science or
machine learning. Its versatility allows you to accomplish tasks like
rendering customized images and videos with an easy to understand syntax so
you can start using it if you have any programming experience with other
languages.

Creating multimedia content with code might be tricky at the beginning, but
once you get started it's hard to stop adding multiple animations or new
segments.

Following this tutorial step by step gives the basic concepts needed to start
using Moviepy and Gizeh with Python to experiment with videos or image
rendering. Animating video objects with these libraries is also doable adding
just a bit of complexity in the calculations.

[django]: https://www.djangoproject.com
[flask]: http://flask.pocoo.org/
[moviepy]: https://zulko.github.io/Moviepy/
[gizeh]: https://github.com/Zulko/Gizeh
[cairo]: https://cairocffi.readthedocs.io/en/stable/
[readme]: https://github.com/stackbuilders/tutorials/blob/tutorials/tutorials/python/python-video-generation/code/Readme.md
[gizeh_rgb]: https://github.com/Zulko/Gizeh#fill-and-stroke
[ffmpeg]: https://ffmpeg.org/
[imagemagick]: https://imagemagick.org/index.php
[link-to-the-code]: https://github.com/stackbuilders/tutorials/tree/tutorials/tutorials/python/python-video-generation/code


