# stuff
Random and mostly useless stuff I write before going to sleep.

I may or may not keep a brief description of what's in here in this README, but only if I feel like it.
Hopefully the code itself will be decent, but that's about it. The small things may not even have tests.

As a general rule, these are just excuses to play with a specific language or with a problem that feels like fun. Sometimes that means reinventing the wheel, and sometimes it means wasting some time.

## Automaze.py
A simple dungeon generator in Python. Can be run "as is" I think, and outputs to the console.
Results may be weird depending on console settings; I have to set the line height to 0.5 on OS X so that
the boxy characters I use to "draw" look kinda square and connect across lines. I know, it's kinda lame in that sense.

Inspired by (read: basically stolen from) http://journal.stuffwithstuff.com/2014/12/21/rooms-and-mazes/  
(Thanks, by the way!)

## Scales.hs
I've (finally!) started to look into musical modes! They're pretty straightforward to compute, but a little messier to visualize on a guitar neck. So this thing prints out a bunch of info about an arbitrary key/mode combination, including a rudimentary representation of a guitar neck.

Visualizations on a console really can only be so good - but I almost feel like this is starting to resemble something useful. I'm pretty sure I'm mixing concepts that don't really belong together - for example, it probably doesn't make sense to say things like "Pentatonic Dorian", but here it's a valid input. It's OK because I'm probably going to be the only person using it, and I care more about being able to see the pentatonic on the fretboard than about the proper terminology, but it's something to keep in mind.

Besides, it's as good an excuse to play with Haskell as any, I guess - I'm still kinda new at it, so yeah. Sometimes I enjoy combining the different things I'm trying to learn.

(Which reminds me - I fear the code for this thing wouldn't be considered particularly pretty by those who know anything about Haskell. Sorry. I try.)
