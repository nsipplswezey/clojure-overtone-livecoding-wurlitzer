# livecoding-wurlitzer

Live coding a reproduction of Chris Staples '[Wurlitzer](https://www.youtube.com/watch?v=edhvr72ZJ_s)'
Starts with sequence buffers. Then instruments. Then instrument triggers. Inspired by overtone examples. Attempts to be literate coding.

## Usage

Intended to be sequentially executed using a repl. I recorded and performed this piece using LightTable which is a very pleasant clojure IDE.

## Notes

Drum samples in this song come from freesound.org. They recently released the second version of their API, which hasn't been seamlessly integrated into overtone. It takes a bit of work to get it up and running.

See [this git issue](https://github.com/overtone/overtone/issues/333) for details on the current patch/workaround. It's not pretty. But it does work if you play with it.

The sample .wav files should be cached in the repo to save you the hassle.
