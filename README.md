# sml-flac

[![CI](https://github.com/sjqtentacles/sml-flac/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-flac/actions/workflows/ci.yml)

Wave 3 FLAC stream helper: `fLaC` detection, STREAMINFO parse/encode round-trip,
frame header parsing, and an LPC residual decode **stub** (tested empty).

Builds on **sml-ogg** page framing patterns and **sml-riff** chunk idioms.
**sml-fft** is vendored under `lib/` for future LPC work but is **not linked**
in `flac.mlb` — loading FFT triggers Poly/ML FP codegen crashes (same class of
issue as sml-geodesy).

## API sketch

```sml
Flac.isFlac bytes
val si = Flac.parseStreamInfo bytes
val hdr = Flac.parseFrameHeader frameBytes 0
Flac.decodeLpcResidual hdr frameBytes   (* stub: [] *)
```

## Building

```sh
make all-tests   # MLton + Poly/ML
```

## License

MIT. See [LICENSE](LICENSE).
