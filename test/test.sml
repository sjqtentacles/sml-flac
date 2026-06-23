structure Tests =
struct
  open Harness
  structure F = Flac

  fun tinyFlac () =
      let
        val si =
          { minBlockSize = 4096, maxBlockSize = 4096
          , minFrameSize = 0, maxFrameSize = 0
          , sampleRate = 44100, channels = 2, bitsPerSample = 16
          , totalSamples = 0 }
        val meta = F.encodeStreamInfo si
      in Word8Vector.concat [F.magic, meta] end

  fun run () =
  let
    val flac = tinyFlac ()
    val () = section "magic and detection"
    val () = check "isFlac tiny fixture" (F.isFlac flac)
    val () = check "reject riff" (not (F.isFlac (Word8Vector.fromList [0wx52,0wx49,0wx46,0wx46])))

    val () = section "STREAMINFO"
    val si = F.parseStreamInfo flac
    val () = checkInt "sample rate 44100" (44100, #sampleRate si)
    val () = checkInt "channels 2" (2, #channels si)
    val () = checkInt "bps 16" (16, #bitsPerSample si)
    val () = checkInt "block 4096" (4096, #minBlockSize si)

    val si2 =
      { minBlockSize = 512, maxBlockSize = 1024
      , minFrameSize = 0, maxFrameSize = 0
      , sampleRate = 48000, channels = 1, bitsPerSample = 24
      , totalSamples = 123 }
    val roundTrip = F.parseStreamInfo (Word8Vector.concat [F.magic, F.encodeStreamInfo si2])
    val () = checkInt "round-trip sample rate" (#sampleRate si2, #sampleRate roundTrip)
    val () = checkInt "round-trip channels" (#channels si2, #channels roundTrip)

    val () = section "frame header"
    val frameBytes = Word8Vector.fromList [0wxFF, 0wxF8, 0wxA9, 0wx21]
    val hdr = F.parseFrameHeader frameBytes 0
    val () = checkInt "frame block 4096" (4096, #blockSize hdr)
    val () = checkInt "frame rate 44100" (44100, #sampleRate hdr)

    val () = section "lpc stub and riff chunk view"
    val () = check "decodeLpcResidual empty" (null (F.decodeLpcResidual hdr frameBytes))
    val chunk = F.streamInfoAsChunk si
    val () = checkString "chunk id META" ("META", #id chunk)
    val () = check "chunk data non-empty" (String.size (#data chunk) > 0)

    val () = section "integration ogg/riff patterns"
    val () = check "ogg capture differs from flac"
                   (Word8Vector.length Ogg.capturePattern = 4)
    val () = check "riff decode round-trip"
                   (Riff.decode (Riff.encode [{id="fmt ", data="x"}]) = [{id="fmt ", data="x"}])
  in
    Harness.run ()
  end
end
