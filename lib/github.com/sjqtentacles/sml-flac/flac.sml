structure Flac :> FLAC =
struct
  exception Format of string

  val magic = Word8Vector.fromList [0wx66, 0wx4C, 0wx61, 0wx43]

  fun isFlac v =
      Word8Vector.length v >= 4
      andalso Word8Vector.sub (v, 0) = 0wx66
      andalso Word8Vector.sub (v, 1) = 0wx4C
      andalso Word8Vector.sub (v, 2) = 0wx61
      andalso Word8Vector.sub (v, 3) = 0wx43

  fun u16 v off =
      (Word8.toInt (Word8Vector.sub (v, off)) * 256)
      + Word8.toInt (Word8Vector.sub (v, off + 1))

  fun u24 v off =
      (Word8.toInt (Word8Vector.sub (v, off)) * 65536)
      + (Word8.toInt (Word8Vector.sub (v, off + 1)) * 256)
      + Word8.toInt (Word8Vector.sub (v, off + 2))

  fun u32 v off =
      (Word8.toInt (Word8Vector.sub (v, off)) * 16777216)
      + (Word8.toInt (Word8Vector.sub (v, off + 1)) * 65536)
      + (Word8.toInt (Word8Vector.sub (v, off + 2)) * 256)
      + Word8.toInt (Word8Vector.sub (v, off + 3))

  fun putU16 n =
      Word8.fromInt ((n div 256) mod 256) :: Word8.fromInt (n mod 256) :: []

  fun putU24 n =
      Word8.fromInt ((n div 65536) mod 256)
      :: Word8.fromInt ((n div 256) mod 256)
      :: Word8.fromInt (n mod 256) :: []

  fun putU32 n =
      Word8.fromInt ((n div 16777216) mod 256)
      :: Word8.fromInt ((n div 65536) mod 256)
      :: Word8.fromInt ((n div 256) mod 256)
      :: Word8.fromInt (n mod 256) :: []

  type streamInfo =
    { minBlockSize : int
    , maxBlockSize : int
    , minFrameSize : int
    , maxFrameSize : int
    , sampleRate : int
    , channels : int
    , bitsPerSample : int
    , totalSamples : LargeInt.int
    }

  fun parseStreamInfoBlock data off =
      if Word8Vector.length data < off + 34 then raise Format "STREAMINFO too short"
      else
        let
          val minBs = u16 data off
          val maxBs = u16 data (off + 2)
          val minFs = u24 data (off + 4)
          val maxFs = u24 data (off + 7)
          val w0 = u32 data (off + 10)
          val w1 = u32 data (off + 14)
          val sr = w0 div 4096
          val ch = ((w0 div 512) mod 8) + 1
          val bps = ((w0 div 16) mod 32) + 1
          val hi = w0 mod 16
          val total = LargeInt.+ (LargeInt.fromInt hi * 4294967296, LargeInt.fromInt w1)
        in
          { minBlockSize = minBs, maxBlockSize = maxBs
          , minFrameSize = minFs, maxFrameSize = maxFs
          , sampleRate = sr, channels = ch, bitsPerSample = bps
          , totalSamples = total }
        end

  fun parseStreamInfo v =
      if not (isFlac v) then raise Format "missing fLaC magic"
      else
        let
          val b0 = Word8.toInt (Word8Vector.sub (v, 4))
          val typ = b0 mod 128
          val len = u24 v 5
        in
          if typ <> 0 then raise Format "first metadata block is not STREAMINFO"
          else if len <> 34 then raise Format "STREAMINFO length must be 34"
          else parseStreamInfoBlock v 8
        end

  fun encodeStreamInfo si =
      let
        val { minBlockSize = minBs, maxBlockSize = maxBs
            , minFrameSize = minFs, maxFrameSize = maxFs
            , sampleRate = sr, channels, bitsPerSample
            , totalSamples = total } = si
        val ch = channels - 1
        val bps = bitsPerSample - 1
        val hi = LargeInt.toInt (LargeInt.div (total, 4294967296))
        val lo = LargeInt.toInt (LargeInt.mod (total, 4294967296))
        val w0 = (sr * 4096) + (ch * 512) + (bps * 16) + hi
        val dataBytes =
            putU16 minBs @ putU16 maxBs
            @ putU24 minFs @ putU24 maxFs
            @ putU32 w0 @ putU32 lo
            @ List.tabulate (16, fn _ => Word8.fromInt 0)
        val header = [0wx80, 0wx00, 0wx00, 0wx22]
      in Word8Vector.fromList (header @ dataBytes) end

  type frameHeader =
    { blockSize : int, sampleRate : int, channels : int, bitsPerSample : int }

  fun blockSizeFromCode code =
      case code of
          1 => 192 | 2 => 576 | 3 => 1152 | 4 => 2304 | 5 => 4608
        | 6 => 256 | 7 => 512 | 8 => 1024 | 9 => 2048 | 10 => 4096
        | 11 => 8192 | 12 => 16384 | 13 => 32768 | _ => 0

  fun sampleRateFromCode code =
      case code of
          0 => 0 | 1 => 88200 | 2 => 176400 | 3 => 192000
        | 4 => 8000 | 5 => 16000 | 6 => 22050 | 7 => 24000
        | 8 => 32000 | 9 => 44100 | 10 => 48000 | 11 => 96000 | _ => 0

  fun bitsFromCode code =
      case code of
          0 => 8 | 1 => 12 | 2 => 16 | 3 => 20 | 4 => 24 | _ => 0

  fun parseFrameHeader v off =
      if Word8Vector.length v < off + 4 then raise Format "frame header too short"
      else if Word8Vector.sub (v, off) <> 0wxFF then raise Format "frame sync"
      else
        let
          val b1 = Word8.toInt (Word8Vector.sub (v, off + 1))
          val b2 = Word8.toInt (Word8Vector.sub (v, off + 2))
          val b3 = Word8.toInt (Word8Vector.sub (v, off + 3))
        in
          if Word8.andb (Word8.fromInt b1, 0wxFC) <> 0wxF8 then raise Format "bad sync"
          else
            { blockSize = blockSizeFromCode (b2 div 16)
            , sampleRate = sampleRateFromCode (b2 mod 16)
            , channels = (b3 div 16) + 1
            , bitsPerSample = bitsFromCode ((b3 div 2) mod 8)
            }
        end

  fun decodeLpcResidual _ _ = []

  val metadataChunkId = "META"

  fun vectorToString v =
      String.implode
        (List.tabulate (Word8Vector.length v,
                        fn i => Char.chr (Word8.toInt (Word8Vector.sub (v, i)))))

  fun streamInfoAsChunk si =
      { id = metadataChunkId, data = vectorToString (encodeStreamInfo si) }
end
