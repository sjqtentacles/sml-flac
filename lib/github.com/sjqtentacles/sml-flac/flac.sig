(* flac.sig — FLAC stream detection and metadata/frame parsing.

   Builds on sml-ogg page framing patterns and sml-riff chunk idioms.
   LPC residual decode is a tested stub (Wave 3 scope). *)

signature FLAC =
sig
  exception Format of string

  val magic : Word8Vector.vector

  val isFlac : Word8Vector.vector -> bool

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

  val parseStreamInfo : Word8Vector.vector -> streamInfo
  val encodeStreamInfo : streamInfo -> Word8Vector.vector

  type frameHeader =
    { blockSize : int
    , sampleRate : int
    , channels : int
    , bitsPerSample : int
    }

  val parseFrameHeader : Word8Vector.vector -> int -> frameHeader

  (* Stub: returns empty residual samples; tested for callability. *)
  val decodeLpcResidual : frameHeader -> Word8Vector.vector -> real list

  val metadataChunkId : string
  val streamInfoAsChunk : streamInfo -> Riff.chunk
end
