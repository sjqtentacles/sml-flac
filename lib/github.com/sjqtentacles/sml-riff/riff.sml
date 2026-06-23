structure Riff :> RIFF =
struct
  type chunk = { id : string, data : string }
  exception Format of string

  fun pad4 n = if n mod 2 = 1 then n + 1 else n

  fun pow256 0 = 1 | pow256 i = 256 * pow256 (i - 1)

  fun u32 n =
    Buffer.build (fn b =>
      let fun byte i = Buffer.addChar b (Char.chr ((n div pow256 i) mod 256))
      in List.app byte [0,1,2,3] end)

  fun readU32 s off =
    let fun b i = Char.ord (String.sub (s, off + i))
    in b 0 + 256 * b 1 + 65536 * b 2 + 16777216 * b 3 end

  fun encodeChunk { id, data } =
    let val sz = String.size data
        val body = if sz mod 2 = 1 then data ^ "\000" else data
    in id ^ u32 sz ^ body end

  fun encode chunks =
    let val body = String.concat (List.map encodeChunk chunks)
    in "RIFF" ^ u32 (String.size body + 4) ^ "WAVE" ^ body end

  fun decode s =
    if String.size s < 12 orelse String.substring (s, 0, 4) <> "RIFF" then raise Format "not RIFF"
    else
      let
        fun readChunks off stop acc =
          if off + 8 > stop then rev acc
          else
            let
              val id = String.substring (s, off, 4)
              val sz = readU32 s (off + 4)
              val data = String.substring (s, off + 8, sz)
              val next = off + 8 + pad4 sz
            in readChunks next stop ({ id = id, data = data } :: acc) end
        val riffSize = readU32 s 4
      in readChunks 12 (8 + riffSize) [] end
end
