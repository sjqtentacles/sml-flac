(* riff.sig — RIFF chunk reader/writer. *)

signature RIFF =
sig
  type chunk = { id : string, data : string }
  exception Format of string

  val encode : chunk list -> string
  val decode : string -> chunk list
end
