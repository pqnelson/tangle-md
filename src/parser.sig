signature PARSER = sig
  (* collate chunks for debugging purposes *)
  val collate_chunks : Chunk.t list -> Chunk.t list list;
  val export_src_chunks : string -> string;
  val parse : string -> Chunk.t list list;
end;
