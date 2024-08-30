signature PARSER = sig
  val export_src_chunks : string -> string;
  val parse : string -> Chunk.t list list;
end;
