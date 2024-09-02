signature CHUNK = sig
  type t;
  type Chunk = t;

  val new : Metadata.t -> substring -> Chunk;
  val from : Metadata.t * substring -> Chunk
  val metadata : Chunk -> Metadata.t;
  val code : Chunk -> substring;
  val is_example : Chunk -> bool;
  val language : Chunk -> string option;
  val has_language : Chunk -> bool;
  val same_name : Chunk -> Chunk -> bool;
  val has_no_name : Chunk -> bool;
  val eq : Chunk -> Chunk -> bool;
end;
