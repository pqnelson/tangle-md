signature METADATA = sig
  type t;
  type Metadata = t;
  type key = string;
  type value = string;
  exception Runaway of string;

  val from_codefence_block : substring -> Metadata * substring;
  val language : Metadata -> string option;
  val is_example : Metadata -> bool;
  val get : Metadata -> key -> value option;
  val dbg : Metadata -> string;
end;
