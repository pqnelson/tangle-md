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
  
  val same_name : Metadata -> Metadata -> bool;
  val has_no_name : Metadata -> bool;
  (* get the key-value pairs as a string, for debugging purposes *)
  val dbg : Metadata -> string;
  
  val eq : Metadata -> Metadata -> bool;
end;
