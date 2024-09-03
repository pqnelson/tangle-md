structure Chunk :> CHUNK = struct
  type t = { meta : Metadata.t
           , code : substring
           };

  type Chunk = t;

  fun new m s = {meta = m, code = s};

  fun from (m,s) = {meta = m, code = s};

  fun metadata ({meta,...} : t) = meta;

  fun code ({code,...} : t) = code;

  fun is_example ({meta, ...} : t) = Metadata.is_example meta;

  fun language ({meta, ...} : t) = Metadata.language meta;

  fun has_language chunk =
    NONE <> (language chunk);

  fun same_language lhs rhs =
    has_language lhs andalso
    has_language rhs andalso (* provably symmetric *)
    language lhs = language rhs;

  fun same_name lhs rhs =
    Metadata.same_name (metadata lhs) (metadata rhs);

  val has_no_name =
    Metadata.has_no_name o metadata;

  fun eq lhs rhs =
    (Metadata.eq (metadata lhs) (metadata rhs)) andalso
    ((Substring.base (code lhs)) = (Substring.base (code rhs)));

  fun file this =
    case (Metadata.get (metadata this) "file") of
        NONE => ""
      | SOME f => f;
end;
