include module type of struct
  include Fuseau
end

module IO_unix = IO_unix
module Net = Net

val main : (unit -> 'a) -> 'a
(** A version of {!Fuseau.main} that uses a Unix-based event loop. *)
