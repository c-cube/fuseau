(** High level concurrent streams.

    The design is closely inspired from web streams
    (see: https://streams.spec.whatwg.org/ and
    https://developer.mozilla.org/en-US/docs/Web/API/Streams_API)
*)

(** Reader *)
module Reader = struct
  (** Reader: an object to read from a readable stream *)
  class type ['a] t =
    object
      inherit Disposable.t
      method closed : bool
      method read : 'a option
    end

  (** Bring your own buffer reader *)
  class type byob_t =
    object
      inherit [Buffer.t] t

      method read_into : Buffer.t -> int -> int -> int
      (** [read buf i len] reads at most [len] bytes into [buf]
        starting at offset [i].
        If it returns [0] it means the stream has come to an end. *)
    end
end

(** Writer *)
module Writer = struct
  (** Writer: an object to write into a writable stream *)
  class type ['a] t =
    object
      inherit Disposable.t
      method closed : bool

      method write : 'a -> unit
      (** Write the given object *)
    end

  (** Bring your own buffer writer *)
  class type byob_t =
    object
      inherit [Buffer.t] t
      method write_slice : Buffer.t -> int -> int -> unit
      method writev : Buffer.t list -> unit
    end
end

exception Already_in_use

(** Writable stream *)
module Writable = struct
  (** Writable stream *)
  class type ['a] t =
    object
      inherit Disposable.t

      method is_in_use : bool
      (** Does the stream currently have an active writer/pipe? *)

      method get_writer : 'a Writer.t
      (** Obtain a writer into this stream.
          @raise Already_in_use if the stream is already being used *)
    end

  class type byte_t =
    object
      inherit [Buffer.t] t

      method get_byob_writer : Writer.byob_t
      (** Obtain a BYOB writer into this stream.
          @raise Already_in_use if the stream is already being used *)
    end
end

(** Readable stream *)
module Readable = struct
  (** Readable stream *)
  class type ['a] t =
    object
      inherit Disposable.t

      method is_in_use : bool
      (** Is the stream being read from by something? If true,
          trying to create more pipes or readers will fail.

          This corresponds to the [locked] property on web streams. *)

      method get_reader : 'a Reader.t
      (** Obtain a reader.
          @raise Already_in_use if the stream is already being used *)

      (* TODO: do we need this general version? Or can it be done in a virtual class?
          method pipe_into : 'a #Writer.t -> unit
      *)
    end

  class type byte_t =
    object
      inherit [Buffer.t] t

      method get_byob_reader : Reader.byob_t
      (** Obtain a BYOB reader.
          @raise Already_in_use if the stream is already being used *)

      method pipe_into_byte_stream : #Writable.byte_t -> unit
    end
end

(** Readable and writable streams *)
module ReadWritable = struct
  (** Readable and writable streams *)
  class type ['a] t =
    object
      inherit ['a] Readable.t
      inherit ['a] Writable.t
    end

  class type byte_t =
    object
      inherit Readable.byte_t
      inherit Writable.byte_t
    end
end
