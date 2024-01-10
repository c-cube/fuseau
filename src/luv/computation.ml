include Picos.Computation

let[@inline] is_done self = not (is_running self)
