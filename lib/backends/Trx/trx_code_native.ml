(* An implementation of the Cde signature, using the 
   MetaOCaml code type and code generation facilities
   MetaOCaml native
*)

include Trx_code_common

let ident = "MetaOCaml native"

(* Use byte-code, for debugging *)
let run_capture_output = run_capture_output_gen Runnative.run 

let run = Runnative.run
