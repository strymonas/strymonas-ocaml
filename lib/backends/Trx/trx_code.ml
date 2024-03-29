(* An implementation of the Cde signature, using the 
   MetaOCaml code type and code generation facilities
   MetaOCaml bytecode
*)

include Trx_code_common

let ident = "MetaOCaml"

(* Use byte-code, for debugging *)
let run_capture_output = run_capture_output_gen Runcode.run 

let run = Runcode.run
