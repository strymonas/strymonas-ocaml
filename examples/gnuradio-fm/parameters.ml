let gain = 1.
let samplingRate = 3_072_000.
let cutoff = 75_000.
let trWidth = 
  let numberOfTaps = 65 in
  53. *. samplingRate /. (22. *. float numberOfTaps)
let maxDev = 75_000.
