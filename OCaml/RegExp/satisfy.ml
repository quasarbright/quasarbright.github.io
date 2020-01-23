open Common
open Common.FSA

let satisfy fsa =
    let fsa = semi_determinize fsa in 
    