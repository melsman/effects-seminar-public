
val name = CommandLine.name()

fun mem c s = CharVector.foldl(fn (c',b) => b orelse c=c') false s

(* not exactly permutation, but... *)
fun relate x y =
    (x <> y andalso
     size x = size y andalso
     CharVector.foldl (fn (c,b) => b andalso mem c x) true y andalso
     CharVector.foldl (fn (c,b) => b andalso mem c y) true x)

fun processSentence s =
    let val ws = String.tokens (fn c => Char.isSpace c orelse Char.isPunct c) s
        fun dups nil acc = acc
          | dups (x::xs) acc = dups xs (acc orelse List.exists (relate x) xs)
    in dups ws false
    end

fun readSentence is : string option =
    let fun loop a =
            case a of
                (acc,true,_) => a
              | (acc,false,_) =>
                loop (case TextIO.input1 is of
                          NONE => (acc,true,true)
                        | SOME #"." => (acc,true,false)
                        | SOME c => (c::acc,false,false))
        val (a,_,e) = loop (nil,false,false)
    in if e then NONE
       else SOME (implode (rev a))
    end

fun loopit (is,c,n) =
    let fun loo a =
            case a of (p,true) => a
                    | (p,false) =>
                      loo let val v = case readSentence is of
                                          SOME s => SOME (processSentence s)
                                        | NONE => NONE
                          in case v of
                                 NONE => (p,true)
                               | SOME b =>
                                 let val (c,n) = p
                                 in ((if b then c+1 else c,
                                      n+1), false)
                                 end
                          end
    in #1(loo ((c,n),false))
    end

fun run (n,f) =
    let fun loop n =
            if n <= 0 then ()
            else (let val is = TextIO.openIn f
                      val (c,n) = loopit (is,0,0)
                  in print ("Sentences containing permuted words: " ^
                            Int.toString c ^ "/" ^ Int.toString n ^ "\n")
                   ; TextIO.closeIn is
                  end
                 ;loop (n-1))
    in loop n
    end

val () =
    case CommandLine.arguments() of
        [f] => run (1,f)
      | _ => print ("Usage: " ^ name ^ " file\n")
