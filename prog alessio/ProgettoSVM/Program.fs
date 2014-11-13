open SVMLibrary

let rec compattamento (lista : char list) : string = match lista with//funzione interna a f# ler la conversione in stringa di una lista di caratteri
                                                     | [] -> ""
                                                     | c :: cs -> (string c) + (compattamento cs) 

let explode (stringa : string) : char list = stringa.ToCharArray() |> Array.toList//funzione che permette di convertire una stringa in una lista di caratteri

let rec scorrimento (n:int) (memory:float list) = match memory with
                                                  |x::xs when n<> 0 -> scorrimento (n-1) xs
                                                  |x::xs -> x
                                                  |_-> failwith "out of memory" 

let rec inverti_lista (lista: char list) :char list= match lista with
                                                     |x::xs -> inverti_lista(xs)@[x]
                                                     |_->[]

let rec valore (reg:char list) (esp:float) : float= match reg with
                                                    |x::xs when x<>']' && x<>'[' ->((float(x)-float('0'))*10.0**esp)+ valore xs (esp+1.0)
                                                    |x::xs -> 0.0+valore xs (esp)
                                                    |_-> 0.0

let num (stringa:string) :int = match explode(stringa) with
                                |x::y::xs when x='[' -> if y='-' then int (valore (inverti_lista(xs)) 0.0)*(-1) else int (valore (inverti_lista(y::xs)) 0.0)
                                |x::xs -> if x='-' then int( valore (inverti_lista(xs)) 0.0 )*(-1) else int( valore (inverti_lista(explode(stringa))) 0.0 )
                                |_->failwith "errore"

let rec cut (x : char) (xs : char list) : char list = match xs with
                                                      |y::ys when y<>x -> y::cut x ys
                                                      |_ -> []

let rec coda (x : char) (xs : char list) : char list = match xs with
                                                       |y::ys when y<>x -> coda x ys
                                                       |y::ys -> ys
                                                       |_ -> []

let rec split (x : char) (xs : char list) : string list = match xs with
                                                          |y::ys when y<>x -> compattamento(cut x xs)::(split x (coda x xs))
                                                          |y::ys -> split x ys
                                                          |_ -> []

let dividi (x : char) (stringa:string) :string list= split x (explode(stringa))

let rec lung (list: string list) :int= match list with
                                       |x::xs -> 1+(lung xs)
                                       |_->0

let registro (reg:string) :int= match explode (reg) with
                                 |x::y::xs when x='[' && y='R'-> 3
                                 |x::y::xs when x='[' -> 2
                                 |x::y::xs when x='R' -> 1
                                 |_->0

let rec salva (x:float) (mem:float list) (n:int) :float list= match mem with
                                                              |y::ys when n> 0 -> y::salva x ys (n-1)
                                                              |y::ys when n=0 -> x::salva x ys (n-1) 
                                                              |y::ys -> y::salva x ys (n-1)
                                                              |_->[] 

let logica (x:float) :bool= if x=1.0 || x=0.0 then true else false

let regi (registri:float list) (nome:string) = match registri with
                                               |x::y::z::w::[] when nome="REG1" -> x
                                               |x::y::z::w::[] when nome="REG2" -> y
                                               |x::y::z::w::[] when nome="REG3" -> z
                                               |x::y::z::w::[] when nome="REG4" -> w
                                               |_-> failwith "errore"

let posizione (nome:string) = match nome with
                              |"REG1" -> 0
                              |"REG2" -> 1
                              |"REG3" -> 2
                              |_ -> 3

let mov (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int) = match registri with
                                                                                                             |x::y::[] when registro(x)=2 && registro(y)=0 -> (salva (float(num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=2 && registro(y)=1 -> (salva (regi reg y) mem (num(x)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=2 && registro(y)=2 -> (salva (scorrimento (num(y)) mem) mem (num(x)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=2 && registro(y)=3 -> (salva (scorrimento (posizione(y)) reg) mem (num(x)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=1 && registro(y)=0 -> (mem,0,salva (float(num(y))) reg (posizione(x)),pc+1)
                                                                                                             |x::y::[] when registro(x)=1 && registro(y)=1 -> (mem,0,salva (regi reg y) reg (posizione(x)),pc+1)
                                                                                                             |x::y::[] when registro(x)=1 && registro(y)=2 -> (mem,0,salva (scorrimento (num(y)) mem) reg (posizione(x)),pc+1)
                                                                                                             |x::y::[] when registro(x)=1 && registro(y)=3 -> (mem,0,salva (scorrimento (posizione(y)) reg) reg (posizione(x)),pc+1)
                                                                                                             |x::y::[] when registro(x)=3 && registro(y)=0 -> (salva (float(num(y))) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=3 && registro(y)=1 -> (salva (regi reg y) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=3 && registro(y)=2 -> (salva (scorrimento (num(y)) mem) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                             |x::y::[] when registro(x)=3 && registro(y)=3 -> (salva (scorrimento (posizione(y)) reg) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                             |_-> failwith "errore in mov"

let fun_and (registri:string list) (mem:float list) (reg:float list) (pc:int) : (float list*int*float list*int)= match registri with
                                                                                                                 |x::y::[] when registro(x)=2 && registro(y)=0 -> if (float(num(y))=1.0) && (scorrimento (num(x)) mem) = float(num(y)) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=2 && registro(y)=1 -> if (regi reg y)=1.0 && (scorrimento (num(x)) mem) = (regi reg y) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=2 && registro(y)=2 -> if (scorrimento (num(y)) mem) = 1.0 && (scorrimento (num(x)) mem) = (scorrimento (num(y)) mem) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=2 && registro(y)=3 -> if (scorrimento (posizione(y)) reg)= 1.0 && (scorrimento (num(x)) mem) = (scorrimento (posizione(y)) reg) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=1 && registro(y)=0 -> if (float(num(y))=1.0) && (regi reg x) = float(num(y)) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                                 |x::y::[] when registro(x)=1 && registro(y)=1 -> if (regi reg y)=1.0 && (regi reg x) = (regi reg y) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                                 |x::y::[] when registro(x)=1 && registro(y)=2 -> if (scorrimento (num(y)) mem) = 1.0 && (regi reg x) = (scorrimento (num(y)) mem) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                                 |x::y::[] when registro(x)=1 && registro(y)=3 -> if (scorrimento (posizione(y)) reg) = 1.0 && (regi reg x) = (scorrimento (posizione(y)) reg) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                                 |x::y::[] when registro(x)=3 && registro(y)=0 -> if (float(num(y))=1.0) && (scorrimento (posizione(x)) reg) = float(num(y)) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=3 && registro(y)=1 -> if (regi reg y)=1.0 && (scorrimento (posizione(x)) reg) = (regi reg y) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=3 && registro(y)=2 -> if (scorrimento (num(y)) mem) = 1.0 && (scorrimento (posizione(x)) reg) = (scorrimento (num(y)) mem) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                                 |x::y::[] when registro(x)=3 && registro(y)=3 -> if (scorrimento (posizione(y)) reg) = 1.0 && (scorrimento (posizione(x)) reg) = (scorrimento (posizione(y)) reg) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                                 |_ -> failwith"errore in and"

let fun_or (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                               |x::y::[] when registro(x)=2 && registro(y)=0 -> if ((scorrimento (num(x)) mem) = 1.0 || float(num(y)) = 1.0) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=2 && registro(y)=1 -> if ((scorrimento (num(x)) mem) = 1.0 || (regi reg y) = 1.0) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=2 && registro(y)=2 -> if ((scorrimento (num(x)) mem) = 1.0 || (scorrimento (num(y)) mem) = 1.0) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=2 && registro(y)=3 -> if ((scorrimento (num(x)) mem) = 1.0 || (scorrimento (posizione(y)) reg) = 1.0) then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=1 && registro(y)=0 -> if ((regi reg x) = 1.0 || float(num(y)) = 1.0) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                               |x::y::[] when registro(x)=1 && registro(y)=1 -> if ((regi reg x) = 1.0 || (regi reg y) = 1.0) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                               |x::y::[] when registro(x)=1 && registro(y)=2 -> if ((regi reg x) = 1.0 || (scorrimento (num(y)) mem) = 1.0) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                               |x::y::[] when registro(x)=1 && registro(y)=3 -> if ((regi reg x) = 1.0 || (scorrimento (posizione(y)) reg) = 1.0) then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                               |x::y::[] when registro(x)=3 && registro(y)=0 -> if ((scorrimento (posizione(y)) reg) = 1.0 || float(num(y)) = 1.0) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=3 && registro(y)=1 -> if ((scorrimento (posizione(y)) reg) = 1.0 || (regi reg y) = 1.0) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=3 && registro(y)=2 -> if ((scorrimento (posizione(y)) reg) = 1.0 || (scorrimento (num(y)) mem) = 1.0) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                               |x::y::[] when registro(x)=3 && registro(y)=3 -> if ((scorrimento (posizione(y)) reg) = 1.0 || (scorrimento (posizione(y)) reg) = 1.0) then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                               |_ -> failwith"errore in or"

let not (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=0 -> if float(num(y)) = 0.0 then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=1 -> if (regi reg y) = 0.0 then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=2 -> if (scorrimento (num(y)) mem) = 0.0 then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=3 -> if (scorrimento (posizione(y)) reg) = 0.0 then (salva (1.0) mem (num(x)),0,reg,pc+1) else (salva (0.0) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=0 -> if float(num(y)) = 0.0 then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=1 -> if (regi reg y) = 0.0 then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=2 -> if (scorrimento (num(y)) mem) = 0.0 then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=3 -> if (scorrimento (posizione(y)) reg) = 0.0 then (mem,0,salva (1.0) reg (posizione(x)),pc+1) else (mem,0,salva (0.0) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=0 -> if float(num(y)) = 0.0 then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=1 -> if (regi reg y) = 0.0 then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=2 -> if (scorrimento (num(y)) mem) = 0.0 then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=3 -> if (scorrimento (posizione(y)) reg) = 0.0 then (salva (1.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1) else (salva (0.0) mem (int(scorrimento (posizione(x)) reg)),0,reg,pc+1)
                                                                                                            |_ -> failwith"errore in not"

let fun_mod (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                                |x::y::[] when registro(x)=2 && registro(y)=0 -> (salva (float(int(scorrimento (num(x)) mem)%num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=2 && registro(y)=1 -> (salva (float(int(scorrimento (num(x)) mem)%int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=2 && registro(y)=2 -> (salva (float(int(scorrimento (num(x)) mem)%int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=2 && registro(y)=3 -> (salva (float(int(scorrimento (num(x)) mem)%int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=1 && registro(y)=0 -> (mem,0,salva (float(int(regi reg x)%num(y))) reg (posizione(x)),pc+1)
                                                                                                                |x::y::[] when registro(x)=1 && registro(y)=1 -> (mem,0,salva (float(int(regi reg x)%int(regi reg y))) reg (posizione(x)),pc+1)
                                                                                                                |x::y::[] when registro(x)=1 && registro(y)=2 -> (mem,0,salva (float(int(regi reg x)%int(scorrimento (num(y)) mem))) reg (posizione(x)),pc+1)
                                                                                                                |x::y::[] when registro(x)=1 && registro(y)=3 -> (mem,0,salva (float(int(regi reg x)%int(scorrimento (posizione(y)) reg))) reg (posizione(x)),pc+1)
                                                                                                                |x::y::[] when registro(x)=3 && registro(y)=0 -> (salva (float(int(scorrimento (posizione(x)) reg)%num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=3 && registro(y)=1 -> (salva (float(int(scorrimento (posizione(x)) reg)%int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=3 && registro(y)=2 -> (salva (float(int(scorrimento (posizione(x)) reg)%int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                                |x::y::[] when registro(x)=3 && registro(y)=3 -> (salva (float(int(scorrimento (posizione(x)) reg)%int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                                |_ -> failwith"errore in mod"

let add (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=0 -> (salva (float(int(scorrimento (num(x)) mem)+num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=1 -> (salva (float(int(scorrimento (num(x)) mem)+int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=2 -> (salva (float(int(scorrimento (num(x)) mem)+int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=3 -> (salva (float(int(scorrimento (num(x)) mem)+int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=0 -> (mem,0,salva (float(int(regi reg x)+num(y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=1 -> (mem,0,salva (float(int(regi reg x)+int(regi reg y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=2 -> (mem,0,salva (float(int(regi reg x)+int(scorrimento (num(y)) mem))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=3 -> (mem,0,salva (float(int(regi reg x)+int(scorrimento (posizione(y)) reg))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=0 -> (salva (float(int(scorrimento (posizione(x)) reg)+num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=1 -> (salva (float(int(scorrimento (posizione(x)) reg)+int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=2 -> (salva (float(int(scorrimento (posizione(x)) reg)+int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=3 -> (salva (float(int(scorrimento (posizione(x)) reg)+int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                            |_-> failwith "errore"

let sub (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=0 -> (salva (float(int(scorrimento (num(x)) mem)-num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=1 -> (salva (float(int(scorrimento (num(x)) mem)-int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=2 -> (salva (float(int(scorrimento (num(x)) mem)-int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=3 -> (salva (float(int(scorrimento (num(x)) mem)-int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=0 -> (mem,0,salva (float(int(regi reg x)-num(y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=1 -> (mem,0,salva (float(int(regi reg x)-int(regi reg y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=2 -> (mem,0,salva (float(int(regi reg x)-int(scorrimento (num(y)) mem))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=3 -> (mem,0,salva (float(int(regi reg x)-int(scorrimento (posizione(y)) reg))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=0 -> (salva (float(int(scorrimento (posizione(x)) reg)-num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=1 -> (salva (float(int(scorrimento (posizione(x)) reg)-int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=2 -> (salva (float(int(scorrimento (posizione(x)) reg)-int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=3 -> (salva (float(int(scorrimento (posizione(x)) reg)-int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                            |_-> failwith "errore"

let mul (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=0 -> (salva (float(int(scorrimento (num(x)) mem)*num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=1 -> (salva (float(int(scorrimento (num(x)) mem)*int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=2 -> (salva (float(int(scorrimento (num(x)) mem)*int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=3 -> (salva (float(int(scorrimento (num(x)) mem)*int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=0 -> (mem,0,salva (float(int(regi reg x)*num(y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=1 -> (mem,0,salva (float(int(regi reg x)*int(regi reg y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=2 -> (mem,0,salva (float(int(regi reg x)*int(scorrimento (num(y)) mem))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=3 -> (mem,0,salva (float(int(regi reg x)*int(scorrimento (posizione(y)) reg))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=0 -> (salva (float(int(scorrimento (posizione(x)) reg)*num(y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=1 -> (salva (float(int(scorrimento (posizione(x)) reg)*int(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=2 -> (salva (float(int(scorrimento (posizione(x)) reg)*int(scorrimento (num(y)) mem))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=3 -> (salva (float(int(scorrimento (posizione(x)) reg)*int(scorrimento (posizione(y)) reg))) mem (num(x)),0,reg,pc+1)
                                                                                                            |_-> failwith "errore"

let div (registri:string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match registri with
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=0 -> (salva ((scorrimento (num(x)) mem)/(float(num(y)))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=1 -> (salva ((scorrimento (num(x)) mem)/(float(regi reg y))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=2 -> (salva ((scorrimento (num(x)) mem)/(scorrimento (num(y)) mem)) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=2 && registro(y)=3 -> (salva ((scorrimento (num(x)) mem)/(scorrimento (posizione(y)) reg)) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=0 -> (mem,0,salva ((regi reg x)/float(num(y))) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=1 -> (mem,0,salva ((regi reg x)/(regi reg y)) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=2 -> (mem,0,salva ((regi reg x)/(scorrimento (num(y)) mem)) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=1 && registro(y)=3 -> (mem,0,salva ((regi reg x)/(scorrimento (posizione(y)) reg)) reg (posizione(x)),pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=0 -> (salva ((scorrimento (posizione(x)) reg)/(float(num(y)))) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=1 -> (salva ((scorrimento (posizione(x)) reg)/(regi reg y)) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=2 -> (salva ((scorrimento (posizione(x)) reg)/(scorrimento (num(y)) mem)) mem (num(x)),0,reg,pc+1)
                                                                                                            |x::y::[] when registro(x)=3 && registro(y)=3 -> (salva ((scorrimento (posizione(x)) reg)/(scorrimento (posizione(y)) reg)) mem (num(x)),0,reg,pc+1)
                                                                                                            |_-> failwith "errore"

let treelem (comando: string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int) = match comando with
                                                                                                                 |x::xs when x="MOV"-> mov xs mem reg pc
                                                                                                                 |x::xs when x="AND"-> fun_and xs mem reg pc
                                                                                                                 |x::xs when x="OR"-> fun_or xs mem reg pc
                                                                                                                 |x::xs when x="NOT"-> not xs mem reg pc
                                                                                                                 |x::xs when x="MOD"-> fun_mod xs mem reg pc
                                                                                                                 |x::xs when x="ADD"-> add xs mem reg pc
                                                                                                                 |x::xs when x="SUB"-> sub xs mem reg pc
                                                                                                                 |x::xs when x="MUL"-> mul xs mem reg pc
                                                                                                                 |x::xs when x="DIV"-> div xs mem reg pc
                                                                                                                 |_->(mem,0,reg,pc)

let dueelem (comando: string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match comando with 
                                                                                                                |x::xs when x="JMP"-> (mem,0,reg,pc+1)
                                                                                                                |_->(mem,0,reg,pc)

let unelem (comando: string list) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match comando with 
                                                                                                               |x::xs when x="NOP" -> (mem,0,reg,pc+1)
                                                                                                               |x::xs when x="END"-> (mem,1,reg,pc+1)
                                                                                                               |_-> (mem,0,reg,pc)

let smistamento (comando:string) (mem:float list) (reg:float list) (pc:int) :(float list*int*float list*int)= match lung(dividi ' ' comando) with
                                                                                                              |3-> treelem (dividi ' ' comando) mem reg pc
                                                                                                              |2-> dueelem (dividi ' ' comando) mem reg pc
                                                                                                              |1-> unelem (dividi ' ' comando) mem reg pc
                                                                                                              |_-> (mem,0,reg,pc)

let rec esecuzione (programma:string list) (memoria:float list*int*float list*int) = match programma,memoria with
                                                                                     |x::xs,(a,0,b,c) -> esecuzione xs (smistamento x a b c)
                                                                                     |_,(a,1,b,c) -> a,b,c
                                                                                     |_-> failwith "no end"

let int_to_char (c : int) = (int c) |> string

let rec conversione (lista:float list) (i:int) :string = match lista with
                                                         |x::[] -> int_to_char(int(x))
                                                         |x::xs when i<>0 -> int_to_char(int(x))+"      "+ conversione xs (i-1)
                                                         |x::xs -> int_to_char(int(x))+"\n\n"+ conversione xs (9)
                                                         |_->""

let conv (lista:float list*float list*int) :string = match lista with
                                                     |(mem,reg,pc) -> conversione mem 9 + "\n\nregistri: " + conversione reg 5 + "\nprogram counter: " + int_to_char(pc)

let test (program : string list) = esecuzione program ([0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0],0,[0.0;0.0;0.0;0.0],0)

//run the program on the SVM -- call your functions in here.
let run_svm (program : string list) :string = conv (test program)

let pause () =  
  match System.Diagnostics.Debugger.IsAttached with  
  | true ->  
      printfn "\nPress any key to continue."  
      System.Console.ReadKey(true) |> ignore  
  | false -> ()

[<EntryPoint>]
let main argv =
  try
    let parsed_program = parse_file "BinaryXor.txt"
    let dump = run_svm parsed_program
    System.Console.WriteLine(dump)
    pause()
    0
  with
    | Failure(msg) ->
        System.Console.WriteLine(msg)
        1
    | :? System.IO.FileNotFoundException as exc ->
        System.Console.WriteLine(exc.Message)
        1
    | :? System.IO.IOException as exc ->
        System.Console.WriteLine(exc.Message)
        1
