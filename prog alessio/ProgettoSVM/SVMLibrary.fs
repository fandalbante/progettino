module SVMLibrary

open System.IO

let string_to_list (s : string) = s.ToCharArray() |> Array.toList
let rec list_to_string (l : char list) =
  match l with
  | [] -> ""
  | c :: cs -> (string c) + (list_to_string cs)

let char_to_int (c : char) = (string c) |> int


//parse the input file returning a list of string for each line, converted to upper cases.
let parse_file (input : string) =
  let parsed_program = File.ReadAllLines(input) |> Array.toList
  [for line in parsed_program do
      yield line.ToUpper()] |> List.filter(fun x -> x <> "" && x.StartsWith("#") |> not)