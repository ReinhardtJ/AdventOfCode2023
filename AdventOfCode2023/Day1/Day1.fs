module AdventOfCode2023.Day1

open System.IO
open Xunit
open Xunit.Abstractions


let digits_1 = Map[
    ("1", 1); 
    ("2", 2); 
    ("3", 3); 
    ("4", 4); 
    ("5", 5); 
    ("6", 6); 
    ("7", 7); 
    ("8", 8); 
    ("9", 9);
]

let digits_2 = Map[
    ("1", 1); ("one", 1); 
    ("2", 2); ("two", 2); 
    ("3", 3); ("three", 3); 
    ("4", 4); ("four", 4); 
    ("5", 5); ("five", 5); 
    ("6", 6); ("six", 6); 
    ("7", 7); ("seven", 7); 
    ("8", 8); ("eight", 8); 
    ("9", 9); ("nine", 9)
]

let get_first_digit_from_sorted_keys (keys: string list) (digits: Map<string,int>): int =
        keys
        |> List.tryHead
        |> Option.bind(fun key -> Map.tryFind key digits)
        |> function
            | Some matchedDigit -> matchedDigit
            | None -> 0
                                               
let get_digits_from (line: string) (digits: Map<string,int>): (int * int)  =
    let keys = Map.toList digits |> List.map fst
    let keysInLine = keys |> List.filter line.Contains
    let keysLeftToRight = keysInLine |> List.sortBy(line.IndexOf)
    let firstDigit = get_first_digit_from_sorted_keys keysLeftToRight digits

    let keysRightToLeft = keysInLine |> List.sortByDescending(line.LastIndexOf)
    let lastDigit = get_first_digit_from_sorted_keys keysRightToLeft digits
    firstDigit, lastDigit
        
let value_of_line (line: string) (digits: Map<string,int>): int =
    let firstDigit, lastDigit = get_digits_from line digits
    firstDigit * 10 + lastDigit

let get_calibration_value_1 (input: string): int =
    let lines = input.Split("\n")
    lines |> Seq.map (fun line -> value_of_line line digits_1) |> Seq.sum

let get_calibration_value_2 (input: string): int =
    let lines = input.Split("\n")
    lines |> Seq.map (fun line -> value_of_line line digits_2) |> Seq.sum
    
type MyTests(output: ITestOutputHelper) =
    [<Fact>]
    let test_get_calibration_value_1 () =
        let input = """1abc2
                       pqr3stu8vwx
                       a1b2c3d4e5f
                       treb7uchet"""
                       
        let result = get_calibration_value_1 input
        Assert.Equal(142, result)

    [<Fact>]
    let produce_calibration_value_1 () =
        let input = File.ReadAllText("Day1/input.txt")
        output.WriteLine($"{get_calibration_value_1 input}")

    [<Fact>]
    let test_get_calibration_value_2 () =
        let input = """two1nine
                       eightwothree
                       abcone2threexyz
                       xtwone3four
                       4nineeightseven2
                       zoneight234
                       7pqrstsixteen"""
        
        let result = get_calibration_value_2 input
        Assert.Equal(281, result)
        
        
    [<Fact>]
    let test_get_value_of_line_2 () =
        let value_of_line_2 = fun line -> value_of_line line digits_2
        Assert.Equal(29, value_of_line_2("two1nine"))
        Assert.Equal(83, value_of_line_2("eightwothree"))
        Assert.Equal(13, value_of_line_2("abcone2threexyz"))
        Assert.Equal(24, value_of_line_2("xtwone3four"))
        Assert.Equal(42, value_of_line_2("4nineeightseven2"))
        Assert.Equal(14, value_of_line_2("zoneight234"))
        Assert.Equal(76, value_of_line_2("7pqrstsixteen"))
        Assert.Equal(0, value_of_line_2(""))
        Assert.Equal(0, value_of_line_2("asdfgvhtsderdfv"))
        Assert.Equal(82, value_of_line_2("eightwo"))
        Assert.Equal(21, value_of_line_2("twone"))
        Assert.Equal(83, value_of_line_2("eighthree"))
        Assert.Equal(58, value_of_line_2("fiveight"))
        Assert.Equal(83, value_of_line_2("eightwothree"))
        Assert.Equal(11, value_of_line_2("oneone"))
        Assert.Equal(11, value_of_line_2("onetwo1"))
        Assert.Equal(11, value_of_line_2("one2one")) // -> 12
        Assert.Equal(11, value_of_line_2("1twoone"))
        Assert.Equal(11, value_of_line_2("one21"))
        Assert.Equal(11, value_of_line_2("1two1"))
        Assert.Equal(11, value_of_line_2("121"))
        
        
    [<Fact>]
    let produce_calibration_value_2 () =
        let input = File.ReadAllText("Day1/input.txt")
        output.WriteLine($"{get_calibration_value_2 input}")