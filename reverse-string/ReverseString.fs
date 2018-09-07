module ReverseString

open System

let rec reverse (a:string): string = if a.Length > 0 then reverse a.[1..]+a.[..0] else ""

