open System.IO
open System.Text
open System

/// Extension functions on the option type 
module Option = 

     /// Given a default value and an option, returns the option value if there else the default value.
    let inline isNull defaultValue = function Some v -> v | None -> defaultValue

     /// Given a default value and an option, returns the option value if there else the default value.
    let inline getValueOr defaultValue = function Some v -> v | None -> defaultValue

     /// Gets the Some value or Unchecked.defaultof<'a>.
    let inline getValueOrDefault (o:'a option) = isNull (Unchecked.defaultof<'a>) o
