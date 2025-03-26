module Monkey.Frontend.CLR.Converter.ConverterErrors.ConverterError

open System

type ConverterError(message: string) =
    inherit Exception(message)

