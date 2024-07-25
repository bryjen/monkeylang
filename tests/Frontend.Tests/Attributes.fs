module FrontendTests.Attributes

open System

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
type TestMessageAttribute(message: string) =
    inherit Attribute()
    member this.Message = message