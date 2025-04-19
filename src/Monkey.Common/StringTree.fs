module Monkey.Common.StringTree

open System


let internal identStr indentation = 
    let indentationStr = "    "
    String.replicate indentation indentationStr
    
    
type StringTree =
    | Node of value:string * children:(StringTree list)
    | Leaf of value:string
with
    member this.AddNode(str: string) =
        match this with
        | Node(value, children) ->
            match children with
            | [] ->
                let newLeaf = Leaf str
                Node(value, [ newLeaf ])
            | head :: tail ->
                let newChildren = (head.AddNode(str)) :: tail
                Node(value, newChildren)
        | Leaf value ->
            let newLeaf = Leaf str
            Node(value, [ newLeaf ])
            
    member this.AddNode([<ParamArray>] strs: string array) =
        match this with
        | Node(value, children) ->
            match children with
            | [] ->
                let newLeaves = strs |> Array.map Leaf |> Array.toList
                Node(value, newLeaves @ children)
            | head :: rest ->
                Node(value, head.AddNode(strs) :: rest)
        | Leaf value ->
            let leaves = strs |> Array.map Leaf |> Array.toList
            Node(value, leaves)
            
    member this.PopNode() =
        match this with
        | Node(value, children) ->
            match children with
            | [] ->
                Leaf value
                
            | [ head ] ->
                match head with
                | Leaf _ ->
                    Leaf(value)
                | _ ->
                    Node(value, [ head.PopNode() ])
                
            | head :: tail ->
                match head with
                | Leaf _ ->
                    Node(value, tail)
                | _ -> 
                    let newHead = head.PopNode()
                    Node(value, newHead :: tail)
        | Leaf value ->
            Leaf value
            
    member this.ModifyCurrent(callback: string -> string) =
        match this with
        | Node(value, children) ->
            match children with
            | [] ->
                Leaf(callback value)
                
            | head :: tail ->
                let newHead = head.ModifyCurrent(callback)
                Node(value, newHead :: tail)
                    
        | Leaf value ->
            Leaf(callback value)
            
    override this.ToString() =
        let lines: ResizeArray<string> = ResizeArray<string>()
        let rec toStringHelper (indentation: int) (tree: StringTree) =
            match tree with
            | Node(value, children) ->
                lines.Add((identStr indentation) + value)
                List.iter (toStringHelper (indentation + 1)) children
            | Leaf value ->
                lines.Add((identStr indentation) + value)
            
        toStringHelper 0 this
        System.String.Join("\n", lines.ToArray())
