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
            
    member this.ModifyParent(callback: string -> string) =
        let rec modifyParentCore current =
            match current with
            | Node(value, children) ->
                match children.Head with  // todo: error handling/with no head?
                | Node(_, children) ->
                    modifyParentCore children.Head
                | Leaf _ -> 
                    Node(callback value, children)
            | Leaf value ->
                Leaf(callback value)
        
        modifyParentCore this
             
    override this.ToString() =
        // credit: 😅
        // "Microsoft Copilot, response to user query, accessed April 19, 2025."
        let rec toStringCore (resizeArr: ResizeArray<string>) tree isRoot prefix isLast =
            match tree with
            | Node (value, children) ->
                let totalChildren = List.length children
                if isRoot then
                    resizeArr.Add(value)
                    for i in 0 .. totalChildren - 1 do
                        toStringCore resizeArr children[i] false "" (i = totalChildren - 1)
                else
                    let line = sprintf "%s%s%s" prefix (if isLast then "└── " else "├── ") value
                    resizeArr.Add(line)
                    let childPrefix = prefix + (if isLast then "    " else "│   ")
                    for i in 0 .. totalChildren - 1 do
                        toStringCore resizeArr children[i] false childPrefix (i = totalChildren - 1)
            | Leaf value ->
                let line = sprintf "%s%s%s" prefix (if isLast then "└── " else "├── ") value
                resizeArr.Add(line)

        let resizeArr = ResizeArray<string>()
        toStringCore resizeArr this true "" true
        System.String.Join("\n", resizeArr.ToArray())
