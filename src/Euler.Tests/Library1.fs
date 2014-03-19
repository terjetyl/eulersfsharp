namespace Euler.Tests

module test =
    open Xunit
    open FsUnit.Xunit

    [<Fact>] 
    let ``with simple scores should get the expected score.`` () =
        [1;2;3] |> should equal 6
