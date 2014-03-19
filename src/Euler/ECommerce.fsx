#r "../packages/FsUnit.xUnit.1.2.2.1/Lib/net40/FsUnit.Xunit.dll"
#r "../packages/FsUnit.xUnit.1.2.2.1/Lib/net40/NHamcrest.dll"
#r "../packages/xunit.1.9.2/lib/net20/xunit.dll"

open System
open Xunit
open FsUnit.Xunit

type Image = { Url: string; Title: string }
type Product = { Name: string; Price: decimal; Description: string; Image: Image; Stock: int }

type Address = { AddressLine: string; Zip: string; City: string }
type User = { Name: string; Email: string; Address: Address; DeliveryAddress: Address }

type CartLine = { Product: Product; Quantity: decimal }
type Cart = { Lines: CartLine list; user: User}
type Card = { CardNumber: string; ExpiryDate: DateTime; Name: string }
type VisaCard = Card
type MasterCard = Card
type Invoice = { PersonalNumber: string }

type PaymentMethod = 
    | Visa of VisaCard
    | MasterCard of MasterCard
    | PaymentAtDelivery
    | PaymentByInvoice of Invoice
    
type Order = { OrderDetails: CartLine list; User: User; DeliveryAddress: Address; PaymentMethod: PaymentMethod }

let addToCart (cartLine: CartLine, cart: Cart) = 
    cartLine :: cart.Lines

let getTotal (cart:Cart) = 
    List.sumBy (fun x -> x.Product.Price * x.Quantity) cart.Lines

let product = { Name = "Product1"; Price = 10.98m; Description = "desc"; Image = { Url = ""; Title = "" }; Stock = 10 }
let cartLine = { Product = product; Quantity = 10.0m }
let address = { AddressLine = "Toftesgate567"; Zip = "0556"; City = "Oslo" }
let user = { Name = "Terje Tyldum"; Email = "terjetyl@gmail.com"; Address = address; DeliveryAddress = address }
let cart = { Lines = [cartLine]; user = user }

let placeOrder(cart:Cart, user: User, address: Address, paymentMethod: PaymentMethod) = 
    let order = { OrderDetails = cart.Lines; User = user; DeliveryAddress = address; PaymentMethod = paymentMethod }
    order

type Products() = 
    member this.list (products: Product list) = 
        products
        |> Seq.iter (fun p -> printfn "%s %s, total %i in stock" p.Name (p.Price.ToString()) p.Stock)
    member this.add (product:Product) =
        printfn "product %s added" product.Name

[<Fact>] 
let ``with simple scores should get the expected score.`` () =
    [1;2;3] |> should equal 6