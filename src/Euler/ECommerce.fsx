type Image = { Url: string; Title: string }
type Product = { Name: string; Price: decimal; Description: string; Image: Image }
type CartLine = { Product: Product; Quantity: decimal }
type Order = { Lines: CartLine list }

type Cart = { Lines: CartLine list }

let addToCart (cartLine: CartLine, cart: Cart) = 
    cartLine :: cart.Lines

let getTotal (cart:Cart) = 
    List.sumBy (fun x -> x.Product.Price * x.Quantity) cart.Lines

let product = { Name = "Product1"; Price = 10.98m; Description = "desc"; Image = { Url = ""; Title = "" } }
let cartLine = { Product = product; Quantity = 10.0m }
let cart = { Lines = [cartLine] }