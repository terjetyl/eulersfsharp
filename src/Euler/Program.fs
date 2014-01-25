// Learn more about F# at http://fsharp.net
module Program

open Xunit
open FsUnit.Xunit
open System
open System.Globalization
open System.Numerics

module public Primes = 
    let sieve_primes top_number = 
        let numbers = [ yield 2L
                        for i in 3L..2L..top_number -> i ]
        let rec sieve ns = 
            match ns with
            | [] -> []
            | x::xs when x*x > top_number -> ns
            | x::xs -> x::sieve (List.filter(fun y -> y%x <> 0L) xs)
        sieve numbers 

    let isPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
             
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    [<Fact>]
    let ``sum of 5 first numbers in sieve should be 28`` () =
        List.sum (sieve_primes 12L) |> should equal 28L

module Eulers1 = 

    let dividableBy3Or5 (x) = (x % 3 = 0) || (x % 5 = 0)

    let answer = [1..999] |> Seq.filter dividableBy3Or5 |> Seq.sum

    // print answer
    printfn "Eulers 1 : %A" answer


module Eulers2 = 

    // calculates the value of the item in the Fibonacci seq at the given position
    let rec Fib(n) = 
        if (n < 2) then
            1
        else
            Fib(n-2) + Fib(n-1)

    let evenNumbers (x) = x % 2 = 0

    let FibSeq = Seq.unfold (fun (a, b) -> Some( a, (b, a + b) ) ) (1, 2)

    let answer = 
        FibSeq
            |> Seq.takeWhile (fun x -> x <= 4000000)
            |> Seq.filter evenNumbers
            |> Seq.sum

    printfn "Eulers 2 : %A" answer

module Eulers3 = 

    let isPrime (possiblePrime:int64) =
        let sqrRootOfPrime = int64(System.Math.Sqrt(float(possiblePrime)))
             
        {1L .. sqrRootOfPrime}
        |> Seq.forall(fun divisor -> 
            match divisor with
            | 1L -> true
            | x when divisor = possiblePrime -> true
            | _ -> possiblePrime % divisor > 0L)

    let findFactors ofNumber =
        {2L .. (ofNumber/2L)}
        |> Seq.filter(fun testNumber -> ofNumber % testNumber = 0L)
        |> Seq.map(fun factor1 -> (factor1, (ofNumber / factor1)))
        |> Seq.takeWhile(fun (factor1, factor2) -> factor1 <= factor2)
        |> Seq.fold (fun acc (factor1, factor2) -> factor1 :: factor2 :: acc) []
        |> Seq.sort
        |> Seq.toList
        |> List.rev

    let sieve_primes top_number = 
        let numbers = [ yield 2L
                        for i in 3L..2L..top_number -> i ]
        let rec sieve ns = 
            match ns with
            | [] -> []
            | x::xs when x*x > top_number -> ns
            | x::xs -> x::sieve (List.filter(fun y -> y%x <> 0L) xs)
        sieve numbers 

    let answer = findFactors 600851475143L |> Seq.filter isPrime |> Seq.toList |> List.rev |> Seq.last
    printfn "Eulers 3 : %A" answer

module Eulers4 = 

    let rev str =
        let si = StringInfo(str)
        let teArr = Array.init si.LengthInTextElements (fun i -> si.SubstringByTextElements(i,1))
        Array.Reverse(teArr) //in-place reversal better performance than Array.rev
        String.Join("", teArr)

    let isPalindrome (x) = 
        x.ToString() = rev(x)

    let palindrome_numbers = seq {
        for i in 100 .. 999 do
            for j in 100 .. 999 do
                if isPalindrome(Convert.ToString(i*j)) then yield (i*j) }

module Eulers5 = 
    let isDivisableByNumbers arr x = 
        arr 
        |> List.exists (fun d -> x % d <> 0)
        |> not

    let smallestNumberDivisibleByAll dividers =
        let max = dividers |> List.max
        Seq.initInfinite ((+) 1 >> (*) max)
        |> Seq.find (isDivisableByNumbers dividers)
            
    let answer = smallestNumberDivisibleByAll [1..20]
          
module Eulers6 = 
    let square x = x * x
    let answer = ([1..100] |> Seq.sum |> square) - ([1..100] |> List.map square |> Seq.sum)

    printfn "Eulers 6 : %A" answer

module Eulers7 =
    let answer = 
        Seq.initInfinite (fun n -> int64 n + 1L) 
        |> Seq.filter Primes.isPrime 
        |> Seq.take 10002 
        |> Seq.max

module Eulers8 = 
    let numbers = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

    let parseInt x =  int (Char.GetNumericValue(x))

    let ints = Seq.map parseInt numbers

    // put in groups of 5 and 5
    let factors = Seq.windowed 5 ints

    let sums x = Seq.reduce (*) x

    let answer = Seq.max (Seq.map sums factors)

module Eulers9 =
    let tripletsSum1000 =
        seq {
            for a in 1 .. 333 do
                for b in a + 1 .. 499 do
                    let c = 1000 - b - a
                    if a < b && b < c then
                         yield (a,b,c)
         }

    let answer =
         tripletsSum1000
         |> Seq.find (fun (a,b,c) -> a * a + b * b = c * c)
         |> fun (a,b,c) -> a*b*c

module Eulers10 = 
    let numbers = Primes.sieve_primes 1999999L
    let answer = List.sum numbers
    printfn "Eulers 10 : %A" answer

module Eulers12 = 
    let factors number = seq {
        for divisor in 1 .. (float >> sqrt >> int) number do
        if number % divisor = 0 then
            yield divisor
            yield number / divisor
    }

    let triangles = 
        Seq.unfold (fun x -> Some(snd x + fst x, (snd x + fst x, snd x + 1))) (0, 1)

    let hasMoreThan500Divisors x = 
        Seq.length (factors x) > 500

    let answer = triangles |> Seq.find hasMoreThan500Divisors
         
module Eulers13 = 

    let allNumbers = 
        "37107287533902102798797998220837590246510135740250\n\
        46376937677490009712648124896970078050417018260538\n\
        74324986199524741059474233309513058123726617309629\n\
        91942213363574161572522430563301811072406154908250\n\
        23067588207539346171171980310421047513778063246676\n\
        89261670696623633820136378418383684178734361726757\n\
        28112879812849979408065481931592621691275889832738\n\
        44274228917432520321923589422876796487670272189318\n\
        47451445736001306439091167216856844588711603153276\n\
        70386486105843025439939619828917593665686757934951\n\
        62176457141856560629502157223196586755079324193331\n\
        64906352462741904929101432445813822663347944758178\n\
        92575867718337217661963751590579239728245598838407\n\
        58203565325359399008402633568948830189458628227828\n\
        80181199384826282014278194139940567587151170094390\n\
        35398664372827112653829987240784473053190104293586\n\
        86515506006295864861532075273371959191420517255829\n\
        71693888707715466499115593487603532921714970056938\n\
        54370070576826684624621495650076471787294438377604\n\
        53282654108756828443191190634694037855217779295145\n\
        36123272525000296071075082563815656710885258350721\n\
        45876576172410976447339110607218265236877223636045\n\
        17423706905851860660448207621209813287860733969412\n\
        81142660418086830619328460811191061556940512689692\n\
        51934325451728388641918047049293215058642563049483\n\
        62467221648435076201727918039944693004732956340691\n\
        15732444386908125794514089057706229429197107928209\n\
        55037687525678773091862540744969844508330393682126\n\
        18336384825330154686196124348767681297534375946515\n\
        80386287592878490201521685554828717201219257766954\n\
        78182833757993103614740356856449095527097864797581\n\
        16726320100436897842553539920931837441497806860984\n\
        48403098129077791799088218795327364475675590848030\n\
        87086987551392711854517078544161852424320693150332\n\
        59959406895756536782107074926966537676326235447210\n\
        69793950679652694742597709739166693763042633987085\n\
        41052684708299085211399427365734116182760315001271\n\
        65378607361501080857009149939512557028198746004375\n\
        35829035317434717326932123578154982629742552737307\n\
        94953759765105305946966067683156574377167401875275\n\
        88902802571733229619176668713819931811048770190271\n\
        25267680276078003013678680992525463401061632866526\n\
        36270218540497705585629946580636237993140746255962\n\
        24074486908231174977792365466257246923322810917141\n\
        91430288197103288597806669760892938638285025333403\n\
        34413065578016127815921815005561868836468420090470\n\
        23053081172816430487623791969842487255036638784583\n\
        11487696932154902810424020138335124462181441773470\n\
        63783299490636259666498587618221225225512486764533\n\
        67720186971698544312419572409913959008952310058822\n\
        95548255300263520781532296796249481641953868218774\n\
        76085327132285723110424803456124867697064507995236\n\
        37774242535411291684276865538926205024910326572967\n\
        23701913275725675285653248258265463092207058596522\n\
        29798860272258331913126375147341994889534765745501\n\
        18495701454879288984856827726077713721403798879715\n\
        38298203783031473527721580348144513491373226651381\n\
        34829543829199918180278916522431027392251122869539\n\
        40957953066405232632538044100059654939159879593635\n\
        29746152185502371307642255121183693803580388584903\n\
        41698116222072977186158236678424689157993532961922\n\
        62467957194401269043877107275048102390895523597457\n\
        23189706772547915061505504953922979530901129967519\n\
        86188088225875314529584099251203829009407770775672\n\
        11306739708304724483816533873502340845647058077308\n\
        82959174767140363198008187129011875491310547126581\n\
        97623331044818386269515456334926366572897563400500\n\
        42846280183517070527831839425882145521227251250327\n\
        55121603546981200581762165212827652751691296897789\n\
        32238195734329339946437501907836945765883352399886\n\
        75506164965184775180738168837861091527357929701337\n\
        62177842752192623401942399639168044983993173312731\n\
        32924185707147349566916674687634660915035914677504\n\
        99518671430235219628894890102423325116913619626622\n\
        73267460800591547471830798392868535206946944540724\n\
        76841822524674417161514036427982273348055556214818\n\
        97142617910342598647204516893989422179826088076852\n\
        87783646182799346313767754307809363333018982642090\n\
        10848802521674670883215120185883543223812876952786\n\
        71329612474782464538636993009049310363619763878039\n\
        62184073572399794223406235393808339651327408011116\n\
        66627891981488087797941876876144230030984490851411\n\
        60661826293682836764744779239180335110989069790714\n\
        85786944089552990653640447425576083659976645795096\n\
        66024396409905389607120198219976047599490197230297\n\
        64913982680032973156037120041377903785566085089252\n\
        16730939319872750275468906903707539413042652315011\n\
        94809377245048795150954100921645863754710598436791\n\
        78639167021187492431995700641917969777599028300699\n\
        15368713711936614952811305876380278410754449733078\n\
        40789923115535562561142322423255033685442488917353\n\
        44889911501440648020369068063960672322193204149535\n\
        41503128880339536053299340368006977710650566631954\n\
        81234880673210146739058568557934581403627822703280\n\
        82616570773948327592232845941706525094512325230608\n\
        22918802058777319719839450180888072429661980811197\n\
        77158542502016545090413245809786882778948721859617\n\
        72107838435069186155435662884062257473692284509516\n\
        20849603980134001723930671666823555245252804609722\n\
        53503534226472524250874054075591789781264330331690"

    let sum =
        allNumbers.Split [|'\n'|]
        |> Array.map (fun s -> BigInteger.Parse(s))
        |> Array.sum

    let answer = sum.ToString().Substring(0, 10)


module Eulers14 = 
    let oddfn = fun x -> x * 3 + 1;
    let evenfn = fun x -> x / 2;

    let getNextCollatz x = 
        if x % 2 = 0 then
            evenfn(x)
        else
            oddfn(x)

    let collatz startnum =
        startnum
        |> Seq.unfold(fun x -> if (x<>1) then Some(x,getNextCollatz(x)) else None)

    let getLength arr = Seq.length arr

    let answer = {1..999999} |> Seq.map collatz |> Seq.map getLength |> Seq.max

module Eulers15 = 
    let fact n = {1I..n} |> Seq.reduce (*)
 
    let answer =
        fact 40I / ((fact 20I) * (fact 20I))
        
module Eulers16 = 
    let answer = (2I**1000).ToString().ToCharArray() |> Seq.map (fun x -> int (x.ToString())) |> Seq.sum;;
    
module Eulers19 = 
    let dates = DateTime(1901,1,1) |> Seq.unfold (fun d -> Some(d, d.AddDays 1.0)) |> Seq.takeWhile (fun d -> d <= DateTime(2000, 12, 31))
    let answer = dates |> Seq.filter (fun x -> x.Day = 1 && x.DayOfWeek = DayOfWeek.Sunday) |> Seq.length

module Eulers20 = 
    let num = [1I..100I] |> Seq.reduce (*)
    let answer = num.ToString().ToCharArray() |> Seq.map (fun x -> int (x.ToString())) |> Seq.sum

module Eulers21 = 
    let sumOfDivisors num = [1..num/2] |> List.filter (fun y -> num%y = 0) |> List.sum
    let isAmicable num = 
        let sum = sumOfDivisors num
        num = sumOfDivisors sum && sum <> num
    let answer = [1..9999] |> List.filter isAmicable |> List.sum

module Eulers22 = 
    let line = readLines @"c:\tmp\names.txt" |> Seq.head
    let names = line.Split(',') |> Seq.map (fun x -> x.Trim('"'))
    let findAlphabeticValue (name:string) = 
        name.ToCharArray() 
        |> Seq.map (fun y -> (Seq.findIndex (fun x -> x = y) ['A'..'Z']) + 1)
        |> Seq.sum
    let answer = 
        names 
        |> Seq.mapi (fun i x -> (i+1)*findAlphabeticValue(x))
        |> Seq.sum