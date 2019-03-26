
/* \ -> lambda ; @ -> All ; * -> Type */
/*
    ocamlrun f ListInteger.f

    NOTE:
        end by ";"
        "Bool"/"Nat" are keywords
        term variable should begin with lower letter.
*/

Will = lambda R. Unit -> R;
Maybe = lambda X. All R. (X -> R) -> R -> R;
nothing = lambda X. (lambda R. lambda f:X->R. lambda r:R. r) as Maybe X;
just = lambda X. lambda x:X. (lambda R. lambda f:X->R. lambda r:R. f x) as Maybe X;
maybe = lambda X. lambda R. lambda r:R. lambda f:X->R. lambda m:Maybe X.
    m [R] f r;
fmapMaybe = lambda X. lambda Y. lambda f:X->Y. lambda m:Maybe X.
    m [Maybe Y] (lambda x:X. just [Y] (f x)) (nothing [Y]);

Pair = lambda X. lambda Y. All R. (X->Y->R) -> R;
pair = lambda X. lambda x:X. lambda Y. lambda y:Y.
    (lambda R. lambda f:X->Y->R. f x y) as Pair X Y;
fst = lambda X. lambda Y. lambda p:Pair X Y. p [X] (lambda x:X. lambda y:Y. x);
snd = lambda X. lambda Y. lambda p:Pair X Y. p [Y] (lambda x:X. lambda y:Y. y);

Either = lambda X. lambda Y. All R. (X->R) -> (Y->R) -> R;
left = lambda X. lambda x:X. lambda Y.
    (lambda R. lambda f:X->R. lambda g:Y->R. f x) as Either X Y;
right = lambda Y. lambda y:Y. lambda X.
    (lambda R. lambda f:X->R. lambda g:Y->R. g y) as Either X Y;
either = lambda R. lambda X. lambda f:X->R. lambda Y. lambda g:Y->R.
    lambda e:Either X Y. e [R] f g;




CList = lambda X. All R. (X -> Will R -> R) -> R -> R;
nilCList = lambda X.
    (lambda R. lambda f:X -> Will R -> R. lambda r:R. r)
    as CList X;
consCList = lambda X. lambda x:X. lambda ts:CList X.
    (lambda R. lambda f:X -> Will R -> R. lambda r:R.
    f x (lambda u:Unit. ts [R] f r)
    ) as CList X;
singletonCList = lambda X. lambda x:X.
    consCList [X] x (nilCList [X]);
foldCList = lambda X. lambda R. lambda f: X -> R -> R. lambda r:R.
    lambda ls:CList X.
    let will_f = lambda x:X. lambda w:Will R. f x (w unit)
    in  ls [R] will_f r;

headCList = lambda X. lambda ls:CList X.
    let f = lambda x:X. lambda r:Will (Maybe X). just [X] x in
    ls [Maybe X] f (nothing [X]);
/* O(n) */
splitCList = lambda X. lambda ls:CList X.
    let f = lambda h:X. lambda r:Will (Maybe (Pair X (CList X))).
        let g = lambda p:Pair X (CList X). p [Pair X (CList X)]
                (lambda x:X. lambda ts:CList X.
                pair [X] h [CList X] (consCList [X] x ts)
                )
        in  just [Pair X (CList X)]
            (r unit [Pair X (CList X)] g (pair [X] h [CList X] (nilCList [X])))
    in  ls [Maybe (Pair X (CList X))] f (nothing [Pair X (CList X)]);
tailCList = lambda X. lambda ls:CList X.
    fmapMaybe [Pair X (CList X)] [CList X]
        (snd [X] [CList X]) (splitCList [X] ls);

/* O(n^2)??
COrd = lambda R. R -> R -> R -> R; /* LT EQ GT */
compareCList = lambda X. lambda R.
    lambda cmp:X->X->R. lambda while:R->CBool.
    lambda lt:R. lambda eq:R. lambda gt.R.
    ...;
*/

ID = All X. X;
id = lambda X. lambda x:X. x;
CBool = All R. R -> R -> R;
trueCBool = (lambda R. lambda x:R. lambda y:R. x) as CBool;
falseCBool = (lambda R. lambda x:R. lambda y:R. y) as CBool;
notCBool = lambda b:CBool. b [CBool] falseCBool trueCBool;

/*
notCBool = lambda b:CBool. (
    lambda R. lambda x:R. lambda y:R.
    b [R] y x
    ) as CBool;
*/

CBit = CBool;
zeroCBit = falseCBool as CBit;
oneCBit = trueCBool as CBit;

CNat = All R. (Will R -> R) -> R -> R;
CNatWithoutWill = All R. (R -> R) -> R -> R;
cnatWithoutWill = lambda n:CNat.
    (lambda R. lambda f:R->R. lambda r:R.
     let will_f = lambda will_r:Will R. f (will_r unit) in
     n [R] will_f r
    )
    as CNatWithoutWill;
zeroCNat = (lambda R. lambda f:Will R -> R. lambda r:R. r) as CNat;
succCNat = lambda n:CNat.
    (lambda R. lambda f:Will R -> R. lambda r:R. f (lambda u:Unit. n [R] f r))
    as CNat;
predCNat = lambda n:CNat.
    let f = lambda m:Maybe CNat. just [CNat] (m [CNat] succCNat zeroCNat) in
    cnatWithoutWill n [Maybe CNat] f (nothing [CNat]);
oneCNat = succCNat zeroCNat;
addCNat = lambda n:CNat. lambda m:CNat.
    cnatWithoutWill n [CNat] succCNat m;
doubleCNat = lambda n:CNat. addCNat n n;

/*
*/

/* binary Pos number
    little-endian
    "" == 1
    "0" == 2
    "1" == 3
    "00" == 4
    "10" == 5
    "01" == 6
    "11" == 7
    "000" == 8
    "100" == 9
    "010" == 10
    "..." == bin2num (reverse ("..."++"1"))
*/


BPos = CList CBit;
/*
oneBPos = (lambda R. lambda f:CBit->Will R->R. lambda r:R. r) as BPos;
twoBPos = (lambda R. lambda f:CBit->Will R->R. lambda r:R.
    f zeroCBit (lambda u:Unit. r)) as BPos;
*/
oneBPos = (nilCList [CBit]) as BPos;
twoBPos = (consCList [CBit] zeroCBit oneBPos) as BPos;
threeBPos = (consCList [CBit] oneCBit oneBPos) as BPos;



/**/
succBPos = lambda pos:BPos. (
    /*[Pair BPos BPos] (prefix, prefix+1)*/
    /*
    let X = Will BPos in
    let Y = X in
    let R = Pair X Y in
    */
    /*
    (lambda X <: Will BPos.
    (lambda Y <: X.
    (lambda R <: Pair X Y.
    */
    let f = lambda b:CBit. lambda w:Will (Pair (Will BPos) (Will BPos)).
            b [(Pair (Will BPos) (Will BPos))]
                /* b == 1 */
                (w unit [(Pair (Will BPos) (Will BPos))] (lambda prefix:(Will BPos). lambda prefix_1:(Will BPos).
                 pair
                    [(Will BPos)] (lambda u:Unit. consCList [CBit] b (prefix unit))
                    [(Will BPos)] (lambda u:Unit. consCList [CBit] zeroCBit (prefix_1 unit))
                ))
                /* b == 0 */
                (w unit [(Pair (Will BPos) (Will BPos))] (lambda prefix:(Will BPos). lambda prefix_1:(Will BPos).
                 let pre = prefix unit in
                 pair
                    [(Will BPos)] (lambda u:Unit. consCList [CBit] b pre)
                    [(Will BPos)] (lambda u:Unit. consCList [CBit] oneCBit pre)
                ))
    in
    let p_p1 = pos [(Pair (Will BPos) (Will BPos))] f (pair [(Will BPos)] (lambda u:Unit. oneBPos) [(Will BPos)] (lambda u:Unit. twoBPos))
    in  snd [(Will BPos)] [(Will BPos)] p_p1 unit
    /*
    ) [Pair X Y]
    ) [X]
    ) [Will BPos]
    */
    ) as BPos;

bpos2cnat =
    let f = lambda b:CBit.lambda n:Will CNat.
            b [CNat->CNat] succCNat (id [CNat]) (doubleCNat (n unit)) as CNat
    in  lambda p:BPos.
        p [CNat] f oneCNat;
bpos2pred_cnat =
    let f = lambda b:CBit.lambda n:Will CNat.
            b [CNat->CNat] succCNat (id [CNat]) (succCNat (doubleCNat (n unit)))
    in  lambda p:BPos.
        p [CNat] f zeroCNat;
cnat2succ_bpos =
    lambda n:CNat.
    cnatWithoutWill n [BPos] succBPos oneBPos;

BNat = BPos;
cnat2bnat = cnat2succ_bpos as CNat -> BNat;
bnat2cnat = bpos2pred_cnat as BNat -> CNat;


/*
TODO
    zipCList
    BNat/BPos
        add/sub...
    FringerTree
I :=λx . x.Prove thatSKK
*/

/*
cnat_ind = (lambda p : CNat -> Type . lambda n:CNat. lambda pn:p n. pn
    ) as (All P:CNat -> Type . All n:CNat. P n -> P n)
*/

