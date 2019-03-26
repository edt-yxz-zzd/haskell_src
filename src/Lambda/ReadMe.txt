
history:
    to encode/encrypt String
    ==>> String
            -[injection]->  [UInt] mod M
            <->             Bytes
            -[injection]->  Bytes/String
    ==>> core: [UInt] mod M <-> UInt <-> UInt <-> Bytes
    ==>> core: bijection
    ==>> how to combine bijections??


1) Category Bijection
    b1 . b2
2) when doing Full Range bijection, fine
    e.g. SInt <-> UInt <-> UInt <-> [UInt]
    when we can static typed SubRange, fine
    e.g.
        type List1 a = (a, [a])
            UInt <-> List1 UInt
        data Unicodes = [UInt] mod 0x110000
            Unicodes <-> UInt
    what if we want to use dynamic subrange?
        how to express:
            "(M1, UInt) <-> [UInt] mod M2 where M1==M2>=2"??
        1) "UInt <-> RadixNumber" where
            data RadixNumber = RadixNumber {radix::QInt, digits::[UInt] mod radix)}
            not good, No "M", map UInt into all modulo!
        2) "(M1, UInt) <-> [UInt] mod M2 where M1::QInt, M2::QInt"
            not good, No "M1 == M2"

