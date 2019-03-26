
r'''
eval:
    from CombinatorSKIBC
        Primitive = S | K | I | B | C
        Variable = x | y | ...
        -- CombinatorSKIBC = Apps Primitive [CombinatorSKIBC] | Variable
        CombinatorSKIBC
            = CombinatorSKIBC_Apps | Variable
        CombinatorSKIBC_Apps -- begin with Primitive not Variable
            = Primitive
            | App CombinatorSKIBC_Apps CombinatorSKIBC
    to  LambdaR | noreturn
        Lambda = Abs Variable Lambda | App Lambda Lambda | Variable
        LambdaReduced
            = LambdaReduced_Apps | Abs Variable LambdaReduced
        LambdaReduced_Apps
            = Variable | App LambdaReduced_Apps LambdaReduced
        LambdaR = LambdaReduced && no (\x. E x) where x not in FV(E)




if stop then there std value

1) given closed CombinatorSKIBC
2) calc required args, and offer arguments
3) evalue until
    3-1) app begins with a primitive, required args, goto 2)
    3-2) app begins with a variables/arguments, stop
        evalue all subterms
        erase unused input tails
        the result is the standard value
            total_required -> term of (primitives | variables | arguments <= total_required)
    3-3) infinite-loop...


0) primitives = S K I B C
    variables = x y z ... -- any closed term
    arguments = 1 2 3 ... -- input

1) length 1
    require:
    S : 3
        S 1 2 3 = (1 3) (2 3)
        S = 3 -> (1 3) (2 3)
    K : 2
        K 1 2 = 1
        K = 2 -> 1
    I : 1
        I 1 = 1
        I = 1 -> 1
    B : 3
        B 1 2 3 = 1 (2 3)
        B = 3 -> 1 (2 3)
    C : 3
        C 1 2 3 = 1 3 2
        C = 3 -> 1 3 2

2) length 2
    S S 1 2 3 = S 2 (1 2) 3 = (2 3) (1 2 3)
        S S = 3 -> (2 3) (1 2 3)
    S K 1 2 = K 2 (1 2) = 2
        S K = 2 -> 2
        S K x = 1 -> 1 = I  --------------------------------
    S I 1 2 = I 2 (1 2) = 2 (1 2)
        S I = 2 -> 2 (1 2)
    S B 1 2 3 = B 2 (1 2) 3 = 2 (1 2 3)
        S B = 3 -> 2 (1 2 3)
    S C 1 2 3 = C 2 (1 2) 3 = 2 3 (1 2)
        S C = 3 -> 2 3 (1 2)

    K / I ...
    B S 1 2 3 4 = S (1 2) 3 4 = (1 2 4) (3 4)
        B S = 4 -> (1 2 4) (3 4)
    B K 1 2 3 = K (1 2) 3 = 1 2
        B K = 3 -> 1 2
    B I 1 2 = I (1 2) = 1 2
        B I = 2 -> 1 2 = 1 -> 1 = I ------------------
    B B 1 2 3 4 = B (1 2) 3 4 = (1 2) (3 4)
        B B = 4 -> (1 2) (3 4)
    B C 1 2 3 = C (1 2) 3 = 1 2


'''
import Stream
#rint(Stream)
#rint(Stream.__file__)
from Stream import *
from Stream import CharStream

class Term:
    def __init__(self, fv:frozenset, is_reduced):
        # this field is mutable once
        self.cached_reduced_term = None
        # other fields are immutable
        self.fv = fv # a set of (int, name)
        self.is_reduced = is_reduced
        #rint(self)
        #rint(self.fv)
        assert all(isinstance(n, Name) for n in fv)
    def __contains__(self, var):
        assert isVariable(var)
        return var.name in self.fv
    def is_closed(self):
        return len(self.fv) == 0
    def to_str(self, at_first:bool, at_last:bool):
        raise NotImplementedError
    def sub(self, var, term):
        raise NotImplementedError
    def __str__(self):
        return self.to_str(at_first=True, at_last=True)


class Name:
    def __init__(self, i, base_name):
        self.args = (i, base_name)
        self.hash = hash(self.args)
    @property
    def base_name(self):
        return self.args[1]
    def __hash__(self):
        return self.hash
    def __ne__(self, other):
        return not (self == other)
    def __eq__(self, other):
        assert type(other) is Name
        return hash(self) == hash(other) and self.args == other.args

class Variable(Term):
    __fresh_old = 0
    def __init__(self, base_name=None):
        # base_name is None then yield a fresh var
        # name = (i, base_name) = (0, not None) | (not 0, None)
        if base_name is not None:
            n = 0
        else:
            __class__.__fresh_old += 1
            n = __class__.__fresh_old
        v = self.__name = Name(n, base_name)
        self.hash = hash(v)
        super().__init__(frozenset([v]), True)
    @property
    def name(self):
        return self.__name
    def __hash__(self):
        return self.hash
    def __ne__(self, other):
        return not (self == other)
    def __eq__(self, other):
        assert isinstance(other, Variable)
        return hash(self) == hash(other) and self.__name == other.__name
    def to_str(self, at_first:bool, at_last:bool):
        n, base_name = self.__name.args
        if base_name is None:
            assert n > 0
            return '${}'.format(n)
        elif n == 0:
            return str(base_name)
        else:
            raise ValueError
    def sub(self, var, term):
        if var == self:
            return term
        return self

class App(Term):
    def __init__(self, t1, t2):
        assert isinstance(t1, Term)
        assert isinstance(t2, Term)
        self.t1 = t1
        self.t2 = t2
        is_reduced = False
        if t1.is_reduced and t2.is_reduced:
            is_reduced = isApp(t1) or isVariable(t1)
        super().__init__(t1.fv | t2.fv, is_reduced)
    def get_app(self):
        return self.t1, self.t2
    def to_str(self, at_first:bool, at_last:bool):
        fmt = '{} {}' if at_first else '({} {})'
        t1, t2 = self.get_app()

        # T T - t1 t2
        # T F - t1 t2 t3... -- t2 not at last
        # F T - f ... (t1 t2)
        # F F - f ... (t1 t2) t3...
        at_first1 = True
        at_last1 = False
        at_first2 = False
        at_last2 = not (at_first and not at_last)
        s = fmt.format  ( t1.to_str(at_first1, at_last1)
                        , t2.to_str(at_first2, at_last2)
                        )
        return s
    def sub(self, var, term):
        if var not in self:
            return self
        t1, t2 = self.get_app()
        return App(t1.sub(var, term), t2.sub(var, term))


def isEta(var, body):
    assert isVariable(var)
    assert isTerm(body)
    # \x. E x where x not in E
    if isApp(body):
        t1, t2 = body.get_app()
        return isVariable(t2) and t2 == var and var not in t1
    return False

class Abs(Term):
    # var:Variable, body:Term
    # why not var:Name?
    # Name = (int, base_name)
    # but Variable.name = (0, base_name not None) | (int > 0, None)
    #   only a subset of name
    def __init__(self, var, body):
        assert isinstance(var, Variable)
        assert isinstance(body, Term)
        self.var = var
        self.body = body
        #rint(var, body.fv, frozenset)
        fv = body.fv - frozenset([var.name])
        #rint(fv)
        is_reduced = body.is_reduced
        if is_reduced:
            if isEta(var, body):
                is_reduced = False
        super().__init__(fv, is_reduced)
    def get_abs(self):
        return self.var, self.body
    def to_str(self, at_first:bool, at_last:bool):
        fmt = r'\{}.{}'
        var, body = self.get_abs()
        return fmt.format(var, body)
    def call_on(self, t):
        assert isTerm(t)
        var, body = self.get_abs()
        return body.sub(var, t)
    def sub(self, var, term):
        if var not in self:
            return self
        x, body = self.get_abs()
        assert var != x
        if x in term:
            z = Variable() # for fresh var
            body = body.sub(x, z)
            x = z
        assert x not in term
        assert x != var
        body = body.sub(var, term)
        return Abs(x, body)



empty_set = frozenset()
class Primitive(Abs):
    def __init__(self, nargs:int, closed_lambda_term):
        assert nargs >= 0
        assert isClosedLambdaTerm(closed_lambda_term)
        assert isAbs(closed_lambda_term)
        assert closed_lambda_term.is_reduced
        self.closed_lambda_term = closed_lambda_term
        self.nargs = nargs
        var, body = closed_lambda_term.get_abs()
        super().__init__(var, body)
        assert self.is_reduced


def isTerm(t):
    return isinstance(t, Term)
def isClosedTerm(t):
    return isTerm(t) and t.is_closed()
def isVariable(t):
    return isinstance(t, Variable)
def isApp(t):
    return isinstance(t, App)
def isAbs(t):
    return isinstance(t, Abs)
def isPrimitive(t):
    return isinstance(t, Primitive)
class IsTerm:
    def __call__(self, t):
        return self.is_term(t)
    def is_app_t1(self, t):
        return self.is_term(t)
    def is_app_t2(self, t):
        return self.is_term(t)
    def is_abs_body(self, t):
        return self.is_term(t)
    def is_good_abs(self, var, body):
        return self.is_abs_body(body)
    def otherwise(self, t):
        return isVariable(t)
    def is_term(self, t):
        if isApp(t):
            t1, t2 = t.get_app()
            return self.is_app_t1(t1) and self.is_app_t2(t2)
        elif isAbs(t):
            var, body = t.get_abs()
            return self.is_good_abs(var, body)
        else:
            return self.otherwise(t)


class IsCombinator_Apps(IsTerm):
    # not begin with Variable
    def is_app_t2(self, t):
        return isCombinator(t)
    def is_abs_body(self, t):
        raise NotImplementedError
    def is_good_abs(self, var, body):
        return False
    def otherwise(self, t):
        return isPrimitive(t)
isCombinator_Apps = IsCombinator_Apps()
def isCombinator(t):
    # not begin with variables
    return isCombinator_Apps(t) or isVariable(t)



class IsLambda(IsTerm):pass
isLambda = IsLambda()
def isClosedLambdaTerm(t):
    return isClosedTerm(t) and isLambda(t)

class IsLambdaReduced_Apps(IsTerm):
    # begin with Variable
    def is_app_t2(self, t):
        return isLambdaReduced(t)
    def is_abs_body(self, t):
        raise NotImplementedError
    def is_good_abs(self, var, body):
        return False
isLambdaReduced_Apps = IsLambdaReduced_Apps()
def isLambdaReduced(t):
    while isAbs(t):
        var, t = t.get_abs()
    return isLambdaReduced_Apps(t)

class HasNoEta(IsTerm):
    def is_good_abs(self, var, body):
        if isApp(body):
            t1, t2 = body.get_app()
            if t2 == var and var not in t1:
                return False
        return self.is_term(body)
hasNoEta = HasNoEta()
def isLambdaR(t):
    return isLambdaReduced(t) and hasNoEta(t)



def evalLambdaTerm(lambda_term):
    # assert isLambda(t)
    # return result; not inplace
    t = lambda_term
    if t.is_reduced:
        return t
    if t.cached_reduced_term is not None:
        return t.cached_reduced_term

    while not t.is_reduced:
        if isApp(t):
            t1, t2 = t.get_app()
            t1 = evalLambdaTerm(t1)
            t2 = evalLambdaTerm(t2)
            if isAbs(t1):
                t = t1.call_on(t2) # no inplace
                continue
            else:
                t = App(t1, t2)
        elif isAbs(t):
            var, body = t.get_abs()
            body = evalLambdaTerm(body)
            assert body.is_reduced
            if isEta(var, body):
                t, _ = body.get_app()
            else:
                t = Abs(var, body)
        else:
            raise TypeError
        break
    assert t.is_reduced
    lambda_term.cached_reduced_term = t # set once here
    return t

def _apps(a):
    # a : A
    # A = Term | [A, A...]
    if isTerm(a):
        return a
    else:
        return apps(*a)
def apps(f, *args):
    f = _apps(f)
    for a in map(_apps, args):
        f = App(f, a)
    return f
def abss(names, f, *args):
    vars_ = vars(names)
    body = apps(f, *args)
    for var in reversed(vars_):
        body = Abs(var, body)
    return body
def vars(names):
    return list(map(Variable, names))
S = abss('xyz', vars('xz'), vars('yz'))
assert str(S) == r'\x.\y.\z.x z (y z)'

def parseName(s):
    return s.read_identifier()
def parseLambdaAtom(s):
    h, ts = s.any_char()
    if h == '(':
        ts = ts.skip_spaces()
        v, ts = parseLambda(ts)
        ts = ts.skip_spaces()
        ts = ts.skip_prefix(')')
        return v, ts
    # variable
    # bug: name, ts = parseName(ts)
    ts = s # restore the h
    name, ts = parseName(ts)
    return Variable(name), ts

def parseLambda_final(s:CharStream):
    ts = s.skip_spaces()
    v, ts = parseLambda(ts)
    ts = ts.skip_spaces()
    ts.eof()
    return v
def parseLambda(s:CharStream):
    # no spaces at beginning
    # return lambda_term, remain_stream
    h, ts = s.any_char()
    if h == '\\':
        ts = ts.skip_spaces()
        name, ts = parseName(ts)
        ts = ts.skip_spaces()
        ts = ts.skip_prefix('.')
        body, ts = parseLambda(ts)
        return abss([name], body), ts
    # apps
    # parse Atom+
    # bug: ls, ts = ts.many1(parse_list(parseLambdaAtom, type(ts).spaces))
    ts = s # restore the h
    ls, ts = ts.many1(parse_list(parseLambdaAtom, type(ts).spaces))
    ls = list(map(lambda x: x[0], ls))
    #rint([str(e) for e in ls])
    #rint(s.as_str())
    return apps(ls), ts

def parseLambda_str(s:str):
    ts = CharStream(s, 0, len(s))
    return parseLambda_final(ts)


strS = r'\x.\y.\z.x z (y z)'
strK = r'\x.\y.x'
strI = r'\x.x'
strB = r'\x.\y.\z.x (y z)'
strC = r'\x.\y.\z.x z y'
assert str(S) == strS
#rint(S)
S_ = parseLambda_str(strS)
assert str(S_) == strS
#rint(S_)
S, K, I, B, C = map(parseLambda_str, [strS, strK, strI, strB, strC])
primitives = S, K, I, B, C
#rint(*primitives)

BI = apps(B, I)
# (\x.\y.\z. x (y z)) (\x.x) = \y.\z. (\x.x) (y z) = \y.\z. y z = \y.y
BI = evalLambdaTerm(BI)
assert str(BI) == r'\y.y'
#rint('BI=', BI)



'''
class Term:
    def __init__(self):
        self.value = None
    def require(self):
        # -> Nat | None
        return None
    def eval(self):
        raise NotImplementedError
    def is_value(self):
        return False
    def __call__(self, other, *others):
        if isinstance(self, Apps):
            return self.app(other, *others)
        return Apps(self, other, *others)
class Apps(Term):
    def is_value(self):
        return self.head.is_value() and all(t.is_value() for t in self.ls)
    def require(self):
        return self.head.require
    def reduce(self, f, g, a):
        head = self.head
        ls = self.ls
        while not head.is_value():
            n = head.require()
            if n > len(ls):
                break
            if n < 0: n = 0
            args = ls[:n]
            ls = ls[n:]
            head = head.reduce(*args)
        self._require = head.require() - len(ls)
        self.head = head
        self.ls = [a.reduce() for a in ls]

        return self
    def __init__(self, t, *args):
        assert isinstance(t, Word)
        assert not isinstance(t, Apps)
        self.head = t
        self.ls = list(args)
        n = self.head.require()
        self._require = None
        if n is not None:
            self._require = n - len(args)
            # maybe _require <= 0:
    def app(self, t, *ts):
        assert isinstance(t, Term)
        self._require -= 1 + len(ts)
        self.ls.append(t).extend(ts)
class Word(Term):
    def is_value(self):
        return not self.isPrimitive()
    def __init__(self, w):
        self.w = w
    def isPrimitive(self):
        return False
    def isVariable(self):
        return False
    def isArgument(self):
        return False
class Primitive(Word):
    def reduce(self, *args):
        raise NotImplementedError
    def require(self):
        raise NotImplementedError
    def isPrimitive(self):
        return True
class PS(Primitive):
    def require(self):
        return 3
    def reduce(self, f, g, a):
        return f(a)(g(a))
class Variable(Word):
    def isVariable(self):
        return True
    pass
class Argument(Word):
    def isArgument(self):
        return True
    pass
'''


