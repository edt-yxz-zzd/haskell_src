def make_predicator(f):
    # callable or container
    return f if callable(f) else lambda x: x in f
def ors(default, *preds):
    # debug: call ors by: ors(True, *preds) # NOTE: the '*'
    # callable or container
    default = bool(default)
    if not preds:
        return lambda _: default
    return OrPredicators(*preds)
class OrPredicators:
    def __init__(self, *fs):
        # callable or container
        self.fs = tuple(map(make_predicator, fs))
    def __call__(self, x):
        for f in self.fs:
            if f(x):
                return True
        return False
class Stream:
    # immutable
    def __init__(self, s:'array', begin, end):
        assert 0 <= begin <= end <= len(s)
        self.array = s
        self.begin = begin
        self.end = end
    def as_array(self):
        s, begin, end = self.get_args()
        return s[begin:end]
    def get_args(self):
        # s, begin, end = self.get_args()
        return self.array, self.begin, self.end

    def replace(self, begin=None, end=None):
        if begin is None:
            begin = self.begin
        if end is None:
            end = self.end
        return type(self)(self.array, begin, end)
    def __len__(self):
        return self.end - self.begin
    def may_head(self):
        s, begin, end = self.get_args()
        return s[begin, begin+1]
    def __iter__(self):
        # [(pos, char)]
        s, begin, end = self.get_args()
        for i in range(begin, end):
            yield i, s[i]
    def may_split(self):
        return read_le(1)

    def any_token(self, *preds, charset_name="any_token"):
        ch, ts = self.read_eq(1)
        # bug: f = ors(True, preds)
        f = ors(True, *preds)
        if f(ch):
            return ch, ts
        raise ParseError("excepted: {!r}".format(charset_name))
    def read_eq(self, n):
        s, ts = self.read_le(n)
        if len(s) < n:
            raise ParseError("not enough chars")
        return s, ts
    def read_le(self, n):
        if n <= 0:
            return '', self
        s, begin, end = self.get_args()
        i = min(end, begin+n)
        return s[begin:i], self.replace(begin=i)
    def read_while(self, pred, *preds):
        pred = ors(False, pred, *preds)
        s, begin, end = self.get_args()
        pos = end
        for pos, ch in self:
            if not pred(ch):
                break
        L = pos - begin
        return self.read_le(L)

    def skip_prefix(self, prefix):
        return self.const_prefix(prefix)[1]
    def const_prefix(self, prefix):
        s, begin, end = self.get_args()
        L = len(prefix)
        pre, ts = self.read_le(L)
        if pre != prefix:
            raise ParseError("expected: {!r}".format(prefix))
        return pre, ts
    def many(self, parse):
        try:
            return self.many1(parse)
        except ParseError:
            return [], self
    def many1(self, parse, sepBy=None, endBy=None):
        ts = self
        v, ts = parse(ts)
        ls = [v]
        if sepBy is None:
            p1 = parse
        else:
            def p1(ts):
                try:
                    _, ts = sepBy(ts)
                except Many1Except:
                    raise logic-error
                try:
                    return parse(ts)
                except ParseError as e:
                    raise Many1Error(e)
        if endBy is None:
            p = p1
        else:
            def p(ts):
                try:
                    _, ts = endBy(ts)
                except Many1Except:
                    raise logic-error
                except ParseError as e:
                    # ignore
                    return p1(ts)
                # if success then halt
                raise Many1EndBy(ts)
        try:
            while True:
                v, ts = p(ts)
                if ts is self:
                    raise ParseError('many (""->a)')
                ls.append(v)
        except ParseError:
            # end
            if endBy is not None:
                # error
                raise
            return ls, ts
        except Many1EndBy as e:
            # endBy
            assert endBy is not None
            ts = e.args[0]
            return ls, ts
        except Many1Error as e:
            raise e.args[0]
        raise logic-error
    def parses(self, *parses):
        ts = self
        ls = []
        for parse in parses:
            v, ts = parse(ts)
            ls.append(v)
        return ls, ts
    def between(self, open, close, parse):
        [_, v, _], ts = self.parses(open, parse, close)
        return v, ts
    def __repr__(self):
        return '{}{}'.format(type(self).__name__, self.get_args())
    def eof(self):
        # parser version
        # see also: bool(self)
        if self:
            raise ParseError('expected EOF')
        return None, self
def parse_list(*parses):
    return lambda self: self.parses(*parses)

class CharStream(Stream):
    def __init__(self, s:'str', begin, end):
        assert type(s) is str
        super().__init__(s, begin, end)
    def as_str(self):
        return self.as_array()
    def spaces(self):
        return self.read_while(str.isspace)
    def skip_spaces(self):
        return self.spaces()[1]
    def read_idHead(self):
        return self.any_char(str.isalpha, '_', charset_name='idHead')
    def read_idNotHead(self):
        return self.any_char(str.isalnum, '_', charset_name='idNotHead')
    def read_identifier(self):
        ts = self
        h, ts = ts.read_idHead()
        s, ts = ts.read_while(str.isalnum, '_')
        return h+s, ts
    def any_alpha(self):
        return self.any_char(pred=str.isalpha, charset_name="alpha")
    def any_alnum(self):
        return self.any_char(pred=str.isalnum, charset_name="alpha_number")
    def any_char(self, *preds, charset_name="any_char"):
        return self.any_token(*preds, charset_name=charset_name)
class ParseError(Exception):pass
class Many1Except(Exception):pass
class Many1Error(Many1Except):pass
class Many1EndBy(Many1Except):pass


#rint(list(globals().keys()))
['__name__', '__doc__', '__package__', '__loader__', '__spec__', '__file__', '__cached__', '__builtins__', 'make_predicator', 'ors', 'OrPredicators', 'Stream', 'parse_list', 'CharStream', 'ParseError', 'Many1Except', 'Many1Error', 'Many1EndBy']

__all__ = ['make_predicator', 'ors', 'OrPredicators', 'Stream', 'parse_list', 'CharStream', 'ParseError']

