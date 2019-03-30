# Copyright 2015 Google Inc. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import re
import json
import sys
import unicodedata

from .parser import Parser


if sys.version_info[0] < 3:
    # pylint: disable=redefined-builtin
    str = unicode


def load(fp, encoding=None, cls=None, object_hook=None, parse_float=None,
         parse_int=None, parse_constant=None, object_pairs_hook=None):
    """Deserialize ``fp`` (a ``.read()``-supporting file-like object
    containing a JSON document) to a Python object."""

    s = fp.read()
    return loads(s, encoding=encoding, cls=cls, object_hook=object_hook,
                 parse_float=parse_float, parse_int=parse_int,
                 parse_constant=parse_constant,
                 object_pairs_hook=object_pairs_hook)


def loads(s, encoding=None, cls=None, object_hook=None, parse_float=None,
          parse_int=None, parse_constant=None, object_pairs_hook=None):
    """Deserialize ``s`` (a ``str`` or ``unicode`` instance containing a
    JSON5 document) to a Python object."""

    assert cls is None, 'Custom decoders are not supported'

    if sys.version_info[0] < 3:
        decodable_type = type('')
    else:
        decodable_type = type(b'')
    if isinstance(s, decodable_type):
        encoding = encoding or 'utf-8'
        s = s.decode(encoding)

    if not s:
        raise ValueError('Empty strings are not legal JSON5')
    parser = Parser(s, '<string>')
    ast, err, newpos = parser.parse()
    if err:
        raise ValueError(err)

    def _fp_constant_parser(s):
        return float(s.replace('Infinity', 'inf').replace('NaN', 'nan'))

    if object_pairs_hook:
        dictify = object_pairs_hook
    elif object_hook:
        dictify = lambda pairs: object_hook(dict(pairs))
    else:
        dictify = dict

    parse_float = parse_float or float
    parse_int = parse_int or int
    parse_constant = parse_constant or _fp_constant_parser

    return _walk_ast(ast, dictify, parse_float, parse_int, parse_constant)


def _walk_ast(el, dictify, parse_float, parse_int, parse_constant):
    if el == 'None':
        return None
    if el == 'True':
        return True
    if el == 'False':
        return False
    ty, v = el
    if ty == 'number':
        if v.startswith('0x') or v.startswith('0X'):
            return parse_int(v, base=16)
        elif '.' in v or 'e' in v or 'E' in v:
            return parse_float(v)
        elif 'Infinity' in v or 'NaN' in v:
            return parse_constant(v)
        else:
            return parse_int(v)
    if ty == 'string':
        return v
    if ty == 'object':
        pairs = []
        for key, val_expr in v:
            val = _walk_ast(val_expr, dictify, parse_float, parse_int,
                            parse_constant)
            pairs.append((key, val))
        return dictify(pairs)
    if ty == 'array':
        return [_walk_ast(el, dictify, parse_float, parse_int, parse_constant)
                for el in v]
    raise Exception('unknown el: ' + el)  # pragma: no cover


def dumps(obj, **kwargs):
    """Serialize ``obj`` to a JSON5-formatted ``str``."""

    unsupported_kwargs = set()
    for kw in ('ensure_ascii', 'check_circular', 'allow_nan', 'cls',
               'encoding', 'default'):
        if kw in kwargs:
            unsupported_kwargs.add(kw)
    if unsupported_kwargs:
        raise NotImplementedError("Haven't implemented these kwargs yet: %s" %
                                  ', '.join(unsupported_kwargs))

    if obj is True:
        return u'true'
    if obj is False:
        return u'false'
    if obj == None:
        return u'null'

    t = type(obj)
    if t == type('') or t == type(u''):
        single = "'" in obj
        double = '"' in obj
        if single and double:
            return json.dumps(obj)
        elif single:
            return '"' + obj + '"'
        else:
            return "'" + obj + "'"

    if t is float or t is int:
        return str(obj)

    indent = kwargs.get('indent', None)
    if indent is None:
        separators = kwargs.get('separators', (u', ', u': '))
    else:
        separators = kwargs.get('separators', (u',', u': '))
    if indent is not None:
        level = kwargs.get('level', 1)
        nl = '\n'
        if type(indent) == int:
            if indent > 0:
                indent = ' ' * indent
            else:
                indent = ''
    else:
        indent = ''
        level = 0
        nl = ''


    item_sep, kv_sep = separators
    indent_str = nl + indent * level
    if nl:
        end_str = ',' + nl + indent * (level - 1)
    else:
        end_str = nl + indent * (level - 1)

    item_sep += indent_str
    kwargs['level'] = level + 1

    # In Python3, we'd check if this was an abc.Mapping.
    # For now, just check for the attrs we need to iterate over the object.
    if hasattr(t, 'keys') and hasattr(t, '__getitem__'):
        return _dump_dict(obj, item_sep, kv_sep, indent_str, end_str, **kwargs)

    # In Python3, we'd check if this was an abc.Sequence.
    # For now, just check for the attrs we need to iterate over the object.
    if hasattr(t, '__getitem__') and hasattr(t, '__iter__'):
        return (u'[' + indent_str +
                item_sep.join([dumps(el, **kwargs) for el in obj]) +
                end_str + u']')

    # pragma: no cover
    return u''


def dump(obj, fp, **kwargs):
    """Serialize ``obj`` to a JSON5-formatted stream to ``fp`` (a ``.write()``-
    supporting file-like object)."""

    s = dumps(obj, **kwargs)
    fp.write(str(s))


def _dump_dict(obj, item_sep, kv_sep, indent_str, end_str, **kwargs):
    if kwargs.get('sort_keys', False):
        keys = sorted(obj.keys())
    else:
        keys = obj.keys()

    s = u'{' + indent_str

    skipkeys = kwargs.get('skipkeys', False)
    for i, k in enumerate(keys):
        valid_key, key_str = _dumpkey(k)
        if valid_key:
            s += key_str + kv_sep + dumps(obj[k], **kwargs)
            if i < len(keys) - 1:
                s += item_sep
        elif skipkeys:
            continue
        else:
            raise TypeError('invalid key %s' % str(k))
    s += end_str + u'}'
    return s


def _dumpkey(k):
    if type(k) in (int, float, type(''), long, type(u'')) or k == None:
        if _is_ident(k) and not _is_reserved_word(k):
            return True, k
        return True, json.dumps(k)
    return False, ''


def _is_ident(k):
    k = str(k)
    if not _is_id_start(k[0]) and k[0] not in (u'$', u'_'):
        return False
    for ch in k[1:]:
        if not _is_id_continue(ch) and ch not in (u'$', u'_'):
            return False
    return True

def _is_id_start(ch):
    return unicodedata.category(ch) in (
        'Lu', 'Ll', 'Li', 'Lt', 'Lm', 'Lo', 'Nl')


def _is_id_continue(ch):
    return unicodedata.category(ch) in (
        'Lu', 'Ll', 'Li', 'Lt', 'Lm', 'Lo', 'Nl', 'Nd', 'Mn', 'Mc', 'Pc')


_reserved_word_re = None

def _is_reserved_word(k):
    global _reserved_word_re

    if _reserved_word_re is None:
        # List taken from section 7.6.1 of ECMA-262.
        _reserved_word_re = re.compile('|'.join([
            'break',
            'case',
            'catch',
            'class',
            'const',
            'continue',
            'debugger',
            'default',
            'delete',
            'do',
            'else',
            'enum',
            'export',
            'extends',
            'false',
            'finally',
            'for',
            'function',
            'if',
            'import',
            'in',
            'instanceof',
            'new',
            'null',
            'return',
            'super',
            'switch',
            'this',
            'throw',
            'true',
            'try',
            'typeof',
            'var',
            'void',
            'while',
            'with',
        ]))
    return _reserved_word_re.match(k) is not None
