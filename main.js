(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


// BYTES

function _Bytes_width(bytes)
{
	return bytes.byteLength;
}

var _Bytes_getHostEndianness = F2(function(le, be)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(new Uint8Array(new Uint32Array([1]))[0] === 1 ? le : be));
	});
});


// ENCODERS

function _Bytes_encode(encoder)
{
	var mutableBytes = new DataView(new ArrayBuffer($elm$bytes$Bytes$Encode$getWidth(encoder)));
	$elm$bytes$Bytes$Encode$write(encoder)(mutableBytes)(0);
	return mutableBytes;
}


// SIGNED INTEGERS

var _Bytes_write_i8  = F3(function(mb, i, n) { mb.setInt8(i, n); return i + 1; });
var _Bytes_write_i16 = F4(function(mb, i, n, isLE) { mb.setInt16(i, n, isLE); return i + 2; });
var _Bytes_write_i32 = F4(function(mb, i, n, isLE) { mb.setInt32(i, n, isLE); return i + 4; });


// UNSIGNED INTEGERS

var _Bytes_write_u8  = F3(function(mb, i, n) { mb.setUint8(i, n); return i + 1 ;});
var _Bytes_write_u16 = F4(function(mb, i, n, isLE) { mb.setUint16(i, n, isLE); return i + 2; });
var _Bytes_write_u32 = F4(function(mb, i, n, isLE) { mb.setUint32(i, n, isLE); return i + 4; });


// FLOATS

var _Bytes_write_f32 = F4(function(mb, i, n, isLE) { mb.setFloat32(i, n, isLE); return i + 4; });
var _Bytes_write_f64 = F4(function(mb, i, n, isLE) { mb.setFloat64(i, n, isLE); return i + 8; });


// BYTES

var _Bytes_write_bytes = F3(function(mb, offset, bytes)
{
	for (var i = 0, len = bytes.byteLength, limit = len - 4; i <= limit; i += 4)
	{
		mb.setUint32(offset + i, bytes.getUint32(i));
	}
	for (; i < len; i++)
	{
		mb.setUint8(offset + i, bytes.getUint8(i));
	}
	return offset + len;
});


// STRINGS

function _Bytes_getStringWidth(string)
{
	for (var width = 0, i = 0; i < string.length; i++)
	{
		var code = string.charCodeAt(i);
		width +=
			(code < 0x80) ? 1 :
			(code < 0x800) ? 2 :
			(code < 0xD800 || 0xDBFF < code) ? 3 : (i++, 4);
	}
	return width;
}

var _Bytes_write_string = F3(function(mb, offset, string)
{
	for (var i = 0; i < string.length; i++)
	{
		var code = string.charCodeAt(i);
		offset +=
			(code < 0x80)
				? (mb.setUint8(offset, code)
				, 1
				)
				:
			(code < 0x800)
				? (mb.setUint16(offset, 0xC080 /* 0b1100000010000000 */
					| (code >>> 6 & 0x1F /* 0b00011111 */) << 8
					| code & 0x3F /* 0b00111111 */)
				, 2
				)
				:
			(code < 0xD800 || 0xDBFF < code)
				? (mb.setUint16(offset, 0xE080 /* 0b1110000010000000 */
					| (code >>> 12 & 0xF /* 0b00001111 */) << 8
					| code >>> 6 & 0x3F /* 0b00111111 */)
				, mb.setUint8(offset + 2, 0x80 /* 0b10000000 */
					| code & 0x3F /* 0b00111111 */)
				, 3
				)
				:
			(code = (code - 0xD800) * 0x400 + string.charCodeAt(++i) - 0xDC00 + 0x10000
			, mb.setUint32(offset, 0xF0808080 /* 0b11110000100000001000000010000000 */
				| (code >>> 18 & 0x7 /* 0b00000111 */) << 24
				| (code >>> 12 & 0x3F /* 0b00111111 */) << 16
				| (code >>> 6 & 0x3F /* 0b00111111 */) << 8
				| code & 0x3F /* 0b00111111 */)
			, 4
			);
	}
	return offset;
});


// DECODER

var _Bytes_decode = F2(function(decoder, bytes)
{
	try {
		return $elm$core$Maybe$Just(A2(decoder, bytes, 0).b);
	} catch(e) {
		return $elm$core$Maybe$Nothing;
	}
});

var _Bytes_read_i8  = F2(function(      bytes, offset) { return _Utils_Tuple2(offset + 1, bytes.getInt8(offset)); });
var _Bytes_read_i16 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 2, bytes.getInt16(offset, isLE)); });
var _Bytes_read_i32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getInt32(offset, isLE)); });
var _Bytes_read_u8  = F2(function(      bytes, offset) { return _Utils_Tuple2(offset + 1, bytes.getUint8(offset)); });
var _Bytes_read_u16 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 2, bytes.getUint16(offset, isLE)); });
var _Bytes_read_u32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getUint32(offset, isLE)); });
var _Bytes_read_f32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getFloat32(offset, isLE)); });
var _Bytes_read_f64 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 8, bytes.getFloat64(offset, isLE)); });

var _Bytes_read_bytes = F3(function(len, bytes, offset)
{
	return _Utils_Tuple2(offset + len, new DataView(bytes.buffer, bytes.byteOffset + offset, len));
});

var _Bytes_read_string = F3(function(len, bytes, offset)
{
	var string = '';
	var end = offset + len;
	for (; offset < end;)
	{
		var byte = bytes.getUint8(offset++);
		string +=
			(byte < 128)
				? String.fromCharCode(byte)
				:
			((byte & 0xE0 /* 0b11100000 */) === 0xC0 /* 0b11000000 */)
				? String.fromCharCode((byte & 0x1F /* 0b00011111 */) << 6 | bytes.getUint8(offset++) & 0x3F /* 0b00111111 */)
				:
			((byte & 0xF0 /* 0b11110000 */) === 0xE0 /* 0b11100000 */)
				? String.fromCharCode(
					(byte & 0xF /* 0b00001111 */) << 12
					| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
					| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
				)
				:
				(byte =
					((byte & 0x7 /* 0b00000111 */) << 18
						| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 12
						| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
						| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
					) - 0x10000
				, String.fromCharCode(Math.floor(byte / 0x400) + 0xD800, byte % 0x400 + 0xDC00)
				);
	}
	return _Utils_Tuple2(offset, string);
});

var _Bytes_decodeFailure = F2(function() { throw 0; });



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Main$GotSeed = function (a) {
	return {$: 'GotSeed', a: a};
};
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$random$Random$map3 = F4(
	function (func, _v0, _v1, _v2) {
		var genA = _v0.a;
		var genB = _v1.a;
		var genC = _v2.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v3 = genA(seed0);
				var a = _v3.a;
				var seed1 = _v3.b;
				var _v4 = genB(seed1);
				var b = _v4.a;
				var seed2 = _v4.b;
				var _v5 = genC(seed2);
				var c = _v5.a;
				var seed3 = _v5.b;
				return _Utils_Tuple2(
					A3(func, a, b, c),
					seed3);
			});
	});
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$random$Random$independentSeed = $elm$random$Random$Generator(
	function (seed0) {
		var makeIndependentSeed = F3(
			function (state, b, c) {
				return $elm$random$Random$next(
					A2($elm$random$Random$Seed, state, (1 | (b ^ c)) >>> 0));
			});
		var gen = A2($elm$random$Random$int, 0, 4294967295);
		return A2(
			$elm$random$Random$step,
			A4($elm$random$Random$map3, makeIndependentSeed, gen, gen, gen),
			seed0);
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Basics$round = _Basics_round;
var $author$project$Main$maxPotential = function (_v0) {
	var width = _v0.width;
	var height = _v0.height;
	return ($elm$core$Basics$round(width + height) / 4) | 0;
};
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $author$project$Position$squareOps = {
	directions: _List_fromArray(
		[
			function (_v0) {
			var x = _v0.a;
			var y = _v0.b;
			return _Utils_Tuple2(x + 1, y);
		},
			function (_v1) {
			var x = _v1.a;
			var y = _v1.b;
			return _Utils_Tuple2(x - 1, y);
		},
			function (_v2) {
			var x = _v2.a;
			var y = _v2.b;
			return _Utils_Tuple2(x, y + 1);
		},
			function (_v3) {
			var x = _v3.a;
			var y = _v3.b;
			return _Utils_Tuple2(x, y - 1);
		}
		]),
	toPoint: A2($elm$core$Tuple$mapBoth, $elm$core$Basics$toFloat, $elm$core$Basics$toFloat),
	zero: _Utils_Tuple2(0, 0)
};
var $author$project$Main$positionOps = $author$project$Position$squareOps;
var $elm$core$Basics$pow = _Basics_pow;
var $avh4$elm_color$Color$toRgba = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	return {alpha: a, blue: b, green: g, red: r};
};
var $noahzgordon$elm_color_extra$Color$Convert$colorToXyz = function (cl) {
	var c = function (ch) {
		var ch_ = (ch > 4.045e-2) ? A2($elm$core$Basics$pow, (ch + 5.5e-2) / 1.055, 2.4) : (ch / 12.92);
		return ch_ * 100;
	};
	var _v0 = $avh4$elm_color$Color$toRgba(cl);
	var red = _v0.red;
	var green = _v0.green;
	var blue = _v0.blue;
	var b = c(blue);
	var g = c(green);
	var r = c(red);
	return {x: ((r * 0.4124) + (g * 0.3576)) + (b * 0.1805), y: ((r * 0.2126) + (g * 0.7152)) + (b * 7.22e-2), z: ((r * 1.93e-2) + (g * 0.1192)) + (b * 0.9505)};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $noahzgordon$elm_color_extra$Color$Convert$xyzToLab = function (_v0) {
	var x = _v0.x;
	var y = _v0.y;
	var z = _v0.z;
	var c = function (ch) {
		return (ch > 8.856e-3) ? A2($elm$core$Basics$pow, ch, 1 / 3) : ((7.787 * ch) + (16 / 116));
	};
	var x_ = c(x / 95.047);
	var y_ = c(y / 100);
	var z_ = c(z / 108.883);
	return {a: 500 * (x_ - y_), b: 200 * (y_ - z_), l: (116 * y_) - 16};
};
var $noahzgordon$elm_color_extra$Color$Convert$colorToLab = A2($elm$core$Basics$composeR, $noahzgordon$elm_color_extra$Color$Convert$colorToXyz, $noahzgordon$elm_color_extra$Color$Convert$xyzToLab);
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$Basics$isNaN = _Basics_isNaN;
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $avh4$elm_color$Color$toHsla = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var minColor = A2(
		$elm$core$Basics$min,
		r,
		A2($elm$core$Basics$min, g, b));
	var maxColor = A2(
		$elm$core$Basics$max,
		r,
		A2($elm$core$Basics$max, g, b));
	var l = (minColor + maxColor) / 2;
	var s = _Utils_eq(minColor, maxColor) ? 0 : ((l < 0.5) ? ((maxColor - minColor) / (maxColor + minColor)) : ((maxColor - minColor) / ((2 - maxColor) - minColor)));
	var h1 = _Utils_eq(maxColor, r) ? ((g - b) / (maxColor - minColor)) : (_Utils_eq(maxColor, g) ? (2 + ((b - r) / (maxColor - minColor))) : (4 + ((r - g) / (maxColor - minColor))));
	var h2 = h1 * (1 / 6);
	var h3 = $elm$core$Basics$isNaN(h2) ? 0 : ((h2 < 0) ? (h2 + 1) : h2);
	return {alpha: a, hue: h3, lightness: l, saturation: s};
};
var $author$project$Main$setWhiteAs = function (_v0) {
	var minValue = _v0.a;
	var maxValue = _v0.b;
	return $elm$core$List$filter(
		function (_v1) {
			var color = _v1.b;
			var _v2 = $avh4$elm_color$Color$toHsla(color);
			var alpha = _v2.alpha;
			return function (_v3) {
				var l = _v3.l;
				return (alpha < 0.5) || ((_Utils_cmp(minValue, l) < 1) && (_Utils_cmp(l, maxValue) < 1));
			}(
				$noahzgordon$elm_color_extra$Color$Convert$colorToLab(color));
		});
};
var $author$project$Main$new = function (flag) {
	return {
		grid: $elm$core$Dict$fromList(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var _v1 = _v0.a;
					var x = _v1.a;
					var y = _v1.b;
					return _Utils_Tuple2(
						_Utils_Tuple2(x, y),
						$elm$core$Maybe$Nothing);
				},
				A2(
					$author$project$Main$setWhiteAs,
					_Utils_Tuple2(90, 100),
					flag.image))),
		height: flag.height,
		image: $elm$core$Dict$fromList(flag.image),
		player: $author$project$Main$positionOps.zero,
		potential: $author$project$Main$maxPotential(
			{height: flag.height, width: flag.width}),
		running: false,
		seed: $elm$random$Random$initialSeed(42),
		width: flag.width
	};
};
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2(
		$author$project$Main$new(
			{height: 1, image: _List_Nil, width: 1}),
		A2($elm$random$Random$generate, $author$project$Main$GotSeed, $elm$random$Random$independentSeed));
};
var $author$project$Main$Frame = {$: 'Frame'};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$time$Time$Every = F2(
	function (a, b) {
		return {$: 'Every', a: a, b: b};
	});
var $elm$time$Time$State = F2(
	function (taggers, processes) {
		return {processes: processes, taggers: taggers};
	});
var $elm$time$Time$init = $elm$core$Task$succeed(
	A2($elm$time$Time$State, $elm$core$Dict$empty, $elm$core$Dict$empty));
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$time$Time$addMySub = F2(
	function (_v0, state) {
		var interval = _v0.a;
		var tagger = _v0.b;
		var _v1 = A2($elm$core$Dict$get, interval, state);
		if (_v1.$ === 'Nothing') {
			return A3(
				$elm$core$Dict$insert,
				interval,
				_List_fromArray(
					[tagger]),
				state);
		} else {
			var taggers = _v1.a;
			return A3(
				$elm$core$Dict$insert,
				interval,
				A2($elm$core$List$cons, tagger, taggers),
				state);
		}
	});
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$time$Time$setInterval = _Time_setInterval;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$time$Time$spawnHelp = F3(
	function (router, intervals, processes) {
		if (!intervals.b) {
			return $elm$core$Task$succeed(processes);
		} else {
			var interval = intervals.a;
			var rest = intervals.b;
			var spawnTimer = $elm$core$Process$spawn(
				A2(
					$elm$time$Time$setInterval,
					interval,
					A2($elm$core$Platform$sendToSelf, router, interval)));
			var spawnRest = function (id) {
				return A3(
					$elm$time$Time$spawnHelp,
					router,
					rest,
					A3($elm$core$Dict$insert, interval, id, processes));
			};
			return A2($elm$core$Task$andThen, spawnRest, spawnTimer);
		}
	});
var $elm$time$Time$onEffects = F3(
	function (router, subs, _v0) {
		var processes = _v0.processes;
		var rightStep = F3(
			function (_v6, id, _v7) {
				var spawns = _v7.a;
				var existing = _v7.b;
				var kills = _v7.c;
				return _Utils_Tuple3(
					spawns,
					existing,
					A2(
						$elm$core$Task$andThen,
						function (_v5) {
							return kills;
						},
						$elm$core$Process$kill(id)));
			});
		var newTaggers = A3($elm$core$List$foldl, $elm$time$Time$addMySub, $elm$core$Dict$empty, subs);
		var leftStep = F3(
			function (interval, taggers, _v4) {
				var spawns = _v4.a;
				var existing = _v4.b;
				var kills = _v4.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, interval, spawns),
					existing,
					kills);
			});
		var bothStep = F4(
			function (interval, taggers, id, _v3) {
				var spawns = _v3.a;
				var existing = _v3.b;
				var kills = _v3.c;
				return _Utils_Tuple3(
					spawns,
					A3($elm$core$Dict$insert, interval, id, existing),
					kills);
			});
		var _v1 = A6(
			$elm$core$Dict$merge,
			leftStep,
			bothStep,
			rightStep,
			newTaggers,
			processes,
			_Utils_Tuple3(
				_List_Nil,
				$elm$core$Dict$empty,
				$elm$core$Task$succeed(_Utils_Tuple0)));
		var spawnList = _v1.a;
		var existingDict = _v1.b;
		var killTask = _v1.c;
		return A2(
			$elm$core$Task$andThen,
			function (newProcesses) {
				return $elm$core$Task$succeed(
					A2($elm$time$Time$State, newTaggers, newProcesses));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$time$Time$spawnHelp, router, spawnList, existingDict);
				},
				killTask));
	});
var $elm$time$Time$onSelfMsg = F3(
	function (router, interval, state) {
		var _v0 = A2($elm$core$Dict$get, interval, state.taggers);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Task$succeed(state);
		} else {
			var taggers = _v0.a;
			var tellTaggers = function (time) {
				return $elm$core$Task$sequence(
					A2(
						$elm$core$List$map,
						function (tagger) {
							return A2(
								$elm$core$Platform$sendToApp,
								router,
								tagger(time));
						},
						taggers));
			};
			return A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$succeed(state);
				},
				A2($elm$core$Task$andThen, tellTaggers, $elm$time$Time$now));
		}
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$time$Time$subMap = F2(
	function (f, _v0) {
		var interval = _v0.a;
		var tagger = _v0.b;
		return A2(
			$elm$time$Time$Every,
			interval,
			A2($elm$core$Basics$composeL, f, tagger));
	});
_Platform_effectManagers['Time'] = _Platform_createManager($elm$time$Time$init, $elm$time$Time$onEffects, $elm$time$Time$onSelfMsg, 0, $elm$time$Time$subMap);
var $elm$time$Time$subscription = _Platform_leaf('Time');
var $elm$time$Time$every = F2(
	function (interval, tagger) {
		return $elm$time$Time$subscription(
			A2($elm$time$Time$Every, interval, tagger));
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$subscriptions = function (model) {
	return model.running ? A2(
		$elm$time$Time$every,
		2,
		$elm$core$Basics$always($author$project$Main$Frame)) : $elm$core$Platform$Sub$none;
};
var $author$project$Main$GotBytes = function (a) {
	return {$: 'GotBytes', a: a};
};
var $author$project$Main$UploadedFile = function (a) {
	return {$: 'UploadedFile', a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $avh4$elm_color$Color$RgbaSpace = F4(
	function (a, b, c, d) {
		return {$: 'RgbaSpace', a: a, b: b, c: c, d: d};
	});
var $avh4$elm_color$Color$rgba = F4(
	function (r, g, b, a) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, a);
	});
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $justgook$elm_image$Image$Color$int32ToColor = function (_int) {
	var r = 255 & (_int >>> 24);
	var g = 255 & (_int >> 16);
	var b = 255 & (_int >> 8);
	var a = 255 & _int;
	return A4($avh4$elm_color$Color$rgba, r / 255, g / 255, b / 255, a / 255);
};
var $justgook$elm_image$Image$Internal$ImageData$toList2d = function (info) {
	toList2d:
	while (true) {
		if (info.$ === 'ImageRaw') {
			var arr = info.b;
			return A3(
				$elm$core$Array$foldr,
				F2(
					function (line, acc1) {
						return function (l) {
							return A2($elm$core$List$cons, l, acc1);
						}(
							A3(
								$elm$core$Array$foldr,
								F2(
									function (px, acc2) {
										return A2($elm$core$List$cons, px, acc2);
									}),
								_List_Nil,
								line));
					}),
				_List_Nil,
				arr);
		} else {
			var meta = info.a;
			var fn = info.b;
			var _v1 = fn(meta);
			if (_v1.$ === 'Lazy') {
				return _List_Nil;
			} else {
				var newData = _v1;
				var $temp$info = newData;
				info = $temp$info;
				continue toList2d;
			}
		}
	}
};
var $justgook$elm_image$Image$Color$toList2d = A2(
	$elm$core$Basics$composeR,
	$justgook$elm_image$Image$Internal$ImageData$toList2d,
	$elm$core$List$map(
		$elm$core$List$map($justgook$elm_image$Image$Color$int32ToColor)));
var $author$project$Main$convertImage = function (image) {
	return $elm$core$List$concat(
		A2(
			$elm$core$List$indexedMap,
			F2(
				function (y, list) {
					return A2(
						$elm$core$List$indexedMap,
						F2(
							function (x, color) {
								return _Utils_Tuple2(
									_Utils_Tuple2(x, y),
									color);
							}),
						list);
				}),
			$justgook$elm_image$Image$Color$toList2d(image)));
};
var $elm$bytes$Bytes$Encode$getWidth = function (builder) {
	switch (builder.$) {
		case 'I8':
			return 1;
		case 'I16':
			return 2;
		case 'I32':
			return 4;
		case 'U8':
			return 1;
		case 'U16':
			return 2;
		case 'U32':
			return 4;
		case 'F32':
			return 4;
		case 'F64':
			return 8;
		case 'Seq':
			var w = builder.a;
			return w;
		case 'Utf8':
			var w = builder.a;
			return w;
		default:
			var bs = builder.a;
			return _Bytes_width(bs);
	}
};
var $elm$bytes$Bytes$LE = {$: 'LE'};
var $elm$bytes$Bytes$Encode$write = F3(
	function (builder, mb, offset) {
		switch (builder.$) {
			case 'I8':
				var n = builder.a;
				return A3(_Bytes_write_i8, mb, offset, n);
			case 'I16':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_i16,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'I32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_i32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'U8':
				var n = builder.a;
				return A3(_Bytes_write_u8, mb, offset, n);
			case 'U16':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_u16,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'U32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_u32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'F32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_f32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'F64':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_f64,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'Seq':
				var bs = builder.b;
				return A3($elm$bytes$Bytes$Encode$writeSequence, bs, mb, offset);
			case 'Utf8':
				var s = builder.b;
				return A3(_Bytes_write_string, mb, offset, s);
			default:
				var bs = builder.a;
				return A3(_Bytes_write_bytes, mb, offset, bs);
		}
	});
var $elm$bytes$Bytes$Encode$writeSequence = F3(
	function (builders, mb, offset) {
		writeSequence:
		while (true) {
			if (!builders.b) {
				return offset;
			} else {
				var b = builders.a;
				var bs = builders.b;
				var $temp$builders = bs,
					$temp$mb = mb,
					$temp$offset = A3($elm$bytes$Bytes$Encode$write, b, mb, offset);
				builders = $temp$builders;
				mb = $temp$mb;
				offset = $temp$offset;
				continue writeSequence;
			}
		}
	});
var $elm$bytes$Bytes$Decode$decode = F2(
	function (_v0, bs) {
		var decoder = _v0.a;
		return A2(_Bytes_decode, decoder, bs);
	});
var $justgook$elm_image$Image$Internal$Meta$Bmp = function (a) {
	return {$: 'Bmp', a: a};
};
var $justgook$elm_image$Image$Internal$ImageData$ImageRaw = F2(
	function (a, b) {
		return {$: 'ImageRaw', a: a, b: b};
	});
var $justgook$elm_image$Image$Internal$ImageData$Lazy = F2(
	function (a, b) {
		return {$: 'Lazy', a: a, b: b};
	});
var $elm$bytes$Bytes$Decode$Decoder = function (a) {
	return {$: 'Decoder', a: a};
};
var $elm$bytes$Bytes$Decode$andThen = F2(
	function (callback, _v0) {
		var decodeA = _v0.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v1 = A2(decodeA, bites, offset);
					var newOffset = _v1.a;
					var a = _v1.b;
					var _v2 = callback(a);
					var decodeB = _v2.a;
					return A2(decodeB, bites, newOffset);
				}));
	});
var $elm$bytes$Bytes$Decode$map2 = F3(
	function (func, _v0, _v1) {
		var decodeA = _v0.a;
		var decodeB = _v1.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v2 = A2(decodeA, bites, offset);
					var aOffset = _v2.a;
					var a = _v2.b;
					var _v3 = A2(decodeB, bites, aOffset);
					var bOffset = _v3.a;
					var b = _v3.b;
					return _Utils_Tuple2(
						bOffset,
						A2(func, a, b));
				}));
	});
var $justgook$elm_image$Image$Internal$Decode$andMap = F2(
	function (argument, _function) {
		return A3($elm$bytes$Bytes$Decode$map2, $elm$core$Basics$apL, _function, argument);
	});
var $justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel16 = {$: 'BmpBitsPerPixel16'};
var $justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel24 = {$: 'BmpBitsPerPixel24'};
var $justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel32 = {$: 'BmpBitsPerPixel32'};
var $justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel8 = {$: 'BmpBitsPerPixel8'};
var $elm$bytes$Bytes$Decode$fail = $elm$bytes$Bytes$Decode$Decoder(_Bytes_decodeFailure);
var $elm$bytes$Bytes$Decode$succeed = function (a) {
	return $elm$bytes$Bytes$Decode$Decoder(
		F2(
			function (_v0, offset) {
				return _Utils_Tuple2(offset, a);
			}));
};
var $elm$bytes$Bytes$Decode$unsignedInt16 = function (endianness) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_u16(
			_Utils_eq(endianness, $elm$bytes$Bytes$LE)));
};
var $justgook$elm_image$Image$Internal$BMP$bitsPerPixelDecode = A2(
	$elm$bytes$Bytes$Decode$andThen,
	function (a) {
		switch (a) {
			case 8:
				return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel8);
			case 16:
				return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel16);
			case 24:
				return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel24);
			case 32:
				return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BmpBitsPerPixel32);
			default:
				return $elm$bytes$Bytes$Decode$fail;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE));
var $elm$bytes$Bytes$Decode$unsignedInt32 = function (endianness) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_u32(
			_Utils_eq(endianness, $elm$bytes$Bytes$LE)));
};
var $justgook$elm_image$Image$Internal$BMP$decodeInfo = A2(
	$justgook$elm_image$Image$Internal$Decode$andMap,
	$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
	A2(
		$justgook$elm_image$Image$Internal$Decode$andMap,
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
		A2(
			$justgook$elm_image$Image$Internal$Decode$andMap,
			$justgook$elm_image$Image$Internal$BMP$bitsPerPixelDecode,
			A2(
				$justgook$elm_image$Image$Internal$Decode$andMap,
				$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
				A2(
					$justgook$elm_image$Image$Internal$Decode$andMap,
					$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
					A2(
						$justgook$elm_image$Image$Internal$Decode$andMap,
						$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
						A2(
							$justgook$elm_image$Image$Internal$Decode$andMap,
							$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
							A2(
								$justgook$elm_image$Image$Internal$Decode$andMap,
								$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
								A2(
									$justgook$elm_image$Image$Internal$Decode$andMap,
									$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
									A2(
										$justgook$elm_image$Image$Internal$Decode$andMap,
										$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE),
										$elm$bytes$Bytes$Decode$succeed(
											function (fileSize) {
												return function (_v0) {
													return function (pixelStart) {
														return function (dibHeader) {
															return function (width) {
																return function (height) {
																	return function (color_planes) {
																		return function (bitsPerPixel) {
																			return function (compression) {
																				return function (dataSize) {
																					return {bitsPerPixel: bitsPerPixel, color_planes: color_planes, compression: compression, dataSize: dataSize, dibHeader: dibHeader, fileSize: fileSize, height: height, pixelStart: pixelStart, width: width};
																				};
																			};
																		};
																	};
																};
															};
														};
													};
												};
											})))))))))));
var $elm$bytes$Bytes$Decode$Done = function (a) {
	return {$: 'Done', a: a};
};
var $elm$bytes$Bytes$Decode$Loop = function (a) {
	return {$: 'Loop', a: a};
};
var $elm$bytes$Bytes$Decode$map = F2(
	function (func, _v0) {
		var decodeA = _v0.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v1 = A2(decodeA, bites, offset);
					var aOffset = _v1.a;
					var a = _v1.b;
					return _Utils_Tuple2(
						aOffset,
						func(a));
				}));
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$push = F2(
	function (a, array) {
		var tail = array.d;
		return A2(
			$elm$core$Array$unsafeReplaceTail,
			A2($elm$core$Elm$JsArray$push, a, tail),
			array);
	});
var $justgook$elm_image$Image$Internal$Decode$arrayStep = F2(
	function (decoder, _v0) {
		var n = _v0.a;
		var xs = _v0.b;
		return (n <= 0) ? $elm$bytes$Bytes$Decode$succeed(
			$elm$bytes$Bytes$Decode$Done(xs)) : A2(
			$elm$bytes$Bytes$Decode$map,
			function (x) {
				return $elm$bytes$Bytes$Decode$Loop(
					_Utils_Tuple2(
						n - 1,
						A2($elm$core$Array$push, x, xs)));
			},
			decoder);
	});
var $elm$bytes$Bytes$Decode$loopHelp = F4(
	function (state, callback, bites, offset) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var decoder = _v0.a;
			var _v1 = A2(decoder, bites, offset);
			var newOffset = _v1.a;
			var step = _v1.b;
			if (step.$ === 'Loop') {
				var newState = step.a;
				var $temp$state = newState,
					$temp$callback = callback,
					$temp$bites = bites,
					$temp$offset = newOffset;
				state = $temp$state;
				callback = $temp$callback;
				bites = $temp$bites;
				offset = $temp$offset;
				continue loopHelp;
			} else {
				var result = step.a;
				return _Utils_Tuple2(newOffset, result);
			}
		}
	});
var $elm$bytes$Bytes$Decode$loop = F2(
	function (state, callback) {
		return $elm$bytes$Bytes$Decode$Decoder(
			A2($elm$bytes$Bytes$Decode$loopHelp, state, callback));
	});
var $justgook$elm_image$Image$Internal$Decode$array = F2(
	function (count, decoder) {
		return A2(
			$elm$bytes$Bytes$Decode$loop,
			_Utils_Tuple2(count, $elm$core$Array$empty),
			$justgook$elm_image$Image$Internal$Decode$arrayStep(decoder));
	});
var $elm$bytes$Bytes$Decode$bytes = function (n) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_bytes(n));
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $justgook$elm_image$Image$Internal$Decode$listStep = F2(
	function (decoder, _v0) {
		var n = _v0.a;
		var xs = _v0.b;
		return (n <= 0) ? $elm$bytes$Bytes$Decode$succeed(
			$elm$bytes$Bytes$Decode$Done(xs)) : A2(
			$elm$bytes$Bytes$Decode$map,
			function (x) {
				return $elm$bytes$Bytes$Decode$Loop(
					_Utils_Tuple2(
						n - 1,
						A2($elm$core$List$cons, x, xs)));
			},
			decoder);
	});
var $justgook$elm_image$Image$Internal$Decode$listR = F2(
	function (count, decoder) {
		return A2(
			$elm$bytes$Bytes$Decode$loop,
			_Utils_Tuple2(count, _List_Nil),
			$justgook$elm_image$Image$Internal$Decode$listStep(decoder));
	});
var $justgook$elm_image$Image$Internal$BMP$decode16 = function (info) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Array$fromList,
			$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
				$justgook$elm_image$Image$Internal$Meta$Bmp(info))),
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				return A2(
					$justgook$elm_image$Image$Internal$Decode$listR,
					info.height,
					A2(
						$justgook$elm_image$Image$Internal$Decode$array,
						info.width,
						$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE)));
			},
			$elm$bytes$Bytes$Decode$bytes(info.pixelStart)));
};
var $elm$bytes$Bytes$Decode$unsignedInt8 = $elm$bytes$Bytes$Decode$Decoder(_Bytes_read_u8);
var $justgook$elm_image$Image$Internal$Decode$unsignedInt24 = function (endianness) {
	if (endianness.$ === 'LE') {
		return A3(
			$elm$bytes$Bytes$Decode$map2,
			F2(
				function (b2, b1) {
					return (b1 << 16) | b2;
				}),
			$elm$bytes$Bytes$Decode$unsignedInt16(endianness),
			$elm$bytes$Bytes$Decode$unsignedInt8);
	} else {
		return A3(
			$elm$bytes$Bytes$Decode$map2,
			F2(
				function (b1, b2) {
					return (b1 << 8) | b2;
				}),
			$elm$bytes$Bytes$Decode$unsignedInt16(endianness),
			$elm$bytes$Bytes$Decode$unsignedInt8);
	}
};
var $justgook$elm_image$Image$Internal$BMP$decode24 = function (info) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Array$fromList,
			$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
				$justgook$elm_image$Image$Internal$Meta$Bmp(info))),
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				return A2(
					$justgook$elm_image$Image$Internal$Decode$listR,
					info.height,
					A2(
						$justgook$elm_image$Image$Internal$Decode$array,
						info.width,
						$justgook$elm_image$Image$Internal$Decode$unsignedInt24($elm$bytes$Bytes$LE)));
			},
			$elm$bytes$Bytes$Decode$bytes(info.pixelStart)));
};
var $justgook$elm_image$Image$Internal$BMP$decode32 = function (info) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Array$fromList,
			$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
				$justgook$elm_image$Image$Internal$Meta$Bmp(info))),
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				return A2(
					$justgook$elm_image$Image$Internal$Decode$listR,
					info.height,
					A2(
						$justgook$elm_image$Image$Internal$Decode$array,
						info.width,
						$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE)));
			},
			$elm$bytes$Bytes$Decode$bytes(info.pixelStart)));
};
var $justgook$elm_image$Image$Internal$BMP$decode8 = function (info) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Array$fromList,
			$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
				$justgook$elm_image$Image$Internal$Meta$Bmp(info))),
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				return A2(
					$justgook$elm_image$Image$Internal$Decode$listR,
					info.height,
					A2($justgook$elm_image$Image$Internal$Decode$array, info.width, $elm$bytes$Bytes$Decode$unsignedInt8));
			},
			$elm$bytes$Bytes$Decode$bytes(info.pixelStart)));
};
var $justgook$elm_image$Image$Internal$BMP$imageDecoder = function (info) {
	var _v0 = info.bitsPerPixel;
	switch (_v0.$) {
		case 'BmpBitsPerPixel32':
			return $justgook$elm_image$Image$Internal$BMP$decode32(info);
		case 'BmpBitsPerPixel24':
			return $justgook$elm_image$Image$Internal$BMP$decode24(info);
		case 'BmpBitsPerPixel16':
			return $justgook$elm_image$Image$Internal$BMP$decode16(info);
		default:
			return $justgook$elm_image$Image$Internal$BMP$decode8(info);
	}
};
var $elm$bytes$Bytes$Decode$string = function (n) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_string(n));
};
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $justgook$elm_image$Image$Internal$BMP$decoder = function (bytes) {
	return A2(
		$elm$bytes$Bytes$Decode$andThen,
		function (bm) {
			return (bm === 'BM') ? A2(
				$elm$bytes$Bytes$Decode$map,
				function (bmInfo) {
					return A2(
						$justgook$elm_image$Image$Internal$ImageData$Lazy,
						$justgook$elm_image$Image$Internal$Meta$Bmp(bmInfo),
						function (info) {
							return A2(
								$elm$core$Maybe$withDefault,
								A2($justgook$elm_image$Image$Internal$ImageData$ImageRaw, info, $elm$core$Array$empty),
								A2(
									$elm$bytes$Bytes$Decode$decode,
									$justgook$elm_image$Image$Internal$BMP$imageDecoder(bmInfo),
									bytes));
						});
				},
				$justgook$elm_image$Image$Internal$BMP$decodeInfo) : $elm$bytes$Bytes$Decode$fail;
		},
		$elm$bytes$Bytes$Decode$string(2));
};
var $justgook$elm_image$Image$Internal$BMP$decode = function (bytes) {
	return A2(
		$elm$bytes$Bytes$Decode$decode,
		$justgook$elm_image$Image$Internal$BMP$decoder(bytes),
		bytes);
};
var $justgook$elm_image$Image$Internal$Meta$Gif = function (a) {
	return {$: 'Gif', a: a};
};
var $elm$bytes$Bytes$Decode$map4 = F5(
	function (func, _v0, _v1, _v2, _v3) {
		var decodeA = _v0.a;
		var decodeB = _v1.a;
		var decodeC = _v2.a;
		var decodeD = _v3.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v4 = A2(decodeA, bites, offset);
					var aOffset = _v4.a;
					var a = _v4.b;
					var _v5 = A2(decodeB, bites, aOffset);
					var bOffset = _v5.a;
					var b = _v5.b;
					var _v6 = A2(decodeC, bites, bOffset);
					var cOffset = _v6.a;
					var c = _v6.b;
					var _v7 = A2(decodeD, bites, cOffset);
					var dOffset = _v7.a;
					var d = _v7.b;
					return _Utils_Tuple2(
						dOffset,
						A4(func, a, b, c, d));
				}));
	});
var $justgook$elm_image$Image$Internal$GIF$decodeInfo = A2(
	$elm$bytes$Bytes$Decode$andThen,
	function (info) {
		return A2(
			$elm$bytes$Bytes$Decode$map,
			function (_v0) {
				return info;
			},
			$elm$bytes$Bytes$Decode$unsignedInt8);
	},
	A5(
		$elm$bytes$Bytes$Decode$map4,
		F4(
			function (width, height, mCrPixel, background) {
				var pixel = 7 & mCrPixel;
				var m = mCrPixel >> 7;
				var cr = 7 & (mCrPixel >> 4);
				return {background: background, bits: pixel + 1, cr: cr + 1, height: height, m: m, width: width};
			}),
		$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
		$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8));
var $elm$bytes$Bytes$Decode$map5 = F6(
	function (func, _v0, _v1, _v2, _v3, _v4) {
		var decodeA = _v0.a;
		var decodeB = _v1.a;
		var decodeC = _v2.a;
		var decodeD = _v3.a;
		var decodeE = _v4.a;
		return $elm$bytes$Bytes$Decode$Decoder(
			F2(
				function (bites, offset) {
					var _v5 = A2(decodeA, bites, offset);
					var aOffset = _v5.a;
					var a = _v5.b;
					var _v6 = A2(decodeB, bites, aOffset);
					var bOffset = _v6.a;
					var b = _v6.b;
					var _v7 = A2(decodeC, bites, bOffset);
					var cOffset = _v7.a;
					var c = _v7.b;
					var _v8 = A2(decodeD, bites, cOffset);
					var dOffset = _v8.a;
					var d = _v8.b;
					var _v9 = A2(decodeE, bites, dOffset);
					var eOffset = _v9.a;
					var e = _v9.b;
					return _Utils_Tuple2(
						eOffset,
						A5(func, a, b, c, d, e));
				}));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $justgook$elm_image$Image$Internal$GIF$decodeControlExtension = A6(
	$elm$bytes$Bytes$Decode$map5,
	F5(
		function (_v0, packedField, delay, colorIndex, _v1) {
			var transColor = (!(!(1 & packedField))) ? $elm$core$Maybe$Just(colorIndex) : $elm$core$Maybe$Nothing;
			var disposal = (28 & packedField) >> 2;
			return _Utils_Tuple3(transColor, delay, disposal);
		}),
	$elm$bytes$Bytes$Decode$unsignedInt8,
	$elm$bytes$Bytes$Decode$unsignedInt8,
	$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
	$elm$bytes$Bytes$Decode$unsignedInt8,
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $justgook$elm_image$Image$Internal$GIF$decodeExtensionBlocks = function () {
	var bodyLoop = function (acc) {
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (count) {
				return (count > 0) ? A2(
					$elm$bytes$Bytes$Decode$andThen,
					function (a) {
						return bodyLoop(
							A2($elm$core$List$cons, a, acc));
					},
					$elm$bytes$Bytes$Decode$bytes(count)) : $elm$bytes$Bytes$Decode$succeed(acc);
			},
			$elm$bytes$Bytes$Decode$unsignedInt8);
	};
	return A2(
		$elm$bytes$Bytes$Decode$map,
		function (ext) {
			return {body: ext};
		},
		bodyLoop(_List_Nil));
}();
var $justgook$elm_image$Image$Internal$BitReader$BitReader = function (a) {
	return {$: 'BitReader', a: a};
};
var $justgook$elm_image$Image$Internal$BitReader$andThen = F2(
	function (f, _v0) {
		var g = _v0.a;
		return $justgook$elm_image$Image$Internal$BitReader$BitReader(
			function (s) {
				var _v1 = g(s);
				if (_v1.$ === 'Ok') {
					var _v2 = _v1.a;
					var value = _v2.a;
					var newState = _v2.b;
					var _v3 = f(value);
					var h = _v3.a;
					return h(newState);
				} else {
					var e = _v1.a;
					return $elm$core$Result$Err(e);
				}
			});
	});
var $justgook$elm_image$Image$Internal$Decode$andThen2 = F3(
	function (fn, d1, d2) {
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (a) {
				return A2(
					$elm$bytes$Bytes$Decode$andThen,
					fn(a),
					d2);
			},
			d1);
	});
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = $elm$core$Elm$JsArray$length(toAppend);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2($elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3($elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2($elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$builderFromArray = function (_v0) {
	var len = _v0.a;
	var tree = _v0.c;
	var tail = _v0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2($elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3($elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var $elm$core$Array$append = F2(
	function (a, _v0) {
		var aTail = a.d;
		var bLen = _v0.a;
		var bTree = _v0.c;
		var bTail = _v0.d;
		if (_Utils_cmp(bLen, $elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				$elm$core$Array$appendHelpTree,
				bTail,
				A3($elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				$elm$core$Array$builderToArray,
				true,
				A2(
					$elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						$elm$core$Elm$JsArray$foldl,
						foldHelper,
						$elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $justgook$elm_image$Image$Internal$BitReader$map = F2(
	function (f, _v0) {
		var g = _v0.a;
		return $justgook$elm_image$Image$Internal$BitReader$BitReader(
			function (s) {
				var _v1 = g(s);
				if (_v1.$ === 'Ok') {
					var _v2 = _v1.a;
					var value = _v2.a;
					var newState = _v2.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(
							f(value),
							newState));
				} else {
					var e = _v1.a;
					return $elm$core$Result$Err(e);
				}
			});
	});
var $justgook$elm_image$Image$Internal$BitReader$moveFromReserve = F2(
	function (nbits, state) {
		var masked = (nbits === 32) ? (state.reserve << state.bitsAvailable) : ((((1 << nbits) - 1) & state.reserve) << state.bitsAvailable);
		return {bitsAvailable: state.bitsAvailable + nbits, buffer: state.buffer, reserve: state.reserve >>> nbits, reserveAvailable: state.reserveAvailable - nbits, tag: masked | state.tag};
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm$bytes$Bytes$width = _Bytes_width;
var $justgook$elm_image$Image$Internal$BitReader$runDecoder = F3(
	function (width, valueDecoder, state) {
		var decoder = A3(
			$elm$bytes$Bytes$Decode$map2,
			$elm$core$Tuple$pair,
			valueDecoder,
			$elm$bytes$Bytes$Decode$bytes(
				$elm$bytes$Bytes$width(state.buffer) - width));
		var _v0 = A2($elm$bytes$Bytes$Decode$decode, decoder, state.buffer);
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return $elm$core$Result$Ok(value);
		} else {
			return $elm$core$Result$Err('BitReader.runDecoder: Unexpected end of Bytes');
		}
	});
var $justgook$elm_image$Image$Internal$BitReader$unsignedInt24 = function (endianness) {
	if (endianness.$ === 'LE') {
		return A3(
			$elm$bytes$Bytes$Decode$map2,
			F2(
				function (b2, b1) {
					return (b1 << 16) | b2;
				}),
			$elm$bytes$Bytes$Decode$unsignedInt16(endianness),
			$elm$bytes$Bytes$Decode$unsignedInt8);
	} else {
		return A3(
			$elm$bytes$Bytes$Decode$map2,
			F2(
				function (b1, b2) {
					return (b1 << 16) | b2;
				}),
			$elm$bytes$Bytes$Decode$unsignedInt16(endianness),
			$elm$bytes$Bytes$Decode$unsignedInt8);
	}
};
var $justgook$elm_image$Image$Internal$BitReader$readMoreBits = function (state) {
	readMoreBits:
	while (true) {
		var freeSpaceOnTag = 32 - state.bitsAvailable;
		if ((_Utils_cmp(freeSpaceOnTag, state.reserveAvailable) < 1) && (state.reserveAvailable > 0)) {
			return $elm$core$Result$Ok(
				A2($justgook$elm_image$Image$Internal$BitReader$moveFromReserve, freeSpaceOnTag, state));
		} else {
			if (!$elm$bytes$Bytes$width(state.buffer)) {
				return $elm$core$Result$Ok(
					A2($justgook$elm_image$Image$Internal$BitReader$moveFromReserve, state.reserveAvailable, state));
			} else {
				var state1 = A2($justgook$elm_image$Image$Internal$BitReader$moveFromReserve, state.reserveAvailable, state);
				var _v0 = function () {
					var _v1 = $elm$bytes$Bytes$width(state.buffer);
					switch (_v1) {
						case 0:
							return _Utils_Tuple3(
								0,
								0,
								$elm$bytes$Bytes$Decode$succeed(0));
						case 1:
							return _Utils_Tuple3(1, 8, $elm$bytes$Bytes$Decode$unsignedInt8);
						case 2:
							return _Utils_Tuple3(
								2,
								16,
								$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE));
						case 3:
							return _Utils_Tuple3(
								3,
								24,
								$justgook$elm_image$Image$Internal$BitReader$unsignedInt24($elm$bytes$Bytes$LE));
						default:
							return _Utils_Tuple3(
								4,
								32,
								$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE));
					}
				}();
				var width = _v0.a;
				var additionallyAvailable = _v0.b;
				var decoder = _v0.c;
				var _v2 = A3($justgook$elm_image$Image$Internal$BitReader$runDecoder, width, decoder, state1);
				if (_v2.$ === 'Err') {
					var e = _v2.a;
					return $elm$core$Result$Err(e);
				} else {
					var _v3 = _v2.a;
					var newReserve = _v3.a;
					var newBuffer = _v3.b;
					var $temp$state = {bitsAvailable: state1.bitsAvailable, buffer: newBuffer, reserve: newReserve, reserveAvailable: additionallyAvailable, tag: state1.tag};
					state = $temp$state;
					continue readMoreBits;
				}
			}
		}
	}
};
var $justgook$elm_image$Image$Internal$BitReader$readBits = F2(
	function (numberOfBits, base) {
		return $justgook$elm_image$Image$Internal$BitReader$BitReader(
			function (state) {
				if (!numberOfBits) {
					return $elm$core$Result$Ok(
						_Utils_Tuple2(base, state));
				} else {
					var _v0 = (_Utils_cmp(state.bitsAvailable, numberOfBits) < 0) ? $justgook$elm_image$Image$Internal$BitReader$readMoreBits(state) : $elm$core$Result$Ok(state);
					if (_v0.$ === 'Err') {
						var e = _v0.a;
						return $elm$core$Result$Err(e);
					} else {
						var d = _v0.a;
						var val = d.tag & (65535 >>> (16 - numberOfBits));
						var newTag = d.tag >>> numberOfBits;
						return $elm$core$Result$Ok(
							_Utils_Tuple2(
								val + base,
								{bitsAvailable: d.bitsAvailable - numberOfBits, buffer: d.buffer, reserve: d.reserve, reserveAvailable: d.reserveAvailable, tag: newTag}));
					}
				}
			});
	});
var $elm$core$Dict$sizeHelp = F2(
	function (n, dict) {
		sizeHelp:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return n;
			} else {
				var left = dict.d;
				var right = dict.e;
				var $temp$n = A2($elm$core$Dict$sizeHelp, n + 1, right),
					$temp$dict = left;
				n = $temp$n;
				dict = $temp$dict;
				continue sizeHelp;
			}
		}
	});
var $elm$core$Dict$size = function (dict) {
	return A2($elm$core$Dict$sizeHelp, 0, dict);
};
var $justgook$elm_image$Image$Internal$Lzw$bitDecoder = function (acc) {
	var eoi = acc.eoi;
	var table = acc.table;
	var indexStream = acc.indexStream;
	var read = acc.read;
	var indexBuffer = acc.indexBuffer;
	return A2(
		$justgook$elm_image$Image$Internal$BitReader$map,
		function (code) {
			if ((!_Utils_eq(code, eoi)) && ((A2($elm$core$Basics$pow, 2, read) - 1) < 4095)) {
				var _v0 = A2($elm$core$Dict$get, code, table);
				if (_v0.$ === 'Just') {
					var v = _v0.a;
					var tableKey = $elm$core$Dict$size(table);
					var newRead = (_Utils_cmp(
						tableKey,
						A2($elm$core$Basics$pow, 2, read) - 1) > -1) ? (read + 1) : read;
					var k = A2(
						$elm$core$Maybe$withDefault,
						-404,
						$elm$core$List$head(v));
					var tableValue = _Utils_ap(
						indexBuffer,
						_List_fromArray(
							[k]));
					return $elm$bytes$Bytes$Decode$Loop(
						_Utils_update(
							acc,
							{
								indexBuffer: v,
								indexStream: A2(
									$elm$core$Array$append,
									indexStream,
									$elm$core$Array$fromList(v)),
								read: newRead,
								table: A3($elm$core$Dict$insert, tableKey, tableValue, table)
							}));
				} else {
					var newRead = (_Utils_cmp(
						code,
						A2($elm$core$Basics$pow, 2, read) - 1) > -1) ? (read + 1) : read;
					var k = A2(
						$elm$core$Maybe$withDefault,
						-404,
						$elm$core$List$head(indexBuffer));
					var tableValue = _Utils_ap(
						indexBuffer,
						_List_fromArray(
							[k]));
					return $elm$bytes$Bytes$Decode$Loop(
						_Utils_update(
							acc,
							{
								indexBuffer: tableValue,
								indexStream: A2(
									$elm$core$Array$append,
									indexStream,
									$elm$core$Array$fromList(tableValue)),
								read: newRead,
								table: A3($elm$core$Dict$insert, code, tableValue, table)
							}));
				}
			} else {
				return $elm$bytes$Bytes$Decode$Done(indexStream);
			}
		},
		A2($justgook$elm_image$Image$Internal$BitReader$readBits, read, 0));
};
var $justgook$elm_image$Image$Internal$BitReader$decode = F2(
	function (bytes, _v0) {
		var reader = _v0.a;
		var initialState = {bitsAvailable: 0, buffer: bytes, reserve: 0, reserveAvailable: 0, tag: 0};
		var _v1 = reader(initialState);
		if (_v1.$ === 'Ok') {
			var _v2 = _v1.a;
			var value = _v2.a;
			return $elm$core$Result$Ok(value);
		} else {
			var e = _v1.a;
			return $elm$core$Result$Err(e);
		}
	});
var $justgook$elm_image$Image$Internal$BitReader$error = function (e) {
	return $justgook$elm_image$Image$Internal$BitReader$BitReader(
		function (s) {
			return $elm$core$Result$Err(e);
		});
};
var $justgook$elm_image$Image$Internal$Lzw$initDecodeTable = function (lastColorIndex) {
	var eoi = lastColorIndex + 2;
	var cc = lastColorIndex + 1;
	var table = A3(
		$elm$core$Dict$insert,
		eoi,
		_List_fromArray(
			[eoi]),
		A3(
			$elm$core$Dict$insert,
			cc,
			_List_fromArray(
				[cc]),
			A3(
				$elm$core$List$foldl,
				function (k) {
					return A2(
						$elm$core$Dict$insert,
						k,
						_List_fromArray(
							[k]));
				},
				$elm$core$Dict$empty,
				A2($elm$core$List$range, 0, lastColorIndex))));
	return _Utils_Tuple3(cc, eoi, table);
};
var $justgook$elm_image$Image$Internal$BitReader$loopHelp = F3(
	function (accum, callback, state) {
		loopHelp:
		while (true) {
			var _v0 = callback(accum);
			var decoder = _v0.a;
			var _v1 = decoder(state);
			if (_v1.$ === 'Err') {
				var e = _v1.a;
				return $elm$core$Result$Err(e);
			} else {
				if (_v1.a.a.$ === 'Loop') {
					var _v2 = _v1.a;
					var newAccum = _v2.a.a;
					var newState = _v2.b;
					var $temp$accum = newAccum,
						$temp$callback = callback,
						$temp$state = newState;
					accum = $temp$accum;
					callback = $temp$callback;
					state = $temp$state;
					continue loopHelp;
				} else {
					var _v3 = _v1.a;
					var result = _v3.a.a;
					var newState = _v3.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(result, newState));
				}
			}
		}
	});
var $justgook$elm_image$Image$Internal$BitReader$loop = F2(
	function (state, callback) {
		return $justgook$elm_image$Image$Internal$BitReader$BitReader(
			A2($justgook$elm_image$Image$Internal$BitReader$loopHelp, state, callback));
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $justgook$elm_image$Image$Internal$Lzw$decoder = function (colorCountPowerOfTwo) {
	return A3(
		$justgook$elm_image$Image$Internal$Decode$andThen2,
		F2(
			function (firstCodeSize_, count_firstBlock) {
				var firstCodeSize = firstCodeSize_ + 1;
				return A2(
					$elm$bytes$Bytes$Decode$andThen,
					function (bytes) {
						var _v0 = $justgook$elm_image$Image$Internal$Lzw$initDecodeTable(colorCountPowerOfTwo);
						var cc = _v0.a;
						var eoi = _v0.b;
						var table = _v0.c;
						return A2(
							$elm$core$Maybe$withDefault,
							$elm$bytes$Bytes$Decode$fail,
							A2(
								$elm$core$Maybe$map,
								$elm$bytes$Bytes$Decode$succeed,
								$elm$core$Result$toMaybe(
									A2(
										$justgook$elm_image$Image$Internal$BitReader$decode,
										bytes,
										A2(
											$justgook$elm_image$Image$Internal$BitReader$andThen,
											function (reset) {
												return (!_Utils_eq(reset, cc)) ? $justgook$elm_image$Image$Internal$BitReader$error('No reset Bit present') : A2(
													$justgook$elm_image$Image$Internal$BitReader$andThen,
													function (first) {
														var value = A2(
															$elm$core$Maybe$withDefault,
															_List_Nil,
															A2($elm$core$Dict$get, first, table));
														var acc = {
															eoi: eoi,
															indexBuffer: value,
															indexStream: $elm$core$Array$fromList(value),
															read: firstCodeSize,
															table: table
														};
														return A2($justgook$elm_image$Image$Internal$BitReader$loop, acc, $justgook$elm_image$Image$Internal$Lzw$bitDecoder);
													},
													A2($justgook$elm_image$Image$Internal$BitReader$readBits, firstCodeSize, 0));
											},
											A2($justgook$elm_image$Image$Internal$BitReader$readBits, firstCodeSize, 0))))));
					},
					$elm$bytes$Bytes$Decode$bytes(count_firstBlock));
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$GIF$decodeFrames = F2(
	function (info, acc) {
		var ext = acc.a;
		var dsc = acc.b;
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (headSymbol) {
				return (headSymbol === 33) ? A2(
					$elm$bytes$Bytes$Decode$andThen,
					function (extension) {
						switch (extension) {
							case 1:
								return A2(
									$elm$bytes$Bytes$Decode$andThen,
									function (_v1) {
										return A2(
											$justgook$elm_image$Image$Internal$GIF$decodeFrames,
											info,
											_Utils_Tuple2(ext, dsc));
									},
									$justgook$elm_image$Image$Internal$GIF$decodeExtensionBlocks);
							case 255:
								return A2(
									$elm$bytes$Bytes$Decode$andThen,
									function (_v2) {
										return A2(
											$justgook$elm_image$Image$Internal$GIF$decodeFrames,
											info,
											_Utils_Tuple2(ext, dsc));
									},
									$justgook$elm_image$Image$Internal$GIF$decodeExtensionBlocks);
							case 254:
								return A2(
									$elm$bytes$Bytes$Decode$andThen,
									function (_v3) {
										return A2(
											$justgook$elm_image$Image$Internal$GIF$decodeFrames,
											info,
											_Utils_Tuple2(ext, dsc));
									},
									$justgook$elm_image$Image$Internal$GIF$decodeExtensionBlocks);
							case 249:
								return A2(
									$elm$bytes$Bytes$Decode$andThen,
									function (a) {
										return A2(
											$justgook$elm_image$Image$Internal$GIF$decodeFrames,
											info,
											_Utils_Tuple2(a, dsc));
									},
									$justgook$elm_image$Image$Internal$GIF$decodeControlExtension);
							default:
								return $elm$bytes$Bytes$Decode$fail;
						}
					},
					$elm$bytes$Bytes$Decode$unsignedInt8) : ((headSymbol === 44) ? A2(
					$elm$bytes$Bytes$Decode$andThen,
					function (dsc_) {
						return A2(
							$elm$bytes$Bytes$Decode$map,
							function (data) {
								return _Utils_Tuple2(
									ext,
									A2(
										$elm$core$Array$push,
										_Utils_update(
											dsc_,
											{data: data}),
										dsc));
							},
							$justgook$elm_image$Image$Internal$Lzw$decoder(
								A2($elm$core$Basics$pow, 2, info.bits) - 1));
					},
					A6(
						$elm$bytes$Bytes$Decode$map5,
						F5(
							function (left, top, w, h, mipx) {
								return {data: $elm$core$Array$empty, h: h, i: 1 & (mipx >> 6), left: left, m: mipx >> 7, px: 7 & mipx, top: top, w: w};
							}),
						$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
						$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
						$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
						$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
						$elm$bytes$Bytes$Decode$unsignedInt8)) : $elm$bytes$Bytes$Decode$succeed(acc));
			},
			$elm$bytes$Bytes$Decode$unsignedInt8);
	});
var $elm$bytes$Bytes$BE = {$: 'BE'};
var $justgook$elm_image$Image$Internal$GIF$decodePalette = function (bits) {
	return A2(
		$justgook$elm_image$Image$Internal$Decode$array,
		A2($elm$core$Basics$pow, 2, bits),
		A2(
			$elm$bytes$Bytes$Decode$map,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Bitwise$shiftLeftBy(8),
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Basics$add(255),
					$elm$core$Bitwise$shiftRightZfBy(0))),
			$justgook$elm_image$Image$Internal$Decode$unsignedInt24($elm$bytes$Bytes$BE)));
};
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Elm$JsArray$map = _JsArray_map;
var $elm$core$Array$map = F2(
	function (func, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return $elm$core$Array$SubTree(
					A2($elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return $elm$core$Array$Leaf(
					A2($elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2($elm$core$Elm$JsArray$map, helper, tree),
			A2($elm$core$Elm$JsArray$map, func, tail));
	});
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (_v0.$ === 'SubTree') {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $justgook$elm_image$Image$Internal$GIF$decodeLazyImage = function (info) {
	return A2(
		$elm$bytes$Bytes$Decode$andThen,
		function (_v1) {
			var background = _v1.a.background;
			var palette = _v1.a.palette;
			var _v2 = _v1.b;
			var transColor = _v2.a;
			var dsc = _v1.c;
			var updatedPalette = A2(
				$elm$core$Maybe$withDefault,
				palette,
				A2(
					$elm$core$Maybe$map,
					function (c) {
						return A3($elm$core$Array$set, c, 0, palette);
					},
					transColor));
			var _v3 = A2($elm$core$Array$get, 0, dsc);
			if (_v3.$ === 'Just') {
				var data = _v3.a.data;
				return $elm$bytes$Bytes$Decode$succeed(
					A2(
						$elm$core$Array$map,
						function (i) {
							return A2(
								$elm$core$Maybe$withDefault,
								0,
								A2($elm$core$Array$get, i, updatedPalette));
						},
						data));
			} else {
				return $elm$bytes$Bytes$Decode$fail;
			}
		},
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (info_) {
				return A2(
					$elm$bytes$Bytes$Decode$map,
					function (_v0) {
						var ext = _v0.a;
						var dsc = _v0.b;
						return _Utils_Tuple3(info_, ext, dsc);
					},
					A2(
						$justgook$elm_image$Image$Internal$GIF$decodeFrames,
						info_,
						_Utils_Tuple2(
							_Utils_Tuple3($elm$core$Maybe$Nothing, 0, 0),
							$elm$core$Array$empty)));
			},
			A2(
				$elm$bytes$Bytes$Decode$map,
				function (palette) {
					return {background: info.background, bits: info.bits, cr: info.cr, height: info.height, m: info.m, palette: palette, width: info.width};
				},
				$justgook$elm_image$Image$Internal$GIF$decodePalette(info.bits))));
};
var $justgook$elm_image$Image$Internal$GIF$decodeSignature = A2(
	$elm$bytes$Bytes$Decode$andThen,
	function (signature) {
		return (_Utils_eq(
			signature,
			_List_fromArray(
				[97, 55, 56, 70, 73, 71])) || _Utils_eq(
			signature,
			_List_fromArray(
				[97, 57, 56, 70, 73, 71]))) ? $elm$bytes$Bytes$Decode$succeed(_Utils_Tuple0) : $elm$bytes$Bytes$Decode$fail;
	},
	A2($justgook$elm_image$Image$Internal$Decode$listR, 6, $elm$bytes$Bytes$Decode$unsignedInt8));
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$core$Array$sliceLeft = F2(
	function (from, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		if (!from) {
			return array;
		} else {
			if (_Utils_cmp(
				from,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					len - from,
					$elm$core$Array$shiftStep,
					$elm$core$Elm$JsArray$empty,
					A3(
						$elm$core$Elm$JsArray$slice,
						from - $elm$core$Array$tailIndex(len),
						$elm$core$Elm$JsArray$length(tail),
						tail));
			} else {
				var skipNodes = (from / $elm$core$Array$branchFactor) | 0;
				var helper = F2(
					function (node, acc) {
						if (node.$ === 'SubTree') {
							var subTree = node.a;
							return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
						} else {
							var leaf = node.a;
							return A2($elm$core$List$cons, leaf, acc);
						}
					});
				var leafNodes = A3(
					$elm$core$Elm$JsArray$foldr,
					helper,
					_List_fromArray(
						[tail]),
					tree);
				var nodesToInsert = A2($elm$core$List$drop, skipNodes, leafNodes);
				if (!nodesToInsert.b) {
					return $elm$core$Array$empty;
				} else {
					var head = nodesToInsert.a;
					var rest = nodesToInsert.b;
					var firstSlice = from - (skipNodes * $elm$core$Array$branchFactor);
					var initialBuilder = {
						nodeList: _List_Nil,
						nodeListSize: 0,
						tail: A3(
							$elm$core$Elm$JsArray$slice,
							firstSlice,
							$elm$core$Elm$JsArray$length(head),
							head)
					};
					return A2(
						$elm$core$Array$builderToArray,
						true,
						A3($elm$core$List$foldl, $elm$core$Array$appendHelpBuilder, initialBuilder, rest));
				}
			}
		}
	});
var $elm$core$Array$fetchNewTail = F4(
	function (shift, end, treeEnd, tree) {
		fetchNewTail:
		while (true) {
			var pos = $elm$core$Array$bitMask & (treeEnd >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var sub = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$end = end,
					$temp$treeEnd = treeEnd,
					$temp$tree = sub;
				shift = $temp$shift;
				end = $temp$end;
				treeEnd = $temp$treeEnd;
				tree = $temp$tree;
				continue fetchNewTail;
			} else {
				var values = _v0.a;
				return A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, values);
			}
		}
	});
var $elm$core$Array$hoistTree = F3(
	function (oldShift, newShift, tree) {
		hoistTree:
		while (true) {
			if ((_Utils_cmp(oldShift, newShift) < 1) || (!$elm$core$Elm$JsArray$length(tree))) {
				return tree;
			} else {
				var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, 0, tree);
				if (_v0.$ === 'SubTree') {
					var sub = _v0.a;
					var $temp$oldShift = oldShift - $elm$core$Array$shiftStep,
						$temp$newShift = newShift,
						$temp$tree = sub;
					oldShift = $temp$oldShift;
					newShift = $temp$newShift;
					tree = $temp$tree;
					continue hoistTree;
				} else {
					return tree;
				}
			}
		}
	});
var $elm$core$Array$sliceTree = F3(
	function (shift, endIdx, tree) {
		var lastPos = $elm$core$Array$bitMask & (endIdx >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, lastPos, tree);
		if (_v0.$ === 'SubTree') {
			var sub = _v0.a;
			var newSub = A3($elm$core$Array$sliceTree, shift - $elm$core$Array$shiftStep, endIdx, sub);
			return (!$elm$core$Elm$JsArray$length(newSub)) ? A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree) : A3(
				$elm$core$Elm$JsArray$unsafeSet,
				lastPos,
				$elm$core$Array$SubTree(newSub),
				A3($elm$core$Elm$JsArray$slice, 0, lastPos + 1, tree));
		} else {
			return A3($elm$core$Elm$JsArray$slice, 0, lastPos, tree);
		}
	});
var $elm$core$Array$sliceRight = F2(
	function (end, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		if (_Utils_eq(end, len)) {
			return array;
		} else {
			if (_Utils_cmp(
				end,
				$elm$core$Array$tailIndex(len)) > -1) {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					startShift,
					tree,
					A3($elm$core$Elm$JsArray$slice, 0, $elm$core$Array$bitMask & end, tail));
			} else {
				var endIdx = $elm$core$Array$tailIndex(end);
				var depth = $elm$core$Basics$floor(
					A2(
						$elm$core$Basics$logBase,
						$elm$core$Array$branchFactor,
						A2($elm$core$Basics$max, 1, endIdx - 1)));
				var newShift = A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep);
				return A4(
					$elm$core$Array$Array_elm_builtin,
					end,
					newShift,
					A3(
						$elm$core$Array$hoistTree,
						startShift,
						newShift,
						A3($elm$core$Array$sliceTree, startShift, endIdx, tree)),
					A4($elm$core$Array$fetchNewTail, startShift, end, endIdx, tree));
			}
		}
	});
var $elm$core$Array$translateIndex = F2(
	function (index, _v0) {
		var len = _v0.a;
		var posIndex = (index < 0) ? (len + index) : index;
		return (posIndex < 0) ? 0 : ((_Utils_cmp(posIndex, len) > 0) ? len : posIndex);
	});
var $elm$core$Array$slice = F3(
	function (from, to, array) {
		var correctTo = A2($elm$core$Array$translateIndex, to, array);
		var correctFrom = A2($elm$core$Array$translateIndex, from, array);
		return (_Utils_cmp(correctFrom, correctTo) > 0) ? $elm$core$Array$empty : A2(
			$elm$core$Array$sliceLeft,
			correctFrom,
			A2($elm$core$Array$sliceRight, correctTo, array));
	});
var $justgook$elm_image$Image$Internal$Array2d$splitAt = F2(
	function (index, xs) {
		var len = $elm$core$Array$length(xs);
		var _v0 = _Utils_Tuple2(
			index > 0,
			_Utils_cmp(index, len) < 0);
		if (_v0.a) {
			if (_v0.b) {
				return _Utils_Tuple2(
					A3($elm$core$Array$slice, 0, index, xs),
					A3($elm$core$Array$slice, index, len, xs));
			} else {
				return _Utils_Tuple2(xs, $elm$core$Array$empty);
			}
		} else {
			if (_v0.b) {
				return _Utils_Tuple2($elm$core$Array$empty, xs);
			} else {
				return _Utils_Tuple2($elm$core$Array$empty, $elm$core$Array$empty);
			}
		}
	});
var $justgook$elm_image$Image$Internal$Array2d$fromArray_ = F3(
	function (w, arr, acc) {
		fromArray_:
		while (true) {
			if (_Utils_cmp(
				$elm$core$Array$length(arr),
				w) > 0) {
				var _v0 = A2($justgook$elm_image$Image$Internal$Array2d$splitAt, w, arr);
				var a1 = _v0.a;
				var a2 = _v0.b;
				var $temp$w = w,
					$temp$arr = a2,
					$temp$acc = A2($elm$core$Array$push, a1, acc);
				w = $temp$w;
				arr = $temp$arr;
				acc = $temp$acc;
				continue fromArray_;
			} else {
				return A2($elm$core$Array$push, arr, acc);
			}
		}
	});
var $justgook$elm_image$Image$Internal$Array2d$fromArray = F2(
	function (w, arr) {
		return A3($justgook$elm_image$Image$Internal$Array2d$fromArray_, w, arr, $elm$core$Array$empty);
	});
var $justgook$elm_image$Image$Internal$GIF$mainDecoder = function (size) {
	return A2(
		$elm$bytes$Bytes$Decode$andThen,
		function (info) {
			return A2(
				$elm$bytes$Bytes$Decode$map,
				function (rest) {
					return A2(
						$justgook$elm_image$Image$Internal$ImageData$Lazy,
						$justgook$elm_image$Image$Internal$Meta$Gif(
							{height: info.height, width: info.width}),
						function (header) {
							return A2(
								$justgook$elm_image$Image$Internal$ImageData$ImageRaw,
								header,
								A2(
									$elm$core$Maybe$withDefault,
									$elm$core$Array$empty,
									A2(
										$elm$core$Maybe$map,
										function (arr) {
											return A2($justgook$elm_image$Image$Internal$Array2d$fromArray, info.width, arr);
										},
										A2(
											$elm$bytes$Bytes$Decode$decode,
											$justgook$elm_image$Image$Internal$GIF$decodeLazyImage(info),
											rest))));
						});
				},
				$elm$bytes$Bytes$Decode$bytes(size - 13));
		},
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				return $justgook$elm_image$Image$Internal$GIF$decodeInfo;
			},
			$justgook$elm_image$Image$Internal$GIF$decodeSignature));
};
var $justgook$elm_image$Image$Internal$GIF$decode = function (bytes) {
	return A2(
		$elm$bytes$Bytes$Decode$decode,
		$justgook$elm_image$Image$Internal$GIF$mainDecoder(
			$elm$bytes$Bytes$width(bytes)),
		bytes);
};
var $justgook$elm_image$Image$Internal$PNG$listStep = F2(
	function (decoder_, _v0) {
		var length_ = _v0.a;
		var acc = _v0.b;
		return A2(
			$elm$bytes$Bytes$Decode$map,
			function (_v1) {
				var bytesTaken = _v1.a;
				var newAcc = _v1.b;
				return ((length_ - bytesTaken) > 0) ? $elm$bytes$Bytes$Decode$Loop(
					_Utils_Tuple2(length_ - bytesTaken, newAcc)) : $elm$bytes$Bytes$Decode$Done(newAcc);
			},
			decoder_(acc));
	});
var $justgook$elm_image$Image$Internal$PNG$chunkLoopR = F2(
	function (length, decoder) {
		return A2(
			$elm$bytes$Bytes$Decode$loop,
			_Utils_Tuple2(length, $elm$core$Maybe$Nothing),
			$justgook$elm_image$Image$Internal$PNG$listStep(decoder));
	});
var $justgook$elm_image$Image$Internal$PNG$Collecting = function (a) {
	return {$: 'Collecting', a: a};
};
var $elm$bytes$Bytes$Encode$Bytes = function (a) {
	return {$: 'Bytes', a: a};
};
var $elm$bytes$Bytes$Encode$bytes = $elm$bytes$Bytes$Encode$Bytes;
var $elm$bytes$Bytes$Encode$Seq = F2(
	function (a, b) {
		return {$: 'Seq', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$getWidths = F2(
	function (width, builders) {
		getWidths:
		while (true) {
			if (!builders.b) {
				return width;
			} else {
				var b = builders.a;
				var bs = builders.b;
				var $temp$width = width + $elm$bytes$Bytes$Encode$getWidth(b),
					$temp$builders = bs;
				width = $temp$width;
				builders = $temp$builders;
				continue getWidths;
			}
		}
	});
var $elm$bytes$Bytes$Encode$sequence = function (builders) {
	return A2(
		$elm$bytes$Bytes$Encode$Seq,
		A2($elm$bytes$Bytes$Encode$getWidths, 0, builders),
		builders);
};
var $justgook$elm_image$Image$Internal$PNG$decodeIDAT = F2(
	function (image, length) {
		return A2(
			$elm$bytes$Bytes$Decode$map,
			function (b) {
				var _v0 = image.data;
				switch (_v0.$) {
					case 'Collecting':
						var e = _v0.a;
						return _Utils_update(
							image,
							{
								data: $justgook$elm_image$Image$Internal$PNG$Collecting(
									$elm$bytes$Bytes$Encode$sequence(
										_List_fromArray(
											[
												e,
												$elm$bytes$Bytes$Encode$bytes(b)
											])))
							});
					case 'None':
						return _Utils_update(
							image,
							{
								data: $justgook$elm_image$Image$Internal$PNG$Collecting(
									$elm$bytes$Bytes$Encode$bytes(b))
							});
					default:
						return image;
				}
			},
			$elm$bytes$Bytes$Decode$bytes(length));
	});
var $justgook$elm_image$Image$Internal$PNG$ImageData = function (a) {
	return {$: 'ImageData', a: a};
};
var $justgook$elm_image$Image$Internal$Meta$Png = function (a) {
	return {$: 'Png', a: a};
};
var $elm$bytes$Bytes$Encode$encode = _Bytes_encode;
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $justgook$elm_image$Image$Internal$Decode$foldlStep = F2(
	function (decoder, _v0) {
		var n = _v0.a;
		var acc = _v0.b;
		return (n <= 0) ? $elm$bytes$Bytes$Decode$succeed(
			$elm$bytes$Bytes$Decode$Done(acc)) : A2(
			$elm$bytes$Bytes$Decode$map,
			function (x) {
				return $elm$bytes$Bytes$Decode$Loop(
					_Utils_Tuple2(n - 1, x));
			},
			decoder(acc));
	});
var $justgook$elm_image$Image$Internal$Decode$foldl = F3(
	function (count, decoder, acc) {
		return A2(
			$elm$bytes$Bytes$Decode$loop,
			_Utils_Tuple2(count, acc),
			$justgook$elm_image$Image$Internal$Decode$foldlStep(decoder));
	});
var $justgook$elm_image$Image$Internal$PNG$decodeLine = F3(
	function (pxDecode, width, acc) {
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (filterType) {
				return A3(
					$justgook$elm_image$Image$Internal$Decode$foldl,
					width,
					pxDecode(filterType),
					A2($elm$core$Array$push, $elm$core$Array$empty, acc));
			},
			$elm$bytes$Bytes$Decode$unsignedInt8);
	});
var $justgook$elm_image$Image$Internal$PNG$dataWrapDecode = F3(
	function (pxDecode, width, height) {
		return A3(
			$justgook$elm_image$Image$Internal$Decode$foldl,
			height,
			A2($justgook$elm_image$Image$Internal$PNG$decodeLine, pxDecode, width),
			$elm$core$Array$empty);
	});
var $justgook$elm_image$Image$Internal$PNG$pixelDecode8None_ = $elm$bytes$Bytes$Decode$unsignedInt8;
var $justgook$elm_image$Image$Internal$Array2d$lastIndex_ = function (arr) {
	return $elm$core$Array$length(arr) - 1;
};
var $justgook$elm_image$Image$Internal$Array2d$last = function (arr) {
	return A2(
		$elm$core$Maybe$andThen,
		function (subArr) {
			return A2(
				$elm$core$Array$get,
				$justgook$elm_image$Image$Internal$Array2d$lastIndex_(subArr),
				subArr);
		},
		A2(
			$elm$core$Array$get,
			$justgook$elm_image$Image$Internal$Array2d$lastIndex_(arr),
			arr));
};
var $justgook$elm_image$Image$Internal$Array2d$lastLength = function (arr) {
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		A2(
			$elm$core$Maybe$map,
			$elm$core$Array$length,
			A2(
				$elm$core$Array$get,
				$justgook$elm_image$Image$Internal$Array2d$lastIndex_(arr),
				arr)));
};
var $justgook$elm_image$Image$Internal$PNG$arrayPaeth = function (arr) {
	return A2(
		$elm$core$Maybe$withDefault,
		_Utils_Tuple3(0, 0, 0),
		A2(
			$elm$core$Maybe$map,
			function (subArr) {
				var currentIndex = $justgook$elm_image$Image$Internal$Array2d$lastLength(arr);
				return _Utils_Tuple3(
					A2(
						$elm$core$Maybe$withDefault,
						0,
						$justgook$elm_image$Image$Internal$Array2d$last(arr)),
					A2(
						$elm$core$Maybe$withDefault,
						0,
						A2($elm$core$Array$get, currentIndex, subArr)),
					A2(
						$elm$core$Maybe$withDefault,
						0,
						A2($elm$core$Array$get, currentIndex - 1, subArr)));
			},
			A2(
				$elm$core$Array$get,
				$elm$core$Array$length(arr) - 2,
				arr)));
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $justgook$elm_image$Image$Internal$PNG$paeth = F3(
	function (a, b, c) {
		var p = (a + b) - c;
		var pa = $elm$core$Basics$abs(p - a);
		var pb = $elm$core$Basics$abs(p - b);
		var pc = $elm$core$Basics$abs(p - c);
		return ((_Utils_cmp(pa, pb) < 1) && (_Utils_cmp(pa, pc) < 1)) ? a : ((_Utils_cmp(pb, pc) < 1) ? b : c);
	});
var $justgook$elm_image$Image$Internal$PNG$pixelDecode8Paeth_ = function (acc) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		function (a_) {
			var _v0 = $justgook$elm_image$Image$Internal$PNG$arrayPaeth(acc);
			var prevA_ = _v0.a;
			var prevB_ = _v0.b;
			var prevC_ = _v0.c;
			var prevA = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & prevA_, 255 & prevB_, 255 & prevC_);
			var a = a_ + prevA;
			return a;
		},
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixelDecode8Sub_ = function (acc) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		function (a_) {
			var prev = A2(
				$elm$core$Maybe$withDefault,
				0,
				$justgook$elm_image$Image$Internal$Array2d$last(acc));
			var prevA = 255 & prev;
			var a = a_ + prevA;
			return a;
		},
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$arrayUp = function (arr) {
	return A2(
		$elm$core$Maybe$andThen,
		$elm$core$Array$get(
			$justgook$elm_image$Image$Internal$Array2d$lastLength(arr)),
		A2(
			$elm$core$Array$get,
			$elm$core$Array$length(arr) - 2,
			arr));
};
var $justgook$elm_image$Image$Internal$PNG$pixelDecode8Up_ = function (acc) {
	return A2(
		$elm$bytes$Bytes$Decode$map,
		function (a_) {
			var prev = A2(
				$elm$core$Maybe$withDefault,
				0,
				$justgook$elm_image$Image$Internal$PNG$arrayUp(acc));
			var prevA = 255 & prev;
			var a = a_ + prevA;
			return a;
		},
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixel8DecodePlain = F2(
	function (filterType, acc) {
		switch (filterType) {
			case 0:
				return $justgook$elm_image$Image$Internal$PNG$pixelDecode8None_;
			case 1:
				return $justgook$elm_image$Image$Internal$PNG$pixelDecode8Sub_(acc);
			case 2:
				return $justgook$elm_image$Image$Internal$PNG$pixelDecode8Up_(acc);
			case 4:
				return $justgook$elm_image$Image$Internal$PNG$pixelDecode8Paeth_(acc);
			default:
				return $justgook$elm_image$Image$Internal$PNG$pixelDecode8None_;
		}
	});
var $justgook$elm_image$Image$Internal$Array2d$push = F2(
	function (item, arr) {
		return A2(
			$elm$core$Maybe$withDefault,
			arr,
			A2(
				$elm$core$Maybe$map,
				function (arr2) {
					return A3(
						$elm$core$Array$set,
						$justgook$elm_image$Image$Internal$Array2d$lastIndex_(arr),
						A2($elm$core$Array$push, item, arr2),
						arr);
				},
				A2(
					$elm$core$Array$get,
					$justgook$elm_image$Image$Internal$Array2d$lastIndex_(arr),
					arr)));
	});
var $justgook$elm_image$Image$Internal$PNG$indexPixel8Decode = F3(
	function (palette, filterType, acc) {
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (i) {
				return A2(
					$elm$core$Maybe$withDefault,
					$elm$bytes$Bytes$Decode$fail,
					A2(
						$elm$core$Maybe$map,
						function (c) {
							return $elm$bytes$Bytes$Decode$succeed(
								A2($justgook$elm_image$Image$Internal$Array2d$push, c, acc));
						},
						A2($elm$core$Array$get, i, palette)));
			},
			A2($justgook$elm_image$Image$Internal$PNG$pixel8DecodePlain, filterType, acc));
	});
var $justgook$elm_image$Image$Internal$PNG$pixelDecode16None_ = $elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$BE);
var $justgook$elm_image$Image$Internal$PNG$packIntoInt16 = F2(
	function (b, a) {
		return ((255 & b) << 8) | (255 & a);
	});
var $justgook$elm_image$Image$Internal$PNG$pixelDecode16Paeth_ = function (acc) {
	return A3(
		$elm$bytes$Bytes$Decode$map2,
		F2(
			function (b_, a_) {
				var _v0 = $justgook$elm_image$Image$Internal$PNG$arrayPaeth(acc);
				var prevA_ = _v0.a;
				var prevB_ = _v0.b;
				var prevC_ = _v0.c;
				var prevA = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & prevA_, 255 & prevB_, 255 & prevC_);
				var a = a_ + prevA;
				var prevB = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & (prevA_ >> 8), 255 & (prevB_ >> 8), 255 & (prevC_ >> 8));
				var b = b_ + prevB;
				return A2($justgook$elm_image$Image$Internal$PNG$packIntoInt16, b, a);
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixelDecode16Sub_ = function (acc) {
	return A3(
		$elm$bytes$Bytes$Decode$map2,
		F2(
			function (b_, a_) {
				var prev = A2(
					$elm$core$Maybe$withDefault,
					0,
					$justgook$elm_image$Image$Internal$Array2d$last(acc));
				var prevA = 255 & prev;
				var prevB = 255 & (prev >> 8);
				var b = b_ + prevB;
				var a = a_ + prevA;
				return A2($justgook$elm_image$Image$Internal$PNG$packIntoInt16, b, a);
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixelDecode16Up_ = function (acc) {
	return A3(
		$elm$bytes$Bytes$Decode$map2,
		F2(
			function (b_, a_) {
				var prev = A2(
					$elm$core$Maybe$withDefault,
					0,
					$justgook$elm_image$Image$Internal$PNG$arrayUp(acc));
				var prevA = 255 & prev;
				var prevB = 255 & (prev >> 8);
				var b = b_ + prevB;
				var a = a_ + prevA;
				return A2($justgook$elm_image$Image$Internal$PNG$packIntoInt16, b, a);
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixel16Decode = F2(
	function (filterType, acc) {
		return A2(
			$elm$bytes$Bytes$Decode$map,
			function (a) {
				return A2($justgook$elm_image$Image$Internal$Array2d$push, a, acc);
			},
			function () {
				switch (filterType) {
					case 0:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode16None_;
					case 1:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode16Sub_(acc);
					case 2:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode16Up_(acc);
					case 4:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode16Paeth_(acc);
					default:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode16None_;
				}
			}());
	});
var $justgook$elm_image$Image$Internal$PNG$pixelDecode32None_ = $elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE);
var $justgook$elm_image$Image$Internal$PNG$packIntoInt32 = F4(
	function (r, g, b, a) {
		return (((255 & r) << 24) | ((255 & g) << 16)) | (((255 & b) << 8) | (255 & a));
	});
var $justgook$elm_image$Image$Internal$PNG$pixelDecode32Paeth_ = function (acc) {
	return A5(
		$elm$bytes$Bytes$Decode$map4,
		F4(
			function (r_, g_, b_, a_) {
				var _v0 = $justgook$elm_image$Image$Internal$PNG$arrayPaeth(acc);
				var prevA_ = _v0.a;
				var prevB_ = _v0.b;
				var prevC_ = _v0.c;
				var prevA = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & prevA_, 255 & prevB_, 255 & prevC_);
				var a = a_ + prevA;
				var prevB = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & (prevA_ >> 8), 255 & (prevB_ >> 8), 255 & (prevC_ >> 8));
				var b = b_ + prevB;
				var prevG = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & (prevA_ >> 16), 255 & (prevB_ >> 16), 255 & (prevC_ >> 16));
				var g = g_ + prevG;
				var prevR = A3($justgook$elm_image$Image$Internal$PNG$paeth, 255 & (prevA_ >> 24), 255 & (prevB_ >> 24), 255 & (prevC_ >> 24));
				var r = r_ + prevR;
				return A4($justgook$elm_image$Image$Internal$PNG$packIntoInt32, r, g, b, a);
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixelDecode32Sub_ = function (acc) {
	return A5(
		$elm$bytes$Bytes$Decode$map4,
		F4(
			function (r_, g_, b_, a_) {
				var prev = A2(
					$elm$core$Maybe$withDefault,
					0,
					$justgook$elm_image$Image$Internal$Array2d$last(acc));
				var prevA = 255 & prev;
				var prevB = 255 & (prev >> 8);
				var prevG = 255 & (prev >> 16);
				var prevR = prev >>> 24;
				var r = r_ + prevR;
				var g = g_ + prevG;
				var b = b_ + prevB;
				var a = a_ + prevA;
				return A4($justgook$elm_image$Image$Internal$PNG$packIntoInt32, r, g, b, a);
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixelDecode32Up_ = function (acc) {
	return A5(
		$elm$bytes$Bytes$Decode$map4,
		F4(
			function (r_, g_, b_, a_) {
				var prev = A2(
					$elm$core$Maybe$withDefault,
					0,
					$justgook$elm_image$Image$Internal$PNG$arrayUp(acc));
				var prevA = 255 & prev;
				var prevB = 255 & (prev >> 8);
				var prevG = 255 & (prev >> 16);
				var prevR = prev >>> 24;
				var r = r_ + prevR;
				var g = g_ + prevG;
				var b = b_ + prevB;
				var a = a_ + prevA;
				return A4($justgook$elm_image$Image$Internal$PNG$packIntoInt32, r, g, b, a);
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8);
};
var $justgook$elm_image$Image$Internal$PNG$pixel32Decode = F2(
	function (filterType, acc) {
		return A2(
			$elm$bytes$Bytes$Decode$map,
			function (a) {
				return A2($justgook$elm_image$Image$Internal$Array2d$push, a, acc);
			},
			function () {
				switch (filterType) {
					case 0:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode32None_;
					case 1:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode32Sub_(acc);
					case 2:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode32Up_(acc);
					case 4:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode32Paeth_(acc);
					default:
						return $justgook$elm_image$Image$Internal$PNG$pixelDecode32None_;
				}
			}());
	});
var $justgook$elm_image$Image$Internal$PNG$dataDecode = F2(
	function (header, palette) {
		var width = header.width;
		var height = header.height;
		var color = header.color;
		_v0$3:
		while (true) {
			switch (color.$) {
				case 'IndexedColour':
					if (color.a.$ === 'BitDepth1_2_4_8__8') {
						var _v1 = color.a;
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
								$justgook$elm_image$Image$Internal$Meta$Png(header)),
							A3(
								$justgook$elm_image$Image$Internal$PNG$dataWrapDecode,
								$justgook$elm_image$Image$Internal$PNG$indexPixel8Decode(palette),
								width,
								height));
					} else {
						break _v0$3;
					}
				case 'GreyscaleAlpha':
					if (color.a.$ === 'BitDepth8_16__8') {
						var _v2 = color.a;
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
								$justgook$elm_image$Image$Internal$Meta$Png(header)),
							A3($justgook$elm_image$Image$Internal$PNG$dataWrapDecode, $justgook$elm_image$Image$Internal$PNG$pixel16Decode, width, height));
					} else {
						break _v0$3;
					}
				case 'TrueColourAlpha':
					if (color.a.$ === 'BitDepth8_16__8') {
						var _v3 = color.a;
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$ImageData$ImageRaw(
								$justgook$elm_image$Image$Internal$Meta$Png(header)),
							A3($justgook$elm_image$Image$Internal$PNG$dataWrapDecode, $justgook$elm_image$Image$Internal$PNG$pixel32Decode, width, height));
					} else {
						break _v0$3;
					}
				default:
					break _v0$3;
			}
		}
		return $elm$bytes$Bytes$Decode$fail;
	});
var $folkertdev$elm_flate$Inflate$ZLib$InflateError = function (a) {
	return {$: 'InflateError', a: a};
};
var $folkertdev$elm_flate$Inflate$ZLib$InvalidAdler = function (a) {
	return {$: 'InvalidAdler', a: a};
};
var $folkertdev$elm_flate$Inflate$ZLib$InvalidChecksum = {$: 'InvalidChecksum'};
var $folkertdev$elm_flate$Inflate$ZLib$InvalidMethod = {$: 'InvalidMethod'};
var $folkertdev$elm_flate$Inflate$ZLib$InvalidSlice = {$: 'InvalidSlice'};
var $folkertdev$elm_flate$Inflate$ZLib$InvalidWindowSize = {$: 'InvalidWindowSize'};
var $folkertdev$elm_flate$Inflate$ZLib$PresetDictionaryIssue = {$: 'PresetDictionaryIssue'};
var $folkertdev$elm_flate$Checksum$Adler32$a32 = {base: 65521, nmax: 5552};
var $folkertdev$elm_flate$Checksum$Adler32$step8Bytes = F5(
	function (remaining, s1, s2, word1, word2) {
		var byte8 = 255 & word2;
		var byte7 = 255 & (word2 >>> 8);
		var byte6 = 255 & (word2 >>> 16);
		var byte5 = 255 & (word2 >>> 24);
		var byte4 = 255 & word1;
		var byte3 = 255 & (word1 >>> 8);
		var byte2 = 255 & (word1 >>> 16);
		var byte1 = 255 & (word1 >>> 24);
		var s1a_1 = s1 + byte1;
		var s1a_2 = s1a_1 + byte2;
		var s1a_3 = s1a_2 + byte3;
		var s1a_4 = s1a_3 + byte4;
		var s2a_1 = s2 + s1a_1;
		var s2a_2 = s2a_1 + s1a_2;
		var s2a_3 = s2a_2 + s1a_3;
		var s2a_4 = s2a_3 + s1a_4;
		var s1b_1 = s1a_4 + byte5;
		var s1b_2 = s1b_1 + byte6;
		var s1b_3 = s1b_2 + byte7;
		var s1b_4 = s1b_3 + byte8;
		var s2b_1 = s2a_4 + s1b_1;
		var s2b_2 = s2b_1 + s1b_2;
		var s2b_3 = s2b_2 + s1b_3;
		var s2b_4 = s2b_3 + s1b_4;
		return $elm$bytes$Bytes$Decode$Loop(
			{remaining: remaining - 8, s1: s1b_4, s2: s2b_4});
	});
var $folkertdev$elm_flate$Checksum$Adler32$processChunkHelp = function (_v0) {
	var remaining = _v0.remaining;
	var s1 = _v0.s1;
	var s2 = _v0.s2;
	return (remaining >= 8) ? A3(
		$elm$bytes$Bytes$Decode$map2,
		A3($folkertdev$elm_flate$Checksum$Adler32$step8Bytes, remaining, s1, s2),
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE),
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE)) : ((remaining > 0) ? A2(
		$elm$bytes$Bytes$Decode$map,
		function (_byte) {
			return $elm$bytes$Bytes$Decode$Loop(
				{remaining: remaining - 1, s1: s1 + _byte, s2: (s1 + _byte) + s2});
		},
		$elm$bytes$Bytes$Decode$unsignedInt8) : $elm$bytes$Bytes$Decode$succeed(
		$elm$bytes$Bytes$Decode$Done(
			{s1: s1 % $folkertdev$elm_flate$Checksum$Adler32$a32.base, s2: s2 % $folkertdev$elm_flate$Checksum$Adler32$a32.base})));
};
var $folkertdev$elm_flate$Checksum$Adler32$processChunk = function (config) {
	return A2($elm$bytes$Bytes$Decode$loop, config, $folkertdev$elm_flate$Checksum$Adler32$processChunkHelp);
};
var $folkertdev$elm_flate$Checksum$Adler32$chunkedFold = function (_v0) {
	var bufferSize = _v0.bufferSize;
	var maxBlockSize = _v0.maxBlockSize;
	var go = function (_v1) {
		var remainingLength = _v1.remainingLength;
		var s1 = _v1.s1;
		var s2 = _v1.s2;
		return (!remainingLength) ? $elm$bytes$Bytes$Decode$succeed(
			$elm$bytes$Bytes$Decode$Done(
				{s1: s1, s2: s2})) : ((_Utils_cmp(remainingLength, maxBlockSize) < 0) ? A2(
			$elm$bytes$Bytes$Decode$map,
			$elm$bytes$Bytes$Decode$Done,
			$folkertdev$elm_flate$Checksum$Adler32$processChunk(
				{remaining: remainingLength, s1: s1, s2: s2})) : A2(
			$elm$bytes$Bytes$Decode$map,
			function (result) {
				return $elm$bytes$Bytes$Decode$Loop(
					{remainingLength: remainingLength - maxBlockSize, s1: result.s1, s2: result.s2});
			},
			$folkertdev$elm_flate$Checksum$Adler32$processChunk(
				{remaining: maxBlockSize, s1: s1, s2: s2})));
	};
	return A2(
		$elm$bytes$Bytes$Decode$loop,
		{remainingLength: bufferSize, s1: 1, s2: 0},
		go);
};
var $folkertdev$elm_flate$Checksum$Adler32$adler32 = function (buffer) {
	var _v0 = A2(
		$elm$bytes$Bytes$Decode$decode,
		$folkertdev$elm_flate$Checksum$Adler32$chunkedFold(
			{
				bufferSize: $elm$bytes$Bytes$width(buffer),
				maxBlockSize: $folkertdev$elm_flate$Checksum$Adler32$a32.nmax
			}),
		buffer);
	if (_v0.$ === 'Nothing') {
		return 0;
	} else {
		var s1 = _v0.a.s1;
		var s2 = _v0.a.s2;
		return ((s2 << 16) | s1) >>> 0;
	}
};
var $folkertdev$elm_flate$Inflate$BitReader$decode = F2(
	function (bytes, _v0) {
		var reader = _v0.a;
		var initialState = {bitsAvailable: 0, buffer: bytes, reserve: 0, reserveAvailable: 0, tag: 0};
		var _v1 = reader(initialState);
		if (_v1.$ === 'Ok') {
			var _v2 = _v1.a;
			var value = _v2.a;
			return $elm$core$Result$Ok(value);
		} else {
			var e = _v1.a;
			return $elm$core$Result$Err(e);
		}
	});
var $folkertdev$elm_flate$Experimental$ByteArray$ByteArray = F3(
	function (a, b, c) {
		return {$: 'ByteArray', a: a, b: b, c: c};
	});
var $folkertdev$elm_flate$Experimental$ByteArray$empty = A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, $elm$core$Array$empty, 0, 0);
var $folkertdev$elm_flate$Inflate$BitReader$BitReader = function (a) {
	return {$: 'BitReader', a: a};
};
var $folkertdev$elm_flate$Inflate$BitReader$loopHelp = F3(
	function (accum, callback, state) {
		loopHelp:
		while (true) {
			var _v0 = callback(accum);
			var decoder = _v0.a;
			var _v1 = decoder(state);
			if (_v1.$ === 'Err') {
				var e = _v1.a;
				return $elm$core$Result$Err(e);
			} else {
				if (_v1.a.a.$ === 'Loop') {
					var _v2 = _v1.a;
					var newAccum = _v2.a.a;
					var newState = _v2.b;
					var $temp$accum = newAccum,
						$temp$callback = callback,
						$temp$state = newState;
					accum = $temp$accum;
					callback = $temp$callback;
					state = $temp$state;
					continue loopHelp;
				} else {
					var _v3 = _v1.a;
					var result = _v3.a.a;
					var newState = _v3.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(result, newState));
				}
			}
		}
	});
var $folkertdev$elm_flate$Inflate$BitReader$loop = F2(
	function (state, callback) {
		return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
			A2($folkertdev$elm_flate$Inflate$BitReader$loopHelp, state, callback));
	});
var $folkertdev$elm_flate$Inflate$BitReader$map = F2(
	function (f, _v0) {
		var g = _v0.a;
		return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
			function (s) {
				var _v1 = g(s);
				if (_v1.$ === 'Ok') {
					var _v2 = _v1.a;
					var value = _v2.a;
					var newState = _v2.b;
					return $elm$core$Result$Ok(
						_Utils_Tuple2(
							f(value),
							newState));
				} else {
					var e = _v1.a;
					return $elm$core$Result$Err(e);
				}
			});
	});
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$bytes$Bytes$Encode$U16 = F2(
	function (a, b) {
		return {$: 'U16', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$unsignedInt16 = $elm$bytes$Bytes$Encode$U16;
var $elm$bytes$Bytes$Encode$U32 = F2(
	function (a, b) {
		return {$: 'U32', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$unsignedInt32 = $elm$bytes$Bytes$Encode$U32;
var $elm$bytes$Bytes$Encode$U8 = function (a) {
	return {$: 'U8', a: a};
};
var $elm$bytes$Bytes$Encode$unsignedInt8 = $elm$bytes$Bytes$Encode$U8;
var $folkertdev$elm_flate$Experimental$ByteArray$toBytes = function (_v0) {
	var array = _v0.a;
	var finalSize = _v0.b;
	var finalBytes = _v0.c;
	var initial = function () {
		var finalInt32 = finalBytes >>> ((4 - finalSize) * 8);
		switch (finalSize) {
			case 4:
				return _List_fromArray(
					[
						A2($elm$bytes$Bytes$Encode$unsignedInt32, $elm$bytes$Bytes$BE, finalBytes)
					]);
			case 1:
				return _List_fromArray(
					[
						$elm$bytes$Bytes$Encode$unsignedInt8(finalInt32)
					]);
			case 2:
				return _List_fromArray(
					[
						A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$BE, finalInt32)
					]);
			case 3:
				return _List_fromArray(
					[
						A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$BE, finalInt32 >> 8),
						$elm$bytes$Bytes$Encode$unsignedInt8(255 & finalInt32)
					]);
			default:
				return _List_Nil;
		}
	}();
	var folder = F2(
		function (element, accum) {
			return A2(
				$elm$core$List$cons,
				A2($elm$bytes$Bytes$Encode$unsignedInt32, $elm$bytes$Bytes$BE, element),
				accum);
		});
	return $elm$bytes$Bytes$Encode$encode(
		$elm$bytes$Bytes$Encode$sequence(
			A3($elm$core$Array$foldr, folder, initial, array)));
};
var $folkertdev$elm_flate$Inflate$BitReader$andThen = F2(
	function (f, _v0) {
		var g = _v0.a;
		return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
			function (s) {
				var _v1 = g(s);
				if (_v1.$ === 'Ok') {
					var _v2 = _v1.a;
					var value = _v2.a;
					var newState = _v2.b;
					var _v3 = f(value);
					var h = _v3.a;
					return h(newState);
				} else {
					var e = _v1.a;
					return $elm$core$Result$Err(e);
				}
			});
	});
var $folkertdev$elm_flate$Experimental$ByteArray$push = F2(
	function (value, input) {
		var array = input.a;
		var finalSize = input.b;
		var finalBytes = input.c;
		if (finalSize === 4) {
			return A3(
				$folkertdev$elm_flate$Experimental$ByteArray$ByteArray,
				A2($elm$core$Array$push, finalBytes, array),
				1,
				value << 24);
		} else {
			if (!finalSize) {
				return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, 1, value << 24);
			} else {
				var offset = finalSize;
				var internalIndex = $elm$core$Array$length(array) - 1;
				var _new = ((255 & value) << ((3 - offset) * 8)) | finalBytes;
				var mask = 4278190080 >>> (offset * 8);
				return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize + 1, _new);
			}
		}
	});
var $folkertdev$elm_flate$Experimental$ByteArray$pushMany = F3(
	function (nbytes, value_, input) {
		var array = input.a;
		var finalSize = input.b;
		var finalBytes = input.c;
		var value = (nbytes === 4) ? value_ : (((1 << (nbytes * 8)) - 1) & value_);
		if (!nbytes) {
			return input;
		} else {
			if (finalSize === 4) {
				return A3(
					$folkertdev$elm_flate$Experimental$ByteArray$ByteArray,
					A2($elm$core$Array$push, finalBytes, array),
					nbytes,
					value << ((4 - nbytes) * 8));
			} else {
				if (!finalSize) {
					return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, nbytes, value << ((4 - nbytes) * 8));
				} else {
					var freeSpace = 4 - finalSize;
					if (_Utils_cmp(nbytes, freeSpace) > 0) {
						var bytesLeftOver = (finalSize + nbytes) - 4;
						var forFinal = value >>> (bytesLeftOver * 8);
						var newFinal = finalBytes | forFinal;
						var amount = ((8 - finalSize) - nbytes) * 8;
						var forNextFinal = (((1 << (bytesLeftOver * 8)) - 1) & value) << amount;
						return A3(
							$folkertdev$elm_flate$Experimental$ByteArray$ByteArray,
							A2($elm$core$Array$push, newFinal, array),
							nbytes - freeSpace,
							forNextFinal);
					} else {
						var amount = (4 - (finalSize + nbytes)) * 8;
						var forFinal = value << amount;
						var newFinal = finalBytes | forFinal;
						return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize + nbytes, newFinal);
					}
				}
			}
		}
	});
var $folkertdev$elm_flate$Experimental$ByteArray$appendBytesHelp = function (_v0) {
	var remaining = _v0.a;
	var bytearray = _v0.b;
	var array = bytearray.a;
	var finalSize = bytearray.b;
	var finalBytes = bytearray.c;
	return (remaining >= 4) ? A2(
		$elm$bytes$Bytes$Decode$map,
		function (_new) {
			return $elm$bytes$Bytes$Decode$Loop(
				_Utils_Tuple2(
					remaining - 4,
					A3($folkertdev$elm_flate$Experimental$ByteArray$pushMany, 4, _new, bytearray)));
		},
		$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE)) : ((remaining >= 1) ? A2(
		$elm$bytes$Bytes$Decode$map,
		function (_new) {
			return $elm$bytes$Bytes$Decode$Loop(
				_Utils_Tuple2(
					remaining - 1,
					A2($folkertdev$elm_flate$Experimental$ByteArray$push, _new, bytearray)));
		},
		$elm$bytes$Bytes$Decode$unsignedInt8) : $elm$bytes$Bytes$Decode$succeed(
		$elm$bytes$Bytes$Decode$Done(bytearray)));
};
var $folkertdev$elm_flate$Experimental$ByteArray$appendBytes = F2(
	function (bytes, barray) {
		var array = barray.a;
		var finalSize = barray.b;
		var finalBytes = barray.c;
		var decoder = A2(
			$elm$bytes$Bytes$Decode$loop,
			_Utils_Tuple2(
				$elm$bytes$Bytes$width(bytes),
				barray),
			$folkertdev$elm_flate$Experimental$ByteArray$appendBytesHelp);
		var _v0 = A2($elm$bytes$Bytes$Decode$decode, decoder, bytes);
		if (_v0.$ === 'Just') {
			var v = _v0.a;
			return v;
		} else {
			return barray;
		}
	});
var $elm$core$Array$repeat = F2(
	function (n, e) {
		return A2(
			$elm$core$Array$initialize,
			n,
			function (_v0) {
				return e;
			});
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $folkertdev$elm_flate$Inflate$Internal$buildTree = F3(
	function (lengths, offset, num) {
		var tableDict = function () {
			var updater = function (maybeValue) {
				if (maybeValue.$ === 'Nothing') {
					return $elm$core$Maybe$Just(1);
				} else {
					var v = maybeValue.a;
					return $elm$core$Maybe$Just(v + 1);
				}
			};
			var folder = F3(
				function (key, value, accum) {
					return ((_Utils_cmp(key, offset) > -1) && (_Utils_cmp(key, num + offset) < 0)) ? A3($elm$core$Dict$update, value, updater, accum) : accum;
				});
			return A3($elm$core$Dict$foldl, folder, $elm$core$Dict$empty, lengths);
		}();
		var offsetsDict = A3(
			$elm$core$Dict$foldl,
			F3(
				function (key, value, _v4) {
					var sum = _v4.a;
					var dict = _v4.b;
					return _Utils_Tuple2(
						sum + value,
						A3($elm$core$Dict$insert, key, sum, dict));
				}),
			_Utils_Tuple2(0, $elm$core$Dict$empty),
			tableDict);
		var newTable = function () {
			var helper = F4(
				function (key, value, i, array) {
					helper:
					while (true) {
						if (_Utils_cmp(i, key) > 0) {
							var $temp$key = key,
								$temp$value = value,
								$temp$i = i - 1,
								$temp$array = A2($elm$core$List$cons, 0, array);
							key = $temp$key;
							value = $temp$value;
							i = $temp$i;
							array = $temp$array;
							continue helper;
						} else {
							return A2($elm$core$List$cons, value, array);
						}
					}
				});
			var foldHelp = F3(
				function (key, value, _v3) {
					var i = _v3.a;
					var array = _v3.b;
					return _Utils_Tuple2(
						key - 1,
						A4(helper, key, value, i, array));
				});
			var anotherGo = F2(
				function (i, array) {
					anotherGo:
					while (true) {
						if (i >= 0) {
							var $temp$i = i - 1,
								$temp$array = A2($elm$core$List$cons, 0, array);
							i = $temp$i;
							array = $temp$array;
							continue anotherGo;
						} else {
							return array;
						}
					}
				});
			return function (_v2) {
				var a = _v2.a;
				var b = _v2.b;
				return A2(anotherGo, a, b);
			}(
				A3(
					$elm$core$Dict$foldr,
					foldHelp,
					_Utils_Tuple2(15, _List_Nil),
					tableDict));
		}();
		var go2 = F3(
			function (i, currentTranslation, currentOffsets) {
				go2:
				while (true) {
					if ((i - num) < 0) {
						var _v0 = A2($elm$core$Dict$get, offset + i, lengths);
						if (_v0.$ === 'Nothing') {
							var $temp$i = i + 1,
								$temp$currentTranslation = currentTranslation,
								$temp$currentOffsets = currentOffsets;
							i = $temp$i;
							currentTranslation = $temp$currentTranslation;
							currentOffsets = $temp$currentOffsets;
							continue go2;
						} else {
							var v = _v0.a;
							if (!(!v)) {
								var _v1 = A2($elm$core$Dict$get, v, currentOffsets);
								if (_v1.$ === 'Nothing') {
									return currentTranslation;
								} else {
									var w = _v1.a;
									var $temp$i = i + 1,
										$temp$currentTranslation = A3($elm$core$Array$set, w, i, currentTranslation),
										$temp$currentOffsets = A3($elm$core$Dict$insert, v, w + 1, currentOffsets);
									i = $temp$i;
									currentTranslation = $temp$currentTranslation;
									currentOffsets = $temp$currentOffsets;
									continue go2;
								}
							} else {
								var $temp$i = i + 1,
									$temp$currentTranslation = currentTranslation,
									$temp$currentOffsets = currentOffsets;
								i = $temp$i;
								currentTranslation = $temp$currentTranslation;
								currentOffsets = $temp$currentOffsets;
								continue go2;
							}
						}
					} else {
						return currentTranslation;
					}
				}
			});
		var translation2 = A3(
			go2,
			0,
			A2($elm$core$Array$repeat, num, 0),
			offsetsDict.b);
		return {table: newTable, trans: translation2};
	});
var $folkertdev$elm_flate$Inflate$Internal$clcIndices = _List_fromArray(
	[16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]);
var $folkertdev$elm_flate$Inflate$BitSet$BitSet320 = function (a) {
	return function (b) {
		return function (c) {
			return function (d) {
				return function (e) {
					return function (f) {
						return function (g) {
							return function (h) {
								return function (i) {
									return function (j) {
										return {$: 'BitSet320', a: a, b: b, c: c, d: d, e: e, f: f, g: g, h: h, i: i, j: j};
									};
								};
							};
						};
					};
				};
			};
		};
	};
};
var $folkertdev$elm_flate$Inflate$BitSet$insert = F2(
	function (n, input) {
		var b1 = input.a;
		var b2 = input.b;
		var b3 = input.c;
		var b4 = input.d;
		var b5 = input.e;
		var b6 = input.f;
		var b7 = input.g;
		var b8 = input.h;
		var b9 = input.i;
		var b10 = input.j;
		if (n >= 320) {
			return input;
		} else {
			var bit = 1 << (n % 32);
			var _v0 = (n / 32) | 0;
			switch (_v0) {
				case 0:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(bit | b1)(b2)(b3)(b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 1:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(bit | b2)(b3)(b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 2:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(bit | b3)(b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 3:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(bit | b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 4:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(bit | b5)(b6)(b7)(b8)(b9)(b10);
				case 5:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(bit | b6)(b7)(b8)(b9)(b10);
				case 6:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(bit | b7)(b8)(b9)(b10);
				case 7:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(b7)(bit | b8)(b9)(b10);
				case 8:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(b7)(b8)(bit | b9)(b10);
				case 9:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(b7)(b8)(b9)(bit | b10);
				default:
					return input;
			}
		}
	});
var $folkertdev$elm_flate$Inflate$BitSet$member = F2(
	function (n, input) {
		var b1 = input.a;
		var b2 = input.b;
		var b3 = input.c;
		var b4 = input.d;
		var b5 = input.e;
		var b6 = input.f;
		var b7 = input.g;
		var b8 = input.h;
		var b9 = input.i;
		var b10 = input.j;
		if (n >= 320) {
			return false;
		} else {
			var bit = 1 << (n % 32);
			var _v0 = (n / 32) | 0;
			switch (_v0) {
				case 0:
					return (bit & b1) > 0;
				case 1:
					return (bit & b2) > 0;
				case 2:
					return (bit & b3) > 0;
				case 3:
					return (bit & b4) > 0;
				case 4:
					return (bit & b5) > 0;
				case 5:
					return (bit & b6) > 0;
				case 6:
					return (bit & b7) > 0;
				case 7:
					return (bit & b8) > 0;
				case 8:
					return (bit & b9) > 0;
				case 9:
					return (bit & b10) > 0;
				default:
					return false;
			}
		}
	});
var $elm$core$Bitwise$complement = _Bitwise_complement;
var $folkertdev$elm_flate$Inflate$BitSet$remove = F2(
	function (n, input) {
		var b1 = input.a;
		var b2 = input.b;
		var b3 = input.c;
		var b4 = input.d;
		var b5 = input.e;
		var b6 = input.f;
		var b7 = input.g;
		var b8 = input.h;
		var b9 = input.i;
		var b10 = input.j;
		if (n >= 320) {
			return input;
		} else {
			var bit = ~(1 << (n % 32));
			var _v0 = (n / 32) | 0;
			switch (_v0) {
				case 0:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(bit & b1)(b2)(b3)(b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 1:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(bit & b2)(b3)(b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 2:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(bit & b3)(b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 3:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(bit & b4)(b5)(b6)(b7)(b8)(b9)(b10);
				case 4:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(bit & b5)(b6)(b7)(b8)(b9)(b10);
				case 5:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(bit & b6)(b7)(b8)(b9)(b10);
				case 6:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(bit & b7)(b8)(b9)(b10);
				case 7:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(b7)(bit & b8)(b9)(b10);
				case 8:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(b7)(b8)(bit & b9)(b10);
				case 9:
					return $folkertdev$elm_flate$Inflate$BitSet$BitSet320(b1)(b2)(b3)(b4)(b5)(b6)(b7)(b8)(b9)(bit & b10);
				default:
					return input;
			}
		}
	});
var $folkertdev$elm_flate$Inflate$Internal$copySegment = F5(
	function (i, value, bitset, lengths, length) {
		var end = i + length;
		var go = F3(
			function (j, currentBitSet, accum) {
				go:
				while (true) {
					if ((j - end) < 0) {
						if (!(!value)) {
							var $temp$j = j + 1,
								$temp$currentBitSet = A2($folkertdev$elm_flate$Inflate$BitSet$insert, j, currentBitSet),
								$temp$accum = A3($elm$core$Dict$insert, j, value, accum);
							j = $temp$j;
							currentBitSet = $temp$currentBitSet;
							accum = $temp$accum;
							continue go;
						} else {
							if (A2($folkertdev$elm_flate$Inflate$BitSet$member, j, currentBitSet)) {
								var $temp$j = j + 1,
									$temp$currentBitSet = A2($folkertdev$elm_flate$Inflate$BitSet$remove, j, currentBitSet),
									$temp$accum = A2($elm$core$Dict$remove, j, accum);
								j = $temp$j;
								currentBitSet = $temp$currentBitSet;
								accum = $temp$accum;
								continue go;
							} else {
								var $temp$j = j + 1,
									$temp$currentBitSet = currentBitSet,
									$temp$accum = accum;
								j = $temp$j;
								currentBitSet = $temp$currentBitSet;
								accum = $temp$accum;
								continue go;
							}
						}
					} else {
						return _Utils_Tuple2(currentBitSet, accum);
					}
				}
			});
		var _v0 = A3(go, i, bitset, lengths);
		var newBitSet = _v0.a;
		var newLengths = _v0.b;
		return _Utils_Tuple3(i + length, newBitSet, newLengths);
	});
var $folkertdev$elm_flate$Inflate$Internal$decodeSymbolInnerLoop = F5(
	function (table, cur, tag, bitsAvailable, sum) {
		decodeSymbolInnerLoop:
		while (true) {
			var newTag = tag >>> 1;
			if (!table.b) {
				return {bitsAvailable: bitsAvailable, cur: cur, sum: sum, tag: tag};
			} else {
				var value = table.a;
				var rest = table.b;
				var newerCur = ((cur << 1) + (tag & 1)) - value;
				var newSum = sum + value;
				if (newerCur >= 0) {
					var $temp$table = rest,
						$temp$cur = newerCur,
						$temp$tag = newTag,
						$temp$bitsAvailable = bitsAvailable - 1,
						$temp$sum = newSum;
					table = $temp$table;
					cur = $temp$cur;
					tag = $temp$tag;
					bitsAvailable = $temp$bitsAvailable;
					sum = $temp$sum;
					continue decodeSymbolInnerLoop;
				} else {
					return {bitsAvailable: bitsAvailable - 1, cur: newerCur, sum: newSum, tag: newTag};
				}
			}
		}
	});
var $folkertdev$elm_flate$Inflate$BitReader$moveFromReserve = F2(
	function (nbits, state) {
		var masked = (nbits === 32) ? (state.reserve << state.bitsAvailable) : ((((1 << nbits) - 1) & state.reserve) << state.bitsAvailable);
		return {bitsAvailable: state.bitsAvailable + nbits, buffer: state.buffer, reserve: state.reserve >>> nbits, reserveAvailable: state.reserveAvailable - nbits, tag: masked | state.tag};
	});
var $folkertdev$elm_flate$Inflate$BitReader$runDecoder = F3(
	function (width, valueDecoder, state) {
		var decoder = A3(
			$elm$bytes$Bytes$Decode$map2,
			$elm$core$Tuple$pair,
			valueDecoder,
			$elm$bytes$Bytes$Decode$bytes(
				$elm$bytes$Bytes$width(state.buffer) - width));
		var _v0 = A2($elm$bytes$Bytes$Decode$decode, decoder, state.buffer);
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return $elm$core$Result$Ok(value);
		} else {
			return $elm$core$Result$Err('BitReader.runDecoder: Unexpected end of Bytes');
		}
	});
var $folkertdev$elm_flate$Inflate$BitReader$unsignedInt24 = function (endianness) {
	if (endianness.$ === 'LE') {
		return A3(
			$elm$bytes$Bytes$Decode$map2,
			F2(
				function (b2, b1) {
					return (b1 << 16) | b2;
				}),
			$elm$bytes$Bytes$Decode$unsignedInt16(endianness),
			$elm$bytes$Bytes$Decode$unsignedInt8);
	} else {
		return A3(
			$elm$bytes$Bytes$Decode$map2,
			F2(
				function (b1, b2) {
					return (b1 << 16) | b2;
				}),
			$elm$bytes$Bytes$Decode$unsignedInt16(endianness),
			$elm$bytes$Bytes$Decode$unsignedInt8);
	}
};
var $folkertdev$elm_flate$Inflate$BitReader$readMoreBits = function (state) {
	readMoreBits:
	while (true) {
		var freeSpaceOnTag = 32 - state.bitsAvailable;
		if ((_Utils_cmp(freeSpaceOnTag, state.reserveAvailable) < 1) && (state.reserveAvailable > 0)) {
			return $elm$core$Result$Ok(
				A2($folkertdev$elm_flate$Inflate$BitReader$moveFromReserve, freeSpaceOnTag, state));
		} else {
			if (!$elm$bytes$Bytes$width(state.buffer)) {
				return $elm$core$Result$Ok(
					A2($folkertdev$elm_flate$Inflate$BitReader$moveFromReserve, state.reserveAvailable, state));
			} else {
				var state1 = A2($folkertdev$elm_flate$Inflate$BitReader$moveFromReserve, state.reserveAvailable, state);
				var _v0 = function () {
					var _v1 = $elm$bytes$Bytes$width(state.buffer);
					switch (_v1) {
						case 0:
							return _Utils_Tuple3(
								0,
								0,
								$elm$bytes$Bytes$Decode$succeed(0));
						case 1:
							return _Utils_Tuple3(1, 8, $elm$bytes$Bytes$Decode$unsignedInt8);
						case 2:
							return _Utils_Tuple3(
								2,
								16,
								$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE));
						case 3:
							return _Utils_Tuple3(
								3,
								24,
								$folkertdev$elm_flate$Inflate$BitReader$unsignedInt24($elm$bytes$Bytes$LE));
						default:
							return _Utils_Tuple3(
								4,
								32,
								$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$LE));
					}
				}();
				var width = _v0.a;
				var additionallyAvailable = _v0.b;
				var decoder = _v0.c;
				var _v2 = A3($folkertdev$elm_flate$Inflate$BitReader$runDecoder, width, decoder, state1);
				if (_v2.$ === 'Err') {
					var e = _v2.a;
					return $elm$core$Result$Err(e);
				} else {
					var _v3 = _v2.a;
					var newReserve = _v3.a;
					var newBuffer = _v3.b;
					var $temp$state = {bitsAvailable: state1.bitsAvailable, buffer: newBuffer, reserve: newReserve, reserveAvailable: additionallyAvailable, tag: state1.tag};
					state = $temp$state;
					continue readMoreBits;
				}
			}
		}
	}
};
var $folkertdev$elm_flate$Inflate$Internal$decodeSymbol = F2(
	function (table, tree) {
		return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
			function (state) {
				var _v0 = (state.bitsAvailable < 16) ? $folkertdev$elm_flate$Inflate$BitReader$readMoreBits(state) : $elm$core$Result$Ok(state);
				if (_v0.$ === 'Err') {
					var e = _v0.a;
					return $elm$core$Result$Err(e);
				} else {
					var d = _v0.a;
					var _v1 = A5($folkertdev$elm_flate$Inflate$Internal$decodeSymbolInnerLoop, table, 0, d.tag, d.bitsAvailable, 0);
					var cur = _v1.cur;
					var tag = _v1.tag;
					var bitsAvailable = _v1.bitsAvailable;
					var sum = _v1.sum;
					var _v2 = A2($elm$core$Array$get, sum + cur, tree.trans);
					if (_v2.$ === 'Nothing') {
						return $elm$core$Result$Err('Index into trans tree out of bounds');
					} else {
						var result = _v2.a;
						return $elm$core$Result$Ok(
							_Utils_Tuple2(
								result,
								{bitsAvailable: bitsAvailable, buffer: d.buffer, reserve: d.reserve, reserveAvailable: d.reserveAvailable, tag: tag}));
					}
				}
			});
	});
var $folkertdev$elm_flate$Inflate$BitReader$readBits = F2(
	function (numberOfBits, base) {
		return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
			function (state) {
				if (!numberOfBits) {
					return $elm$core$Result$Ok(
						_Utils_Tuple2(base, state));
				} else {
					var _v0 = (_Utils_cmp(state.bitsAvailable, numberOfBits) < 0) ? $folkertdev$elm_flate$Inflate$BitReader$readMoreBits(state) : $elm$core$Result$Ok(state);
					if (_v0.$ === 'Err') {
						var e = _v0.a;
						return $elm$core$Result$Err(e);
					} else {
						var d = _v0.a;
						var val = d.tag & (65535 >>> (16 - numberOfBits));
						var newTag = d.tag >>> numberOfBits;
						return $elm$core$Result$Ok(
							_Utils_Tuple2(
								val + base,
								{bitsAvailable: d.bitsAvailable - numberOfBits, buffer: d.buffer, reserve: d.reserve, reserveAvailable: d.reserveAvailable, tag: newTag}));
					}
				}
			});
	});
var $folkertdev$elm_flate$Inflate$BitReader$succeed = function (x) {
	return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
		function (s) {
			return $elm$core$Result$Ok(
				_Utils_Tuple2(x, s));
		});
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $folkertdev$elm_flate$Inflate$Internal$decodeDynamicTreeLength = F4(
	function (codeTree, hlit, hdist, _v0) {
		var i = _v0.a;
		var bitset = _v0.b;
		var lengths = _v0.c;
		if (_Utils_cmp(i, hlit + hdist) < 0) {
			var table = A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail(codeTree.table));
			return A2(
				$folkertdev$elm_flate$Inflate$BitReader$andThen,
				function (sym) {
					switch (sym) {
						case 16:
							var prev = A2(
								$elm$core$Maybe$withDefault,
								0,
								A2($elm$core$Dict$get, i - 1, lengths));
							return A2(
								$folkertdev$elm_flate$Inflate$BitReader$map,
								A2(
									$elm$core$Basics$composeR,
									A4($folkertdev$elm_flate$Inflate$Internal$copySegment, i, prev, bitset, lengths),
									$elm$bytes$Bytes$Decode$Loop),
								A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 2, 3));
						case 17:
							return A2(
								$folkertdev$elm_flate$Inflate$BitReader$map,
								A2(
									$elm$core$Basics$composeR,
									A4($folkertdev$elm_flate$Inflate$Internal$copySegment, i, 0, bitset, lengths),
									$elm$bytes$Bytes$Decode$Loop),
								A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 3, 3));
						case 18:
							return A2(
								$folkertdev$elm_flate$Inflate$BitReader$map,
								A2(
									$elm$core$Basics$composeR,
									A4($folkertdev$elm_flate$Inflate$Internal$copySegment, i, 0, bitset, lengths),
									$elm$bytes$Bytes$Decode$Loop),
								A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 7, 11));
						case 0:
							return A2($folkertdev$elm_flate$Inflate$BitSet$member, i, bitset) ? $folkertdev$elm_flate$Inflate$BitReader$succeed(
								$elm$bytes$Bytes$Decode$Loop(
									_Utils_Tuple3(
										i + 1,
										bitset,
										A2($elm$core$Dict$remove, i, lengths)))) : $folkertdev$elm_flate$Inflate$BitReader$succeed(
								$elm$bytes$Bytes$Decode$Loop(
									_Utils_Tuple3(i + 1, bitset, lengths)));
						default:
							return $folkertdev$elm_flate$Inflate$BitReader$succeed(
								$elm$bytes$Bytes$Decode$Loop(
									_Utils_Tuple3(
										i + 1,
										A2($folkertdev$elm_flate$Inflate$BitSet$insert, i, bitset),
										A3($elm$core$Dict$insert, i, sym, lengths))));
					}
				},
				A2($folkertdev$elm_flate$Inflate$Internal$decodeSymbol, table, codeTree));
		} else {
			return $folkertdev$elm_flate$Inflate$BitReader$succeed(
				$elm$bytes$Bytes$Decode$Done(lengths));
		}
	});
var $folkertdev$elm_flate$Inflate$BitSet$empty = $folkertdev$elm_flate$Inflate$BitSet$BitSet320(0)(0)(0)(0)(0)(0)(0)(0)(0)(0);
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $folkertdev$elm_flate$Inflate$Internal$decodeTreeLengths = F4(
	function (hlit, hdist, hclen, codeLengths) {
		var clcs = A2($elm$core$List$take, hclen, $folkertdev$elm_flate$Inflate$Internal$clcIndices);
		var initialLengths = function () {
			var go = F3(
				function (xs, ys, accum) {
					go:
					while (true) {
						if (!xs.b) {
							return accum;
						} else {
							var index = xs.a;
							var restIndex = xs.b;
							if (!ys.b) {
								return accum;
							} else {
								var codeLength = ys.a;
								var restCodeLength = ys.b;
								if (!(!codeLength)) {
									var $temp$xs = restIndex,
										$temp$ys = restCodeLength,
										$temp$accum = A3($elm$core$Dict$insert, index, codeLength, accum);
									xs = $temp$xs;
									ys = $temp$ys;
									accum = $temp$accum;
									continue go;
								} else {
									var $temp$xs = restIndex,
										$temp$ys = restCodeLength,
										$temp$accum = accum;
									xs = $temp$xs;
									ys = $temp$ys;
									accum = $temp$accum;
									continue go;
								}
							}
						}
					}
				});
			return A3(go, clcs, codeLengths, $elm$core$Dict$empty);
		}();
		var codeTree = A3($folkertdev$elm_flate$Inflate$Internal$buildTree, initialLengths, 0, 19);
		var initialBitSet = A3(
			$elm$core$Dict$foldl,
			F2(
				function (i, _v0) {
					return $folkertdev$elm_flate$Inflate$BitSet$insert(i);
				}),
			$folkertdev$elm_flate$Inflate$BitSet$empty,
			initialLengths);
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$loop,
			_Utils_Tuple3(0, initialBitSet, initialLengths),
			A3($folkertdev$elm_flate$Inflate$Internal$decodeDynamicTreeLength, codeTree, hlit, hdist));
	});
var $folkertdev$elm_flate$Inflate$BitReader$exactly = F2(
	function (tableCount, decoder) {
		var helper = function (_v0) {
			var n = _v0.a;
			var xs = _v0.b;
			return (n <= 0) ? $folkertdev$elm_flate$Inflate$BitReader$succeed(
				$elm$bytes$Bytes$Decode$Done(
					$elm$core$List$reverse(xs))) : A2(
				$folkertdev$elm_flate$Inflate$BitReader$map,
				function (x) {
					return $elm$bytes$Bytes$Decode$Loop(
						_Utils_Tuple2(
							n - 1,
							A2($elm$core$List$cons, x, xs)));
				},
				decoder);
		};
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$loop,
			_Utils_Tuple2(tableCount, _List_Nil),
			helper);
	});
var $folkertdev$elm_flate$Inflate$Internal$cont = F3(
	function (hlit, hdist, hclen) {
		var buildTrees = function (lengths) {
			return _Utils_Tuple2(
				A3($folkertdev$elm_flate$Inflate$Internal$buildTree, lengths, 0, hlit),
				A3($folkertdev$elm_flate$Inflate$Internal$buildTree, lengths, hlit, hdist));
		};
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$map,
			buildTrees,
			A2(
				$folkertdev$elm_flate$Inflate$BitReader$andThen,
				A3($folkertdev$elm_flate$Inflate$Internal$decodeTreeLengths, hlit, hdist, hclen),
				A2(
					$folkertdev$elm_flate$Inflate$BitReader$exactly,
					hclen,
					A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 3, 0))));
	});
var $folkertdev$elm_flate$Inflate$BitReader$map2 = F3(
	function (f, _v0, _v1) {
		var fa = _v0.a;
		var fb = _v1.a;
		return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
			function (state) {
				var _v2 = fa(state);
				if (_v2.$ === 'Err') {
					var e = _v2.a;
					return $elm$core$Result$Err(e);
				} else {
					var _v3 = _v2.a;
					var a = _v3.a;
					var newState = _v3.b;
					var _v4 = fb(newState);
					if (_v4.$ === 'Err') {
						var e = _v4.a;
						return $elm$core$Result$Err(e);
					} else {
						var _v5 = _v4.a;
						var b = _v5.a;
						var newerState = _v5.b;
						return $elm$core$Result$Ok(
							_Utils_Tuple2(
								A2(f, a, b),
								newerState));
					}
				}
			});
	});
var $folkertdev$elm_flate$Inflate$BitReader$andMap = F2(
	function (a, f) {
		return A3($folkertdev$elm_flate$Inflate$BitReader$map2, $elm$core$Basics$apL, f, a);
	});
var $folkertdev$elm_flate$Inflate$BitReader$map3 = F4(
	function (f, a, b, c) {
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$andMap,
			c,
			A2(
				$folkertdev$elm_flate$Inflate$BitReader$andMap,
				b,
				A2(
					$folkertdev$elm_flate$Inflate$BitReader$andMap,
					a,
					$folkertdev$elm_flate$Inflate$BitReader$succeed(f))));
	});
var $folkertdev$elm_flate$Inflate$Internal$decodeTrees = A2(
	$folkertdev$elm_flate$Inflate$BitReader$andThen,
	$elm$core$Basics$identity,
	A4(
		$folkertdev$elm_flate$Inflate$BitReader$map3,
		$folkertdev$elm_flate$Inflate$Internal$cont,
		A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 5, 257),
		A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 5, 1),
		A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 4, 4)));
var $folkertdev$elm_flate$Inflate$BitReader$error = function (e) {
	return $folkertdev$elm_flate$Inflate$BitReader$BitReader(
		function (s) {
			return $elm$core$Result$Err(e);
		});
};
var $folkertdev$elm_flate$Inflate$BitReader$getBit = A2($folkertdev$elm_flate$Inflate$BitReader$readBits, 1, 0);
var $folkertdev$elm_flate$Experimental$ByteArray$get = F2(
	function (index, _v0) {
		var array = _v0.a;
		var finalSize = _v0.b;
		var finalBytes = _v0.c;
		var offset = index % 4;
		if (_Utils_cmp(
			index,
			($elm$core$Array$length(array) * 4) + finalSize) > -1) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (_Utils_cmp(
				index,
				$elm$core$Array$length(array) * 4) > -1) {
				return $elm$core$Maybe$Just(255 & (finalBytes >>> (8 * (3 - offset))));
			} else {
				var internalIndex = (index / 4) | 0;
				var _v1 = A2($elm$core$Array$get, internalIndex, array);
				if (_v1.$ === 'Nothing') {
					return $elm$core$Maybe$Nothing;
				} else {
					var int32 = _v1.a;
					return $elm$core$Maybe$Just(255 & (int32 >>> (8 * (3 - offset))));
				}
			}
		}
	});
var $folkertdev$elm_flate$Experimental$ByteArray$copyToBackInternal = F5(
	function (startIndex, size, array, finalSize, finalBytes) {
		copyToBackInternal:
		while (true) {
			var offset = startIndex % 4;
			var internalIndex = (startIndex / 4) | 0;
			if (size <= 0) {
				return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize, finalBytes);
			} else {
				if (_Utils_cmp(
					startIndex + 4,
					(($elm$core$Array$length(array) - 1) * 4) + finalSize) > -1) {
					var _v0 = A2(
						$folkertdev$elm_flate$Experimental$ByteArray$get,
						startIndex,
						A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize, finalBytes));
					if (_v0.$ === 'Nothing') {
						return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize, finalBytes);
					} else {
						var value = _v0.a;
						var _v1 = A2(
							$folkertdev$elm_flate$Experimental$ByteArray$push,
							value,
							A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize, finalBytes));
						var newArray = _v1.a;
						var newFinalSize = _v1.b;
						var newFinalBytes = _v1.c;
						var $temp$startIndex = startIndex + 1,
							$temp$size = size - 1,
							$temp$array = newArray,
							$temp$finalSize = newFinalSize,
							$temp$finalBytes = newFinalBytes;
						startIndex = $temp$startIndex;
						size = $temp$size;
						array = $temp$array;
						finalSize = $temp$finalSize;
						finalBytes = $temp$finalBytes;
						continue copyToBackInternal;
					}
				} else {
					var _v2 = A2($elm$core$Array$get, internalIndex, array);
					if (_v2.$ === 'Nothing') {
						return A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize, finalBytes);
					} else {
						var value = _v2.a;
						var written = A2($elm$core$Basics$min, 4 - offset, size);
						var maskedFront = value << (8 * offset);
						var maskedBack = function () {
							if (_Utils_cmp(4 - offset, size) > 0) {
								var bytesWeNeedToRemove = 4 - size;
								var bytesWeHave = (3 - offset) + 1;
								return maskedFront >> (bytesWeNeedToRemove * 8);
							} else {
								return maskedFront >> (offset * 8);
							}
						}();
						var _v3 = A3(
							$folkertdev$elm_flate$Experimental$ByteArray$pushMany,
							written,
							maskedBack,
							A3($folkertdev$elm_flate$Experimental$ByteArray$ByteArray, array, finalSize, finalBytes));
						var x = _v3.a;
						var y = _v3.b;
						var z = _v3.c;
						var $temp$startIndex = startIndex + written,
							$temp$size = size - written,
							$temp$array = x,
							$temp$finalSize = y,
							$temp$finalBytes = z;
						startIndex = $temp$startIndex;
						size = $temp$size;
						array = $temp$array;
						finalSize = $temp$finalSize;
						finalBytes = $temp$finalBytes;
						continue copyToBackInternal;
					}
				}
			}
		}
	});
var $folkertdev$elm_flate$Experimental$ByteArray$copyToBack = F3(
	function (startIndex, size, _v0) {
		var array = _v0.a;
		var finalSize = _v0.b;
		var finalBytes = _v0.c;
		return A5($folkertdev$elm_flate$Experimental$ByteArray$copyToBackInternal, startIndex, size, array, finalSize, finalBytes);
	});
var $folkertdev$elm_flate$Inflate$Internal$HuffmanTable = function (a) {
	return {$: 'HuffmanTable', a: a};
};
var $elm$core$Array$foldl = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldl, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldl,
			func,
			A3($elm$core$Elm$JsArray$foldl, helper, baseCase, tree),
			tail);
	});
var $folkertdev$elm_flate$Inflate$Internal$buildBitsBase = F2(
	function (delta, first) {
		var initializer = function (i) {
			return (_Utils_cmp(i, delta) < 0) ? 0 : (((i - delta) / delta) | 0);
		};
		var folder = F2(
			function (bit, _v0) {
				var sum = _v0.a;
				var accum = _v0.b;
				return _Utils_Tuple2(
					sum + (1 << bit),
					A2(
						$elm$core$Array$push,
						{base: sum, bits: bit},
						accum));
			});
		var bits = A2($elm$core$Array$initialize, 30, initializer);
		var base = A3(
			$elm$core$Array$foldl,
			folder,
			_Utils_Tuple2(first, $elm$core$Array$empty),
			bits).b;
		return $folkertdev$elm_flate$Inflate$Internal$HuffmanTable(base);
	});
var $folkertdev$elm_flate$Inflate$Internal$hardcodedDistanceTable = A2($folkertdev$elm_flate$Inflate$Internal$buildBitsBase, 2, 1);
var $folkertdev$elm_flate$Inflate$Internal$hardcodedLengthTable = function (_v0) {
	var array = _v0.a;
	return $folkertdev$elm_flate$Inflate$Internal$HuffmanTable(
		A3(
			$elm$core$Array$set,
			28,
			{base: 258, bits: 0},
			array));
}(
	A2($folkertdev$elm_flate$Inflate$Internal$buildBitsBase, 4, 3));
var $folkertdev$elm_flate$Inflate$Internal$readHuffmanTable = F2(
	function (index, _v0) {
		var table = _v0.a;
		return A2($elm$core$Array$get, index, table);
	});
var $folkertdev$elm_flate$Inflate$Internal$decodeLength = function (symbol) {
	var _v0 = A2($folkertdev$elm_flate$Inflate$Internal$readHuffmanTable, symbol - 257, $folkertdev$elm_flate$Inflate$Internal$hardcodedLengthTable);
	if (_v0.$ === 'Nothing') {
		return $folkertdev$elm_flate$Inflate$BitReader$error(
			function () {
				var _v1 = $folkertdev$elm_flate$Inflate$Internal$hardcodedDistanceTable;
				var internal = _v1.a;
				return 'index out of bounds in hardcodedLengthTable: requested index ' + ($elm$core$String$fromInt(symbol - 257) + ('but hardcodedLengthTable has length ' + $elm$core$String$fromInt(
					$elm$core$Array$length(internal))));
			}());
	} else {
		var entry = _v0.a;
		return A2($folkertdev$elm_flate$Inflate$BitReader$readBits, entry.bits, entry.base);
	}
};
var $folkertdev$elm_flate$Inflate$Internal$decodeOffset = F2(
	function (outputLength, dt) {
		var table_ = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			$elm$core$List$tail(dt.table));
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$andThen,
			function (distance) {
				var _v0 = A2($folkertdev$elm_flate$Inflate$Internal$readHuffmanTable, distance, $folkertdev$elm_flate$Inflate$Internal$hardcodedDistanceTable);
				if (_v0.$ === 'Nothing') {
					return $folkertdev$elm_flate$Inflate$BitReader$error(
						function () {
							var _v1 = $folkertdev$elm_flate$Inflate$Internal$hardcodedDistanceTable;
							var internal = _v1.a;
							return 'index out of bounds in hardcodedDistanceTable: requested index ' + ($elm$core$String$fromInt(distance) + ('but hardcodedLengthTable has length ' + $elm$core$String$fromInt(
								$elm$core$Array$length(internal))));
						}());
				} else {
					var entry = _v0.a;
					return A2(
						$folkertdev$elm_flate$Inflate$BitReader$map,
						function (v) {
							return outputLength - v;
						},
						A2($folkertdev$elm_flate$Inflate$BitReader$readBits, entry.bits, entry.base));
				}
			},
			A2($folkertdev$elm_flate$Inflate$Internal$decodeSymbol, table_, dt));
	});
var $folkertdev$elm_flate$Inflate$Internal$inflateBlockDataHelp = F2(
	function (trees, _v0) {
		var outputLength = _v0.a;
		var output = _v0.b;
		var table = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			$elm$core$List$tail(trees.literal.table));
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$andThen,
			function (symbol) {
				return (symbol === 256) ? $folkertdev$elm_flate$Inflate$BitReader$succeed(
					$elm$bytes$Bytes$Decode$Done(output)) : ((symbol < 256) ? $folkertdev$elm_flate$Inflate$BitReader$succeed(
					$elm$bytes$Bytes$Decode$Loop(
						_Utils_Tuple2(
							outputLength + 1,
							A2($folkertdev$elm_flate$Experimental$ByteArray$push, symbol, output)))) : A3(
					$folkertdev$elm_flate$Inflate$BitReader$map2,
					F2(
						function (length, offset) {
							return $elm$bytes$Bytes$Decode$Loop(
								_Utils_Tuple2(
									outputLength + length,
									A3($folkertdev$elm_flate$Experimental$ByteArray$copyToBack, offset, length, output)));
						}),
					$folkertdev$elm_flate$Inflate$Internal$decodeLength(symbol),
					A2($folkertdev$elm_flate$Inflate$Internal$decodeOffset, outputLength, trees.distance)));
			},
			A2($folkertdev$elm_flate$Inflate$Internal$decodeSymbol, table, trees.literal));
	});
var $folkertdev$elm_flate$Inflate$Internal$inflateBlockData = F3(
	function (trees, outputLength, output) {
		return A2(
			$folkertdev$elm_flate$Inflate$BitReader$loop,
			_Utils_Tuple2(outputLength, output),
			$folkertdev$elm_flate$Inflate$Internal$inflateBlockDataHelp(trees));
	});
var $folkertdev$elm_flate$Inflate$Internal$uncompressedBlockDecoder = function (bufferWidth) {
	var decodeLengths = A3(
		$elm$bytes$Bytes$Decode$map2,
		$elm$core$Tuple$pair,
		$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE),
		$elm$bytes$Bytes$Decode$unsignedInt16($elm$bytes$Bytes$LE));
	return A2(
		$elm$bytes$Bytes$Decode$andThen,
		function (_v0) {
			var length = _v0.a;
			var invlength = _v0.b;
			if (!_Utils_eq(length, (~invlength) & 65535)) {
				return $elm$bytes$Bytes$Decode$fail;
			} else {
				var remainingSize = (bufferWidth - 4) - length;
				return A3(
					$elm$bytes$Bytes$Decode$map2,
					$elm$core$Tuple$pair,
					$elm$bytes$Bytes$Decode$bytes(length),
					$elm$bytes$Bytes$Decode$bytes(remainingSize));
			}
		},
		decodeLengths);
};
var $folkertdev$elm_flate$Inflate$Internal$inflateUncompressedBlock = $folkertdev$elm_flate$Inflate$BitReader$BitReader(
	function (state) {
		var _v0 = A2(
			$elm$bytes$Bytes$Decode$decode,
			$folkertdev$elm_flate$Inflate$Internal$uncompressedBlockDecoder(
				$elm$bytes$Bytes$width(state.buffer)),
			state.buffer);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Result$Err('inflateUncompressedBlock: ran out of bounds');
		} else {
			var _v1 = _v0.a;
			var block = _v1.a;
			var newBuffer = _v1.b;
			return $elm$core$Result$Ok(
				_Utils_Tuple2(
					block,
					_Utils_update(
						state,
						{buffer: newBuffer})));
		}
	});
var $folkertdev$elm_flate$Experimental$ByteArray$length = function (_v0) {
	var array = _v0.a;
	var finalSize = _v0.b;
	var finalBytes = _v0.c;
	var _v1 = $elm$core$Array$length(array) * 4;
	if (!_v1) {
		return finalSize;
	} else {
		var l = _v1;
		return l + finalSize;
	}
};
var $folkertdev$elm_flate$Inflate$Internal$sdtree = {
	table: _List_fromArray(
		[0, 0, 0, 0, 0, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]),
	trans: A2(
		$elm$core$Array$append,
		$elm$core$Array$fromList(
			_List_fromArray(
				[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31])),
		A2($elm$core$Array$repeat, 288 - 32, 0))
};
var $folkertdev$elm_flate$Inflate$BitReader$flushHelp = function (state0) {
	var availableSpace = 32 - state0.bitsAvailable;
	var state = A2(
		$folkertdev$elm_flate$Inflate$BitReader$moveFromReserve,
		A2($elm$core$Basics$min, availableSpace, state0.reserveAvailable),
		state0);
	var reserveEncoder = (state.reserveAvailable > 24) ? _List_fromArray(
		[
			A2($elm$bytes$Bytes$Encode$unsignedInt32, $elm$bytes$Bytes$LE, state.reserve)
		]) : ((state.reserveAvailable > 16) ? _List_fromArray(
		[
			A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$LE, state.reserve),
			$elm$bytes$Bytes$Encode$unsignedInt8(state.reserve >> 16)
		]) : ((state.reserveAvailable > 8) ? _List_fromArray(
		[
			A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$LE, state.reserve)
		]) : ((state.reserveAvailable > 1) ? _List_fromArray(
		[
			$elm$bytes$Bytes$Encode$unsignedInt8(state.reserve)
		]) : _List_Nil)));
	var tagEncoder = (state.bitsAvailable > 24) ? _List_fromArray(
		[
			A2($elm$bytes$Bytes$Encode$unsignedInt32, $elm$bytes$Bytes$LE, state.tag)
		]) : ((state.bitsAvailable > 16) ? _List_fromArray(
		[
			A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$LE, state.tag),
			$elm$bytes$Bytes$Encode$unsignedInt8(state.tag >> 16)
		]) : ((state.bitsAvailable > 8) ? _List_fromArray(
		[
			A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$LE, state.tag)
		]) : ((state.bitsAvailable > 1) ? _List_fromArray(
		[
			$elm$bytes$Bytes$Encode$unsignedInt8(state.tag)
		]) : _List_Nil)));
	return $elm$bytes$Bytes$Encode$encode(
		$elm$bytes$Bytes$Encode$sequence(
			_Utils_ap(
				tagEncoder,
				_Utils_ap(
					reserveEncoder,
					_List_fromArray(
						[
							$elm$bytes$Bytes$Encode$bytes(state.buffer)
						])))));
};
var $folkertdev$elm_flate$Inflate$BitReader$flush = function (state) {
	return {
		bitsAvailable: 0,
		buffer: $folkertdev$elm_flate$Inflate$BitReader$flushHelp(state),
		reserve: 0,
		reserveAvailable: 0,
		tag: 0
	};
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $folkertdev$elm_flate$Inflate$BitReader$skipToByteBoundary = $folkertdev$elm_flate$Inflate$BitReader$BitReader(
	function (s) {
		var available = s.bitsAvailable + s.reserveAvailable;
		var untilBoundary = A2($elm$core$Basics$modBy, 8, available);
		var _v0 = A2($folkertdev$elm_flate$Inflate$BitReader$readBits, untilBoundary, 0);
		var step = _v0.a;
		var _v1 = step(s);
		if (_v1.$ === 'Err') {
			var e = _v1.a;
			return $elm$core$Result$Err(e);
		} else {
			var _v2 = _v1.a;
			var newState = _v2.b;
			var _v3 = $folkertdev$elm_flate$Inflate$BitReader$readMoreBits(newState);
			if (_v3.$ === 'Err') {
				var e = _v3.a;
				return $elm$core$Result$Err(e);
			} else {
				var newerState = _v3.a;
				return $elm$core$Result$Ok(
					_Utils_Tuple2(
						_Utils_Tuple0,
						$folkertdev$elm_flate$Inflate$BitReader$flush(newerState)));
			}
		}
	});
var $folkertdev$elm_flate$Inflate$Internal$sltree = {
	table: _List_fromArray(
		[0, 0, 0, 0, 0, 0, 0, 24, 152, 112, 0, 0, 0, 0, 0, 0]),
	trans: $elm$core$Array$fromList(
		_List_fromArray(
			[256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 280, 281, 282, 283, 284, 285, 286, 287, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255]))
};
var $folkertdev$elm_flate$Inflate$Internal$uncompressHelp = function (output) {
	var uncompressBlock = function (btype) {
		switch (btype) {
			case 0:
				return A2(
					$folkertdev$elm_flate$Inflate$BitReader$map,
					function (bytes) {
						return A2($folkertdev$elm_flate$Experimental$ByteArray$appendBytes, bytes, output);
					},
					A2(
						$folkertdev$elm_flate$Inflate$BitReader$andThen,
						function (_v1) {
							return $folkertdev$elm_flate$Inflate$Internal$inflateUncompressedBlock;
						},
						$folkertdev$elm_flate$Inflate$BitReader$skipToByteBoundary));
			case 1:
				return A3(
					$folkertdev$elm_flate$Inflate$Internal$inflateBlockData,
					{distance: $folkertdev$elm_flate$Inflate$Internal$sdtree, literal: $folkertdev$elm_flate$Inflate$Internal$sltree},
					$folkertdev$elm_flate$Experimental$ByteArray$length(output),
					output);
			case 2:
				return A2(
					$folkertdev$elm_flate$Inflate$BitReader$andThen,
					function (_v2) {
						var ltree = _v2.a;
						var dtree = _v2.b;
						return A3(
							$folkertdev$elm_flate$Inflate$Internal$inflateBlockData,
							{distance: dtree, literal: ltree},
							$folkertdev$elm_flate$Experimental$ByteArray$length(output),
							output);
					},
					$folkertdev$elm_flate$Inflate$Internal$decodeTrees);
			default:
				return $folkertdev$elm_flate$Inflate$BitReader$error(
					'invalid block type: ' + ($elm$core$String$fromInt(btype) + ' (only 0, 1 and 2 are valid block types)'));
		}
	};
	var readTwoBits = A3(
		$folkertdev$elm_flate$Inflate$BitReader$map2,
		F2(
			function (b1, b2) {
				return b1 + (2 * b2);
			}),
		$folkertdev$elm_flate$Inflate$BitReader$getBit,
		$folkertdev$elm_flate$Inflate$BitReader$getBit);
	var go = F2(
		function (isFinal, blockType) {
			return (!(!isFinal)) ? A2(
				$folkertdev$elm_flate$Inflate$BitReader$map,
				$elm$bytes$Bytes$Decode$Done,
				uncompressBlock(blockType)) : A2(
				$folkertdev$elm_flate$Inflate$BitReader$map,
				$elm$bytes$Bytes$Decode$Loop,
				uncompressBlock(blockType));
		});
	return A2(
		$folkertdev$elm_flate$Inflate$BitReader$andThen,
		$elm$core$Basics$identity,
		A3($folkertdev$elm_flate$Inflate$BitReader$map2, go, $folkertdev$elm_flate$Inflate$BitReader$getBit, readTwoBits));
};
var $folkertdev$elm_flate$Inflate$Internal$uncompress = A2(
	$folkertdev$elm_flate$Inflate$BitReader$map,
	A2($elm$core$Basics$composeR, $folkertdev$elm_flate$Experimental$ByteArray$toBytes, $elm$core$List$singleton),
	A2($folkertdev$elm_flate$Inflate$BitReader$loop, $folkertdev$elm_flate$Experimental$ByteArray$empty, $folkertdev$elm_flate$Inflate$Internal$uncompressHelp));
var $folkertdev$elm_flate$Inflate$Internal$inflate = function (buffer) {
	var _v0 = A2($folkertdev$elm_flate$Inflate$BitReader$decode, buffer, $folkertdev$elm_flate$Inflate$Internal$uncompress);
	if (_v0.$ === 'Err') {
		var e = _v0.a;
		return $elm$core$Result$Err(e);
	} else {
		var values = _v0.a;
		return $elm$core$Result$Ok(
			$elm$bytes$Bytes$Encode$encode(
				$elm$bytes$Bytes$Encode$sequence(
					A2($elm$core$List$map, $elm$bytes$Bytes$Encode$bytes, values))));
	}
};
var $folkertdev$elm_flate$Inflate$ZLib$Slices = F4(
	function (cmf, flg, buffer, a32) {
		return {a32: a32, buffer: buffer, cmf: cmf, flg: flg};
	});
var $folkertdev$elm_flate$Inflate$ZLib$decodeAdler32Checksum = $elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE);
var $folkertdev$elm_flate$Inflate$ZLib$slice = function (buffer) {
	var decoder = A5(
		$elm$bytes$Bytes$Decode$map4,
		$folkertdev$elm_flate$Inflate$ZLib$Slices,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$bytes(
			($elm$bytes$Bytes$width(buffer) - 2) - 4),
		$folkertdev$elm_flate$Inflate$ZLib$decodeAdler32Checksum);
	return A2($elm$bytes$Bytes$Decode$decode, decoder, buffer);
};
var $folkertdev$elm_flate$Inflate$ZLib$inflate = function (buffer) {
	var _v0 = $folkertdev$elm_flate$Inflate$ZLib$slice(buffer);
	if (_v0.$ === 'Nothing') {
		return $elm$core$Result$Err($folkertdev$elm_flate$Inflate$ZLib$InvalidSlice);
	} else {
		var sliced = _v0.a;
		if (!(!A2($elm$core$Basics$modBy, 31, (256 * sliced.cmf) + sliced.flg))) {
			return $elm$core$Result$Err($folkertdev$elm_flate$Inflate$ZLib$InvalidChecksum);
		} else {
			if ((sliced.cmf & 15) !== 8) {
				return $elm$core$Result$Err($folkertdev$elm_flate$Inflate$ZLib$InvalidMethod);
			} else {
				if ((sliced.cmf >> 4) > 7) {
					return $elm$core$Result$Err($folkertdev$elm_flate$Inflate$ZLib$InvalidWindowSize);
				} else {
					if (!(!(sliced.flg & 32))) {
						return $elm$core$Result$Err($folkertdev$elm_flate$Inflate$ZLib$PresetDictionaryIssue);
					} else {
						var _v1 = $folkertdev$elm_flate$Inflate$Internal$inflate(sliced.buffer);
						if (_v1.$ === 'Err') {
							var e = _v1.a;
							return $elm$core$Result$Err(
								$folkertdev$elm_flate$Inflate$ZLib$InflateError(e));
						} else {
							var resultBuffer = _v1.a;
							var found = $folkertdev$elm_flate$Checksum$Adler32$adler32(resultBuffer);
							return (!_Utils_eq(sliced.a32, found)) ? $elm$core$Result$Err(
								$folkertdev$elm_flate$Inflate$ZLib$InvalidAdler(
									{found: found, shouldBe: sliced.a32})) : $elm$core$Result$Ok(resultBuffer);
						}
					}
				}
			}
		}
	}
};
var $folkertdev$elm_flate$Inflate$Inflate$inflateZLib = function (buffer) {
	var _v0 = $folkertdev$elm_flate$Inflate$ZLib$inflate(buffer);
	if (_v0.$ === 'Err') {
		var e = _v0.a;
		return $elm$core$Maybe$Nothing;
	} else {
		var value = _v0.a;
		return $elm$core$Maybe$Just(value);
	}
};
var $folkertdev$elm_flate$Flate$inflateZlib = $folkertdev$elm_flate$Inflate$Inflate$inflateZLib;
var $justgook$elm_image$Image$Internal$PNG$imageDecoder = F3(
	function (header, palette, total) {
		var width = header.width;
		var height = header.height;
		var color = header.color;
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			A2(
				$elm$core$Basics$composeR,
				$folkertdev$elm_flate$Flate$inflateZlib,
				A2(
					$elm$core$Basics$composeR,
					$elm$core$Maybe$andThen(
						$elm$bytes$Bytes$Decode$decode(
							A2($justgook$elm_image$Image$Internal$PNG$dataDecode, header, palette))),
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Maybe$map($elm$bytes$Bytes$Decode$succeed),
						$elm$core$Maybe$withDefault($elm$bytes$Bytes$Decode$fail)))),
			$elm$bytes$Bytes$Decode$bytes(total));
	});
var $justgook$elm_image$Image$Internal$PNG$decodeIEND = F2(
	function (image, length) {
		var header = image.header;
		var palette = image.palette;
		return A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				var _v1 = image.data;
				if (_v1.$ === 'Collecting') {
					var e = _v1.a;
					var bytes = $elm$bytes$Bytes$Encode$encode(e);
					var output = A2(
						$justgook$elm_image$Image$Internal$ImageData$Lazy,
						$justgook$elm_image$Image$Internal$Meta$Png(header),
						function (info) {
							return A2(
								$elm$core$Maybe$withDefault,
								A2($justgook$elm_image$Image$Internal$ImageData$ImageRaw, info, $elm$core$Array$empty),
								A2(
									$elm$bytes$Bytes$Decode$decode,
									A3(
										$justgook$elm_image$Image$Internal$PNG$imageDecoder,
										header,
										palette,
										$elm$bytes$Bytes$width(bytes)),
									bytes));
						});
					return $elm$bytes$Bytes$Decode$succeed(
						_Utils_update(
							image,
							{
								data: $justgook$elm_image$Image$Internal$PNG$ImageData(output)
							}));
				} else {
					return $elm$bytes$Bytes$Decode$fail;
				}
			},
			$elm$bytes$Bytes$Decode$bytes(length));
	});
var $justgook$elm_image$Image$Internal$PNG$None = {$: 'None'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth8_16__8 = {$: 'BitDepth8_16__8'};
var $justgook$elm_image$Image$Internal$Meta$Greyscale = function (a) {
	return {$: 'Greyscale', a: a};
};
var $justgook$elm_image$Image$Internal$Meta$GreyscaleAlpha = function (a) {
	return {$: 'GreyscaleAlpha', a: a};
};
var $justgook$elm_image$Image$Internal$Meta$IndexedColour = function (a) {
	return {$: 'IndexedColour', a: a};
};
var $justgook$elm_image$Image$Internal$Meta$TrueColour = function (a) {
	return {$: 'TrueColour', a: a};
};
var $justgook$elm_image$Image$Internal$Meta$TrueColourAlpha = function (a) {
	return {$: 'TrueColourAlpha', a: a};
};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__1 = {$: 'BitDepth1_2_4_8__1'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__2 = {$: 'BitDepth1_2_4_8__2'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__4 = {$: 'BitDepth1_2_4_8__4'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__8 = {$: 'BitDepth1_2_4_8__8'};
var $justgook$elm_image$Image$Internal$PNG$fromIntBitDepth1_2_4_8 = function (i) {
	switch (i) {
		case 1:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__1);
		case 2:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__2);
		case 4:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__4);
		case 8:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8__8);
		default:
			return $elm$bytes$Bytes$Decode$fail;
	}
};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__1 = {$: 'BitDepth1_2_4_8_16__1'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__16 = {$: 'BitDepth1_2_4_8_16__16'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__2 = {$: 'BitDepth1_2_4_8_16__2'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__4 = {$: 'BitDepth1_2_4_8_16__4'};
var $justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__8 = {$: 'BitDepth1_2_4_8_16__8'};
var $justgook$elm_image$Image$Internal$PNG$fromIntBitDepth1_2_4_8_16 = function (i) {
	switch (i) {
		case 1:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__1);
		case 2:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__2);
		case 4:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__4);
		case 8:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__8);
		case 6:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth1_2_4_8_16__16);
		default:
			return $elm$bytes$Bytes$Decode$fail;
	}
};
var $justgook$elm_image$Image$Internal$Meta$BitDepth8_16__16 = {$: 'BitDepth8_16__16'};
var $justgook$elm_image$Image$Internal$PNG$fromIntBitDepth8_16 = function (i) {
	switch (i) {
		case 8:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth8_16__8);
		case 16:
			return $elm$bytes$Bytes$Decode$succeed($justgook$elm_image$Image$Internal$Meta$BitDepth8_16__16);
		default:
			return $elm$bytes$Bytes$Decode$fail;
	}
};
var $justgook$elm_image$Image$Internal$PNG$decodeIHDRColor = A2(
	$elm$bytes$Bytes$Decode$andThen,
	$elm$core$Basics$identity,
	A3(
		$elm$bytes$Bytes$Decode$map2,
		F2(
			function (depth, color) {
				switch (color) {
					case 0:
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$Meta$Greyscale,
							$justgook$elm_image$Image$Internal$PNG$fromIntBitDepth1_2_4_8_16(depth));
					case 2:
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$Meta$TrueColour,
							$justgook$elm_image$Image$Internal$PNG$fromIntBitDepth8_16(depth));
					case 3:
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$Meta$IndexedColour,
							$justgook$elm_image$Image$Internal$PNG$fromIntBitDepth1_2_4_8(depth));
					case 4:
						return A2(
							$elm$bytes$Bytes$Decode$map,
							$justgook$elm_image$Image$Internal$Meta$GreyscaleAlpha,
							$justgook$elm_image$Image$Internal$PNG$fromIntBitDepth8_16(depth));
					case 6:
						return $elm$bytes$Bytes$Decode$succeed(
							$justgook$elm_image$Image$Internal$Meta$TrueColourAlpha($justgook$elm_image$Image$Internal$Meta$BitDepth8_16__8));
					default:
						return $elm$bytes$Bytes$Decode$fail;
				}
			}),
		$elm$bytes$Bytes$Decode$unsignedInt8,
		$elm$bytes$Bytes$Decode$unsignedInt8));
var $justgook$elm_image$Image$Internal$PNG$decodeInterlace = A2(
	$elm$bytes$Bytes$Decode$andThen,
	function (interlace) {
		switch (interlace) {
			case 0:
				return $elm$bytes$Bytes$Decode$succeed(false);
			case 1:
				return $elm$bytes$Bytes$Decode$succeed(true);
			default:
				return $elm$bytes$Bytes$Decode$fail;
		}
	},
	$elm$bytes$Bytes$Decode$unsignedInt8);
var $justgook$elm_image$Image$Internal$PNG$decodeIHDR = A2(
	$justgook$elm_image$Image$Internal$Decode$andMap,
	$justgook$elm_image$Image$Internal$PNG$decodeInterlace,
	A2(
		$justgook$elm_image$Image$Internal$Decode$andMap,
		$elm$bytes$Bytes$Decode$unsignedInt8,
		A2(
			$justgook$elm_image$Image$Internal$Decode$andMap,
			$elm$bytes$Bytes$Decode$unsignedInt8,
			A2(
				$justgook$elm_image$Image$Internal$Decode$andMap,
				$justgook$elm_image$Image$Internal$PNG$decodeIHDRColor,
				A2(
					$justgook$elm_image$Image$Internal$Decode$andMap,
					$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE),
					A2(
						$justgook$elm_image$Image$Internal$Decode$andMap,
						$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE),
						$elm$bytes$Bytes$Decode$succeed(
							F6(
								function (width, height, color, _v0, _v1, adam7) {
									return {
										data: $justgook$elm_image$Image$Internal$PNG$None,
										header: {adam7: adam7, chunks: $elm$core$Dict$empty, color: color, height: height, width: width},
										palette: $elm$core$Array$empty
									};
								}))))))));
var $justgook$elm_image$Image$Internal$PNG$decodePLTE = F2(
	function (image, length) {
		return A2(
			$elm$bytes$Bytes$Decode$map,
			function (palette) {
				return _Utils_update(
					image,
					{palette: palette});
			},
			A2(
				$justgook$elm_image$Image$Internal$Decode$array,
				(length / 3) | 0,
				$justgook$elm_image$Image$Internal$Decode$unsignedInt24($elm$bytes$Bytes$BE)));
	});
var $justgook$elm_image$Image$Internal$PNG$saveChunk = F3(
	function (image, kind, value) {
		var header = image.header;
		var name = A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$bytes$Bytes$Decode$decode,
				$elm$bytes$Bytes$Decode$string(4),
				$elm$bytes$Bytes$Encode$encode(
					A2($elm$bytes$Bytes$Encode$unsignedInt32, $elm$bytes$Bytes$BE, kind))));
		return _Utils_update(
			image,
			{
				header: _Utils_update(
					header,
					{
						chunks: A3($elm$core$Dict$insert, name, value, header.chunks)
					})
			});
	});
var $justgook$elm_image$Image$Internal$PNG$decodeUnknown = F3(
	function (kind, image, length) {
		var header = image.header;
		return A2(
			$elm$bytes$Bytes$Decode$map,
			A2($justgook$elm_image$Image$Internal$PNG$saveChunk, image, kind),
			$elm$bytes$Bytes$Decode$bytes(length));
	});
var $justgook$elm_image$Image$Internal$PNG$arrayExtraUpdate = F3(
	function (n, f, a) {
		var element = A2($elm$core$Array$get, n, a);
		if (element.$ === 'Nothing') {
			return a;
		} else {
			var element_ = element.a;
			return A3(
				$elm$core$Array$set,
				n,
				f(element_),
				a);
		}
	});
var $justgook$elm_image$Image$Internal$PNG$decode_tRNS = F2(
	function (image, length) {
		var _v0 = image.header.color;
		if (_v0.$ === 'IndexedColour') {
			return A2(
				$elm$bytes$Bytes$Decode$map,
				function (_v2) {
					var palette = _v2.b;
					return _Utils_update(
						image,
						{palette: palette});
				},
				A3(
					$justgook$elm_image$Image$Internal$Decode$foldl,
					length,
					function (_v1) {
						var i = _v1.a;
						var acc = _v1.b;
						return A2(
							$elm$bytes$Bytes$Decode$map,
							function (alpha) {
								return _Utils_Tuple2(
									i + 1,
									A3(
										$justgook$elm_image$Image$Internal$PNG$arrayExtraUpdate,
										i,
										A2(
											$elm$core$Basics$composeR,
											$elm$core$Bitwise$shiftLeftBy(8),
											A2(
												$elm$core$Basics$composeR,
												$elm$core$Bitwise$or(alpha),
												$elm$core$Bitwise$shiftRightZfBy(0))),
										acc));
							},
							$elm$bytes$Bytes$Decode$unsignedInt8);
					},
					_Utils_Tuple2(0, image.palette)));
		} else {
			return $elm$bytes$Bytes$Decode$fail;
		}
	});
var $justgook$elm_image$Image$Internal$PNG$chunkCollector = F3(
	function (length, acc, kind) {
		var _v0 = _Utils_Tuple3(acc, kind, length);
		if (_v0.a.$ === 'Nothing') {
			if ((_v0.b === 1229472850) && (_v0.c === 13)) {
				var _v1 = _v0.a;
				return A2($elm$bytes$Bytes$Decode$map, $elm$core$Maybe$Just, $justgook$elm_image$Image$Internal$PNG$decodeIHDR);
			} else {
				return $elm$bytes$Bytes$Decode$fail;
			}
		} else {
			switch (_v0.b) {
				case 1347179589:
					var image = _v0.a.a;
					return A2(
						$elm$bytes$Bytes$Decode$map,
						$elm$core$Maybe$Just,
						A2($justgook$elm_image$Image$Internal$PNG$decodePLTE, image, length));
				case 1951551059:
					var image = _v0.a.a;
					return A2(
						$elm$bytes$Bytes$Decode$map,
						$elm$core$Maybe$Just,
						A2($justgook$elm_image$Image$Internal$PNG$decode_tRNS, image, length));
				case 1229209940:
					var image = _v0.a.a;
					return A2(
						$elm$bytes$Bytes$Decode$map,
						$elm$core$Maybe$Just,
						A2($justgook$elm_image$Image$Internal$PNG$decodeIDAT, image, length));
				case 1229278788:
					var image = _v0.a.a;
					return A2(
						$elm$bytes$Bytes$Decode$map,
						$elm$core$Maybe$Just,
						A2($justgook$elm_image$Image$Internal$PNG$decodeIEND, image, length));
				default:
					var image = _v0.a.a;
					return A2(
						$elm$bytes$Bytes$Decode$map,
						$elm$core$Maybe$Just,
						A3($justgook$elm_image$Image$Internal$PNG$decodeUnknown, kind, image, length));
			}
		}
	});
var $justgook$elm_image$Image$Internal$PNG$decodeChunk = function (acc) {
	return A2(
		$elm$bytes$Bytes$Decode$andThen,
		$elm$core$Basics$identity,
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (length) {
				return A3(
					$elm$bytes$Bytes$Decode$map2,
					F2(
						function (kindAndData, crc) {
							return A2(
								$elm$bytes$Bytes$Decode$map,
								$elm$core$Tuple$pair(length + 12),
								A2(
									$elm$core$Maybe$withDefault,
									$elm$bytes$Bytes$Decode$fail,
									A2(
										$elm$core$Maybe$map,
										$elm$bytes$Bytes$Decode$succeed,
										A2(
											$elm$bytes$Bytes$Decode$decode,
											A2(
												$elm$bytes$Bytes$Decode$andThen,
												A2($justgook$elm_image$Image$Internal$PNG$chunkCollector, length, acc),
												$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE)),
											kindAndData))));
						}),
					$elm$bytes$Bytes$Decode$bytes(length + 4),
					$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE));
			},
			$elm$bytes$Bytes$Decode$unsignedInt32($elm$bytes$Bytes$BE)));
};
var $justgook$elm_image$Image$Internal$PNG$mainDecoder = function (chunksLength) {
	return A2(
		$elm$bytes$Bytes$Decode$andThen,
		function (maybeImage) {
			if (maybeImage.$ === 'Just') {
				var image = maybeImage.a;
				var _v2 = image.data;
				if (_v2.$ === 'ImageData') {
					var data = _v2.a;
					return $elm$bytes$Bytes$Decode$succeed(data);
				} else {
					return $elm$bytes$Bytes$Decode$fail;
				}
			} else {
				return $elm$bytes$Bytes$Decode$fail;
			}
		},
		A2(
			$elm$bytes$Bytes$Decode$andThen,
			function (_v0) {
				return A2($justgook$elm_image$Image$Internal$PNG$chunkLoopR, chunksLength, $justgook$elm_image$Image$Internal$PNG$decodeChunk);
			},
			A2(
				$elm$bytes$Bytes$Decode$andThen,
				function (signature) {
					return _Utils_eq(
						signature,
						_List_fromArray(
							[10, 26, 10, 13, 71, 78, 80, 137])) ? $elm$bytes$Bytes$Decode$succeed(_Utils_Tuple0) : $elm$bytes$Bytes$Decode$fail;
				},
				A2($justgook$elm_image$Image$Internal$Decode$listR, 8, $elm$bytes$Bytes$Decode$unsignedInt8))));
};
var $justgook$elm_image$Image$Internal$PNG$decode = function (bytes) {
	return A2(
		$elm$bytes$Bytes$Decode$decode,
		$justgook$elm_image$Image$Internal$PNG$mainDecoder(
			$elm$bytes$Bytes$width(bytes) - 8),
		bytes);
};
var $justgook$elm_image$Image$orThenLazy = F2(
	function (ma, mb) {
		if (mb.$ === 'Nothing') {
			return ma(_Utils_Tuple0);
		} else {
			return mb;
		}
	});
var $justgook$elm_image$Image$decode = function (bytes) {
	return A2(
		$justgook$elm_image$Image$orThenLazy,
		function (_v1) {
			return $justgook$elm_image$Image$Internal$GIF$decode(bytes);
		},
		A2(
			$justgook$elm_image$Image$orThenLazy,
			function (_v0) {
				return $justgook$elm_image$Image$Internal$BMP$decode(bytes);
			},
			$justgook$elm_image$Image$Internal$PNG$decode(bytes)));
};
var $justgook$elm_image$Image$Internal$Meta$dimensions = function (meta) {
	switch (meta.$) {
		case 'Png':
			var width = meta.a.width;
			var height = meta.a.height;
			return {height: height, width: width};
		case 'Bmp':
			var width = meta.a.width;
			var height = meta.a.height;
			return {height: height, width: width};
		case 'Gif':
			var width = meta.a.width;
			var height = meta.a.height;
			return {height: height, width: width};
		default:
			var width = meta.a.width;
			var height = meta.a.height;
			return {height: height, width: width};
	}
};
var $justgook$elm_image$Image$Internal$ImageData$getInfo = function (image) {
	if (image.$ === 'ImageRaw') {
		var meta = image.a;
		return meta;
	} else {
		var meta = image.a;
		return meta;
	}
};
var $justgook$elm_image$Image$Internal$ImageData$dimensions = function (image) {
	return $justgook$elm_image$Image$Internal$Meta$dimensions(
		$justgook$elm_image$Image$Internal$ImageData$getInfo(image));
};
var $justgook$elm_image$Image$dimensions = $justgook$elm_image$Image$Internal$ImageData$dimensions;
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm$core$Debug$log = _Debug_log;
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $author$project$Main$resizeBy = F2(
	function (factor, list) {
		return A2(
			$elm$core$List$filterMap,
			function (_v2) {
				var pos = _v2.a;
				var v = _v2.b;
				return (_Utils_cmp(
					v.value,
					$elm$core$Basics$round(((1 / (factor * factor)) * 4) / 8)) > 0) ? $elm$core$Maybe$Just(
					_Utils_Tuple2(pos, v.color)) : $elm$core$Maybe$Nothing;
			},
			$elm$core$Dict$toList(
				A3(
					$elm$core$List$foldl,
					function (_v0) {
						var _v1 = _v0.a;
						var x = _v1.a;
						var y = _v1.b;
						var color = _v0.b;
						return A2(
							$elm$core$Dict$update,
							_Utils_Tuple2(
								$elm$core$Basics$round(x * factor),
								$elm$core$Basics$round(y * factor)),
							function (maybe) {
								return $elm$core$Maybe$Just(
									A2(
										$elm$core$Maybe$withDefault,
										{color: color, value: 1},
										A2(
											$elm$core$Maybe$map,
											function (v) {
												return _Utils_update(
													v,
													{value: v.value + 1});
											},
											maybe)));
							});
					},
					$elm$core$Dict$empty,
					list)));
	});
var $author$project$Main$size = 150;
var $author$project$Main$stepSize = function (_v0) {
	var width = _v0.width;
	var height = _v0.height;
	return 1 + $elm$core$Basics$round((width * height) / 1000);
};
var $elm$file$File$toBytes = _File_toBytes;
var $avh4$elm_color$Color$black = A4($avh4$elm_color$Color$RgbaSpace, 0 / 255, 0 / 255, 0 / 255, 1.0);
var $author$project$Main$applyMove = F2(
	function (_v0, model) {
		var newPlayer = _v0.a;
		var seed = _v0.b;
		var newPotential = model.potential - 2;
		return _Utils_update(
			model,
			{
				grid: A3(
					$elm$core$Dict$insert,
					newPlayer,
					$elm$core$Maybe$Just(
						{
							color: A2(
								$elm$core$Maybe$withDefault,
								$avh4$elm_color$Color$black,
								A2(
									$elm$core$Dict$get,
									A3(
										$elm$core$Tuple$mapBoth,
										$elm$core$Basics$round,
										$elm$core$Basics$round,
										$author$project$Main$positionOps.toPoint(newPlayer)),
									model.image)),
							from: model.player,
							potential: newPotential,
							to: _List_Nil
						}),
					A3(
						$elm$core$Dict$update,
						model.player,
						$elm$core$Maybe$map(
							$elm$core$Maybe$map(
								function (cell) {
									return _Utils_update(
										cell,
										{
											to: A2($elm$core$List$cons, newPlayer, cell.to)
										});
								})),
						model.grid)),
				player: newPlayer,
				potential: newPotential,
				seed: seed
			});
	});
var $elm$random$Random$map2 = F3(
	function (func, _v0, _v1) {
		var genA = _v0.a;
		var genB = _v1.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v2 = genA(seed0);
				var a = _v2.a;
				var seed1 = _v2.b;
				var _v3 = genB(seed1);
				var b = _v3.a;
				var seed2 = _v3.b;
				return _Utils_Tuple2(
					A2(func, a, b),
					seed2);
			});
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $author$project$Main$moveBack = function (model) {
	var newPotential = A2(
		$elm$core$Basics$min,
		model.potential + 1,
		$author$project$Main$maxPotential(
			{height: model.height, width: model.width}));
	var _v0 = A2($elm$core$Dict$get, model.player, model.grid);
	if ((_v0.$ === 'Just') && (_v0.a.$ === 'Just')) {
		var from = _v0.a.a.from;
		return _Utils_update(
			model,
			{
				grid: A3(
					$elm$core$Dict$update,
					model.player,
					$elm$core$Maybe$map(
						$elm$core$Maybe$map(
							function (cell) {
								return _Utils_update(
									cell,
									{potential: newPotential});
							})),
					model.grid),
				player: from,
				potential: newPotential
			});
	} else {
		var _v1 = A2(
			$elm$random$Random$step,
			A3(
				$elm$random$Random$map2,
				F2(
					function (x, y) {
						return _Utils_Tuple2(x, y);
					}),
				A2(
					$elm$random$Random$int,
					0,
					$elm$core$Basics$round(model.width)),
				A2(
					$elm$random$Random$int,
					0,
					$elm$core$Basics$round(model.height))),
			model.seed);
		var pos = _v1.a;
		var seed = _v1.b;
		return A2($elm$core$Dict$member, pos, model.grid) ? _Utils_update(
			model,
			{seed: seed}) : _Utils_update(
			model,
			{
				grid: A3($elm$core$Dict$insert, pos, $elm$core$Maybe$Nothing, model.grid),
				player: pos,
				seed: seed
			});
	}
};
var $author$project$Position$neighbors = F2(
	function (args, pos) {
		return A2(
			$elm$core$List$filterMap,
			function (f) {
				var newPos = f(pos);
				return args.validator(newPos) ? $elm$core$Maybe$Just(newPos) : $elm$core$Maybe$Nothing;
			},
			args.directions);
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Main$validPositions = function (model) {
	var vecTo = F2(
		function (_v1, _v2) {
			var x1 = _v1.a;
			var y1 = _v1.b;
			var x2 = _v2.a;
			var y2 = _v2.b;
			return _Utils_Tuple2(x1 - x2, y1 - y2);
		});
	var inbounds = F2(
		function (v, maxV) {
			return (0 < v) && (_Utils_cmp(
				v,
				$elm$core$Basics$round(maxV)) < 0);
		});
	var straights = A2(
		$author$project$Position$neighbors,
		{
			directions: $author$project$Main$positionOps.directions,
			validator: function (_v0) {
				var x = _v0.a;
				var y = _v0.b;
				return A2(inbounds, x, model.width) && (A2(inbounds, y, model.height) && (!A2(
					$elm$core$Dict$member,
					_Utils_Tuple2(x, y),
					model.grid)));
			}
		},
		model.player);
	var from = A2(
		$elm$core$Maybe$withDefault,
		model.player,
		A2(
			$elm$core$Maybe$andThen,
			$elm$core$Maybe$map(
				function ($) {
					return $.from;
				}),
			A2($elm$core$Dict$get, model.player, model.grid)));
	return A2(
		$elm$core$List$map,
		function (pos) {
			return _Utils_Tuple2(
				_Utils_eq(
					A2(vecTo, pos, model.player),
					A2(vecTo, model.player, from)) ? 2 : 1,
				pos);
		},
		straights);
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$getByWeight = F3(
	function (_v0, others, countdown) {
		getByWeight:
		while (true) {
			var weight = _v0.a;
			var value = _v0.b;
			if (!others.b) {
				return value;
			} else {
				var second = others.a;
				var otherOthers = others.b;
				if (_Utils_cmp(
					countdown,
					$elm$core$Basics$abs(weight)) < 1) {
					return value;
				} else {
					var $temp$_v0 = second,
						$temp$others = otherOthers,
						$temp$countdown = countdown - $elm$core$Basics$abs(weight);
					_v0 = $temp$_v0;
					others = $temp$others;
					countdown = $temp$countdown;
					continue getByWeight;
				}
			}
		}
	});
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $elm$random$Random$weighted = F2(
	function (first, others) {
		var normalize = function (_v0) {
			var weight = _v0.a;
			return $elm$core$Basics$abs(weight);
		};
		var total = normalize(first) + $elm$core$List$sum(
			A2($elm$core$List$map, normalize, others));
		return A2(
			$elm$random$Random$map,
			A2($elm$random$Random$getByWeight, first, others),
			A2($elm$random$Random$float, 0, total));
	});
var $author$project$Main$updateFrame = function (model) {
	var moves = $author$project$Main$validPositions(model);
	if (!moves.b) {
		return $author$project$Main$moveBack(model);
	} else {
		var head = moves.a;
		var tail = moves.b;
		return (model.potential > 0) ? A2(
			$author$project$Main$applyMove,
			A2(
				$elm$random$Random$step,
				A2($elm$random$Random$weighted, head, tail),
				model.seed),
			model) : $author$project$Main$moveBack(
			_Utils_update(
				model,
				{
					potential: ($author$project$Main$maxPotential(
						{height: model.height, width: model.width}) / 2) | 0
				}));
	}
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'Frame':
				return _Utils_Tuple2(
					A3(
						$elm$core$List$foldl,
						function (_v1) {
							return $author$project$Main$updateFrame;
						},
						model,
						A2(
							$elm$core$List$repeat,
							$author$project$Main$stepSize(
								{height: model.height, width: model.width}),
							_Utils_Tuple0)),
					$elm$core$Platform$Cmd$none);
			case 'GotSeed':
				var seed = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{seed: seed}),
					$elm$core$Platform$Cmd$none);
			case 'Select':
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['image/png', 'image/bmp']),
						$author$project$Main$UploadedFile));
			case 'UploadedFile':
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$perform,
						$author$project$Main$GotBytes,
						$elm$file$File$toBytes(file)));
			default:
				var bytes = msg.a;
				var _v2 = $justgook$elm_image$Image$decode(bytes);
				if (_v2.$ === 'Just') {
					var image = _v2.a;
					var dim = $justgook$elm_image$Image$dimensions(image);
					var factor = $author$project$Main$size / A2($elm$core$Basics$max, dim.width, dim.height);
					var img = A2(
						$author$project$Main$resizeBy,
						factor,
						$author$project$Main$convertImage(image));
					var _v3 = _Utils_Tuple2(
						$elm$core$Basics$round(dim.width * factor),
						$elm$core$Basics$round(dim.height * factor));
					var width = _v3.a;
					var height = _v3.b;
					return _Utils_eq(img, _List_Nil) ? A2(
						$elm$core$Debug$log,
						'no transparency',
						_Utils_Tuple2(model, $elm$core$Platform$Cmd$none)) : _Utils_Tuple2(
						function (m) {
							return _Utils_update(
								m,
								{
									grid: A3($elm$core$Dict$insert, m.player, $elm$core$Maybe$Nothing, m.grid),
									running: true
								});
						}(
							$author$project$Main$new(
								{height: height, image: img, width: width})),
						$elm$core$Platform$Cmd$none);
				} else {
					return A2(
						$elm$core$Debug$log,
						'no image',
						_Utils_Tuple2(model, $elm$core$Platform$Cmd$none));
				}
		}
	});
var $author$project$Main$Select = {$: 'Select'};
var $noahzgordon$elm_color_extra$Color$Convert$labToXyz = function (_v0) {
	var l = _v0.l;
	var a = _v0.a;
	var b = _v0.b;
	var y = (l + 16) / 116;
	var c = function (ch) {
		var ch_ = (ch * ch) * ch;
		return (ch_ > 8.856e-3) ? ch_ : ((ch - (16 / 116)) / 7.787);
	};
	return {
		x: c(y + (a / 500)) * 95.047,
		y: c(y) * 100,
		z: c(y - (b / 200)) * 108.883
	};
};
var $elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var $avh4$elm_color$Color$rgb = F3(
	function (r, g, b) {
		return A4($avh4$elm_color$Color$RgbaSpace, r, g, b, 1.0);
	});
var $noahzgordon$elm_color_extra$Color$Convert$xyzToColor = function (_v0) {
	var x = _v0.x;
	var y = _v0.y;
	var z = _v0.z;
	var z_ = z / 100;
	var y_ = y / 100;
	var x_ = x / 100;
	var r = ((x_ * 3.2404542) + (y_ * (-1.5371385))) + (z_ * (-0.4986));
	var g = ((x_ * (-0.969266)) + (y_ * 1.8760108)) + (z_ * 4.1556e-2);
	var c = function (ch) {
		var ch_ = (ch > 3.1308e-3) ? ((1.055 * A2($elm$core$Basics$pow, ch, 1 / 2.4)) - 5.5e-2) : (12.92 * ch);
		return A3($elm$core$Basics$clamp, 0, 1, ch_);
	};
	var b = ((x_ * 5.56434e-2) + (y_ * (-0.2040259))) + (z_ * 1.0572252);
	return A3(
		$avh4$elm_color$Color$rgb,
		c(r),
		c(g),
		c(b));
};
var $noahzgordon$elm_color_extra$Color$Convert$labToColor = A2($elm$core$Basics$composeR, $noahzgordon$elm_color_extra$Color$Convert$labToXyz, $noahzgordon$elm_color_extra$Color$Convert$xyzToColor);
var $author$project$Main$adjustColor = F3(
	function (_v0, potential, color) {
		var width = _v0.width;
		var height = _v0.height;
		var _v1 = $noahzgordon$elm_color_extra$Color$Convert$colorToLab(color);
		var l = _v1.l;
		var a = _v1.a;
		var b = _v1.b;
		var _v2 = 1 - (potential / $author$project$Main$maxPotential(
			{height: height, width: width}));
		return $noahzgordon$elm_color_extra$Color$Convert$labToColor(
			{
				a: a,
				b: b,
				l: A2($elm$core$Basics$max, 0, (l * 90) / 100)
			});
	});
var $joakin$elm_canvas$Canvas$Internal$Canvas$Circle = F2(
	function (a, b) {
		return {$: 'Circle', a: a, b: b};
	});
var $joakin$elm_canvas$Canvas$circle = F2(
	function (pos, radius) {
		return A2($joakin$elm_canvas$Canvas$Internal$Canvas$Circle, pos, radius);
	});
var $joakin$elm_canvas$Canvas$Internal$Canvas$Fill = function (a) {
	return {$: 'Fill', a: a};
};
var $joakin$elm_canvas$Canvas$Internal$Canvas$SettingDrawOp = function (a) {
	return {$: 'SettingDrawOp', a: a};
};
var $joakin$elm_canvas$Canvas$Settings$fill = function (color) {
	return $joakin$elm_canvas$Canvas$Internal$Canvas$SettingDrawOp(
		$joakin$elm_canvas$Canvas$Internal$Canvas$Fill(color));
};
var $author$project$Main$zoom = 10;
var $author$project$Main$lineWidth = $author$project$Main$zoom / 4;
var $joakin$elm_canvas$Canvas$Internal$Canvas$DrawableShapes = function (a) {
	return {$: 'DrawableShapes', a: a};
};
var $joakin$elm_canvas$Canvas$Internal$Canvas$NotSpecified = {$: 'NotSpecified'};
var $joakin$elm_canvas$Canvas$Renderable = function (a) {
	return {$: 'Renderable', a: a};
};
var $joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke = F2(
	function (a, b) {
		return {$: 'FillAndStroke', a: a, b: b};
	});
var $joakin$elm_canvas$Canvas$Internal$Canvas$Stroke = function (a) {
	return {$: 'Stroke', a: a};
};
var $joakin$elm_canvas$Canvas$mergeDrawOp = F2(
	function (op1, op2) {
		var _v0 = _Utils_Tuple2(op1, op2);
		_v0$7:
		while (true) {
			switch (_v0.b.$) {
				case 'FillAndStroke':
					var _v1 = _v0.b;
					var c = _v1.a;
					var sc = _v1.b;
					return A2($joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c, sc);
				case 'Fill':
					switch (_v0.a.$) {
						case 'Fill':
							var c = _v0.b.a;
							return $joakin$elm_canvas$Canvas$Internal$Canvas$Fill(c);
						case 'Stroke':
							var c1 = _v0.a.a;
							var c2 = _v0.b.a;
							return A2($joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c2, c1);
						case 'FillAndStroke':
							var _v2 = _v0.a;
							var c = _v2.a;
							var sc = _v2.b;
							var c2 = _v0.b.a;
							return A2($joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c2, sc);
						default:
							break _v0$7;
					}
				case 'Stroke':
					switch (_v0.a.$) {
						case 'Stroke':
							var c = _v0.b.a;
							return $joakin$elm_canvas$Canvas$Internal$Canvas$Stroke(c);
						case 'Fill':
							var c1 = _v0.a.a;
							var c2 = _v0.b.a;
							return A2($joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c1, c2);
						case 'FillAndStroke':
							var _v3 = _v0.a;
							var c = _v3.a;
							var sc = _v3.b;
							var sc2 = _v0.b.a;
							return A2($joakin$elm_canvas$Canvas$Internal$Canvas$FillAndStroke, c, sc2);
						default:
							break _v0$7;
					}
				default:
					if (_v0.a.$ === 'NotSpecified') {
						break _v0$7;
					} else {
						var whatever = _v0.a;
						var _v5 = _v0.b;
						return whatever;
					}
			}
		}
		var _v4 = _v0.a;
		var whatever = _v0.b;
		return whatever;
	});
var $joakin$elm_canvas$Canvas$addSettingsToRenderable = F2(
	function (settings, renderable) {
		var addSetting = F2(
			function (setting, _v1) {
				var r = _v1.a;
				return $joakin$elm_canvas$Canvas$Renderable(
					function () {
						switch (setting.$) {
							case 'SettingCommand':
								var cmd = setting.a;
								return _Utils_update(
									r,
									{
										commands: A2($elm$core$List$cons, cmd, r.commands)
									});
							case 'SettingCommands':
								var cmds = setting.a;
								return _Utils_update(
									r,
									{
										commands: A3($elm$core$List$foldl, $elm$core$List$cons, r.commands, cmds)
									});
							case 'SettingUpdateDrawable':
								var f = setting.a;
								return _Utils_update(
									r,
									{
										drawable: f(r.drawable)
									});
							default:
								var op = setting.a;
								return _Utils_update(
									r,
									{
										drawOp: A2($joakin$elm_canvas$Canvas$mergeDrawOp, r.drawOp, op)
									});
						}
					}());
			});
		return A3($elm$core$List$foldl, addSetting, renderable, settings);
	});
var $joakin$elm_canvas$Canvas$shapes = F2(
	function (settings, ss) {
		return A2(
			$joakin$elm_canvas$Canvas$addSettingsToRenderable,
			settings,
			$joakin$elm_canvas$Canvas$Renderable(
				{
					commands: _List_Nil,
					drawOp: $joakin$elm_canvas$Canvas$Internal$Canvas$NotSpecified,
					drawable: $joakin$elm_canvas$Canvas$Internal$Canvas$DrawableShapes(ss)
				}));
	});
var $author$project$Main$bigCircle = F2(
	function (_v0, _v1) {
		var width = _v0.width;
		var height = _v0.height;
		var potential = _v0.potential;
		var color = _v0.color;
		var x = _v1.a;
		var y = _v1.b;
		var dimensions = {height: height, width: width};
		return A2(
			$joakin$elm_canvas$Canvas$shapes,
			_List_fromArray(
				[
					$joakin$elm_canvas$Canvas$Settings$fill(
					A3($author$project$Main$adjustColor, dimensions, potential, color))
				]),
			$elm$core$List$singleton(
				A2(
					$joakin$elm_canvas$Canvas$circle,
					_Utils_Tuple2(x, y),
					$author$project$Main$lineWidth * 2)));
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $joakin$elm_canvas$Canvas$Internal$Canvas$Rect = F3(
	function (a, b, c) {
		return {$: 'Rect', a: a, b: b, c: c};
	});
var $joakin$elm_canvas$Canvas$rect = F3(
	function (pos, width, height) {
		return A3($joakin$elm_canvas$Canvas$Internal$Canvas$Rect, pos, width, height);
	});
var $avh4$elm_color$Color$white = A4($avh4$elm_color$Color$RgbaSpace, 255 / 255, 255 / 255, 255 / 255, 1.0);
var $author$project$Main$clearScreen = function (_v0) {
	var width = _v0.width;
	var height = _v0.height;
	return A2(
		$joakin$elm_canvas$Canvas$shapes,
		_List_fromArray(
			[
				$joakin$elm_canvas$Canvas$Settings$fill($avh4$elm_color$Color$white)
			]),
		_List_fromArray(
			[
				A3(
				$joakin$elm_canvas$Canvas$rect,
				_Utils_Tuple2(0, 0),
				width * $author$project$Main$zoom,
				height * $author$project$Main$zoom)
			]));
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $elm$html$Html$canvas = _VirtualDom_node('canvas');
var $joakin$elm_canvas$Canvas$cnvs = A2($elm$html$Html$canvas, _List_Nil, _List_Nil);
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$commands = function (list) {
	return A2(
		$elm$html$Html$Attributes$property,
		'cmds',
		A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, list));
};
var $elm$html$Html$Attributes$height = function (n) {
	return A2(
		_VirtualDom_attribute,
		'height',
		$elm$core$String$fromInt(n));
};
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$empty = _List_Nil;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn = F2(
	function (name, args) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('function')),
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(name)),
					_Utils_Tuple2(
					'args',
					A2($elm$json$Json$Encode$list, $elm$core$Basics$identity, args))
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$beginPath = A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'beginPath', _List_Nil);
var $elm$json$Json$Encode$float = _Json_wrap;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$clearRect = F4(
	function (x, y, width, height) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'clearRect',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y),
					$elm$json$Json$Encode$float(width),
					$elm$json$Json$Encode$float(height)
				]));
	});
var $joakin$elm_canvas$Canvas$renderClear = F4(
	function (_v0, w, h, cmds) {
		var x = _v0.a;
		var y = _v0.b;
		return A2(
			$elm$core$List$cons,
			A4($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$clearRect, x, y, w, h),
			cmds);
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arc = F6(
	function (x, y, radius, startAngle, endAngle, anticlockwise) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'arc',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y),
					$elm$json$Json$Encode$float(radius),
					$elm$json$Json$Encode$float(startAngle),
					$elm$json$Json$Encode$float(endAngle),
					$elm$json$Json$Encode$bool(anticlockwise)
				]));
	});
var $elm$core$Basics$pi = _Basics_pi;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$circle = F3(
	function (x, y, r) {
		return A6($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arc, x, y, r, 0, 2 * $elm$core$Basics$pi, false);
	});
var $elm$core$Basics$cos = _Basics_cos;
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo = F2(
	function (x, y) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'moveTo',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y)
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$rect = F4(
	function (x, y, w, h) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'rect',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y),
					$elm$json$Json$Encode$float(w),
					$elm$json$Json$Encode$float(h)
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arcTo = F5(
	function (x1, y1, x2, y2, radius) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'arcTo',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(x1),
					$elm$json$Json$Encode$float(y1),
					$elm$json$Json$Encode$float(x2),
					$elm$json$Json$Encode$float(y2),
					$elm$json$Json$Encode$float(radius)
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$bezierCurveTo = F6(
	function (cp1x, cp1y, cp2x, cp2y, x, y) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'bezierCurveTo',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(cp1x),
					$elm$json$Json$Encode$float(cp1y),
					$elm$json$Json$Encode$float(cp2x),
					$elm$json$Json$Encode$float(cp2y),
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y)
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineTo = F2(
	function (x, y) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'lineTo',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y)
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$quadraticCurveTo = F4(
	function (cpx, cpy, x, y) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'quadraticCurveTo',
			_List_fromArray(
				[
					$elm$json$Json$Encode$float(cpx),
					$elm$json$Json$Encode$float(cpy),
					$elm$json$Json$Encode$float(x),
					$elm$json$Json$Encode$float(y)
				]));
	});
var $joakin$elm_canvas$Canvas$renderLineSegment = F2(
	function (segment, cmds) {
		switch (segment.$) {
			case 'ArcTo':
				var _v1 = segment.a;
				var x = _v1.a;
				var y = _v1.b;
				var _v2 = segment.b;
				var x2 = _v2.a;
				var y2 = _v2.b;
				var radius = segment.c;
				return A2(
					$elm$core$List$cons,
					A5($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arcTo, x, y, x2, y2, radius),
					cmds);
			case 'BezierCurveTo':
				var _v3 = segment.a;
				var cp1x = _v3.a;
				var cp1y = _v3.b;
				var _v4 = segment.b;
				var cp2x = _v4.a;
				var cp2y = _v4.b;
				var _v5 = segment.c;
				var x = _v5.a;
				var y = _v5.b;
				return A2(
					$elm$core$List$cons,
					A6($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$bezierCurveTo, cp1x, cp1y, cp2x, cp2y, x, y),
					cmds);
			case 'LineTo':
				var _v6 = segment.a;
				var x = _v6.a;
				var y = _v6.b;
				return A2(
					$elm$core$List$cons,
					A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineTo, x, y),
					cmds);
			case 'MoveTo':
				var _v7 = segment.a;
				var x = _v7.a;
				var y = _v7.b;
				return A2(
					$elm$core$List$cons,
					A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x, y),
					cmds);
			default:
				var _v8 = segment.a;
				var cpx = _v8.a;
				var cpy = _v8.b;
				var _v9 = segment.b;
				var x = _v9.a;
				var y = _v9.b;
				return A2(
					$elm$core$List$cons,
					A4($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$quadraticCurveTo, cpx, cpy, x, y),
					cmds);
		}
	});
var $elm$core$Basics$sin = _Basics_sin;
var $joakin$elm_canvas$Canvas$renderShape = F2(
	function (shape, cmds) {
		switch (shape.$) {
			case 'Rect':
				var _v1 = shape.a;
				var x = _v1.a;
				var y = _v1.b;
				var w = shape.b;
				var h = shape.c;
				return A2(
					$elm$core$List$cons,
					A4($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$rect, x, y, w, h),
					A2(
						$elm$core$List$cons,
						A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x, y),
						cmds));
			case 'Circle':
				var _v2 = shape.a;
				var x = _v2.a;
				var y = _v2.b;
				var r = shape.b;
				return A2(
					$elm$core$List$cons,
					A3($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$circle, x, y, r),
					A2(
						$elm$core$List$cons,
						A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x + r, y),
						cmds));
			case 'Path':
				var _v3 = shape.a;
				var x = _v3.a;
				var y = _v3.b;
				var segments = shape.b;
				return A3(
					$elm$core$List$foldl,
					$joakin$elm_canvas$Canvas$renderLineSegment,
					A2(
						$elm$core$List$cons,
						A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo, x, y),
						cmds),
					segments);
			default:
				var _v4 = shape.a;
				var x = _v4.a;
				var y = _v4.b;
				var radius = shape.b;
				var startAngle = shape.c;
				var endAngle = shape.d;
				var anticlockwise = shape.e;
				return A2(
					$elm$core$List$cons,
					A2(
						$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo,
						x + (radius * $elm$core$Basics$cos(endAngle)),
						y + (radius * $elm$core$Basics$sin(endAngle))),
					A2(
						$elm$core$List$cons,
						A6($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$arc, x, y, radius, startAngle, endAngle, anticlockwise),
						A2(
							$elm$core$List$cons,
							A2(
								$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$moveTo,
								x + (radius * $elm$core$Basics$cos(startAngle)),
								y + (radius * $elm$core$Basics$sin(startAngle))),
							cmds)));
		}
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$NonZero = {$: 'NonZero'};
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillRuleToString = function (fillRule) {
	if (fillRule.$ === 'NonZero') {
		return 'nonzero';
	} else {
		return 'evenodd';
	}
};
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fill = function (fillRule) {
	return A2(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
		'fill',
		_List_fromArray(
			[
				$elm$json$Json$Encode$string(
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillRuleToString(fillRule))
			]));
};
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field = F2(
	function (name, value) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('field')),
					_Utils_Tuple2(
					'name',
					$elm$json$Json$Encode$string(name)),
					_Utils_Tuple2('value', value)
				]));
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $avh4$elm_color$Color$toCssString = function (_v0) {
	var r = _v0.a;
	var g = _v0.b;
	var b = _v0.c;
	var a = _v0.d;
	var roundTo = function (x) {
		return $elm$core$Basics$round(x * 1000) / 1000;
	};
	var pct = function (x) {
		return $elm$core$Basics$round(x * 10000) / 100;
	};
	return $elm$core$String$concat(
		_List_fromArray(
			[
				'rgba(',
				$elm$core$String$fromFloat(
				pct(r)),
				'%,',
				$elm$core$String$fromFloat(
				pct(g)),
				'%,',
				$elm$core$String$fromFloat(
				pct(b)),
				'%,',
				$elm$core$String$fromFloat(
				roundTo(a)),
				')'
			]));
};
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillStyle = function (color) {
	return A2(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'fillStyle',
		$elm$json$Json$Encode$string(
			$avh4$elm_color$Color$toCssString(color)));
};
var $joakin$elm_canvas$Canvas$renderShapeFill = F2(
	function (c, cmds) {
		return A2(
			$elm$core$List$cons,
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fill($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$NonZero),
			A2(
				$elm$core$List$cons,
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillStyle(c),
				cmds));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$stroke = A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'stroke', _List_Nil);
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeStyle = function (color) {
	return A2(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'strokeStyle',
		$elm$json$Json$Encode$string(
			$avh4$elm_color$Color$toCssString(color)));
};
var $joakin$elm_canvas$Canvas$renderShapeStroke = F2(
	function (c, cmds) {
		return A2(
			$elm$core$List$cons,
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$stroke,
			A2(
				$elm$core$List$cons,
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeStyle(c),
				cmds));
	});
var $joakin$elm_canvas$Canvas$renderShapeDrawOp = F2(
	function (drawOp, cmds) {
		switch (drawOp.$) {
			case 'NotSpecified':
				return A2($joakin$elm_canvas$Canvas$renderShapeFill, $avh4$elm_color$Color$black, cmds);
			case 'Fill':
				var c = drawOp.a;
				return A2($joakin$elm_canvas$Canvas$renderShapeFill, c, cmds);
			case 'Stroke':
				var c = drawOp.a;
				return A2($joakin$elm_canvas$Canvas$renderShapeStroke, c, cmds);
			default:
				var fc = drawOp.a;
				var sc = drawOp.b;
				return A2(
					$joakin$elm_canvas$Canvas$renderShapeStroke,
					sc,
					A2($joakin$elm_canvas$Canvas$renderShapeFill, fc, cmds));
		}
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillText = F4(
	function (text, x, y, maybeMaxWidth) {
		if (maybeMaxWidth.$ === 'Nothing') {
			return A2(
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'fillText',
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(text),
						$elm$json$Json$Encode$float(x),
						$elm$json$Json$Encode$float(y)
					]));
		} else {
			var maxWidth = maybeMaxWidth.a;
			return A2(
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'fillText',
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(text),
						$elm$json$Json$Encode$float(x),
						$elm$json$Json$Encode$float(y),
						$elm$json$Json$Encode$float(maxWidth)
					]));
		}
	});
var $joakin$elm_canvas$Canvas$renderTextFill = F5(
	function (txt, x, y, color, cmds) {
		return A2(
			$elm$core$List$cons,
			A4($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillText, txt.text, x, y, txt.maxWidth),
			A2(
				$elm$core$List$cons,
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fillStyle(color),
				cmds));
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeText = F4(
	function (text, x, y, maybeMaxWidth) {
		if (maybeMaxWidth.$ === 'Nothing') {
			return A2(
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'strokeText',
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(text),
						$elm$json$Json$Encode$float(x),
						$elm$json$Json$Encode$float(y)
					]));
		} else {
			var maxWidth = maybeMaxWidth.a;
			return A2(
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
				'strokeText',
				_List_fromArray(
					[
						$elm$json$Json$Encode$string(text),
						$elm$json$Json$Encode$float(x),
						$elm$json$Json$Encode$float(y),
						$elm$json$Json$Encode$float(maxWidth)
					]));
		}
	});
var $joakin$elm_canvas$Canvas$renderTextStroke = F5(
	function (txt, x, y, color, cmds) {
		return A2(
			$elm$core$List$cons,
			A4($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeText, txt.text, x, y, txt.maxWidth),
			A2(
				$elm$core$List$cons,
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$strokeStyle(color),
				cmds));
	});
var $joakin$elm_canvas$Canvas$renderTextDrawOp = F3(
	function (drawOp, txt, cmds) {
		var _v0 = txt.point;
		var x = _v0.a;
		var y = _v0.b;
		switch (drawOp.$) {
			case 'NotSpecified':
				return A5($joakin$elm_canvas$Canvas$renderTextFill, txt, x, y, $avh4$elm_color$Color$black, cmds);
			case 'Fill':
				var c = drawOp.a;
				return A5($joakin$elm_canvas$Canvas$renderTextFill, txt, x, y, c, cmds);
			case 'Stroke':
				var c = drawOp.a;
				return A5($joakin$elm_canvas$Canvas$renderTextStroke, txt, x, y, c, cmds);
			default:
				var fc = drawOp.a;
				var sc = drawOp.b;
				return A5(
					$joakin$elm_canvas$Canvas$renderTextStroke,
					txt,
					x,
					y,
					sc,
					A5($joakin$elm_canvas$Canvas$renderTextFill, txt, x, y, fc, cmds));
		}
	});
var $joakin$elm_canvas$Canvas$renderText = F3(
	function (drawOp, txt, cmds) {
		return A3($joakin$elm_canvas$Canvas$renderTextDrawOp, drawOp, txt, cmds);
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$drawImage = F9(
	function (sx, sy, sw, sh, dx, dy, dw, dh, imageObj) {
		return A2(
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn,
			'drawImage',
			_List_fromArray(
				[
					imageObj,
					$elm$json$Json$Encode$float(sx),
					$elm$json$Json$Encode$float(sy),
					$elm$json$Json$Encode$float(sw),
					$elm$json$Json$Encode$float(sh),
					$elm$json$Json$Encode$float(dx),
					$elm$json$Json$Encode$float(dy),
					$elm$json$Json$Encode$float(dw),
					$elm$json$Json$Encode$float(dh)
				]));
	});
var $joakin$elm_canvas$Canvas$Internal$Texture$drawTexture = F4(
	function (x, y, t, cmds) {
		return A2(
			$elm$core$List$cons,
			function () {
				if (t.$ === 'TImage') {
					var image = t.a;
					return A9($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$drawImage, 0, 0, image.width, image.height, x, y, image.width, image.height, image.json);
				} else {
					var sprite = t.a;
					var image = t.b;
					return A9($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$drawImage, sprite.x, sprite.y, sprite.width, sprite.height, x, y, sprite.width, sprite.height, image.json);
				}
			}(),
			cmds);
	});
var $joakin$elm_canvas$Canvas$renderTexture = F3(
	function (_v0, t, cmds) {
		var x = _v0.a;
		var y = _v0.b;
		return A4($joakin$elm_canvas$Canvas$Internal$Texture$drawTexture, x, y, t, cmds);
	});
var $joakin$elm_canvas$Canvas$renderDrawable = F3(
	function (drawable, drawOp, cmds) {
		switch (drawable.$) {
			case 'DrawableText':
				var txt = drawable.a;
				return A3($joakin$elm_canvas$Canvas$renderText, drawOp, txt, cmds);
			case 'DrawableShapes':
				var ss = drawable.a;
				return A2(
					$joakin$elm_canvas$Canvas$renderShapeDrawOp,
					drawOp,
					A3(
						$elm$core$List$foldl,
						$joakin$elm_canvas$Canvas$renderShape,
						A2($elm$core$List$cons, $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$beginPath, cmds),
						ss));
			case 'DrawableTexture':
				var p = drawable.a;
				var t = drawable.b;
				return A3($joakin$elm_canvas$Canvas$renderTexture, p, t, cmds);
			default:
				var p = drawable.a;
				var w = drawable.b;
				var h = drawable.c;
				return A4($joakin$elm_canvas$Canvas$renderClear, p, w, h, cmds);
		}
	});
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$restore = A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'restore', _List_Nil);
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$save = A2($joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$fn, 'save', _List_Nil);
var $joakin$elm_canvas$Canvas$renderOne = F2(
	function (_v0, cmds) {
		var data = _v0.a;
		var commands = data.commands;
		var drawable = data.drawable;
		var drawOp = data.drawOp;
		return A2(
			$elm$core$List$cons,
			$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$restore,
			A3(
				$joakin$elm_canvas$Canvas$renderDrawable,
				drawable,
				drawOp,
				_Utils_ap(
					commands,
					A2($elm$core$List$cons, $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$save, cmds))));
	});
var $joakin$elm_canvas$Canvas$render = function (entities) {
	return A3($elm$core$List$foldl, $joakin$elm_canvas$Canvas$renderOne, $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$empty, entities);
};
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $joakin$elm_canvas$Canvas$Internal$Texture$TImage = function (a) {
	return {$: 'TImage', a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $joakin$elm_canvas$Canvas$Internal$Texture$decodeTextureImage = A2(
	$elm$json$Json$Decode$andThen,
	function (image) {
		return A4(
			$elm$json$Json$Decode$map3,
			F3(
				function (tagName, width, height) {
					return (tagName === 'IMG') ? $elm$core$Maybe$Just(
						$joakin$elm_canvas$Canvas$Internal$Texture$TImage(
							{height: height, json: image, width: width})) : $elm$core$Maybe$Nothing;
				}),
			A2($elm$json$Json$Decode$field, 'tagName', $elm$json$Json$Decode$string),
			A2($elm$json$Json$Decode$field, 'width', $elm$json$Json$Decode$float),
			A2($elm$json$Json$Decode$field, 'height', $elm$json$Json$Decode$float));
	},
	$elm$json$Json$Decode$value);
var $joakin$elm_canvas$Canvas$Internal$Texture$decodeImageLoadEvent = A2($elm$json$Json$Decode$field, 'target', $joakin$elm_canvas$Canvas$Internal$Texture$decodeTextureImage);
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $joakin$elm_canvas$Canvas$renderTextureSource = function (textureSource) {
	var url = textureSource.a;
	var onLoad = textureSource.b;
	return _Utils_Tuple2(
		url,
		A2(
			$elm$html$Html$img,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$src(url),
					A2($elm$html$Html$Attributes$attribute, 'crossorigin', 'anonymous'),
					A2($elm$html$Html$Attributes$style, 'display', 'none'),
					A2(
					$elm$html$Html$Events$on,
					'load',
					A2($elm$json$Json$Decode$map, onLoad, $joakin$elm_canvas$Canvas$Internal$Texture$decodeImageLoadEvent)),
					A2(
					$elm$html$Html$Events$on,
					'error',
					$elm$json$Json$Decode$succeed(
						onLoad($elm$core$Maybe$Nothing)))
				]),
			_List_Nil));
};
var $elm$html$Html$Attributes$width = function (n) {
	return A2(
		_VirtualDom_attribute,
		'width',
		$elm$core$String$fromInt(n));
};
var $joakin$elm_canvas$Canvas$toHtmlWith = F3(
	function (options, attrs, entities) {
		return A3(
			$elm$html$Html$Keyed$node,
			'elm-canvas',
			A2(
				$elm$core$List$cons,
				$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$commands(
					$joakin$elm_canvas$Canvas$render(entities)),
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$height(options.height),
					A2(
						$elm$core$List$cons,
						$elm$html$Html$Attributes$width(options.width),
						attrs))),
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2('__canvas', $joakin$elm_canvas$Canvas$cnvs),
				A2($elm$core$List$map, $joakin$elm_canvas$Canvas$renderTextureSource, options.textures)));
	});
var $joakin$elm_canvas$Canvas$toHtml = F3(
	function (_v0, attrs, entities) {
		var w = _v0.a;
		var h = _v0.b;
		return A3(
			$joakin$elm_canvas$Canvas$toHtmlWith,
			{height: h, textures: _List_Nil, width: w},
			attrs,
			entities);
	});
var $author$project$Main$pointToPixel = F2(
	function (args, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2((args.width / 2) + (x * $author$project$Main$zoom), (args.height / 2) + (y * $author$project$Main$zoom));
	});
var $author$project$Main$circle = F2(
	function (_v0, p) {
		var width = _v0.width;
		var height = _v0.height;
		var potential = _v0.potential;
		var color = _v0.color;
		var dimensions = {height: height, width: width};
		return A2(
			$joakin$elm_canvas$Canvas$shapes,
			_List_fromArray(
				[
					$joakin$elm_canvas$Canvas$Settings$fill(
					A3($author$project$Main$adjustColor, dimensions, potential, color))
				]),
			$elm$core$List$singleton(
				A2(
					$joakin$elm_canvas$Canvas$circle,
					A2($author$project$Main$pointToPixel, dimensions, p),
					$author$project$Main$lineWidth * 1.5)));
	});
var $joakin$elm_canvas$Canvas$Settings$Line$RoundCap = {$: 'RoundCap'};
var $joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommand = function (a) {
	return {$: 'SettingCommand', a: a};
};
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineCap = function (cap) {
	return A2(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'lineCap',
		$elm$json$Json$Encode$string(cap));
};
var $joakin$elm_canvas$Canvas$Settings$Line$lineCapToString = function (cap) {
	switch (cap.$) {
		case 'ButtCap':
			return 'butt';
		case 'RoundCap':
			return 'round';
		default:
			return 'square';
	}
};
var $joakin$elm_canvas$Canvas$Settings$Line$lineCap = function (cap) {
	return $joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommand(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineCap(
			$joakin$elm_canvas$Canvas$Settings$Line$lineCapToString(cap)));
};
var $joakin$elm_canvas$Canvas$Internal$Canvas$LineTo = function (a) {
	return {$: 'LineTo', a: a};
};
var $joakin$elm_canvas$Canvas$lineTo = function (point) {
	return $joakin$elm_canvas$Canvas$Internal$Canvas$LineTo(point);
};
var $joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineWidth = function (value) {
	return A2(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$field,
		'lineWidth',
		$elm$json$Json$Encode$float(value));
};
var $joakin$elm_canvas$Canvas$Settings$Line$lineWidth = function (width) {
	return $joakin$elm_canvas$Canvas$Internal$Canvas$SettingCommand(
		$joakin$elm_canvas$Canvas$Internal$CustomElementJsonApi$lineWidth(width));
};
var $joakin$elm_canvas$Canvas$Internal$Canvas$Path = F2(
	function (a, b) {
		return {$: 'Path', a: a, b: b};
	});
var $joakin$elm_canvas$Canvas$path = F2(
	function (startingPoint, segments) {
		return A2($joakin$elm_canvas$Canvas$Internal$Canvas$Path, startingPoint, segments);
	});
var $joakin$elm_canvas$Canvas$Settings$stroke = function (color) {
	return $joakin$elm_canvas$Canvas$Internal$Canvas$SettingDrawOp(
		$joakin$elm_canvas$Canvas$Internal$Canvas$Stroke(color));
};
var $author$project$Main$line = F3(
	function (_v0, p1, p2) {
		var width = _v0.width;
		var height = _v0.height;
		var potential = _v0.potential;
		var color = _v0.color;
		var dimensions = {height: height, width: width};
		return A2(
			$joakin$elm_canvas$Canvas$shapes,
			_List_fromArray(
				[
					$joakin$elm_canvas$Canvas$Settings$stroke(
					A3($author$project$Main$adjustColor, dimensions, potential, color)),
					$joakin$elm_canvas$Canvas$Settings$Line$lineWidth($author$project$Main$lineWidth),
					$joakin$elm_canvas$Canvas$Settings$Line$lineCap($joakin$elm_canvas$Canvas$Settings$Line$RoundCap)
				]),
			$elm$core$List$singleton(
				A2(
					$joakin$elm_canvas$Canvas$path,
					A2($author$project$Main$pointToPixel, dimensions, p1),
					_List_fromArray(
						[
							$joakin$elm_canvas$Canvas$lineTo(
							A2($author$project$Main$pointToPixel, dimensions, p2))
						]))));
	});
var $author$project$Main$viewCell = F2(
	function (_v0, _v1) {
		var width = _v0.width;
		var height = _v0.height;
		var pos = _v1.a;
		var cell = _v1.b;
		return A2(
			$elm$core$List$cons,
			A3(
				$author$project$Main$line,
				{color: cell.color, height: height, potential: cell.potential, width: width},
				pos,
				$author$project$Main$positionOps.toPoint(cell.from)),
			_Utils_eq(cell.to, _List_Nil) ? $elm$core$List$singleton(
				A2(
					$author$project$Main$circle,
					{color: cell.color, height: height, potential: cell.potential, width: width},
					pos)) : A2(
				$elm$core$List$map,
				A2(
					$author$project$Main$line,
					{color: cell.color, height: height, potential: cell.potential, width: width},
					pos),
				A2($elm$core$List$map, $author$project$Main$positionOps.toPoint, cell.to)));
	});
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'display', 'flex'),
				A2($elm$html$Html$Attributes$style, 'justify-content', 'center'),
				A2($elm$html$Html$Attributes$style, 'align-items', 'center')
			]),
		_List_fromArray(
			[
				A3(
				$joakin$elm_canvas$Canvas$toHtml,
				_Utils_Tuple2(
					$elm$core$Basics$round(model.width * $author$project$Main$zoom),
					$elm$core$Basics$round(model.height * $author$project$Main$zoom)),
				_List_Nil,
				_Utils_ap(
					_List_fromArray(
						[
							$author$project$Main$clearScreen(
							{height: model.height, width: model.width}),
							A2(
							$author$project$Main$bigCircle,
							{color: $avh4$elm_color$Color$black, height: model.height, potential: model.potential, width: model.width},
							$author$project$Main$positionOps.toPoint(model.player))
						]),
					A2(
						$elm$core$List$concatMap,
						$author$project$Main$viewCell(
							{height: model.height, width: model.width}),
						A2(
							$elm$core$List$filterMap,
							function (_v0) {
								var pos = _v0.a;
								var maybe = _v0.b;
								return A2(
									$elm$core$Maybe$map,
									function (cell) {
										return _Utils_Tuple2(
											$author$project$Main$positionOps.toPoint(pos),
											cell);
									},
									maybe);
							},
							$elm$core$Dict$toList(model.grid))))),
				A2(
				$elm$html$Html$button,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick($author$project$Main$Select)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Select Image')
					]))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));